pub mod elaborator;

use crate::{
    errors::ParserError,
    lexer::{self},
};

#[derive(Debug)]
pub struct AST {
    program: Block,
}

pub type Block = Vec<Statement>;
pub type Ident = String;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Type {
    Int,
    Bool,
}

#[derive(Debug, Clone)]
pub enum SimpStatement {
    Assign(Ident, Option<lexer::Binop>, Expression),
    Declaration(Type, Ident),
    Initialisation(Type, Ident, Expression),
}

#[derive(Debug, Clone)]
pub enum Statement {
    Simp(SimpStatement),
    Block(Block),
    If(Expression, Box<Statement>),
    IfElse(Expression, Box<Statement>, Box<Statement>),
    While(Expression, Box<Statement>),
    For(
        Option<SimpStatement>,
        Expression,
        Option<SimpStatement>,
        Box<Statement>,
    ),
    Continue,
    Break,
    Return(Expression),
}

#[derive(Debug, Clone)]
pub enum Expression {
    True,
    False,
    Var(Ident),
    Const(i32),
    Binop(Box<Expression>, lexer::Binop, Box<Expression>),
    Unop(lexer::Unop, Box<Expression>),
    Ternary(Box<Expression>, Box<Expression>, Box<Expression>),
}

pub fn parse(input: Vec<lexer::LexToken>) -> Result<AST, ParserError> {
    Parser::new(input).parse()
}

struct Parser {
    input_tokens: Vec<lexer::LexToken>,
    position: usize,
}

fn debug_str<T: std::fmt::Debug>(val: T) -> String {
    format!("{val:?}")
}

impl Parser {
    fn new(input_tokens: Vec<lexer::LexToken>) -> Self {
        Self {
            input_tokens,
            position: 0,
        }
    }

    fn parse(&mut self) -> Result<AST, ParserError> {
        self.parse_program()
    }

    fn current(&self) -> Option<&lexer::LexToken> {
        self.input_tokens.get(self.position)
    }

    fn next(&mut self) -> Option<&lexer::LexToken> {
        self.position += 1;
        self.current()
    }

    fn expect(&mut self, exp_token: lexer::LexToken) -> Result<(), ParserError> {
        let cur = self.current();

        if cur.is_none() {
            return Err(ParserError::EOF(debug_str(exp_token)));
        }

        let cur = cur.unwrap();

        if cur != &exp_token {
            return Err(ParserError::Mismatch {
                found: debug_str(cur),
                expected: debug_str(exp_token),
            });
        }

        self.next();
        Ok(())
    }

    fn parse_program(&mut self) -> Result<AST, ParserError> {
        self.expect(lexer::LexToken::Reserved(String::from("int")))?;
        self.expect(lexer::LexToken::Ident(String::from("main")))?;
        self.expect(lexer::LexToken::ParenOpen)?;
        self.expect(lexer::LexToken::ParenClose)?;

        Ok(AST {
            program: self.parse_block()?,
        })
    }

    fn parse_block(&mut self) -> Result<Block, ParserError> {
        let mut stmts = vec![];

        self.expect(lexer::LexToken::BraceOpen)?;
        while let Some(stmt) = self.parse_statement()? {
            stmts.push(stmt);
        }
        self.expect(lexer::LexToken::BraceClose)?;

        Ok(stmts)
    }

    fn parse_statement(&mut self) -> Result<Option<Statement>, ParserError> {
        match self.current() {
            Some(lexer::LexToken::Ident(_)) => {
                self.parse_simp().map(Statement::Simp).map(Option::Some)
            }
            Some(lexer::LexToken::ParenOpen) => {
                self.parse_simp().map(Statement::Simp).map(Option::Some)
            }
            Some(lexer::LexToken::Reserved(word)) => match word.as_str() {
                "int" => self.parse_simp().map(Statement::Simp).map(Option::Some),
                "bool" => self.parse_simp().map(Statement::Simp).map(Option::Some),
                "if" => self.parse_if().map(Option::Some),
                "while" => self.parse_while().map(Option::Some),
                "for" => self.parse_for().map(Option::Some),
                "continue" => {
                    self.next();
                    self.expect(lexer::LexToken::Semicolon)?;
                    Ok(Some(Statement::Continue))
                }
                "break" => {
                    self.next();
                    self.expect(lexer::LexToken::Semicolon)?;
                    Ok(Some(Statement::Break))
                }
                "return" => self.parse_return().map(Option::Some),
                word => Err(ParserError::Mismatch {
                    found: word.to_owned(),
                    expected: "statement".to_owned(),
                }),
            },
            Some(lexer::LexToken::BraceOpen) => self
                .parse_block()
                .map(|block| Some(Statement::Block(block))),
            _ => Ok(None),
        }
    }

    fn expect_statement(&mut self) -> Result<Statement, ParserError> {
        let maybe_stmt = self.parse_statement()?;
        if maybe_stmt.is_some() {
            return Ok(maybe_stmt.unwrap());
        }

        match self.current() {
            None => Err(ParserError::EOF("statement".to_string())),
            Some(token) => Err(ParserError::Mismatch {
                found: debug_str(token),
                expected: "statement".to_string(),
            }),
        }
    }

    fn parse_simp_wo_semi(&mut self) -> Result<SimpStatement, ParserError> {
        match self.current() {
            Some(lexer::LexToken::Ident(_)) => self.parse_assignment(),
            Some(lexer::LexToken::ParenOpen) => self.parse_assignment(),
            Some(lexer::LexToken::Reserved(word)) => match word.as_str() {
                "int" => self.parse_definition(),
                "bool" => self.parse_definition(),
                _ => Err(ParserError::Mismatch {
                    found: word.to_owned(),
                    expected: "identifier, int, bool or open paren".to_string(),
                }),
            },
            Some(token) => Err(ParserError::Mismatch {
                found: debug_str(token),
                expected: "identifier, int, bool or open paren".to_string(),
            }),
            None => Err(ParserError::EOF(
                "identifier, int, bool or open paren".to_string(),
            )),
        }
    }

    fn parse_simp(&mut self) -> Result<SimpStatement, ParserError> {
        let res = self.parse_simp_wo_semi()?;

        self.expect(lexer::LexToken::Semicolon)?;

        Ok(res)
    }

    fn parse_assignment(&mut self) -> Result<SimpStatement, ParserError> {
        let identifier = self.parse_lvalue()?;

        let as_op = match self.current() {
            None => return Err(ParserError::EOF(String::from("assignment operator"))),
            Some(lexer::LexToken::Asnop(op)) => op.clone(),
            Some(token) => {
                return Err(ParserError::Mismatch {
                    found: debug_str(token),
                    expected: String::from("assignment operator"),
                });
            }
        };
        self.next();

        let exp = self.parse_expression()?;

        Ok(SimpStatement::Assign(identifier, as_op, exp))
    }

    fn parse_lvalue(&mut self) -> Result<Ident, ParserError> {
        match self.current() {
            Some(lexer::LexToken::Ident(ident)) => {
                let ident = ident.to_owned();
                self.next();
                Ok(ident)
            }
            Some(lexer::LexToken::ParenOpen) => {
                self.next();
                let res = self.parse_lvalue()?;
                self.expect(lexer::LexToken::ParenClose)?;
                Ok(res)
            }
            Some(token) => Err(ParserError::Mismatch {
                found: debug_str(token),
                expected: "identifier (possibly in parens)".to_owned(),
            }),
            None => Err(ParserError::EOF(
                "identifier (possibly in parens)".to_owned(),
            )),
        }
    }

    fn parse_definition(&mut self) -> Result<SimpStatement, ParserError> {
        let typ = match self.current() {
            Some(lexer::LexToken::Reserved(word)) => {
                if word == "int" {
                    Type::Int
                } else if word == "bool" {
                    Type::Bool
                } else {
                    return Err(ParserError::Mismatch {
                        found: word.clone(),
                        expected: "type, i.e. int or bool".to_string(),
                    });
                }
            }
            Some(token) => {
                return Err(ParserError::Mismatch {
                    found: debug_str(token),
                    expected: "type, i.e. int or bool".to_string(),
                });
            }
            None => return Err(ParserError::EOF("type, i.e. int or bool".to_string())),
        };

        let ident = match self.next() {
            Some(lexer::LexToken::Ident(id)) => id.to_owned(),
            Some(token) => {
                return Err(ParserError::Mismatch {
                    found: debug_str(token),
                    expected: "identifier".to_string(),
                });
            }
            None => return Err(ParserError::EOF("identifier".to_string())),
        };

        if let Some(lexer::LexToken::Asnop(None)) = self.next() {
            self.next();
            let exp = self.parse_expression()?;

            return Ok(SimpStatement::Initialisation(typ, ident, exp));
        }

        Ok(SimpStatement::Declaration(typ, ident))
    }

    fn parse_if(&mut self) -> Result<Statement, ParserError> {
        self.expect(lexer::LexToken::Reserved("if".to_string()))?;
        self.expect(lexer::LexToken::ParenOpen)?;
        let cond = self.parse_expression()?;
        self.expect(lexer::LexToken::ParenClose)?;
        let then_block = self.expect_statement()?;

        if self
            .current()
            .is_some_and(|cur| cur == &lexer::LexToken::Reserved("else".to_string()))
        {
            self.next();
            let else_block = self.expect_statement()?;

            return Ok(Statement::IfElse(
                cond,
                Box::new(then_block),
                Box::new(else_block),
            ));
        }

        Ok(Statement::If(cond, Box::new(then_block)))
    }

    fn parse_while(&mut self) -> Result<Statement, ParserError> {
        self.expect(lexer::LexToken::Reserved("while".to_string()))?;
        self.expect(lexer::LexToken::ParenOpen)?;
        let cond = self.parse_expression()?;
        self.expect(lexer::LexToken::ParenClose)?;
        let body = self.expect_statement()?;

        Ok(Statement::While(cond, Box::new(body)))
    }

    fn parse_for(&mut self) -> Result<Statement, ParserError> {
        self.expect(lexer::LexToken::Reserved("for".to_string()))?;
        self.expect(lexer::LexToken::ParenOpen)?;
        let init = if self.current() == Some(&lexer::LexToken::Semicolon) {
            self.next();
            None
        } else {
            Some(self.parse_simp()?)
        };
        let cond = self.parse_expression()?;
        self.expect(lexer::LexToken::Semicolon)?;
        let step = if self.current() == Some(&lexer::LexToken::ParenClose) {
            None
        } else {
            Some(self.parse_simp_wo_semi()?)
        };
        self.expect(lexer::LexToken::ParenClose)?;
        let body = self.expect_statement()?;

        Ok(Statement::For(init, cond, step, Box::new(body)))
    }

    fn parse_return(&mut self) -> Result<Statement, ParserError> {
        self.expect(lexer::LexToken::Reserved("return".to_string()))?;
        let ret = self.parse_expression()?;
        self.expect(lexer::LexToken::Semicolon)?;

        Ok(Statement::Return(ret))
    }

    fn parse_expression(&mut self) -> Result<Expression, ParserError> {
        self.parse_exp0()
    }

    fn parse_exp0(&mut self) -> Result<Expression, ParserError> {
        let fst = self.parse_exp1()?;

        if let Some(lexer::LexToken::QuestionMark) = self.current() {
            self.next();
            let true_branch = self.parse_exp0()?;
            self.expect(lexer::LexToken::Colon)?;
            let false_branch = self.parse_exp0()?;

            return Ok(Expression::Ternary(
                Box::new(fst),
                Box::new(true_branch),
                Box::new(false_branch),
            ));
        }

        Ok(fst)
    }

    fn parse_exp1(&mut self) -> Result<Expression, ParserError> {
        let mut fst = self.parse_exp2()?;

        while let Some(lexer::LexToken::Binop(lexer::Binop::LogOr)) = self.current() {
            self.next();
            let other = self.parse_exp2()?;

            fst = Expression::Binop(Box::new(fst), lexer::Binop::LogOr, Box::new(other));
        }

        Ok(fst)
    }

    fn parse_exp2(&mut self) -> Result<Expression, ParserError> {
        let mut fst = self.parse_exp3()?;

        while let Some(lexer::LexToken::Binop(lexer::Binop::LogAnd)) = self.current() {
            self.next();
            let other = self.parse_exp3()?;

            fst = Expression::Binop(Box::new(fst), lexer::Binop::LogAnd, Box::new(other));
        }

        Ok(fst)
    }

    fn parse_exp3(&mut self) -> Result<Expression, ParserError> {
        let mut fst = self.parse_exp4()?;

        while let Some(lexer::LexToken::Binop(lexer::Binop::BitOr)) = self.current() {
            self.next();
            let other = self.parse_exp4()?;

            fst = Expression::Binop(Box::new(fst), lexer::Binop::BitOr, Box::new(other));
        }

        Ok(fst)
    }

    fn parse_exp4(&mut self) -> Result<Expression, ParserError> {
        let mut fst = self.parse_exp5()?;

        while let Some(lexer::LexToken::Binop(lexer::Binop::BitXor)) = self.current() {
            self.next();
            let other = self.parse_exp5()?;

            fst = Expression::Binop(Box::new(fst), lexer::Binop::BitXor, Box::new(other));
        }

        Ok(fst)
    }

    fn parse_exp5(&mut self) -> Result<Expression, ParserError> {
        let mut fst = self.parse_exp6()?;

        while let Some(lexer::LexToken::Binop(lexer::Binop::BitAnd)) = self.current() {
            self.next();
            let other = self.parse_exp6()?;

            fst = Expression::Binop(Box::new(fst), lexer::Binop::BitAnd, Box::new(other));
        }

        Ok(fst)
    }

    fn parse_exp6(&mut self) -> Result<Expression, ParserError> {
        let mut fst = self.parse_exp7()?;

        while let Some(lexer::LexToken::Binop(op)) = self.current() {
            let op = op.clone();
            if op == lexer::Binop::Eq || op == lexer::Binop::Neq {
                self.next();
                let other = self.parse_exp7()?;

                fst = Expression::Binop(Box::new(fst), op, Box::new(other));
            } else {
                break;
            }
        }

        Ok(fst)
    }

    fn parse_exp7(&mut self) -> Result<Expression, ParserError> {
        let mut fst = self.parse_exp8()?;

        while let Some(lexer::LexToken::Binop(op)) = self.current() {
            let op = op.clone();
            if op == lexer::Binop::Less
                || op == lexer::Binop::Leq
                || op == lexer::Binop::Greater
                || op == lexer::Binop::Geq
            {
                self.next();
                let other = self.parse_exp8()?;

                fst = Expression::Binop(Box::new(fst), op, Box::new(other));
            } else {
                break;
            }
        }

        Ok(fst)
    }

    fn parse_exp8(&mut self) -> Result<Expression, ParserError> {
        let mut fst = self.parse_exp9()?;

        while let Some(lexer::LexToken::Binop(op)) = self.current() {
            let op = op.clone();
            if op == lexer::Binop::ShiftL || op == lexer::Binop::ShiftR {
                self.next();
                let other = self.parse_exp9()?;

                fst = Expression::Binop(Box::new(fst), op, Box::new(other));
            } else {
                break;
            }
        }

        Ok(fst)
    }

    fn parse_exp9(&mut self) -> Result<Expression, ParserError> {
        let mut fst = self.parse_exp10()?;

        loop {
            if let Some(lexer::LexToken::Binop(lexer::Binop::Add)) = self.current() {
                self.next();
                let other = self.parse_exp10()?;

                fst = Expression::Binop(Box::new(fst), lexer::Binop::Add, Box::new(other));
            } else if let Some(lexer::LexToken::Minus) = self.current() {
                self.next();
                let other = self.parse_exp10()?;

                fst = Expression::Binop(Box::new(fst), lexer::Binop::Minus, Box::new(other));
            } else {
                break;
            }
        }

        Ok(fst)
    }

    fn parse_exp10(&mut self) -> Result<Expression, ParserError> {
        let mut fst = self.parse_exp11()?;

        while let Some(lexer::LexToken::Binop(op)) = self.current() {
            let op = op.clone();
            if op == lexer::Binop::Mul || op == lexer::Binop::Div || op == lexer::Binop::Mod {
                self.next();
                let other = self.parse_exp11()?;

                fst = Expression::Binop(Box::new(fst), op, Box::new(other));
            } else {
                break;
            }
        }

        Ok(fst)
    }

    fn parse_exp11(&mut self) -> Result<Expression, ParserError> {
        if let Some(lexer::LexToken::Unop(op)) = self.current() {
            let op = op.clone();
            if op == lexer::Unop::LogNot || op == lexer::Unop::BitNot {
                self.next();
                let arg = self.parse_exp11()?;

                return Ok(Expression::Unop(op, Box::new(arg)));
            }
        }
        if let Some(lexer::LexToken::Minus) = self.current() {
            self.next();
            let arg = self.parse_exp11()?;

            return Ok(Expression::Unop(lexer::Unop::Neg, Box::new(arg)));
        }

        self.parse_exp12()
    }

    fn parse_exp12(&mut self) -> Result<Expression, ParserError> {
        match self.current() {
            None => Err(ParserError::EOF("expression".to_string())),
            Some(lexer::LexToken::Reserved(word)) => {
                if word == "true" {
                    self.next();
                    return Ok(Expression::True);
                }
                if word == "false" {
                    self.next();
                    return Ok(Expression::False);
                }
                Err(ParserError::Mismatch {
                    found: word.to_owned(),
                    expected: "expression".to_string(),
                })
            }
            Some(lexer::LexToken::Ident(ident)) => {
                let ident = ident.to_owned();
                self.next();
                Ok(Expression::Var(ident))
            }
            Some(&lexer::LexToken::IntConst(c)) => {
                self.next();
                Ok(Expression::Const(c))
            }
            Some(lexer::LexToken::ParenOpen) => {
                self.next();
                let exp = self.parse_expression()?;
                self.expect(lexer::LexToken::ParenClose)?;
                Ok(exp)
            }
            Some(token) => Err(ParserError::Mismatch {
                found: debug_str(token),
                expected: "expression".to_owned(),
            }),
        }
    }
}
