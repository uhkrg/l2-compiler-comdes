use crate::{lexer, parser::Type};

use super::{AST, Ident, SimpStatement, Statement};

#[derive(Debug)]
pub enum EAST {
    Assign(Ident, Expression),
    While(Expression, Box<EAST>),
    Continue,
    Return(Expression),
    Declare(Ident, Type, Box<EAST>),
    If(Expression, Box<EAST>, Box<EAST>),
    For(Box<EAST>, Expression, Box<EAST>, Box<EAST>),
    Break,
    Seq(Vec<EAST>),
    Nop,
}

#[derive(Debug)]
pub enum Expression {
    True,
    False,
    Var(Ident),
    Const(i32),
    Binop(Box<Expression>, Binop, Box<Expression>),
    Unop(lexer::Unop, Box<Expression>),
    Ternary(Box<Expression>, Box<Expression>, Box<Expression>),
}

#[derive(Debug)]
pub enum Binop {
    Less,
    Leq,
    Greater,
    Geq,
    Eq,
    Neq,
    Add,
    Minus,
    Mul,
    Div,
    Mod,
    BitAnd,
    BitXor,
    BitOr,
    ShiftL,
    ShiftR,
}

pub fn elaborate(input: AST) -> EAST {
    elaborate_block(&input.program)
}

fn elaborate_block(input: &[Statement]) -> EAST {
    let mut stmts = vec![];

    let mut idx = 0;
    while idx < input.len() {
        match &input[idx] {
            Statement::Block(block) => match elaborate_block(&block) {
                EAST::Seq(mut stms) => stmts.append(&mut stms),
                EAST::Nop => {}
                stmt => stmts.push(stmt),
            },
            Statement::If(exp, stmt) => stmts.push(EAST::If(
                elaborate_exp(exp),
                Box::new(elaborate_block(&[*stmt.to_owned()])),
                Box::new(EAST::Nop),
            )),
            Statement::IfElse(exp, then_body, else_body) => stmts.push(EAST::If(
                elaborate_exp(exp),
                Box::new(elaborate_block(&[*then_body.to_owned()])),
                Box::new(elaborate_block(&[*else_body.to_owned()])),
            )),
            Statement::While(exp, stmt) => stmts.push(EAST::While(
                elaborate_exp(exp),
                Box::new(elaborate_block(&[*stmt.to_owned()])),
            )),
            Statement::For(init, cond, step, stmt) => stmts.push(EAST::For(
                Box::new(
                    init.as_ref()
                        .map(|stmt| elaborate_block(&[super::Statement::Simp(stmt.clone())]))
                        .unwrap_or(EAST::Nop),
                ),
                elaborate_exp(cond),
                Box::new(
                    step.as_ref()
                        .map(|stmt| elaborate_block(&[super::Statement::Simp(stmt.clone())]))
                        .unwrap_or(EAST::Nop),
                ),
                Box::new(elaborate_block(&[*stmt.to_owned()])),
            )),
            Statement::Break => stmts.push(EAST::Break),
            Statement::Continue => stmts.push(EAST::Continue),
            Statement::Return(exp) => stmts.push(EAST::Return(elaborate_exp(exp))),
            Statement::Simp(SimpStatement::Assign(var, None, exp)) => {
                stmts.push(EAST::Assign(var.to_owned(), elaborate_exp(exp)))
            }
            Statement::Simp(SimpStatement::Assign(var, Some(op), exp)) => stmts.push(EAST::Assign(
                var.clone(),
                elaborate_exp(&super::Expression::Binop(
                    Box::new(super::Expression::Var(var.to_owned())),
                    op.clone(),
                    Box::new(exp.clone()),
                )),
            )),
            Statement::Simp(SimpStatement::Declaration(typ, var)) => {
                stmts.push(EAST::Declare(
                    var.to_owned(),
                    typ.clone(),
                    Box::new(elaborate_block(&input[idx + 1..])),
                ));
                break;
            }
            Statement::Simp(SimpStatement::Initialisation(typ, var, exp)) => {
                let mut remaining = vec![elaborate_block(&[Statement::Simp(
                    SimpStatement::Assign(var.clone(), None, exp.clone()),
                )])];
                match elaborate_block(&input[idx + 1..]) {
                    EAST::Seq(mut stms) => remaining.append(&mut stms),
                    EAST::Nop => {
                        stmts.push(EAST::Declare(
                            var.to_owned(),
                            typ.clone(),
                            Box::new(remaining.remove(0)),
                        ));
                        break;
                    }
                    stmt => remaining.push(stmt),
                }

                stmts.push(EAST::Declare(
                    var.to_owned(),
                    typ.clone(),
                    Box::new(EAST::Seq(remaining)),
                ));
                break;
            }
        }

        idx += 1;
    }

    if stmts.len() == 0 {
        EAST::Nop
    } else if stmts.len() == 1 {
        stmts.remove(0)
    } else {
        EAST::Seq(stmts)
    }
}

fn elaborate_exp(exp: &super::Expression) -> Expression {
    match exp {
        super::Expression::True => Expression::True,
        super::Expression::False => Expression::False,
        super::Expression::Var(var) => Expression::Var(var.clone()),
        &super::Expression::Const(c) => Expression::Const(c),
        super::Expression::Binop(left, op, right) => elaborate_exp_binop(
            Box::new(elaborate_exp(&*left)),
            op,
            Box::new(elaborate_exp(&*right)),
        ),
        super::Expression::Unop(unop, expression) => {
            Expression::Unop(unop.clone(), Box::new(elaborate_exp(&*expression)))
        }
        super::Expression::Ternary(cond, true_, false_) => Expression::Ternary(
            Box::new(elaborate_exp(&*cond)),
            Box::new(elaborate_exp(&*true_)),
            Box::new(elaborate_exp(&*false_)),
        ),
    }
}

fn elaborate_exp_binop(
    left: Box<Expression>,
    op: &lexer::Binop,
    right: Box<Expression>,
) -> Expression {
    match op {
        lexer::Binop::Less => Expression::Binop(left, Binop::Less, right),
        lexer::Binop::Leq => Expression::Binop(left, Binop::Leq, right),
        lexer::Binop::Greater => Expression::Binop(left, Binop::Greater, right),
        lexer::Binop::Geq => Expression::Binop(left, Binop::Geq, right),
        lexer::Binop::Eq => Expression::Binop(left, Binop::Eq, right),
        lexer::Binop::Neq => Expression::Binop(left, Binop::Neq, right),
        lexer::Binop::Add => Expression::Binop(left, Binop::Add, right),
        lexer::Binop::Minus => Expression::Binop(left, Binop::Minus, right),
        lexer::Binop::Mul => Expression::Binop(left, Binop::Mul, right),
        lexer::Binop::Div => Expression::Binop(left, Binop::Div, right),
        lexer::Binop::Mod => Expression::Binop(left, Binop::Mod, right),
        lexer::Binop::BitAnd => Expression::Binop(left, Binop::BitAnd, right),
        lexer::Binop::BitXor => Expression::Binop(left, Binop::BitXor, right),
        lexer::Binop::BitOr => Expression::Binop(left, Binop::BitOr, right),
        lexer::Binop::ShiftL => Expression::Binop(left, Binop::ShiftL, right),
        lexer::Binop::ShiftR => Expression::Binop(left, Binop::ShiftR, right),
        lexer::Binop::LogAnd => Expression::Ternary(left, right, Box::new(Expression::False)),
        lexer::Binop::LogOr => Expression::Ternary(left, Box::new(Expression::True), right),
    }
}
