use crate::{
    lexer::Unop,
    parser::{
        Ident,
        elaborator::{Binop, EAST, Expression},
    },
};

#[derive(Clone, Debug)]
pub enum PureExpression {
    True,
    False,
    IntConst(i32),
    Var(Ident),
    BinOp(Box<PureExpression>, PureBinop, Box<PureExpression>),
    Unop(Unop, Box<PureExpression>),
}

#[derive(Clone, Debug)]
pub enum PureBinop {
    Less,
    Leq,
    Greater,
    Geq,
    Eq,
    Neq,
    Add,
    Minus,
    Mul,
    BitAnd,
    BitXor,
    BitOr,
    ShiftL,
    ShiftR,
}

type Label = i64;

#[derive(Clone, Debug)]
pub enum Command {
    Assign(Ident, PureExpression),
    AssignBinop(Ident, PureExpression, CommandBinop, PureExpression),
    CondJump(PureExpression, Label),
    Jump(Label),
    Label(Label),
    Return(PureExpression),
}

#[derive(Clone, Debug)]
pub enum CommandBinop {
    Div,
    Mod,
}

pub type TreeIR = Vec<Command>;

pub fn translate_tree_ir(east: EAST) -> TreeIR {
    TranslationContext::new().translate_statement(east, -1, -1)
}

struct TranslationContext {
    pub temp_counter: i64,
    pub label_counter: i64,
}

enum ParsedBinop {
    Pure(PureBinop),
    Command(CommandBinop),
}

fn transform_binop(op: &Binop) -> ParsedBinop {
    match op {
        Binop::Less => ParsedBinop::Pure(PureBinop::Less),
        Binop::Leq => ParsedBinop::Pure(PureBinop::Leq),
        Binop::Greater => ParsedBinop::Pure(PureBinop::Greater),
        Binop::Geq => ParsedBinop::Pure(PureBinop::Geq),
        Binop::Eq => ParsedBinop::Pure(PureBinop::Eq),
        Binop::Neq => ParsedBinop::Pure(PureBinop::Neq),
        Binop::Add => ParsedBinop::Pure(PureBinop::Add),
        Binop::Minus => ParsedBinop::Pure(PureBinop::Minus),
        Binop::Mul => ParsedBinop::Pure(PureBinop::Mul),
        Binop::Div => ParsedBinop::Command(CommandBinop::Div),
        Binop::Mod => ParsedBinop::Command(CommandBinop::Mod),
        Binop::BitAnd => ParsedBinop::Pure(PureBinop::BitAnd),
        Binop::BitXor => ParsedBinop::Pure(PureBinop::BitXor),
        Binop::BitOr => ParsedBinop::Pure(PureBinop::BitOr),
        Binop::ShiftL => ParsedBinop::Pure(PureBinop::ShiftL),
        Binop::ShiftR => ParsedBinop::Pure(PureBinop::ShiftR),
    }
}

impl TranslationContext {
    fn new() -> Self {
        Self {
            temp_counter: 0,
            label_counter: 0,
        }
    }

    fn next_temp(&mut self) -> Ident {
        let t = self.temp_counter;
        self.temp_counter += 1;
        format!("t_{t}")
    }

    fn next_label(&mut self) -> Label {
        let l = self.label_counter;
        self.label_counter += 1;
        l
    }

    fn translate_expression(&mut self, exp: &Expression) -> (Vec<Command>, PureExpression) {
        match exp {
            Expression::True => (vec![], PureExpression::True),
            Expression::False => (vec![], PureExpression::False),
            Expression::Var(v) => (vec![], PureExpression::Var(format!("o_{v}"))),
            Expression::Const(c) => (vec![], PureExpression::IntConst(*c)),
            Expression::Binop(exp1, binop, exp2) => match transform_binop(binop) {
                ParsedBinop::Pure(op) => {
                    let (mut c1, p1) = self.translate_expression(exp1);
                    let (mut c2, p2) = self.translate_expression(exp2);
                    c1.append(&mut c2);
                    (c1, PureExpression::BinOp(Box::new(p1), op, Box::new(p2)))
                }
                ParsedBinop::Command(op) => {
                    let (mut c1, p1) = self.translate_expression(exp1);
                    let (mut c2, p2) = self.translate_expression(exp2);
                    c1.append(&mut c2);
                    let t = self.next_temp();
                    c1.push(Command::AssignBinop(t.clone(), p1, op, p2));
                    (c1, PureExpression::Var(t))
                }
            },
            Expression::Unop(unop, exp) => {
                let (c, p) = self.translate_expression(exp);
                (c, PureExpression::Unop(unop.to_owned(), Box::new(p)))
            }
            Expression::Ternary(cond, then_block, else_block) => {
                let (c_cond, p_cond) = self.translate_expression(cond);
                let (mut c_then, p_then) = self.translate_expression(then_block);
                let (mut c_else, p_else) = self.translate_expression(else_block);
                let t = self.next_temp();
                let then_label = self.next_label();
                let after_label = self.next_label();
                let mut c = c_cond;
                c.push(Command::CondJump(p_cond, then_label));
                c.append(&mut c_else);
                c.push(Command::Assign(t.clone(), p_else));
                c.push(Command::Jump(after_label));
                c.push(Command::Label(then_label));
                c.append(&mut c_then);
                c.push(Command::Assign(t.clone(), p_then));
                c.push(Command::Label(after_label));
                (c, PureExpression::Var(t))
            }
        }
    }

    fn translate_statement(
        &mut self,
        stmt: EAST,
        cont_label: Label,
        break_label: Label,
    ) -> Vec<Command> {
        match stmt {
            EAST::Declare(_, _, stmt) => self.translate_statement(*stmt, cont_label, break_label),
            EAST::Assign(var, exp) => {
                let (mut c, p) = self.translate_expression(&exp);
                c.push(Command::Assign(var, p));
                c
            }
            EAST::If(cond, then_body, else_body) => {
                let (mut c, p_cond) = self.translate_expression(&cond);
                let mut c_then = self.translate_statement(*then_body, cont_label, break_label);
                let mut c_else = self.translate_statement(*else_body, cont_label, break_label);
                let then_label = self.next_label();
                let after_label = self.next_label();
                c.push(Command::CondJump(p_cond, then_label));
                c.append(&mut c_else);
                c.push(Command::Jump(after_label));
                c.push(Command::Label(then_label));
                c.append(&mut c_then);
                c.push(Command::Label(after_label));
                c
            }
            EAST::While(cond, body) => {
                let (mut c_cond, p_cond) = self.translate_expression(&cond);
                let body_label = self.next_label();
                let cond_label = self.next_label();
                let after_label = self.next_label();
                let mut c_body = self.translate_statement(*body, cond_label, after_label);
                let mut c = c_cond.clone();
                c.push(Command::CondJump(
                    PureExpression::Unop(Unop::LogNot, Box::new(p_cond.clone())),
                    after_label,
                ));
                c.push(Command::Label(body_label));
                c.append(&mut c_body);
                c.push(Command::Label(cond_label));
                c.append(&mut c_cond);
                c.push(Command::CondJump(p_cond, body_label));
                c.push(Command::Label(after_label));
                c
            }
            EAST::For(init, cond, step, body) => {
                let (mut c_cond, p_cond) = self.translate_expression(&cond);
                let c_init = self.translate_statement(*init, cont_label, break_label);
                let mut c_step = self.translate_statement(*step, cont_label, break_label);
                let body_label = self.next_label();
                let cond_label = self.next_label();
                let after_label = self.next_label();
                let mut c_body = self.translate_statement(*body, cond_label, after_label);
                let mut c = c_init;
                c.append(&mut c_cond.clone());
                c.push(Command::CondJump(
                    PureExpression::Unop(Unop::LogNot, Box::new(p_cond.clone())),
                    after_label,
                ));
                c.push(Command::Label(body_label));
                c.append(&mut c_body);
                c.push(Command::Label(cond_label));
                c.append(&mut c_step);
                c.append(&mut c_cond);
                c.push(Command::CondJump(p_cond, body_label));
                c.push(Command::Label(after_label));
                c
            }
            EAST::Break => vec![Command::Jump(break_label)],
            EAST::Continue => vec![Command::Jump(cont_label)],
            EAST::Return(exp) => {
                let (mut c, p) = self.translate_expression(&exp);
                c.push(Command::Return(p));
                c
            }
            EAST::Seq(stmts) => stmts
                .into_iter()
                .flat_map(|stmt| self.translate_statement(stmt, cont_label, break_label))
                .collect(),
            EAST::Nop => vec![],
        }
    }
}
