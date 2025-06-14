use std::collections::HashMap;

use crate::{
    ir::tree_ir::{self, TreeIR},
    lexer::Unop,
    parser::{Ident, elaborator::Binop},
};

pub type BlockIR = Vec<BasicBlock>;

pub type BasicBlock = Vec<Statement>;

pub type Label = usize;

#[derive(Debug)]
pub enum Statement {
    Assign(Ident, Value),
    AssignBinop(Ident, Value, Binop, Value),
    AssignUnop(Ident, Unop, Value),
    CondJump(Value, Label),
    Jump(Label),
    Return(Value),
}

#[derive(Debug, Clone)]
pub enum Value {
    True,
    False,
    IntConst(i32),
    Var(Ident),
}

pub fn tree_ir_to_block_ir(ir: TreeIR) -> BlockIR {
    TranslationContext::new().translate_tree_ir(ir)
}

struct TranslationContext {
    temp_counter: i64,
    label_mapping: HashMap<usize, usize>,
}

impl TranslationContext {
    fn new() -> Self {
        Self {
            temp_counter: 0,
            label_mapping: HashMap::new(),
        }
    }

    fn next_temp(&mut self) -> Ident {
        let t = self.temp_counter;
        self.temp_counter += 1;
        format!("b_{t}")
    }

    fn translate_pure_expression(&mut self, exp: tree_ir::PureExpression) -> (BasicBlock, Value) {
        match exp {
            tree_ir::PureExpression::True => (vec![], Value::True),
            tree_ir::PureExpression::False => (vec![], Value::False),
            tree_ir::PureExpression::IntConst(c) => (vec![], Value::IntConst(c)),
            tree_ir::PureExpression::Var(v) => (vec![], Value::Var(v)),
            tree_ir::PureExpression::BinOp(exp1, pure_binop, exp2) => {
                let (mut b1, v1) = self.translate_pure_expression(*exp1);
                let (mut b2, v2) = self.translate_pure_expression(*exp2);
                let t = self.next_temp();
                b1.append(&mut b2);
                b1.push(Statement::AssignBinop(t.clone(), v1, pure_binop.into(), v2));
                (b1, Value::Var(t))
            }
            tree_ir::PureExpression::Unop(unop, exp) => {
                let (mut b, v) = self.translate_pure_expression(*exp);
                let t = self.next_temp();
                b.push(Statement::AssignUnop(t.clone(), unop, v));
                (b, Value::Var(t))
            }
        }
    }

    fn translate_tree_ir(&mut self, ir: tree_ir::TreeIR) -> BlockIR {
        let mut blocks = vec![vec![]];
        fn next_block(blocks: &mut Vec<Vec<Statement>>) {
            if !blocks.last().unwrap().is_empty() {
                blocks.push(vec![]);
            }
        }

        for cmd in ir {
            match cmd {
                tree_ir::Command::Assign(var, exp) => {
                    let (mut b, v) = self.translate_pure_expression(exp);
                    blocks.last_mut().unwrap().append(&mut b);
                    blocks.last_mut().unwrap().push(Statement::Assign(var, v));
                }
                tree_ir::Command::AssignBinop(var, exp1, cmd_binop, exp2) => {
                    let (mut b1, v1) = self.translate_pure_expression(exp1);
                    let (mut b2, v2) = self.translate_pure_expression(exp2);
                    blocks.last_mut().unwrap().append(&mut b1);
                    blocks.last_mut().unwrap().append(&mut b2);
                    blocks.last_mut().unwrap().push(Statement::AssignBinop(
                        var,
                        v1,
                        cmd_binop.into(),
                        v2,
                    ));
                }
                tree_ir::Command::CondJump(exp, target) => {
                    let (mut b, v) = self.translate_pure_expression(exp);
                    blocks.last_mut().unwrap().append(&mut b);
                    blocks
                        .last_mut()
                        .unwrap()
                        .push(Statement::CondJump(v, target as usize));
                    next_block(&mut blocks);
                }
                tree_ir::Command::Jump(target) => {
                    blocks
                        .last_mut()
                        .unwrap()
                        .push(Statement::Jump(target as usize));
                    next_block(&mut blocks);
                }
                tree_ir::Command::Label(l) => {
                    next_block(&mut blocks);
                    self.label_mapping.insert(l as usize, blocks.len() - 1);
                }
                tree_ir::Command::Return(exp) => {
                    let (mut b, v) = self.translate_pure_expression(exp);
                    blocks.last_mut().unwrap().append(&mut b);
                    blocks.last_mut().unwrap().push(Statement::Return(v));
                    next_block(&mut blocks);
                }
            }
        }
        for block in &mut blocks {
            for stmt in block {
                if let Statement::CondJump(v, target) = stmt {
                    *stmt = Statement::CondJump(v.to_owned(), self.label_mapping[target]);
                } else if let Statement::Jump(target) = stmt {
                    *stmt = Statement::Jump(self.label_mapping[target]);
                }
            }
        }
        blocks
    }
}
