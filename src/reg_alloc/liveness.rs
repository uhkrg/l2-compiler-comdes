use std::collections::HashSet;

use crate::{
    ir::block_ir::{BasicBlock, BlockIR, Value},
    parser::Ident,
    ssa::liveness::calc_liveness,
};

pub fn liveness_after(ir: &BlockIR) -> Vec<Vec<HashSet<Ident>>> {
    let block_liveness = calc_liveness(&ir);
    ir.iter()
        .enumerate()
        .map(|(idx, b)| {
            liveness_after_basic_block(
                b,
                match b.last() {
                    Some(crate::ir::block_ir::Statement::CondJump(_, t)) => block_liveness
                        .get(*t)
                        .unwrap()
                        .union(block_liveness.get(idx + 1).unwrap())
                        .cloned()
                        .collect(),
                    Some(crate::ir::block_ir::Statement::Jump(t)) => {
                        block_liveness.get(*t).unwrap().clone()
                    }
                    Some(crate::ir::block_ir::Statement::Return(_)) => HashSet::new(),
                    _ => block_liveness.get(idx + 1).unwrap().clone(),
                },
            )
        })
        .collect()
}

fn liveness_after_basic_block(block: &BasicBlock, mut live: HashSet<Ident>) -> Vec<HashSet<Ident>> {
    let add_generated = |live: &mut HashSet<Ident>, v: &Value| match v {
        Value::True => (),
        Value::False => (),
        Value::IntConst(_) => (),
        Value::Var(v) => {
            live.insert(v.to_owned());
        }
    };
    let mut livenesses = vec![];
    for stmt in block.iter().rev() {
        match stmt {
            crate::ir::block_ir::Statement::Assign(var, value) => {
                live.remove(var);
                add_generated(&mut live, value);
            }
            crate::ir::block_ir::Statement::AssignBinop(var, val1, _, val2) => {
                live.remove(var);
                add_generated(&mut live, val1);
                add_generated(&mut live, val2);
            }
            crate::ir::block_ir::Statement::AssignUnop(var, _, val) => {
                live.remove(var);
                add_generated(&mut live, val);
            }
            crate::ir::block_ir::Statement::CondJump(value, _) => add_generated(&mut live, value),
            crate::ir::block_ir::Statement::Jump(_) => (),
            crate::ir::block_ir::Statement::Return(value) => add_generated(&mut live, value),
        }
        livenesses.push(live.clone());
    }
    livenesses.reverse();
    livenesses
}
