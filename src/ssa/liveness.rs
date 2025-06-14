use std::collections::{HashSet, VecDeque};

use crate::{
    ir::block_ir::{BasicBlock, BlockIR, Statement, Value},
    parser::Ident,
};

pub fn calc_liveness(ir: &BlockIR) -> Vec<HashSet<Ident>> {
    let (mut generates, kills) = ir
        .iter()
        .map(generates_kills)
        .collect::<(Vec<HashSet<Ident>>, Vec<HashSet<Ident>>)>();
    let trav_order = traversal_order(ir);
    let follows = ir
        .iter()
        .enumerate()
        .map(|(i, b)| following(i, b))
        .collect::<Vec<_>>();
    let mut changed = true;
    while changed {
        changed = false;
        for &idx in &trav_order {
            let from_follows = follows[idx]
                .iter()
                .map(|&i| &generates[i])
                .fold(HashSet::new(), |mut acc, next| {
                    acc.extend(next.iter().cloned());
                    acc
                })
                .difference(&kills[idx])
                .cloned()
                .collect::<HashSet<_>>()
                .difference(&generates[idx])
                .cloned()
                .collect::<HashSet<_>>();
            if !from_follows.is_empty() {
                changed = true;
                generates[idx].extend(from_follows);
            }
        }
    }
    generates
}

fn generates_kills(block: &BasicBlock) -> (HashSet<Ident>, HashSet<Ident>) {
    let mut generated = HashSet::new();
    let mut killed = HashSet::new();
    let mut add_generated = |killed: &HashSet<Ident>, v: &Value| match v {
        Value::True => (),
        Value::False => (),
        Value::IntConst(_) => (),
        Value::Var(v) => {
            if !killed.contains(v) {
                generated.insert(v.to_owned());
            }
        }
    };
    for stmt in block {
        match stmt {
            crate::ir::block_ir::Statement::Assign(var, value) => {
                add_generated(&killed, value);
                killed.insert(var.to_string());
            }
            crate::ir::block_ir::Statement::AssignBinop(var, val1, _, val2) => {
                add_generated(&killed, val1);
                add_generated(&killed, val2);
                killed.insert(var.to_string());
            }
            crate::ir::block_ir::Statement::AssignUnop(var, _, val) => {
                add_generated(&killed, val);
                killed.insert(var.to_string());
            }
            crate::ir::block_ir::Statement::CondJump(value, _) => add_generated(&killed, value),
            crate::ir::block_ir::Statement::Jump(_) => (),
            crate::ir::block_ir::Statement::Return(value) => add_generated(&killed, value),
        }
    }
    (generated, killed)
}

fn traversal_order(ir: &BlockIR) -> Vec<usize> {
    let block_count = ir.len();
    let mut visited_b = vec![false; block_count];
    let mut visited = vec![];
    let mut q = VecDeque::from([0]);
    while let Some(idx) = q.pop_front() {
        if visited_b[idx] {
            continue;
        }
        visited_b[idx] = true;
        visited.push(idx);
        q.extend(following(idx, &ir[idx]));
    }
    visited.reverse();
    visited
}

fn following(idx: usize, block: &BasicBlock) -> Vec<usize> {
    match block.last() {
        Some(Statement::Jump(t)) => vec![*t],
        Some(Statement::CondJump(_, t)) => {
            vec![*t, idx + 1]
        }
        Some(Statement::Return(_)) => vec![],
        _ => vec![idx + 1],
    }
}
