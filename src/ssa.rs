use std::collections::VecDeque;

use crate::ir::block_ir::{BlockIR, Statement};

mod liveness;

pub fn ssa(ir: BlockIR) -> BlockIR {
    remove_unreachable(ir)
}

fn remove_unreachable(ir: BlockIR) -> BlockIR {
    let mut reachable = vec![false; ir.len()];
    let mut q = VecDeque::from([0]);
    while let Some(idx) = q.pop_front() {
        if reachable[idx] {
            continue;
        }
        reachable[idx] = true;
        match ir[idx].last() {
            Some(Statement::Jump(t)) => q.push_back(*t),
            Some(Statement::CondJump(_, t)) => {
                q.push_back(*t);
                q.push_back(idx + 1);
            }
            Some(Statement::Return(_)) => (),
            _ => q.push_back(idx + 1),
        }
    }
    let offset = reachable
        .iter()
        .scan(0, |so_far, reachable| {
            if !*reachable {
                *so_far += 1
            }
            Some(*so_far)
        })
        .collect::<Vec<_>>();
    let mut ir = ir
        .into_iter()
        .enumerate()
        .filter(|(idx, _)| reachable[*idx])
        .map(|(_, v)| v)
        .collect::<Vec<_>>();
    for block in &mut ir {
        let stmt = block.last_mut().unwrap();
        if let Statement::CondJump(v, target) = stmt {
            *stmt = Statement::CondJump(v.to_owned(), *target - offset[*target]);
        } else if let Statement::Jump(target) = stmt {
            *stmt = Statement::Jump(*target - offset[*target]);
        }
    }
    ir
}
