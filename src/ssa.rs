use crate::{
    ir::block_ir::{BlockIR, Statement, Value},
    ssa::initial::Block,
};

mod initial;
mod liveness;
mod unreachable;

pub fn ssa(ir: BlockIR) -> Vec<Block> {
    initial::ssaify(unreachable::remove_unreachable(ir))
    // TODO minimize SSA
}

pub fn resolve_phis(mut ir: Vec<Block>) -> BlockIR {
    let mut blocks = vec![];
    blocks.resize_with(ir.len(), || vec![]);
    let gen_moves = |ir: &Vec<Block>, idx: usize, target: usize| {
        ir[target]
            .parameters
            .iter()
            .map(|(var, ver_var)| {
                Statement::Assign(
                    ver_var.to_owned(),
                    Value::Var(ir[idx].end.get(var).unwrap().to_owned()),
                )
            })
            .collect::<Vec<_>>()
    };
    for idx in 0..ir.len() {
        let last = ir[idx].block.pop().unwrap();
        if let Statement::Jump(target) = last {
            let moves = gen_moves(&ir, idx, target);
            ir[idx].block.extend(moves);
            ir[idx].block.push(Statement::Jump(target));
        } else if let Statement::Return(val) = last {
            ir[idx].block.push(Statement::Return(val));
        } else {
            if let Statement::CondJump(val, target) = last {
                let mut new_block = vec![];
                new_block.extend(gen_moves(&ir, idx, target));
                new_block.push(Statement::Jump(target));
                ir[idx].block.push(Statement::CondJump(val, blocks.len()));
                blocks.push(new_block);
            } else {
                ir[idx].block.push(last);
            }
            let moves = gen_moves(&ir, idx, idx + 1);
            ir[idx].block.extend(moves);
        }
        blocks[idx].append(&mut ir[idx].block);
    }
    blocks
}
