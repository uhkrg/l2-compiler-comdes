use crate::{ir::block_ir::BlockIR, ssa::initial::Block};

mod initial;
mod liveness;
mod unreachable;

pub fn ssa(ir: BlockIR) -> Vec<Block> {
    initial::ssaify(unreachable::remove_unreachable(ir))
}
