use std::collections::HashMap;

use crate::{
    ir::block_ir::{BasicBlock, BlockIR, Statement, Value},
    parser::Ident,
    ssa::liveness::calc_liveness,
};

#[derive(Debug)]
pub struct Block {
    pub parameters: HashMap<Ident, Ident>,
    pub block: BasicBlock,
    pub end: HashMap<Ident, Ident>,
}

fn calc_name(v: &Ident, map: &HashMap<Ident, u64>) -> Ident {
    format!("{}_{v}", map.get(v).unwrap())
}

pub fn ssaify(ir: BlockIR) -> Vec<Block> {
    let mut version_map = HashMap::new();
    let liveness = calc_liveness(&ir);
    ir.into_iter()
        .enumerate()
        .map(|(idx, mut block)| {
            let parameters = liveness[idx]
                .iter()
                .map(|v| (v.to_owned(), next_var_ver(v, &mut version_map)))
                .collect();
            ssaify_basic_block(&mut block, &mut version_map);
            let end = version_map
                .keys()
                .cloned()
                .map(|k| {
                    let new = calc_name(&k, &version_map);
                    (k, new)
                })
                .collect();
            Block {
                parameters,
                block,
                end,
            }
        })
        .collect()
}

fn transform_val(val: &Value, version_map: &mut HashMap<Ident, u64>) -> Value {
    match val {
        Value::True => Value::True,
        Value::False => Value::False,
        Value::IntConst(c) => Value::IntConst(*c),
        Value::Var(v) => Value::Var(calc_name(v, version_map)),
    }
}

fn next_var_ver(var: &Ident, version_map: &mut HashMap<Ident, u64>) -> Ident {
    version_map
        .entry(var.to_owned())
        .and_modify(|ver| *ver += 1)
        .or_insert(0);
    calc_name(var, version_map)
}

fn ssaify_basic_block(block: &mut BasicBlock, version_map: &mut HashMap<Ident, u64>) {
    for stmt in block {
        *stmt = match stmt {
            Statement::Assign(var, val) => {
                let val = transform_val(val, version_map);
                Statement::Assign(next_var_ver(var, version_map), val)
            }
            Statement::AssignBinop(var, val1, binop, val2) => {
                let val1 = transform_val(val1, version_map);
                let val2 = transform_val(val2, version_map);
                Statement::AssignBinop(next_var_ver(var, version_map), val1, binop.to_owned(), val2)
            }
            Statement::AssignUnop(var, unop, val) => {
                let val = transform_val(val, version_map);
                Statement::AssignUnop(next_var_ver(var, version_map), unop.to_owned(), val)
            }
            Statement::CondJump(val, target) => {
                Statement::CondJump(transform_val(val, version_map), *target)
            }
            Statement::Jump(target) => Statement::Jump(*target),
            Statement::Return(val) => Statement::Return(transform_val(val, version_map)),
        };
    }
}
