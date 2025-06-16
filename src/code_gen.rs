use std::{collections::HashMap, fmt::Display};

use crate::{
    ir::block_ir::{BlockIR, Statement, Value},
    lexer::Unop,
    parser::elaborator::Binop,
    reg_alloc::Location,
};

const TEMPLATE: &'static str = ".global _start\n.global main\n.text\n_start:\ncall main\nmov %rax, %rdi\nmov $0x3C, %rax\nsyscall\nmain:\n";

pub fn asm(ir: BlockIR, regs: &HashMap<String, Location>) -> String {
    let mut code = String::from(TEMPLATE);
    for (block_num, block) in ir.into_iter().enumerate() {
        code.push_str(&format!("l{block_num}:\n"));
        block
            .into_iter()
            .for_each(|stmt| code.push_str(&asm_stmt(stmt, regs)));
    }
    code
}

fn asm_stmt(stmt: Statement, regs: &HashMap<String, Location>) -> String {
    match stmt {
        Statement::Assign(var, Value::Var(val)) => {
            let loc_var = var_to_loc(&var, regs);
            let loc_val = var_to_loc(&val, regs);
            if loc_val == loc_var {
                String::new()
            } else if loc_val.is_reg() || loc_var.is_reg() {
                format!("mov {loc_val}, {loc_var}\n")
            } else {
                format!("mov {loc_val}, %eax\nmov %eax, {loc_var}\n")
            }
        }
        Statement::Assign(var, val) => format!("movl {val}, {}\n", var_to_loc(&var, regs)),
        Statement::AssignBinop(var, val1, binop, val2) => {
            asm_assign_binop(var, val1, binop, val2, regs)
        }
        Statement::AssignUnop(var, unop, Value::Var(val)) => {
            let loc_var = var_to_loc(&var, regs);
            let loc_val = var_to_loc(&val, regs);
            if loc_val == loc_var {
                format!("{unop} {loc_var}\n")
            } else if loc_var.is_reg() || loc_val.is_reg() {
                format!("mov {loc_val}, {loc_var}\n{unop} {loc_var}\n")
            } else {
                format!("mov {loc_val}, %eax\n{unop} %eax\nmov %eax, {loc_var}\n")
            }
        }
        Statement::AssignUnop(var, unop, val) => {
            let loc_var = var_to_loc(&var, regs);
            format!("mov {val}, {loc_var}\n{unop} {loc_var}\n")
        }
        Statement::CondJump(Value::Var(var), target) => {
            let loc_var = var_to_loc(&var, regs);
            format!("cmp {loc_var}, 0\njnz l{target}\n")
        }
        Statement::CondJump(val, target) => {
            format!("cmp {val}, 0\njnz l{target}\n")
        }
        Statement::Jump(target) => format!("jmp l{target}\n"),
        Statement::Return(Value::Var(v)) => format!("mov {}, %eax\nret\n", var_to_loc(&v, regs)),
        Statement::Return(val) => format!("mov {val}, %eax\nret\n"),
    }
}

fn asm_assign_binop(
    var: String,
    val1: Value,
    binop: Binop,
    val2: Value,
    regs: &HashMap<String, Location>,
) -> String {
    let loc_var = var_to_loc(&var, regs);
    match binop {
        Binop::Less => asm_assign_binop_cmp(var, val1, binop, val2, regs),
        Binop::Leq => asm_assign_binop_cmp(var, val1, binop, val2, regs),
        Binop::Greater => asm_assign_binop_cmp(var, val1, binop, val2, regs),
        Binop::Geq => asm_assign_binop_cmp(var, val1, binop, val2, regs),
        Binop::Eq => asm_assign_binop_cmp(var, val1, binop, val2, regs),
        Binop::Neq => asm_assign_binop_cmp(var, val1, binop, val2, regs),
        Binop::Add => asm_assign_binop_assoc_comm(var, val1, binop, val2, regs),
        Binop::Minus => {
            if (loc_var.is_reg() || (is_reg_var(&val1, regs) && is_reg_var(&val2, regs)))
                && (format!("{loc_var}") != val2.to_asm(regs))
            {
                if format!("{loc_var}") == val1.to_asm(regs) {
                    format!("sub {}, {loc_var}\n", val2.to_asm(regs))
                } else {
                    format!(
                        "mov {}, {loc_var}\nsub {}, {loc_var}\n",
                        val1.to_asm(regs),
                        val2.to_asm(regs)
                    )
                }
            } else {
                format!(
                    "mov {}, %eax\nsub {}, %eax\nmov %eax, {loc_var}\n",
                    val1.to_asm(regs),
                    val2.to_asm(regs)
                )
            }
        }
        Binop::Mul => {
            if loc_var.is_reg() {
                if format!("{loc_var}") == val2.to_asm(regs) {
                    format!("imul {}, {loc_var}\n", val1.to_asm(regs))
                } else if format!("{loc_var}") == val1.to_asm(regs) {
                    format!("imul {}, {loc_var}\n", val2.to_asm(regs))
                } else {
                    format!(
                        "mov {}, {loc_var}\nimul {}, {loc_var}\n",
                        val1.to_asm(regs),
                        val2.to_asm(regs)
                    )
                }
            } else {
                format!(
                    "mov {}, %eax\nimul {}, %eax\nmov %eax, {loc_var}\n",
                    val1.to_asm(regs),
                    val2.to_asm(regs)
                )
            }
        }
        Binop::Div => asm_assign_binop_div_mod(var, val1, binop, val2, regs),
        Binop::Mod => asm_assign_binop_div_mod(var, val1, binop, val2, regs),
        Binop::BitAnd => asm_assign_binop_assoc_comm(var, val1, binop, val2, regs),
        Binop::BitXor => asm_assign_binop_assoc_comm(var, val1, binop, val2, regs),
        Binop::BitOr => asm_assign_binop_assoc_comm(var, val1, binop, val2, regs),
        Binop::ShiftL => asm_assign_binop_shift(var, val1, binop, val2, regs),
        Binop::ShiftR => asm_assign_binop_shift(var, val1, binop, val2, regs),
    }
}

fn asm_assign_binop_cmp(
    var: String,
    val1: Value,
    binop: Binop,
    val2: Value,
    regs: &HashMap<String, Location>,
) -> String {
    let loc_var = var_to_loc(&var, regs);
    let cmov = match binop {
        Binop::Less => "cmovb",
        Binop::Leq => "cmovbe",
        Binop::Greater => "cmova",
        Binop::Geq => "cmovae",
        Binop::Eq => "cmove",
        Binop::Neq => "cmovne",
        _ => unreachable!(),
    };
    let cmp = if is_reg_var(&val2, regs) {
        format!("cmp {}, {}\n", val1.to_asm(regs), val2.to_asm(regs))
    } else {
        format!(
            "mov {}, %eax\ncmp {}, %eax\n",
            val2.to_asm(regs),
            val1.to_asm(regs)
        )
    };
    format!("{cmp}movl $0, {loc_var}\nmov $-1, %eax\n{cmov} %eax, {loc_var}\n")
}

fn asm_assign_binop_assoc_comm(
    var: String,
    val1: Value,
    binop: Binop,
    val2: Value,
    regs: &HashMap<String, Location>,
) -> String {
    let loc_var = var_to_loc(&var, regs);
    let op = match binop {
        Binop::Add => "add",
        Binop::BitAnd => "and",
        Binop::BitXor => "xor",
        Binop::BitOr => "or",
        _ => unreachable!(),
    };
    if loc_var.is_reg() || (is_reg_var(&val1, regs) && is_reg_var(&val2, regs)) {
        if format!("{loc_var}") == val2.to_asm(regs) {
            format!("{op} {}, {loc_var}\n", val1.to_asm(regs))
        } else if format!("{loc_var}") == val1.to_asm(regs) {
            format!("{op} {}, {loc_var}\n", val2.to_asm(regs))
        } else {
            format!(
                "mov {}, {loc_var}\n{op} {}, {loc_var}\n",
                val1.to_asm(regs),
                val2.to_asm(regs)
            )
        }
    } else {
        format!(
            "mov {}, %eax\n{op} {}, %eax\nmov %eax, {loc_var}\n",
            val1.to_asm(regs),
            val2.to_asm(regs)
        )
    }
}

fn asm_assign_binop_div_mod(
    var: String,
    val1: Value,
    binop: Binop,
    val2: Value,
    regs: &HashMap<String, Location>,
) -> String {
    let loc_var = var_to_loc(&var, regs);
    let res_reg = match binop {
        Binop::Div => "%eax",
        Binop::Mod => "%edx",
        _ => unreachable!(),
    };
    if is_reg_var(&val2, regs) {
        format!(
            "mov {}, %eax\ncdq\nidiv {}\nmov {res_reg}, {loc_var}\n",
            val1.to_asm(regs),
            val2.to_asm(regs)
        )
    } else {
        format!(
            "mov {}, %eax\nmov {}, %ecx\ncdq\nidiv %ecx\nmov {res_reg}, {loc_var}\n",
            val1.to_asm(regs),
            val2.to_asm(regs)
        )
    }
}

fn asm_assign_binop_shift(
    var: String,
    val1: Value,
    binop: Binop,
    val2: Value,
    regs: &HashMap<String, Location>,
) -> String {
    let loc_var = var_to_loc(&var, regs);
    let op = match binop {
        Binop::ShiftL => "sall",
        Binop::ShiftR => "sarl",
        _ => unreachable!(),
    };
    let shift = |target| {
        if let Value::IntConst(c) = val2 {
            format!("{op} ${}, {target}\n", c & 0xff)
        } else {
            format!("mov {}, %ecx\n{op} %cl, {target}\n", val2.to_asm(regs))
        }
    };
    if loc_var.is_reg() || is_reg_var(&val1, regs) {
        format!(
            "mov {}, {loc_var}\n{}",
            val1.to_asm(regs),
            shift(format!("{loc_var}"))
        )
    } else {
        format!(
            "mov {}, %eax\n{}mov %eax, {loc_var}\n",
            val1.to_asm(regs),
            shift("%eax".to_string())
        )
    }
}

fn var_to_loc(var: &String, regs: &HashMap<String, Location>) -> Location {
    regs.get(var).cloned().unwrap_or(Location::Stack(4))
}

fn is_reg_var(val: &Value, regs: &HashMap<String, Location>) -> bool {
    var_to_loc(
        match val {
            Value::Var(var) => var,
            _ => return false,
        },
        regs,
    )
    .is_reg()
}

impl Display for Location {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Location::Register(r) => write!(f, "{r}"),
            Location::Stack(offset) => write!(f, "-{offset}(%rsp)"),
        }
    }
}

impl Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Value::True => write!(f, "$-1"),
            Value::False => write!(f, "$0"),
            Value::IntConst(c) => write!(f, "${c}"),
            Value::Var(_) => unimplemented!(),
        }
    }
}

impl Value {
    fn to_asm(&self, regs: &HashMap<String, Location>) -> String {
        match self {
            Value::Var(var) => format!("{}", var_to_loc(var, regs)),
            v => format!("{v}"),
        }
    }
}

impl Display for Unop {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Unop::LogNot => write!(f, "notl"),
            Unop::BitNot => write!(f, "notl"),
            Unop::Neg => write!(f, "negl"),
        }
    }
}

impl Location {
    fn is_reg(&self) -> bool {
        match self {
            Location::Register(_) => true,
            Location::Stack(_) => false,
        }
    }
}
