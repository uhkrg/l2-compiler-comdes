use std::collections::HashSet;

use crate::parser::elaborator::{EAST, Expression};

pub fn check_var_inits(east: &EAST) -> Result<(), String> {
    let prepared_east = transform_for(east, None);
    check_var_liveness(&prepared_east).map(|_| ())
}

fn check_var_liveness(east: &EAST) -> Result<HashSet<String>, String> {
    match east {
        EAST::Declare(var, _typ, stmts) => {
            let live_vars = check_var_liveness(&stmts)?;
            if live_vars.contains(var) {
                return Err(var.to_owned());
            }
            Ok(live_vars)
        }
        EAST::Assign(_var, exp) => Ok(used_vars(exp)),
        EAST::If(cond, then_body, else_body) => {
            let mut live = used_vars(cond);
            live.extend(check_var_liveness(&then_body)?);
            live.extend(check_var_liveness(&else_body)?);
            Ok(live)
        }
        EAST::While(cond, body) => {
            let mut live = used_vars(cond);
            live.extend(check_var_liveness(&body)?);
            Ok(live)
        }
        EAST::For(..) => {
            unreachable!("Rewrite for loops before live check for inits")
        }
        EAST::Break => Ok(HashSet::new()),
        EAST::Continue => Ok(HashSet::new()),
        EAST::Return(exp) => Ok(used_vars(exp)),
        EAST::Nop => Ok(HashSet::new()),
        EAST::Seq(stmts) => {
            let mut live = HashSet::new();
            for stmt in stmts.iter().rev() {
                remove_defs(stmt, &mut live);
                live.extend(check_var_liveness(stmt)?);
            }
            Ok(live)
        }
    }
}

fn remove_defs(east: &EAST, live: &mut HashSet<String>) {
    match east {
        EAST::Declare(..) => {}
        EAST::Assign(var, _exp) => {
            live.remove(var);
        }
        EAST::If(_cond, then_body, else_body) => {
            let mut live_else = live.clone();
            remove_defs(&then_body, live);
            remove_defs(&else_body, &mut live_else);
            live.extend(live_else);
        }
        EAST::While(..) => {}
        EAST::For(..) => {
            unreachable!("Rewrite for loops before live check for inits")
        }
        EAST::Break => live.clear(),
        EAST::Continue => live.clear(),
        EAST::Return(_) => live.clear(),
        EAST::Nop => {}
        EAST::Seq(stmts) => {
            for stmt in stmts {
                remove_defs(stmt, live);
            }
        }
    }
}

fn used_vars(exp: &Expression) -> HashSet<String> {
    match exp {
        Expression::True => HashSet::new(),
        Expression::False => HashSet::new(),
        Expression::Var(var) => HashSet::from([var.to_string()]),
        Expression::Const(_) => HashSet::new(),
        Expression::Binop(exp1, _binop, exp2) => {
            let mut used = used_vars(&exp1);
            used.extend(used_vars(&exp2));
            used
        }
        Expression::Unop(_unop, exp) => used_vars(&exp),
        Expression::Ternary(cond, exp1, exp2) => {
            let mut used = used_vars(&cond);
            used.extend(used_vars(&exp1));
            used.extend(used_vars(&exp2));
            used
        }
    }
}

fn transform_for(east: &EAST, step: Option<&EAST>) -> EAST {
    match east {
        EAST::Declare(var, typ, stmts) => EAST::Declare(
            var.to_owned(),
            typ.to_owned(),
            Box::new(transform_for(&stmts, step)),
        ),
        EAST::Assign(var, exp) => EAST::Assign(var.to_owned(), exp.to_owned()),
        EAST::If(exp, then_body, else_body) => EAST::If(
            exp.to_owned(),
            Box::new(transform_for(&then_body, step)),
            Box::new(transform_for(&else_body, step)),
        ),
        EAST::While(exp, body) => EAST::While(exp.to_owned(), Box::new(transform_for(&body, step))),
        EAST::For(init, exp, step, body) => EAST::Seq(vec![
            *init.to_owned(),
            EAST::While(
                exp.to_owned(),
                Box::new(EAST::Seq(vec![
                    transform_for(&body, Some(&step)),
                    *step.to_owned(),
                ])),
            ),
        ]),
        EAST::Return(exp) => EAST::Return(exp.to_owned()),
        EAST::Nop => EAST::Nop,
        EAST::Seq(stmts) => EAST::Seq(stmts.iter().map(|stmt| transform_for(stmt, step)).collect()),
        EAST::Continue => {
            if let Some(step) = step {
                EAST::Seq(vec![step.to_owned(), EAST::Continue])
            } else {
                EAST::Continue
            }
        }
        EAST::Break => {
            if let Some(step) = step {
                EAST::Seq(vec![step.to_owned(), EAST::Break])
            } else {
                EAST::Break
            }
        }
    }
}
