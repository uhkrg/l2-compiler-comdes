use std::collections::HashMap;

use crate::{
    errors::TypeError,
    lexer::Unop,
    parser::{
        Type,
        elaborator::{Binop, EAST, Expression},
    },
};

pub fn type_check(program: EAST) -> Result<(), TypeError> {
    check_stmt(
        program,
        TypeCheckContext {
            var_types: HashMap::new(),
        },
    )
}

#[derive(Clone)]
struct TypeCheckContext {
    var_types: HashMap<String, Type>,
}

fn check_stmt(stmt: EAST, mut ctx: TypeCheckContext) -> Result<(), TypeError> {
    match stmt {
        EAST::Declare(var, typ, scope) => {
            if ctx.var_types.contains_key(&var) {
                return Err(TypeError::Redecleration(var));
            }
            ctx.var_types.insert(var, typ);
            check_stmt(*scope, ctx)
        }
        EAST::If(cond, then_stmt, else_stmt) => {
            let t = check_exp(&cond, &ctx)?;
            if t != Type::Bool {
                return Err(TypeError::Mismatch {
                    expected: Type::Bool,
                    found: t,
                });
            }
            check_stmt(*then_stmt, ctx.clone())?;
            check_stmt(*else_stmt, ctx)
        }
        EAST::While(cond, body) => {
            let t = check_exp(&cond, &ctx)?;
            if t != Type::Bool {
                return Err(TypeError::Mismatch {
                    expected: Type::Bool,
                    found: t,
                });
            }
            check_stmt(*body, ctx)
        }
        EAST::For(init, cond, step, body) => match *init {
            EAST::Declare(var, typ, init_stmt) => {
                if ctx.var_types.contains_key(&var) {
                    return Err(TypeError::Redecleration(var));
                }
                ctx.var_types.insert(var, typ);
                check_stmt(*init_stmt, ctx.clone())?;

                let t = check_exp(&cond, &ctx)?;
                if t != Type::Bool {
                    return Err(TypeError::Mismatch {
                        expected: Type::Bool,
                        found: t,
                    });
                }
                check_stmt(*step, ctx.clone())?;
                check_stmt(*body, ctx)
            }
            init_stmt => {
                let t = check_exp(&cond, &ctx)?;
                if t != Type::Bool {
                    return Err(TypeError::Mismatch {
                        expected: Type::Bool,
                        found: t,
                    });
                }
                check_stmt(init_stmt, ctx.clone())?;
                check_stmt(*step, ctx.clone())?;
                check_stmt(*body, ctx)
            }
        },
        EAST::Assign(var, exp) => {
            let var_t = ctx
                .var_types
                .get(&var)
                .ok_or_else(|| TypeError::MissingDec(var))?;
            let exp_t = check_exp(&exp, &ctx)?;
            if var_t != &exp_t {
                return Err(TypeError::Mismatch {
                    expected: var_t.to_owned(),
                    found: exp_t,
                });
            }
            Ok(())
        }
        EAST::Seq(stmts) => {
            for stmt in stmts {
                check_stmt(stmt, ctx.clone())?;
            }
            Ok(())
        }
        EAST::Return(exp) => {
            let t = check_exp(&exp, &ctx)?;
            if t != Type::Int {
                return Err(TypeError::Mismatch {
                    expected: Type::Int,
                    found: t,
                });
            }
            Ok(())
        }
        EAST::Break => Ok(()),
        EAST::Continue => Ok(()),
        EAST::Nop => Ok(()),
    }
}

const REL_OPS: [Binop; 4] = [Binop::Less, Binop::Leq, Binop::Greater, Binop::Geq];
const POLYEQ: [Binop; 2] = [Binop::Eq, Binop::Neq];

fn check_exp(exp: &Expression, ctx: &TypeCheckContext) -> Result<Type, TypeError> {
    match exp {
        Expression::True => Ok(Type::Bool),
        Expression::False => Ok(Type::Bool),
        Expression::Const(_) => Ok(Type::Int),
        Expression::Binop(e1, op, e2) if REL_OPS.contains(op) => {
            let t1 = check_exp(&e1, ctx)?;
            if t1 != Type::Int {
                return Err(TypeError::Mismatch {
                    expected: Type::Int,
                    found: t1,
                });
            }
            let t2 = check_exp(&e2, ctx)?;
            if t2 != Type::Int {
                return Err(TypeError::Mismatch {
                    expected: Type::Int,
                    found: t2,
                });
            }
            Ok(Type::Bool)
        }
        Expression::Binop(e1, op, e2) if POLYEQ.contains(op) => {
            let t1 = check_exp(&e1, ctx)?;
            let t2 = check_exp(&e2, ctx)?;
            if t1 != t2 {
                return Err(TypeError::Mismatch {
                    expected: t1,
                    found: t2,
                });
            }
            Ok(Type::Bool)
        }
        Expression::Binop(e1, _op, e2) => {
            let t1 = check_exp(&e1, ctx)?;
            if t1 != Type::Int {
                return Err(TypeError::Mismatch {
                    expected: Type::Int,
                    found: t1,
                });
            }
            let t2 = check_exp(&e2, ctx)?;
            if t2 != Type::Int {
                return Err(TypeError::Mismatch {
                    expected: Type::Int,
                    found: t2,
                });
            }
            Ok(Type::Int)
        }
        Expression::Unop(Unop::LogNot, e) => {
            let t = check_exp(&e, ctx)?;
            if t != Type::Bool {
                return Err(TypeError::Mismatch {
                    expected: Type::Bool,
                    found: t,
                });
            }
            Ok(Type::Bool)
        }
        Expression::Unop(_op, e) => {
            let t = check_exp(&e, ctx)?;
            if t != Type::Int {
                return Err(TypeError::Mismatch {
                    expected: Type::Int,
                    found: t,
                });
            }
            Ok(Type::Int)
        }
        Expression::Ternary(cond, e1, e2) => {
            let cond_t = check_exp(&cond, ctx)?;
            if cond_t != Type::Bool {
                return Err(TypeError::Mismatch {
                    expected: Type::Bool,
                    found: cond_t,
                });
            }
            let t1 = check_exp(&e1, ctx)?;
            let t2 = check_exp(&e2, ctx)?;
            if t1 != t2 {
                return Err(TypeError::Mismatch {
                    expected: t1,
                    found: t2,
                });
            }
            Ok(t1)
        }
        Expression::Var(var) => ctx
            .var_types
            .get(var)
            .map(ToOwned::to_owned)
            .ok_or_else(|| TypeError::MissingDec(var.to_string())),
    }
}
