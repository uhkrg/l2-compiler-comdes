use crate::parser::elaborator::EAST;

pub fn check_break_continue_placements(program: &EAST) -> bool {
    match program {
        EAST::Assign(_var, _exp) => true,
        EAST::While(_exp, _body) => true,
        EAST::Continue => false,
        EAST::Return(_exp) => true,
        EAST::Declare(_var, _typ, stmts) => check_break_continue_placements(&stmts),
        EAST::If(_exp, then_body, else_body) => {
            check_break_continue_placements(&then_body)
                && check_break_continue_placements(&else_body)
        }
        EAST::For(init, _exp, step, _body) => {
            check_break_continue_placements(&init) && check_break_continue_placements(&step)
        }
        EAST::Break => false,
        EAST::Seq(stmts) => stmts.iter().all(check_break_continue_placements),
        EAST::Nop => true,
    }
}

pub fn check_steps(program: &EAST) -> bool {
    match program {
        EAST::Assign(_var, _exp) => true,
        EAST::While(_exp, body) => check_steps(&body),
        EAST::Continue => true,
        EAST::Return(_exp) => true,
        EAST::Declare(_var, _typ, stmts) => check_steps(&stmts),
        EAST::If(_exp, then_body, else_body) => check_steps(&then_body) && check_steps(&else_body),
        EAST::For(_init, _exp, step, body) => {
            if let EAST::Declare(_var, _typ, _stmts) = &**step {
                return false;
            }
            check_steps(&body)
        }
        EAST::Break => false,
        EAST::Seq(stmts) => stmts.iter().all(check_steps),
        EAST::Nop => true,
    }
}
