use crate::parser::elaborator::EAST;

pub fn returns_properly(east: &EAST) -> bool {
    match east {
        EAST::Declare(_var, _typ, stmts) => returns_properly(&stmts),
        EAST::Assign(_var, _exp) => false,
        EAST::If(_exp, then_body, else_body) => {
            returns_properly(&then_body) && returns_properly(&else_body)
        }
        EAST::While(_exp, _body) => false,
        EAST::For(_init, _exp, _step, _body) => false,
        EAST::Return(_exp) => true,
        EAST::Nop => false,
        EAST::Seq(stmts) => stmts.iter().any(returns_properly),
        EAST::Continue => {
            unreachable!("We should not come across any \"continue\" while checking returns")
        }
        EAST::Break => {
            unreachable!("We should not come across any \"break\" while checking returns")
        }
    }
}
