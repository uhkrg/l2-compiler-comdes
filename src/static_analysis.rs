use control_flow::{check_break_continue_placements, check_steps};
use returns::returns_properly;

use crate::{errors::StaticAnalysisError, parser::elaborator::EAST};

mod control_flow;
mod returns;
mod type_check;

pub fn run_static_checks(program: EAST) -> Result<(), StaticAnalysisError> {
    if !check_break_continue_placements(&program) {
        return Err(StaticAnalysisError::BreakContinuePlacement);
    }
    if !check_steps(&program) {
        return Err(StaticAnalysisError::DeclareStep);
    }
    if !returns_properly(&program) {
        return Err(StaticAnalysisError::MissingReturn);
    }
    type_check::type_check(program)?;
    Ok(())
}
