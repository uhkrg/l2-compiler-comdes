use control_flow::{check_break_continue_placements, check_steps};
use returns::returns_properly;
use var_init::check_var_inits;

use crate::{errors::StaticAnalysisError, parser::elaborator::EAST};

mod control_flow;
mod returns;
mod type_check;
mod var_init;

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
    if let Err(var) = check_var_inits(&program) {
        return Err(StaticAnalysisError::UnitVar(var));
    }
    type_check::type_check(program)?;
    Ok(())
}
