use crate::{errors::StaticAnalysisError, parser::elaborator::EAST};

mod type_check;

pub fn run_static_checks(program: EAST) -> Result<(), StaticAnalysisError> {
    type_check::type_check(program)?;
    Ok(())
}
