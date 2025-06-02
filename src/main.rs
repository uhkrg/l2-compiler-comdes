use std::process::exit;

use l2_compiler_comdes::{
    lexer,
    parser::{self, elaborator},
    static_analysis,
};

fn main() {
    let input_file = std::env::args()
        .skip(1)
        .next()
        .expect("Need an input file to compile");
    let input = std::fs::read_to_string(input_file).unwrap();

    let lexed = match lexer::lex(&input) {
        Ok(lexed) => lexed,
        Err(error) => {
            eprintln!("{error}");
            exit(error.error_code());
        }
    };
    println!("{lexed:?}\n");

    let parsed = match parser::parse(lexed) {
        Ok(parsed) => parsed,
        Err(error) => {
            eprintln!("{error}");
            exit(42);
        }
    };
    println!("{parsed:?}\n");

    let elaborated = elaborator::elaborate(parsed);
    println!("{elaborated:?}\n");

    match static_analysis::run_static_checks(elaborated.clone()) {
        Ok(()) => (),
        Err(error) => {
            eprintln!("{error}");
            exit(7);
        }
    }
}
