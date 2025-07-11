use std::{
    io::Write,
    process::{Command, Stdio, exit},
};

use l2_compiler_comdes::{
    code_gen, ir, lexer,
    parser::{self, elaborator},
    reg_alloc, ssa, static_analysis,
};

fn main() {
    let input_file = std::env::args()
        .skip(1)
        .next()
        .expect("Need an input file to compile");
    let output_file = std::env::args()
        .skip(2)
        .next()
        .expect("Need an output file to compile to");
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

    let tree_ir = ir::translate_tree_ir(elaborated);
    println!("{tree_ir:?}\n");

    let block_ir = ir::tree_ir_to_block_ir(tree_ir);
    println!("{block_ir:?}\n");

    let ssa = ssa::ssa(block_ir);
    println!("{ssa:?}\n");

    let after_ssa = ssa::resolve_phis(ssa);
    println!("{after_ssa:?}\n");

    let regs = reg_alloc::reg_alloc(&after_ssa);
    println!("{regs:?}\n");

    let asm = code_gen::asm(after_ssa, &regs);
    println!("{asm}");

    let mut assembler = Command::new("gcc")
        .arg("-x")
        .arg("assembler")
        .arg("-nostdlib")
        .arg("-o")
        .arg(output_file)
        .arg("-")
        .stdin(Stdio::piped())
        .spawn()
        .unwrap();
    assembler
        .stdin
        .as_mut()
        .unwrap()
        .write_all(asm.as_bytes())
        .unwrap();
    assembler.wait().unwrap();
}
