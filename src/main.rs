use l2_compiler_comdes::lexer;

fn main() {
    let input_file = std::env::args()
        .skip(1)
        .next()
        .expect("Need an input file to compile");
    let input = std::fs::read_to_string(input_file).unwrap();
    println!("{:?}", lexer::lex(&input));
}
