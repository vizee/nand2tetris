use std::{env, fs};

use jack::codegen::Generator;
use jack::parser::Parser;
use jack::semantics::Analyzer;

fn main() {
    let mut analyzer = Analyzer::new();
    for filename in env::args().skip(1) {
        let source = fs::read(&filename).expect("read source file failed");
        let mut parser = Parser::new(&source);
        if let Some(cls) = parser.top_level() {
            let mut codegen = Generator::new();
            codegen.gen_class(analyzer.analyze_class(cls));
            for instr in codegen.instructions() {
                println!("{}", instr.to_string());
            }
        }
    }
}
