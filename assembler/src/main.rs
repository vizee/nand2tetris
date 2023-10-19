use std::{env, fs};

fn main() {
    for filename in env::args().skip(1) {
        let source = fs::read(filename).expect("read source file failed");
        assembler::assemble(&source, |v| {
            println!("{:016b}", v);
        })
    }
}
