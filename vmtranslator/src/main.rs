use std::path::Path;
use std::{env, fs};

fn main() {
    for filename in env::args().skip(1) {
        let filename = Path::new(&filename);
        let source = fs::read(&filename).expect("read source file failed");
        let class_name = filename
            .file_stem()
            .and_then(|s| s.to_str())
            .expect("class name");
        vmtranslator::translate(&source, class_name, |instr| {
            println!("{}", instr.to_string())
        });
    }
}
