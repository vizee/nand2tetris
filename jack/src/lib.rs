#![feature(str_internals)]

#[macro_use]
mod utils {
    macro_rules! fatal {
        ($($arg:tt)*) => ({
            eprintln!($($arg)*);
            ::std::process::exit(1)
            // panic!($($arg)*)
        });
    }

    macro_rules! fatal_pos {
        ($pos:expr,$msg:tt) => ({
            let pos = &$pos;
            fatal!("{}:{}: {}", pos.ln, pos.col, $msg)
        });

        ($pos:expr,$fmt:tt,$($arg:tt)*) => ({
            let pos = &$pos;
            fatal!(concat!("{}:{}: ", $fmt), pos.ln, pos.col, $($arg)*)
        });
    }
}

pub mod codegen;
pub mod parser;
pub mod semantics;
