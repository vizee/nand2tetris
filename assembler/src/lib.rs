use std::collections::HashMap;

pub struct Dest {
    pub a: bool,
    pub d: bool,
    pub m: bool,
}

impl Dest {
    pub fn new(a: bool, d: bool, m: bool) -> Self {
        Self { a, d, m }
    }

    pub fn none() -> Self {
        Self {
            a: false,
            d: false,
            m: false,
        }
    }

    pub fn a() -> Self {
        Self {
            a: true,
            d: false,
            m: false,
        }
    }

    pub fn d() -> Self {
        Self {
            a: false,
            d: true,
            m: false,
        }
    }

    pub fn m() -> Self {
        Self {
            a: false,
            d: false,
            m: true,
        }
    }
}

pub enum Operand {
    Zero,
    One,
    A,
    D,
    M,
}

impl Operand {
    fn to_char(&self) -> char {
        match self {
            Operand::Zero => '0',
            Operand::One => '1',
            Operand::A => 'A',
            Operand::D => 'D',
            Operand::M => 'M',
        }
    }
}

pub enum Operator {
    Plus,
    Minus,
    And,
    Or,
    Not,
}

impl Operator {
    fn to_char(&self) -> char {
        match self {
            Operator::Plus => '+',
            Operator::Minus => '-',
            Operator::And => '&',
            Operator::Or => '|',
            Operator::Not => '!',
        }
    }
}

pub enum Comp {
    Unary(Option<Operator>, Operand),
    Binary(Operand, Operator, Operand),
}

#[repr(u32)]
pub enum Jump {
    JGT = 0b001,
    JEQ = 0b010,
    JGE = 0b011,
    JLT = 0b100,
    JNE = 0b101,
    JLE = 0b110,
    JMP = 0b111,
}

pub enum Instruction {
    Address(String),
    Compute(Dest, Comp, Option<Jump>),
    Label(String),
}

impl ToString for Instruction {
    fn to_string(&self) -> String {
        match self {
            Instruction::Address(symbol) => "@".to_string() + symbol,
            Instruction::Compute(dest, comp, jump) => {
                let mut s = String::new();
                if dest.a || dest.d || dest.m {
                    if dest.a {
                        s.push('A');
                    }
                    if dest.d {
                        s.push('D');
                    }
                    if dest.m {
                        s.push('M');
                    }
                    s.push('=')
                }
                match comp {
                    Comp::Unary(op, operand) => {
                        if let Some(op) = op {
                            s.push(op.to_char());
                        }
                        s.push(operand.to_char());
                    }
                    Comp::Binary(lhs, op, rhs) => {
                        s.push(lhs.to_char());
                        s.push(op.to_char());
                        s.push(rhs.to_char());
                    }
                }
                if let Some(jump) = jump {
                    s.push(';');
                    match jump {
                        Jump::JGT => s.push_str("JGT"),
                        Jump::JEQ => s.push_str("JEQ"),
                        Jump::JGE => s.push_str("JGE"),
                        Jump::JLT => s.push_str("JLT"),
                        Jump::JNE => s.push_str("JNE"),
                        Jump::JLE => s.push_str("JLE"),
                        Jump::JMP => s.push_str("JMP"),
                    }
                }
                s
            }
            Instruction::Label(symbol) => "(".to_string() + symbol + ")",
        }
    }
}

struct Parser<'a> {
    s: &'a [u8],
    p: usize,
}

impl<'a> Parser<'a> {
    fn new(s: &'a [u8]) -> Self {
        Self { s: s, p: 0 }
    }

    fn eof(&self) -> bool {
        self.p >= self.s.len()
    }

    fn skip_whitespace(&mut self) {
        while self.p < self.s.len() && self.s[self.p].is_ascii_whitespace() {
            self.p += 1;
        }
    }

    fn scan(&mut self, accept: impl Fn(u8) -> bool) -> &'a [u8] {
        let b = self.p;
        let mut i = self.p;
        while i < self.s.len() {
            if !accept(self.s[i]) {
                break;
            }
            i += 1;
        }
        self.p = i;
        &self.s[b..i]
    }

    fn read_symbol(&mut self) -> &'a [u8] {
        self.scan(|c| c.is_ascii_alphanumeric() || matches!(c, b'_' | b'.' | b'$' | b':'))
    }

    fn read_a_instr(&mut self) -> Instruction {
        self.p += 1;
        Instruction::Address(String::from_utf8(self.read_symbol().to_owned()).unwrap())
    }

    fn read_expr(&mut self) -> &'a [u8] {
        self.scan(|c| {
            matches!(
                c,
                b'0' | b'1' | b'D' | b'A' | b'M' | b'-' | b'!' | b'+' | b'&' | b'|'
            )
        })
    }

    fn parse_comp_operand(expr: &[u8]) -> (Operand, &[u8]) {
        (
            match expr[0] {
                b'0' => Operand::Zero,
                b'1' => Operand::One,
                b'A' => Operand::A,
                b'D' => Operand::D,
                b'M' => Operand::M,
                _ => panic!("illegal operand: {}", std::str::from_utf8(expr).unwrap()),
            },
            &expr[1..],
        )
    }

    fn parse_comp_expr(expr: &[u8]) -> Comp {
        if matches!(expr[0], b'!' | b'-') {
            let (operand, _) = Self::parse_comp_operand(&expr[1..]);
            Comp::Unary(
                Some(if expr[0] == b'!' {
                    Operator::Not
                } else {
                    Operator::Minus
                }),
                operand,
            )
        } else {
            let (lhs, rest) = Self::parse_comp_operand(&expr);
            if !rest.is_empty() {
                let op = match rest[0] {
                    b'+' => Operator::Plus,
                    b'-' => Operator::Minus,
                    b'&' => Operator::And,
                    b'|' => Operator::Or,
                    c => panic!("invalid binary operator: {}", c as char),
                };
                let (rhs, _) = Self::parse_comp_operand(&rest[1..]);
                Comp::Binary(lhs, op, rhs)
            } else {
                Comp::Unary(None, lhs)
            }
        }
    }

    fn read_c_instr(&mut self) -> Instruction {
        let mut dest = Dest {
            a: false,
            d: false,
            m: false,
        };
        let dest_or_comp = self.read_expr();
        let comp_expr = if !self.eof() && self.s[self.p] == b'=' {
            for c in dest_or_comp {
                match *c {
                    b'A' => dest.a = true,
                    b'D' => dest.d = true,
                    b'M' => dest.m = true,
                    _ => panic!("unexpected dest {}", *c as char),
                }
            }
            self.p += 1;
            self.read_expr()
        } else {
            dest_or_comp
        };

        let comp = Self::parse_comp_expr(comp_expr);

        let jump = if !self.eof() && self.s[self.p] == b';' {
            self.p += 1;
            let jump = self.scan(|c| c.is_ascii_uppercase());
            Some(match jump {
                b"JGT" => Jump::JGT,
                b"JEQ" => Jump::JEQ,
                b"JGE" => Jump::JGE,
                b"JLT" => Jump::JLT,
                b"JNE" => Jump::JNE,
                b"JLE" => Jump::JLE,
                b"JMP" => Jump::JMP,
                _ => panic!("unexpected jump {}", std::str::from_utf8(jump).unwrap()),
            })
        } else {
            None
        };

        Instruction::Compute(dest, comp, jump)
    }

    fn read_l_instr(&mut self) -> Instruction {
        self.p += 1;
        let symbol = self.read_symbol();
        self.p += 1;
        Instruction::Label(String::from_utf8(symbol.to_owned()).unwrap())
    }

    fn next(&mut self) -> Option<Instruction> {
        // COMMENT = "//" text "\n"
        // SYMBOL = (letter | digit | "_" | "." | "$" | ":")+
        // A-INSTR = "@" (SYMBOL | digit+)
        // L-INSTR = "(" SYMBOL ")"
        // A-INSTR = "@" (SYMBOL | digit+))
        // EXPR = "0" | "1" | "D" | "A" | "M" | (("-" | "!") EXPR) | (EXPR ("+" | "-" | "&" | "|" | "=") EXPR)
        // C-INSTR = (EXPR)? (";" JUMP)?
        loop {
            self.skip_whitespace();
            if self.eof() {
                break;
            }
            if self.s[self.p] == b'/' {
                self.scan(|c| c != b'\n');
                continue;
            }
            return Some(match self.s[self.p] {
                b'@' => self.read_a_instr(),
                b'(' => self.read_l_instr(),
                _ => self.read_c_instr(),
            });
        }
        None
    }
}

fn assemble_comp(comp: Comp) -> u32 {
    // a zx nx zy ny f no
    // if (zx == 1) set x = 0
    // if (nx == 1) set x = !x
    // if (zy == 1) set y = 0
    // if (ny == 1) set y = !y
    // if (f == 1)  set out = x + y
    // if (f == 0)  set out = x & y
    // if (no == 1) set out = !out
    let mut a = false;
    let mut zx = false;
    let mut nx = false;
    let mut zy = false;
    let mut ny = false;
    let mut f = false;
    let mut no = false;

    match comp {
        Comp::Unary(op, operand) => match operand {
            Operand::Zero => {
                zx = true;
                zy = true;
                f = true;
            }
            Operand::One => {
                let neg = matches!(op, Some(Operator::Minus));
                zx = true;
                nx = true;
                zy = true;
                ny = !neg;
                f = true;
                no = !neg;
            }
            Operand::D => {
                let neg = matches!(op, Some(Operator::Minus));
                let not = matches!(op, Some(Operator::Not));
                zy = true;
                ny = true;
                f = neg;
                no = not || neg;
            }
            Operand::A | Operand::M => {
                let neg = matches!(op, Some(Operator::Minus));
                let not = matches!(op, Some(Operator::Not));
                a = matches!(operand, Operand::M);
                zx = true;
                nx = true;
                f = neg;
                no = not || neg;
            }
        },
        Comp::Binary(lhs, op, rhs) => {
            if matches!(op, Operator::Plus | Operator::Minus) {
                f = true;
                let is_lhs_d = matches!(lhs, Operand::D);
                let is_rhs_one = matches!(rhs, Operand::One);
                if matches!(op, Operator::Minus) {
                    zx = !is_lhs_d && is_rhs_one;
                    nx = is_lhs_d ^ is_rhs_one;
                    zy = is_lhs_d && is_rhs_one;
                    ny = !nx;
                    no = !is_rhs_one;
                } else if is_rhs_one {
                    zx = !is_lhs_d;
                    nx = true;
                    zy = is_lhs_d;
                    ny = true;
                    no = true;
                }
            } else {
                let is_or = matches!(op, Operator::Or);
                nx = is_or;
                ny = is_or;
                no = is_or;
            }
            a = matches!(lhs, Operand::M) || matches!(rhs, Operand::M);
        }
    }

    (a as u32) << 6
        | (zx as u32) << 5
        | (nx as u32) << 4
        | (zy as u32) << 3
        | (ny as u32) << 2
        | (f as u32) << 1
        | (no as u32)
}

pub fn assemble(source: &[u8], mut emit: impl FnMut(u32)) {
    let mut sym_table = HashMap::new();
    for i in 0..=15usize {
        sym_table.insert(format!("R{}", i), i);
    }
    sym_table.insert("SP".to_string(), 0);
    sym_table.insert("LCL".to_string(), 1);
    sym_table.insert("ARG".to_string(), 2);
    sym_table.insert("THIS".to_string(), 3);
    sym_table.insert("THAT".to_string(), 4);
    sym_table.insert("SCREEN".to_string(), 16384);
    sym_table.insert("KBD".to_string(), 24576);

    const RAM_BASE: usize = 16;

    let mut parser = Parser::new(&source);
    let mut offset = 0usize;
    let mut instrs = Vec::new();
    while let Some(instr) = parser.next() {
        if let Instruction::Label(label) = &instr {
            sym_table.insert(label.to_owned(), offset);
        } else {
            instrs.push(instr);
            offset += 1;
        }
    }

    let mut ram_alloc = RAM_BASE;
    for instr in instrs {
        match instr {
            Instruction::Address(symbol) => {
                let addr = if symbol.as_bytes()[0].is_ascii_digit() {
                    symbol.parse::<usize>().expect("invalid symbol")
                } else {
                    *sym_table.entry(symbol).or_insert_with(|| {
                        let addr = ram_alloc;
                        ram_alloc += 1;
                        addr
                    })
                };
                emit(addr as u32 & 0x7fff);
            }
            Instruction::Compute(dest, comp, jump) => {
                let ac = assemble_comp(comp);
                let mut d = 0u32;
                if dest.a {
                    d |= 0b100;
                }
                if dest.d {
                    d |= 0b010;
                }
                if dest.m {
                    d |= 0b001;
                }
                let j = jump.map(|j| j as u32).unwrap_or(0);
                emit(0b1110000000000000 | ac << 6 | d << 3 | j);
            }
            _ => {}
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    static SUM_CODE: &'static [u8] = br##"// Adds 1+...+100.
    @i     // i refers to some mem. location.
    M=1    // i=1
    @sum   // sum refers to some mem. location.
    M=0    // sum=0
(LOOP)
    @i
    D=M    // D=i
    @100
    D=D-A  // D=i-100
    @END
    D;JGT  // If (i-100)>0 goto END
    @i
    D=M    // D=i
    @sum
    M=D+M  // sum=sum+i
    @i
    M=M+1  // i=i+1
    @LOOP
    0;JMP  // Goto LOOP
(END)
    @END
    0;JMP  // Infinite loop
"##;

    #[test]
    fn test_parse() {
        let mut parser = Parser::new(SUM_CODE);
        let mut i = 0usize;
        while let Some(instr) = parser.next() {
            i += 1;
            println!("{:02}: {}", i, instr.to_string());
        }
    }

    #[test]
    fn test_assemble_comp() {
        let cases = [
            ("0", 0b0101010),
            ("1", 0b0111111),
            ("-1", 0b0111010),
            ("D", 0b0001100),
            ("A", 0b0110000),
            ("M", 0b1110000),
            ("!D", 0b0001101),
            ("!A", 0b0110001),
            ("!M", 0b1110001),
            ("-D", 0b0001111),
            ("-A", 0b0110011),
            ("-M", 0b1110011),
            ("D+1", 0b0011111),
            ("A+1", 0b0110111),
            ("M+1", 0b1110111),
            ("D-1", 0b0001110),
            ("A-1", 0b0110010),
            ("M-1", 0b1110010),
            ("D+A", 0b0000010),
            ("D+M", 0b1000010),
            ("D-A", 0b0010011),
            ("D-M", 0b1010011),
            ("A-D", 0b0000111),
            ("M-D", 0b1000111),
            ("D&A", 0b0000000),
            ("D&M", 0b1000000),
            ("D|A", 0b0010101),
            ("D|M", 0b1010101),
        ];
        cases.iter().for_each(|(instr, expect)| {
            let instr = Parser::new(instr.as_bytes()).next().unwrap();
            let comp = if let Instruction::Compute(_, comp, _) = instr {
                comp
            } else {
                unreachable!()
            };
            assert_eq!(assemble_comp(comp), *expect)
        })
    }

    #[test]
    fn test_assemble() {
        assemble(SUM_CODE, |v| {
            println!("{:016b}", v);
        });
    }
}
