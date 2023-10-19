use std::collections::HashSet;

use assembler::{Comp, Dest, Jump, Operand, Operator};

pub enum Segment {
    Local,
    Argument,
    This,
    That,
    Constant,
    Static,
    Temp,
    Pointer,
}

impl Segment {
    fn to_str(&self) -> &'static str {
        match self {
            Segment::Local => "local",
            Segment::Argument => "argument",
            Segment::This => "this",
            Segment::That => "that",
            Segment::Constant => "constant",
            Segment::Static => "static",
            Segment::Temp => "temp",
            Segment::Pointer => "pointer",
        }
    }
}

pub enum Instruction {
    Push(Segment, u32),
    Pop(Segment, u32),
    Add,
    Sub,
    Neg,
    Eq,
    Gt,
    Lt,
    And,
    Or,
    Not,
    Label(String),
    Goto(String),
    IfGoto(String),
    Call(String, u32),
    Function(String, u32),
    Return,
}

impl ToString for Instruction {
    fn to_string(&self) -> String {
        match self {
            Instruction::Push(seg, i) => format!("push {} {}", seg.to_str(), i),
            Instruction::Pop(seg, i) => format!("pop {} {}", seg.to_str(), i),
            Instruction::Add => "add".to_string(),
            Instruction::Sub => "sub".to_string(),
            Instruction::Neg => "neg".to_string(),
            Instruction::Eq => "eq".to_string(),
            Instruction::Gt => "gt".to_string(),
            Instruction::Lt => "lt".to_string(),
            Instruction::And => "and".to_string(),
            Instruction::Or => "or".to_string(),
            Instruction::Not => "not".to_string(),
            Instruction::Label(label) => format!("label {}", label),
            Instruction::Goto(label) => format!("goto {}", label),
            Instruction::IfGoto(label) => format!("if-goto {}", label),
            Instruction::Call(name, n) => format!("call {} {}", name, n),
            Instruction::Function(name, n) => format!("function {} {}", name, n),
            Instruction::Return => "return".to_string(),
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

    fn scan(&mut self, mut accept: impl FnMut(u8) -> bool) -> &'a [u8] {
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

    fn skip_whitespace(&mut self) {
        self.scan(|c| c.is_ascii_whitespace());
    }

    fn skip_comment(&mut self) {
        match self.s.get(self.p + 1) {
            Some(b'/') => {
                self.scan(|c| c != b'\n');
            }
            Some(b'*') => {
                self.p += 2;
                let mut prev = 0u8;
                self.scan(|c| {
                    if prev == b'*' && c == b'/' {
                        false
                    } else {
                        prev = c;
                        true
                    }
                });
                self.p += 1;
            }
            _ => panic!("unexpected '/'"),
        }
    }

    fn skip_useless(&mut self) -> bool {
        loop {
            self.skip_whitespace();
            if self.eof() {
                break;
            }
            if self.s[self.p] == b'/' {
                self.skip_comment();
                continue;
            }
            return true;
        }
        false
    }

    fn next_instr(&mut self) -> Option<String> {
        self.skip_useless().then(|| {
            String::from_utf8(self.scan(|c| c.is_ascii_lowercase() || c == b'-').to_vec())
                .expect("instruction")
        })
    }

    fn next_segment(&mut self) -> Option<Segment> {
        self.skip_useless().then(|| {
            let seg = self.scan(|c| c.is_ascii_lowercase());
            match seg {
                b"local" => Segment::Local,
                b"argument" => Segment::Argument,
                b"this" => Segment::This,
                b"that" => Segment::That,
                b"constant" => Segment::Constant,
                b"static" => Segment::Static,
                b"temp" => Segment::Temp,
                b"pointer" => Segment::Pointer,
                _ => panic!("unexpected segment {}", std::str::from_utf8(seg).unwrap()),
            }
        })
    }

    fn next_i(&mut self) -> Option<u32> {
        self.skip_useless().then(|| {
            let digits = self.scan(|c| c.is_ascii_digit() || c == b'-');
            i32::from_str_radix(std::str::from_utf8(digits).unwrap(), 10).expect("i") as u32
        })
    }

    fn next_symbol(&mut self) -> Option<String> {
        self.skip_useless().then(|| {
            // reserve $ as internal symbol
            let chars = self.scan(|c| c.is_ascii_alphanumeric() || matches!(c, b'_' | b'.' | b':'));
            String::from_utf8(chars.to_vec()).expect("symbol")
        })
    }

    fn next(&mut self) -> Option<Instruction> {
        self.next_instr().map(|instr| match instr.as_str() {
            "push" => Instruction::Push(
                self.next_segment().expect("expect segment"),
                self.next_i().expect("expect i"),
            ),
            "pop" => Instruction::Pop(
                self.next_segment().expect("expect segment"),
                self.next_i().expect("expect i"),
            ),
            "add" => Instruction::Add,
            "sub" => Instruction::Sub,
            "neg" => Instruction::Neg,
            "eq" => Instruction::Eq,
            "gt" => Instruction::Gt,
            "lt" => Instruction::Lt,
            "and" => Instruction::And,
            "or" => Instruction::Or,
            "not" => Instruction::Not,
            "label" => Instruction::Label(self.next_symbol().expect("expect label")),
            "goto" => Instruction::Goto(self.next_symbol().expect("expect label")),
            "if-goto" => Instruction::IfGoto(self.next_symbol().expect("expect label")),
            "call" => Instruction::Call(
                self.next_symbol().expect("expect functionName"),
                self.next_i().expect("expect nArgs"),
            ),
            "function" => Instruction::Function(
                self.next_symbol().expect("expect functionName"),
                self.next_i().expect("expect nVars"),
            ),
            "return" => Instruction::Return,
            _ => panic!("unsupported instruction: {}", instr),
        })
    }
}

const R_SP: &'static str = "SP";
const R_LCL: &'static str = "LCL";
const R_ARG: &'static str = "ARG";
const R_THIS: &'static str = "THIS";
const R_THAT: &'static str = "THAT";
const R_R13: &'static str = "R13";
const R_R14: &'static str = "R14";
// const R_R15: &'static str = "R15";
const R_TEMPS: [&'static str; 8] = ["R5", "R6", "R7", "R8", "R9", "R10", "R11", "R12"];
const R_PTRS: [&'static str; 2] = [R_THIS, R_THAT];

struct Translator<E> {
    labels: HashSet<String>,
    context: String,
    instr_cnt: usize,
    class_name: String,
    emit: E,
}

impl<E> Translator<E>
where
    E: FnMut(assembler::Instruction),
{
    fn new(class_name: String, emit: E) -> Self {
        Self {
            labels: HashSet::new(),
            context: "$G".to_string(),
            instr_cnt: 0,
            class_name,
            emit,
        }
    }

    fn emit_ref(&mut self, a: &str) {
        (self.emit)(assembler::Instruction::Address(a.to_string()));
    }

    fn emit_label(&mut self, l: &str) {
        (self.emit)(assembler::Instruction::Label(l.to_string()));
    }

    fn emit_set(&mut self, dest: Dest, operand: Operand) {
        (self.emit)(assembler::Instruction::Compute(
            dest,
            Comp::Unary(None, operand),
            None,
        ));
    }

    fn emit_unary(&mut self, dest: Dest, op: Operator, operand: Operand) {
        (self.emit)(assembler::Instruction::Compute(
            dest,
            Comp::Unary(Some(op), operand),
            None,
        ));
    }

    fn emit_binary(&mut self, dest: Dest, l: Operand, op: Operator, r: Operand) {
        (self.emit)(assembler::Instruction::Compute(
            dest,
            Comp::Binary(l, op, r),
            None,
        ));
    }

    fn emit_jump(&mut self, label: &str, cond: Operand, jump: Jump) {
        self.emit_ref(label);
        (self.emit)(assembler::Instruction::Compute(
            Dest::none(),
            Comp::Unary(None, cond),
            Some(jump),
        ));
    }

    fn emit_add(&mut self, dest: Dest, l: Operand, r: Operand) {
        self.emit_binary(dest, l, Operator::Plus, r);
    }

    fn emit_sub(&mut self, dest: Dest, l: Operand, r: Operand) {
        self.emit_binary(dest, l, Operator::Minus, r);
    }

    fn emit_load_mem_d(&mut self, symbol: &str) {
        self.emit_ref(symbol);
        self.emit_set(Dest::d(), Operand::M);
    }

    fn emit_store_mem(&mut self, dst: &str, operand: Operand) {
        assert!(matches!(operand, Operand::Zero | Operand::One | Operand::D));
        self.emit_ref(dst);
        self.emit_set(Dest::m(), operand);
    }

    fn emit_push(&mut self, operand: Operand) {
        assert!(matches!(operand, Operand::Zero | Operand::One | Operand::D));
        // RAM[SP]=operand
        self.emit_ref(R_SP);
        self.emit_set(Dest::a(), Operand::M);
        self.emit_set(Dest::m(), operand);
        // SP+=1
        self.emit_ref(R_SP);
        self.emit_add(Dest::m(), Operand::M, Operand::One);
    }

    fn emit_push_d(&mut self) {
        self.emit_push(Operand::D);
    }

    fn emit_pop_m(&mut self) {
        // SP-=1
        self.emit_ref(R_SP);
        self.emit_sub(Dest::m(), Operand::M, Operand::One);
        // A=SP
        self.emit_set(Dest::a(), Operand::M);
    }

    fn emit_pop_d(&mut self) {
        self.emit_pop_m();
        // D=RAM[SP]
        self.emit_set(Dest::d(), Operand::M);
    }

    fn emit_push_cmp_d(&mut self, jump: Jump) {
        let true_label = format!("$B.{}.{}.true", self.context, self.instr_cnt);
        let end_label = format!("$B.{}.{}.end", self.context, self.instr_cnt);
        // @<true_label>
        // D;<cond>
        self.emit_jump(&true_label, Operand::D, jump);
        // D=0
        self.emit_set(Dest::d(), Operand::Zero);
        // @(<end_label>)
        // 0;JMP
        self.emit_jump(&end_label, Operand::Zero, Jump::JMP);
        // (<true_label>)
        self.emit_label(&true_label);
        // D=-1
        (self.emit)(assembler::Instruction::Compute(
            Dest::d(),
            Comp::Unary(Some(Operator::Minus), Operand::One),
            None,
        ));
        // (<end_label>)
        self.emit_label(&end_label);
        self.emit_push_d();
    }

    fn translate_push(&mut self, seg: &Segment, i: &u32) {
        match seg {
            Segment::Local | Segment::Argument | Segment::This | Segment::That => {
                let seg = match seg {
                    Segment::Local => R_LCL,
                    Segment::Argument => R_ARG,
                    Segment::This => R_THIS,
                    Segment::That => R_THAT,
                    _ => unreachable!(),
                };
                // A=RAM[seg]+i
                self.emit_ref(i.to_string().as_str());
                self.emit_set(Dest::d(), Operand::A);
                self.emit_ref(seg);
                self.emit_set(Dest::a(), Operand::M);
                self.emit_add(Dest::a(), Operand::D, Operand::A);
                // D=RAM[A]
                self.emit_set(Dest::d(), Operand::M);
            }
            Segment::Constant => {
                // D=i
                self.emit_ref(i.to_string().as_str());
                self.emit_set(Dest::d(), Operand::A);
            }
            Segment::Static => {
                // D=RAM[class_name.i]
                self.emit_load_mem_d(format!("{}.{}", self.class_name, i).as_str());
            }
            Segment::Temp => {
                // D=TEMPS[i]
                self.emit_load_mem_d(R_TEMPS[*i as usize]);
            }
            Segment::Pointer => {
                // D=PTRS[i]
                self.emit_load_mem_d(R_PTRS[*i as usize]);
            }
        }
        self.emit_push_d();
    }

    fn translate_pop(&mut self, seg: &Segment, i: &u32) {
        match seg {
            Segment::Local | Segment::Argument | Segment::This | Segment::That => {
                let seg = match seg {
                    Segment::Local => R_LCL,
                    Segment::Argument => R_ARG,
                    Segment::This => R_THIS,
                    Segment::That => R_THAT,
                    _ => unreachable!(),
                };
                // addr=RAM[seg]+i
                self.emit_ref(seg);
                self.emit_set(Dest::d(), Operand::M);
                self.emit_ref(i.to_string().as_str());
                self.emit_add(Dest::d(), Operand::D, Operand::A);
                self.emit_store_mem(R_R13, Operand::D);
                self.emit_pop_d();
                // RAM[addr]=D
                self.emit_ref(R_R13);
                self.emit_set(Dest::a(), Operand::M);
                self.emit_set(Dest::m(), Operand::D);
            }
            Segment::Static => {
                self.emit_pop_d();
                // RAM[class_name.i]=D
                self.emit_store_mem(format!("{}.{}", self.class_name, i).as_str(), Operand::D);
            }
            Segment::Temp => {
                self.emit_pop_d();
                // RAM[TEMPS[i]]=D
                self.emit_store_mem(R_TEMPS[*i as usize], Operand::D);
            }
            Segment::Pointer => {
                self.emit_pop_d();
                // RAM[PTRS[i]]=D
                self.emit_store_mem(R_PTRS[*i as usize], Operand::D);
            }
            Segment::Constant => panic!("illegel pop constant"),
        }
    }

    fn translate_call(&mut self, name: &str, n: u32) {
        // R13=SP-n
        self.emit_ref(R_SP);
        self.emit_set(Dest::d(), Operand::M);
        self.emit_ref(n.to_string().as_str());
        self.emit_sub(Dest::d(), Operand::D, Operand::A);
        self.emit_store_mem(R_R13, Operand::D);
        let ret_label = format!("$RET.{}.{}", self.context, self.instr_cnt);
        // push <ret_label>
        self.emit_ref(&ret_label);
        self.emit_set(Dest::d(), Operand::A);
        self.emit_push_d();
        // push LCL
        self.emit_load_mem_d(R_LCL);
        self.emit_push_d();
        // push ARG
        self.emit_load_mem_d(R_ARG);
        self.emit_push_d();
        // push THIS
        self.emit_load_mem_d(R_THIS);
        self.emit_push_d();
        // push THAT
        self.emit_load_mem_d(R_THAT);
        self.emit_push_d();
        // ARG=R13
        self.emit_load_mem_d(R_R13);
        self.emit_store_mem(R_ARG, Operand::D);
        // LCL=SP
        self.emit_load_mem_d(R_SP);
        self.emit_store_mem(R_LCL, Operand::D);
        // JMP name
        self.emit_jump(name, Operand::Zero, Jump::JMP);
        // <ret_label>:
        self.emit_label(&ret_label);
    }

    fn translate_function(&mut self, name: &str, n: u32) {
        self.context = name.to_string();
        self.emit_label(name);
        (0..n).for_each(|_| self.emit_push(Operand::Zero));
    }

    fn translate_return(&mut self) {
        // R13=LCL-5 // FP
        self.emit_ref(R_LCL);
        self.emit_set(Dest::d(), Operand::M);
        self.emit_ref("5");
        self.emit_sub(Dest::d(), Operand::D, Operand::A);
        self.emit_store_mem(R_R13, Operand::D);
        // R14=RAM[R13]
        self.emit_set(Dest::a(), Operand::D);
        self.emit_set(Dest::d(), Operand::M);
        self.emit_store_mem(R_R14, Operand::D);
        // D=RAM[SP-1]
        self.emit_ref(R_SP);
        self.emit_sub(Dest::a(), Operand::M, Operand::One);
        self.emit_set(Dest::d(), Operand::M);
        // RAM[ARG]=D
        self.emit_ref(R_ARG);
        self.emit_set(Dest::a(), Operand::M);
        self.emit_set(Dest::m(), Operand::D);
        // SP=ARG+1
        self.emit_add(Dest::d(), Operand::A, Operand::One);
        self.emit_store_mem(R_SP, Operand::D);
        // LCL=RAM[R13+1]
        self.emit_load_mem_d(R_R13);
        self.emit_add(Dest::a(), Operand::D, Operand::One);
        self.emit_set(Dest::d(), Operand::M);
        self.emit_store_mem(R_LCL, Operand::D);
        // ARG=RAM[R13+2]
        self.emit_load_mem_d(R_R13);
        self.emit_ref("2");
        self.emit_add(Dest::a(), Operand::D, Operand::A);
        self.emit_set(Dest::d(), Operand::M);
        self.emit_store_mem(R_ARG, Operand::D);
        // THIS=RAM[R13+3]
        self.emit_load_mem_d(R_R13);
        self.emit_ref("3");
        self.emit_add(Dest::a(), Operand::D, Operand::A);
        self.emit_set(Dest::d(), Operand::M);
        self.emit_store_mem(R_THIS, Operand::D);
        // THAT=RAM[R13+4]
        self.emit_load_mem_d(R_R13);
        self.emit_ref("4");
        self.emit_add(Dest::a(), Operand::D, Operand::A);
        self.emit_set(Dest::d(), Operand::M);
        self.emit_store_mem(R_THAT, Operand::D);
        // JMP R14
        self.emit_ref(R_R14);
        self.emit_set(Dest::a(), Operand::M);
        (self.emit)(assembler::Instruction::Compute(
            Dest::none(),
            Comp::Unary(None, Operand::Zero),
            Some(Jump::JMP),
        ));
    }

    fn label_symbol(&self, label: &str) -> String {
        format!("{}${}", self.context, label)
    }

    fn translate(&mut self, vm_instr: &Instruction) {
        self.instr_cnt += 1;
        match vm_instr {
            Instruction::Push(seg, i) => self.translate_push(seg, i),
            Instruction::Pop(seg, i) => self.translate_pop(seg, i),
            Instruction::Add | Instruction::And | Instruction::Or => {
                self.emit_pop_d();
                self.emit_pop_m();
                self.emit_binary(
                    Dest::d(),
                    Operand::D,
                    match vm_instr {
                        Instruction::Add => Operator::Plus,
                        Instruction::And => Operator::And,
                        _ => Operator::Or,
                    },
                    Operand::M,
                );
                self.emit_push_d();
            }
            Instruction::Sub => {
                self.emit_pop_d();
                self.emit_pop_m();
                self.emit_binary(Dest::d(), Operand::M, Operator::Minus, Operand::D);
                self.emit_push_d();
            }
            Instruction::Not | Instruction::Neg => {
                self.emit_pop_m();
                self.emit_unary(
                    Dest::d(),
                    match vm_instr {
                        Instruction::Not => Operator::Not,
                        _ => Operator::Minus,
                    },
                    Operand::M,
                );
                self.emit_push_d();
            }
            Instruction::Eq | Instruction::Gt | Instruction::Lt => {
                self.emit_pop_d();
                self.emit_pop_m();
                self.emit_binary(Dest::d(), Operand::M, Operator::Minus, Operand::D);
                self.emit_push_cmp_d(match vm_instr {
                    Instruction::Eq => Jump::JEQ,
                    Instruction::Gt => Jump::JGT,
                    _ => Jump::JLT,
                });
            }
            Instruction::Label(label) => {
                let label = self.label_symbol(label);
                if self.labels.contains(&label) {
                    panic!("lable {} defined", label);
                }
                self.labels.insert(label.to_owned());
                self.emit_label(&label);
            }
            Instruction::Goto(label) => {
                let label = self.label_symbol(label);
                self.emit_jump(&label, Operand::Zero, Jump::JMP);
            }
            Instruction::IfGoto(label) => {
                // NOTE：boolean 应该由代码直接生成指令
                self.emit_pop_d();
                let label = self.label_symbol(label);
                self.emit_jump(&label, Operand::D, Jump::JNE);
            }
            Instruction::Call(name, n) => self.translate_call(name, *n),
            Instruction::Function(name, n) => self.translate_function(name, *n),
            Instruction::Return => self.translate_return(),
        }
    }
}

pub fn translate(source: &[u8], class_name: &str, emit: impl FnMut(assembler::Instruction)) {
    let mut parser = Parser::new(source);
    let mut translator = Translator::new(class_name.to_string(), emit);
    while let Some(instr) = parser.next() {
        translator.translate(&instr);
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    static SUM_CODE: &'static [u8] = br###"// Sum code
push constant 0
pop local 0         // initializes sum = 0
label LOOP_START
push argument 0
push local 0
add
pop local 0	        // sum = sum + counter
push argument 0
push constant 1
sub
pop argument 0      // counter--
push argument 0
if-goto LOOP_START  // If counter != 0, goto LOOP_START
push local 0
"###;

    static INSTRS_07: &'static [u8] = br###"
    push local 8
    push argument 7
    push this 6
    push that 5
    push constant 4
    push static 3
    push temp 2
    push pointer 1
    pop local 8
    pop argument 7
    pop this 6
    pop that 5
    pop static 3
    pop temp 2
    pop pointer 0
    pop pointer 1
    push constant 1
    push constant 2
    add
    push constant 3
    push constant 4
    sub
    push constant 5
    neg
    push constant 6
    push constant 7
    eq
    push constant 8
    push constant 9
    gt
    push constant 10
    push constant 11
    lt
    push constant 12
    push constant 13
    and
    push constant 14
    push constant 15
    or
    push constant 16
    not
"###;

    static INSTRS_08: &'static [u8] = br###"
// Computes 3 + 8 * 5
function foo 0
    push constant 3
    push constant 8
    push constant 5
    call mult 2
    add
    return

// Returns arg0 * arg1
function mult 2
    push constant 0
    pop local 0
    push constant 1
    pop local 1
label LOOP
    push local 1
    push argument 1
    gt
    if-goto END
    push local 0
    push argument 0
    add
    pop local 0
    push local 1
    push constant 1
    add
    pop local 1
    goto LOOP
label END
    push local 0
    return
"###;

    #[test]
    fn test_parse() {
        let mut parser = Parser::new(SUM_CODE);
        while let Some(instr) = parser.next() {
            println!("{}", instr.to_string());
        }
    }

    #[test]
    fn test_translator_translate_07() {
        let mut translator = Translator::new("Test07".to_string(), |instr| {
            println!("{}", instr.to_string());
        });
        let mut parser = Parser::new(INSTRS_07);
        while let Some(instr) = parser.next() {
            println!("// {}", instr.to_string());
            translator.translate(&instr);
        }
    }

    #[test]
    fn test_translator_translate_08() {
        let mut translator = Translator::new("Test08".to_string(), |instr| {
            println!(
                "{}{}",
                match instr {
                    assembler::Instruction::Label(_) => "",
                    _ => "  ",
                },
                instr.to_string()
            );
        });
        let mut parser = Parser::new(INSTRS_08);
        while let Some(instr) = parser.next() {
            println!("// {}", instr.to_string());
            translator.translate(&instr);
        }
    }
}
