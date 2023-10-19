use std::collections::HashMap;
use std::mem;

macro_rules! binary_expr {
    ($parser: expr, $ops: pat, $prec: ident) => {{
        let mut lhs = $parser.$prec();
        loop {
            if matches!($parser.cur_tok, $ops) {
                let op = $parser.consume();
                lhs = Expr::Binary(op, Box::new(lhs), Box::new($parser.$prec()));
            } else {
                break lhs;
            }
        }
    }};
}

#[derive(Clone, PartialEq, Eq)]
pub enum Token {
    Invalid,
    EOF,
    Comment(usize, usize),
    Ident(String),
    IntLit(u32),
    CharLit(u8),
    StringLit(String),
    LParen,
    RParen,
    LBrack,
    RBrack,
    LBrace,
    RBrace,
    Comma,
    Semicolon,
    Equal,
    Period,
    Plus,
    Minus,
    Asterisk,
    Slash,
    And,
    Or,
    Tilde,
    Lt,
    Gt,
    Class,
    Constructor,
    Method,
    Function,
    Int,
    Boolean,
    Char,
    Void,
    Var,
    Static,
    Field,
    Let,
    Do,
    If,
    Else,
    While,
    Break,
    Continue,
    Return,
    True,
    False,
    Null,
    This,
}

impl Token {
    pub fn as_str(&self) -> &'static str {
        match self {
            Token::Invalid => "<invalid token>",
            Token::EOF => "EOF",
            Token::Comment(_, _) => "comment",
            Token::Ident(_) => "identifier",
            Token::IntLit(_) => "integer",
            Token::CharLit(_) => "character",
            Token::StringLit(_) => "string",
            Token::LParen => "(",
            Token::RParen => ")",
            Token::LBrack => "[",
            Token::RBrack => "]",
            Token::LBrace => "{",
            Token::RBrace => "}",
            Token::Comma => ",",
            Token::Semicolon => ";",
            Token::Equal => "=",
            Token::Period => ".",
            Token::Plus => "+",
            Token::Minus => "-",
            Token::Asterisk => "*",
            Token::Slash => "/",
            Token::And => "&",
            Token::Or => "|",
            Token::Tilde => "~",
            Token::Lt => "<",
            Token::Gt => ">",
            Token::Class => "class",
            Token::Constructor => "constructor",
            Token::Method => "method",
            Token::Function => "function",
            Token::Int => "int",
            Token::Boolean => "boolean",
            Token::Char => "char",
            Token::Void => "void",
            Token::Var => "var",
            Token::Static => "static",
            Token::Field => "field",
            Token::Let => "let",
            Token::Do => "do",
            Token::If => "if",
            Token::Else => "else",
            Token::While => "while",
            Token::Break => "break",
            Token::Continue => "continue",
            Token::Return => "return",
            Token::True => "true",
            Token::False => "false",
            Token::Null => "null",
            Token::This => "this",
        }
    }

    fn into_string(self) -> String {
        match self {
            Self::Ident(s) | Self::StringLit(s) => s,
            _ => panic!("unexpected token: {}", self.as_str()),
        }
    }
}

#[derive(Clone)]
pub enum TypeRef {
    Primitive(Token),
    Named(String),
}

pub struct VarDef {
    pub name: String,
    pub ty: TypeRef,
}

pub enum Expr {
    Int(u32),
    String(String),
    VarRef(String),
    Call(Box<Expr>, Vec<Expr>),
    Member(Box<Expr>, String),
    Index(Box<Expr>, Box<Expr>),
    Unary(Token, Box<Expr>),
    Binary(Token, Box<Expr>, Box<Expr>),
}

pub enum Statement {
    Empty,
    Expr(Expr),
    Block(Vec<Statement>),
    If(Expr, Box<Statement>, Option<Box<Statement>>),
    While(Expr, Box<Statement>),
    LoopFlow(Token),
    Var(Vec<VarDef>),
    Let(Expr, Expr),
    Do(Option<Expr>, Expr),
    Return(Option<Expr>),
}

pub struct SubroutineDecl {
    pub lead: Token,
    pub name: String,
    pub ret_ty: TypeRef,
    pub args: Vec<VarDef>,
    pub body: Statement,
}

pub struct ClassDecl {
    pub name: String,
    pub fields: Vec<VarDef>,
    pub statics: Vec<VarDef>,
    pub subroutines: Vec<SubroutineDecl>,
}

#[derive(Clone, Copy)]
struct Pos {
    ln: usize,
    col: usize,
}

pub struct Parser<'a> {
    s: &'a [u8],
    p: usize,
    scan_pos: Pos,
    cur_pos: Pos,
    cur_tok: Token,
    keywords: HashMap<&'static str, Token>,
}

impl<'a> Parser<'a> {
    pub fn new(source: &'a [u8]) -> Self {
        let keywords = {
            let mut m = HashMap::new();
            m.insert("class", Token::Class);
            m.insert("constructor", Token::Constructor);
            m.insert("method", Token::Method);
            m.insert("function", Token::Function);
            m.insert("int", Token::Int);
            m.insert("boolean", Token::Boolean);
            m.insert("char", Token::Char);
            m.insert("void", Token::Void);
            m.insert("var", Token::Var);
            m.insert("static", Token::Static);
            m.insert("field", Token::Field);
            m.insert("let", Token::Let);
            m.insert("do", Token::Do);
            m.insert("if", Token::If);
            m.insert("else", Token::Else);
            m.insert("while", Token::While);
            m.insert("break", Token::Break);
            m.insert("continue", Token::Continue);
            m.insert("return", Token::Return);
            m.insert("true", Token::True);
            m.insert("false", Token::False);
            m.insert("null", Token::Null);
            m.insert("this", Token::This);
            m
        };

        let mut p = Self {
            s: source,
            p: 0,
            scan_pos: Pos { ln: 1, col: 1 },
            cur_pos: Pos { ln: 1, col: 1 },
            cur_tok: Token::Invalid,
            keywords,
        };
        p.consume();

        p
    }

    fn mov_p(&mut self, n: usize) {
        for _ in 0..n {
            if self.s[self.p] == b'\n' {
                self.scan_pos.ln += 1;
                self.scan_pos.col = 1;
            } else {
                self.scan_pos.col += 1;
            }
            self.p += 1;
        }
    }

    fn scan(&mut self, mut accept: impl FnMut(u8) -> bool) -> &'a [u8] {
        let b = self.p;
        while self.p < self.s.len() {
            if !accept(self.s[self.p]) {
                break;
            }
            self.mov_p(1);
        }
        &self.s[b..self.p]
    }

    fn skip_whitespace(&mut self) {
        self.scan(|c| c.is_ascii_whitespace());
    }

    fn next_line_comment(&mut self) -> Token {
        let b = self.p;
        self.mov_p(2);
        while self.p < self.s.len() {
            let c = self.s[self.p];
            self.mov_p(1);
            if c == b'\n' {
                break;
            }
        }
        Token::Comment(b, self.p)
    }

    fn next_block_comment(&mut self) -> Token {
        let b = self.p;
        self.mov_p(2);
        let mut prev = 0u8;
        while self.p < self.s.len() {
            let c = self.s[self.p];
            self.mov_p(1);
            if prev == b'*' && c == b'/' {
                break;
            }
            prev = c;
        }
        Token::Comment(b, self.p)
    }

    fn decode_escape_char(&mut self, c: u8) -> (u8, usize) {
        match c {
            b't' => (b'\t', 1),
            b'r' => (b'\r', 1),
            b'n' => (b'\n', 1),
            b'\'' => (b'\'', 1),
            b'"' => (b'"', 1),
            b'\\' => (b'\\', 1),
            b'0' => (b'\0', 1),
            b'x' => {
                let mut ch = 0u8;
                let width = 3;
                if self.p + width > self.s.len() {
                    fatal_pos!(self.scan_pos, "unexpected EOF in escape sequence");
                }
                for j in 1..3 {
                    let x = self.s[self.p + j];
                    match x {
                        b'0'..=b'9' => ch = (ch << 4) + (x - b'0'),
                        b'A'..=b'F' | b'a'..=b'f' => ch = (ch << 4) + 9 + x & 0xf,
                        _ => fatal_pos!(
                            self.cur_pos,
                            "invalid char in numeric escape: {}",
                            x as char
                        ),
                    }
                }
                (ch, width)
            }
            _ => fatal_pos!(self.scan_pos, "invalid escape sequence: \\{}", c as char),
        }
    }

    fn next_char(&mut self) -> Token {
        self.mov_p(1);
        let c = match self.s[self.p] {
            b'\n' => fatal_pos!(self.scan_pos, "unterminated character"),
            b'\'' => fatal_pos!(self.scan_pos, "empty character "),
            b'\\' => {
                self.mov_p(1);
                let (ch, width) = self.decode_escape_char(self.s[self.p]);
                self.mov_p(width);
                ch
            }
            c => c,
        };
        if self.s[self.p] != b'\'' {
            fatal_pos!(self.scan_pos, "unterminated character");
        }
        self.mov_p(1);
        Token::CharLit(c)
    }

    fn next_string(&mut self) -> Token {
        let mut str = Vec::new();
        self.mov_p(1);
        let mut escape = false;
        loop {
            if self.p >= self.s.len() {
                fatal_pos!(self.scan_pos, "unterminated string");
            }
            let c = self.s[self.p];
            if c == b'\n' {
                fatal_pos!(self.scan_pos, "unterminated string");
            }
            if escape {
                let (ch, width) = self.decode_escape_char(c);
                str.push(ch);
                self.mov_p(width);
                escape = false;
            } else if c == b'"' {
                break;
            } else if c == b'\\' {
                escape = true;
                self.mov_p(1);
            } else {
                let width = core::str::utf8_char_width(c);
                if width == 0 {
                    fatal_pos!(self.scan_pos, "invalid char \\x{:x} in string", c);
                }
                if self.p + width > self.s.len() {
                    fatal_pos!(self.scan_pos, "unterminated string");
                }
                str.extend_from_slice(&self.s[self.p..self.p + width]);
                self.mov_p(width);
            }
        }
        self.mov_p(1);
        Token::StringLit(String::from_utf8(str).expect("utf-8 string"))
    }

    fn next_int(&mut self) -> Token {
        Token::IntLit(
            std::str::from_utf8(self.scan(|c| c.is_ascii_digit()))
                .unwrap()
                .parse()
                .expect("integer"),
        )
    }

    fn next_ident_or_keyword(&mut self) -> Token {
        let s = std::str::from_utf8(self.scan(|c| c.is_ascii_alphanumeric() || c == b'_'))
            .expect("utf-8 string");
        self.keywords
            .get(s)
            .map(|kw| kw.clone())
            .unwrap_or_else(|| Token::Ident(s.to_string()))
    }

    fn next_symbol(&mut self, sym: Token) -> Token {
        self.mov_p(1);
        sym
    }

    fn next_token(&mut self) -> Token {
        self.skip_whitespace();
        self.cur_pos = self.scan_pos;
        self.s
            .get(self.p)
            .map(|&c| match c {
                b'(' => self.next_symbol(Token::LParen),
                b')' => self.next_symbol(Token::RParen),
                b'[' => self.next_symbol(Token::LBrack),
                b']' => self.next_symbol(Token::RBrack),
                b'{' => self.next_symbol(Token::LBrace),
                b'}' => self.next_symbol(Token::RBrace),
                b',' => self.next_symbol(Token::Comma),
                b';' => self.next_symbol(Token::Semicolon),
                b'=' => self.next_symbol(Token::Equal),
                b'.' => self.next_symbol(Token::Period),
                b'+' => self.next_symbol(Token::Plus),
                b'-' => self.next_symbol(Token::Minus),
                b'*' => self.next_symbol(Token::Asterisk),
                b'/' => match self.s.get(self.p + 1) {
                    Some(b'/') => self.next_line_comment(),
                    Some(b'*') => self.next_block_comment(),
                    _ => self.next_symbol(Token::Slash),
                },
                b'&' => self.next_symbol(Token::And),
                b'|' => self.next_symbol(Token::Or),
                b'~' => self.next_symbol(Token::Tilde),
                b'<' => self.next_symbol(Token::Lt),
                b'>' => self.next_symbol(Token::Gt),
                b'"' => self.next_string(),
                b'\'' => self.next_char(),
                c if c.is_ascii_digit() => self.next_int(),
                c if c.is_ascii_alphabetic() || c == b'_' => self.next_ident_or_keyword(),
                _ => fatal_pos!(self.scan_pos, "unexpected char {}", c as char),
            })
            .unwrap_or(Token::EOF)
    }

    fn consume(&mut self) -> Token {
        loop {
            let next_tok = self.next_token();
            if matches!(next_tok, Token::Comment(_, _)) {
                continue;
            }
            break mem::replace(&mut self.cur_tok, next_tok);
        }
    }

    fn want(&mut self, tok: &Token) -> bool {
        if &self.cur_tok == tok {
            self.consume();
            true
        } else {
            false
        }
    }

    fn expect(&mut self, accept: impl FnOnce(&Token) -> bool) -> Token {
        if accept(&self.cur_tok) {
            self.consume()
        } else {
            fatal_pos!(self.cur_pos, "unexpected token {}", self.cur_tok.as_str())
        }
    }

    fn expect_token(&mut self, accept: &Token) {
        self.expect(|tok| tok == accept);
    }

    fn expect_ident(&mut self) -> String {
        self.expect(|tok| matches!(tok, Token::Ident(_)))
            .into_string()
    }

    fn primary(&mut self) -> Expr {
        let lead = self.expect(|tok| {
            matches!(
                tok,
                Token::IntLit(_)
                    | Token::StringLit(_)
                    | Token::True
                    | Token::False
                    | Token::Null
                    | Token::This
                    | Token::Ident(_)
                    | Token::LParen
            )
        });
        let mut primary = match lead {
            Token::IntLit(n) => Expr::Int(n),
            Token::StringLit(s) => Expr::String(s),
            Token::This | Token::True | Token::False | Token::Null => {
                Expr::VarRef(lead.as_str().to_string())
            }
            Token::Ident(name) => Expr::VarRef(name),
            Token::LParen => {
                let sub_expr = self.expr();
                self.expect_token(&Token::RParen);
                sub_expr
            }
            _ => unreachable!(),
        };

        loop {
            match self.cur_tok {
                Token::Period => {
                    self.consume();
                    primary = Expr::Member(Box::new(primary), self.expect_ident());
                }
                Token::LBrack => {
                    self.consume();
                    let idx = self.expr();
                    self.expect_token(&Token::RBrack);
                    primary = Expr::Index(Box::new(primary), Box::new(idx));
                }
                Token::LParen => {
                    self.consume();
                    let mut args = Vec::new();
                    loop {
                        if self.want(&Token::RParen) {
                            break;
                        }
                        if !args.is_empty() {
                            self.expect_token(&Token::Comma);
                        }
                        args.push(self.expr());
                    }
                    primary = Expr::Call(Box::new(primary), args);
                }
                _ => break primary,
            }
        }
    }

    fn unary(&mut self) -> Expr {
        match self.cur_tok {
            Token::Tilde | Token::Minus => {
                let op = self.consume();
                Expr::Unary(op, Box::new(self.unary()))
            }
            _ => self.primary(),
        }
    }

    fn mul_expr(&mut self) -> Expr {
        binary_expr!(self, Token::Asterisk | Token::Slash, unary)
    }

    fn add_expr(&mut self) -> Expr {
        binary_expr!(self, Token::Plus | Token::Minus, mul_expr)
    }

    fn rel_expr(&mut self) -> Expr {
        binary_expr!(self, Token::Lt | Token::Gt, add_expr)
    }

    fn eq_expr(&mut self) -> Expr {
        binary_expr!(self, Token::Equal, rel_expr)
    }

    fn logic_expr(&mut self) -> Expr {
        binary_expr!(self, Token::And | Token::Or, eq_expr)
    }

    fn expr(&mut self) -> Expr {
        self.logic_expr()
    }

    fn block(&mut self) -> Statement {
        self.expect_token(&Token::LBrace);

        let mut stmts = Vec::new();

        loop {
            if self.want(&Token::RBrace) {
                break;
            }
            stmts.push(self.stmt());
        }

        Statement::Block(stmts)
    }

    fn if_stmt(&mut self) -> Statement {
        self.expect_token(&Token::If);
        self.expect_token(&Token::LParen);
        let cond = self.expr();
        self.expect_token(&Token::RParen);
        let then = Box::new(self.block());
        let els = self.want(&Token::Else).then(|| match self.cur_tok {
            Token::If => Box::new(self.if_stmt()),
            Token::LBrace => Box::new(self.block()),
            _ => fatal_pos!(self.cur_pos, "unexpected token {}", self.cur_tok.as_str()),
        });

        Statement::If(cond, then, els)
    }

    fn while_stmt(&mut self) -> Statement {
        self.expect_token(&Token::While);
        self.expect_token(&Token::LParen);
        let cond = self.expr();
        self.expect_token(&Token::RParen);
        let body = Box::new(self.block());

        Statement::While(cond, body)
    }

    fn loop_flow_stmt(&mut self) -> Statement {
        Statement::LoopFlow(self.expect(|tok| matches!(tok, Token::Break | Token::Continue)))
    }

    fn var_stmt(&mut self) -> Statement {
        self.expect_token(&Token::Var);
        let ty = self.type_ref();
        let mut defs = Vec::new();
        loop {
            defs.push(VarDef {
                name: self.expect_ident(),
                ty: ty.clone(),
            });
            let tok = self.expect(|tok| matches!(tok, Token::Comma | Token::Semicolon));
            if matches!(tok, Token::Semicolon) {
                break;
            }
        }

        Statement::Var(defs)
    }

    fn let_stmt(&mut self) -> Statement {
        self.expect_token(&Token::Let);
        let l_val = self.primary();
        self.expect_token(&Token::Equal);
        let expr = self.expr();
        self.expect_token(&Token::Semicolon);

        Statement::Let(l_val, expr)
    }

    fn do_stmt(&mut self) -> Statement {
        self.expect_token(&Token::Do);
        let expr = self.expr();
        let (l_val, expr) = if self.want(&Token::Equal) {
            (Some(expr), self.expr())
        } else {
            (None, expr)
        };
        self.expect_token(&Token::Semicolon);

        Statement::Do(l_val, expr)
    }

    fn return_stmt(&mut self) -> Statement {
        self.expect_token(&Token::Return);
        let ret = if !self.want(&Token::Semicolon) {
            let ret = self.expr();
            self.expect_token(&Token::Semicolon);
            Some(ret)
        } else {
            None
        };

        Statement::Return(ret)
    }

    fn stmt(&mut self) -> Statement {
        match self.cur_tok {
            Token::Semicolon => {
                self.consume();
                Statement::Empty
            }
            Token::LBrace => self.block(),
            Token::If => self.if_stmt(),
            Token::While => self.while_stmt(),
            Token::Var => self.var_stmt(),
            Token::Let => self.let_stmt(),
            Token::Do => self.do_stmt(),
            Token::Break | Token::Continue => self.loop_flow_stmt(),
            Token::Return => self.return_stmt(),
            _ => {
                let expr = self.expr();
                self.expect_token(&Token::Semicolon);
                Statement::Expr(expr)
            }
        }
    }

    fn type_ref(&mut self) -> TypeRef {
        let ty_name = self.expect(|tok| {
            matches!(
                tok,
                Token::Int | Token::Boolean | Token::Char | Token::Void | Token::Ident(_)
            )
        });
        match ty_name {
            Token::Int | Token::Boolean | Token::Char | Token::Void => TypeRef::Primitive(ty_name),
            Token::Ident(name) => TypeRef::Named(name),
            _ => unreachable!(),
        }
    }

    fn static_decl(&mut self) -> VarDef {
        self.expect_token(&Token::Static);
        let ty = self.type_ref();
        let name = self.expect_ident();
        self.expect_token(&Token::Semicolon);

        VarDef { name, ty }
    }

    fn field_decl(&mut self) -> VarDef {
        self.expect_token(&Token::Field);
        let ty = self.type_ref();
        let name = self.expect_ident();
        self.expect_token(&Token::Semicolon);

        VarDef { name, ty }
    }

    fn subroutine_decl(&mut self) -> SubroutineDecl {
        let lead =
            self.expect(|tok| matches!(tok, Token::Constructor | Token::Function | Token::Method));

        let ret_ty = self.type_ref();
        let sub_name = self.expect_ident();
        let mut args = Vec::new();
        self.expect_token(&Token::LParen);
        loop {
            if self.want(&Token::RParen) {
                break;
            }
            if !args.is_empty() {
                self.expect_token(&Token::Comma);
            }
            let ty = self.type_ref();
            let name = self.expect_ident();
            args.push(VarDef { name, ty });
        }
        let body = self.block();

        SubroutineDecl {
            lead,
            name: sub_name,
            ret_ty,
            args,
            body,
        }
    }

    fn class_decl(&mut self) -> ClassDecl {
        self.expect_token(&Token::Class);
        let class_name = self.expect_ident();
        self.expect_token(&Token::LBrace);

        let mut class = ClassDecl {
            name: class_name,
            fields: Vec::new(),
            statics: Vec::new(),
            subroutines: Vec::new(),
        };

        loop {
            match &self.cur_tok {
                Token::RBrace => {
                    self.consume();
                    break;
                }
                Token::Static => class.statics.push(self.static_decl()),
                Token::Field => class.fields.push(self.field_decl()),
                Token::Constructor | Token::Function | Token::Method => {
                    class.subroutines.push(self.subroutine_decl())
                }
                _ => fatal_pos!(self.cur_pos, "unexpected token {}", self.cur_tok.as_str()),
            }
        }

        class
    }

    pub fn top_level(&mut self) -> Option<ClassDecl> {
        loop {
            match &self.cur_tok {
                Token::EOF => return None,
                Token::Class => return Some(self.class_decl()),
                tok => fatal_pos!(self.cur_pos, "unexpected token {}", tok.as_str()),
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    static EXAMPLE_1: &'static [u8] = br#"
//
// line comment
/**/
/* block comment */
/*
multi block comment
*/
class List {

    field int data;
    field List next;

    static int nodeCount;

    constructor Point new(int data, List next) {
        let data = data;
        let next = next;
        let nodeCount = nodeCount + 1;
        return this;
    }

    method int getData() {
        return data;
    }

    method List getNext() {
        return next;
    }

    method boolean isZero() {
        if (this.data = 0) {
            return true;
        } else {
            return false;
        }
    }

    method void print() {
        var List current;
        let current = this;
        while (~(current = null)) {
            do Output.printInt(current.getData());
            do Output.printChar(32);
            do current = current.getNext();
        }
        return;
    }

    function int getNodeCount() {
        return nodeCount;
    }

    function Array readNodeCount() {
        var Array a;
        var int i;
        let a = Array.new(nodeCount);
        let i = 0;
        while (i < nodeCount) {
            let a[i] = Keyboard.readInt("Enter\na\x20\"Number\":");
            let i = i + 1;
        }
        return a;
    }
}
"#;

    #[test]
    fn test_lex() {
        let mut parser = Parser::new(EXAMPLE_1);
        loop {
            let tok = parser.next_token();
            let pos = &parser.cur_pos;
            print!("{}:{} ", pos.ln, pos.col);
            match &tok {
                Token::Comment(b, e) => {
                    println!("{}", std::str::from_utf8(&EXAMPLE_1[*b..*e]).unwrap())
                }
                Token::Ident(id) => println!("{}", id),
                Token::IntLit(n) => println!("{}", n),
                Token::StringLit(s) => println!("{}", s),
                Token::Invalid => panic!("unexpected token"),
                Token::EOF => println!("<EOF>"),
                tok => println!("{}", tok.as_str()),
            }
            if matches!(tok, Token::EOF) {
                break;
            }
        }
    }

    #[test]
    fn test_parser() {
        let mut parser = Parser::new(EXAMPLE_1);

        let _ = parser.top_level();
    }
}
