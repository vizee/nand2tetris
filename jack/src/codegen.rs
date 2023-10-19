use vmtranslator::{Instruction, Segment};

use crate::parser::Token;
use crate::semantics::*;

const ALLOC_FUNC: &'static str = "Memory.alloc";

pub struct Generator {
    instrs: Vec<Instruction>,
    cls_name: String,
}

impl Generator {
    pub fn new() -> Self {
        Self {
            instrs: Vec::new(),
            cls_name: String::new(),
        }
    }

    fn i(&mut self, instr: Instruction) {
        self.instrs.push(instr);
    }

    fn push(&mut self, seg: Segment, v: u32) {
        self.i(Instruction::Push(seg, v))
    }

    fn pop(&mut self, seg: Segment, v: u32) {
        self.i(Instruction::Pop(seg, v))
    }

    fn pop_discard(&mut self) {
        self.i(Instruction::Pop(Segment::Temp, 7));
    }

    fn call(&mut self, fn_name: String, args: usize) {
        self.i(Instruction::Call(fn_name, args as u32))
    }

    fn new_object(&mut self, ty: &Type) {
        self.push(Segment::Constant, ty.as_class().fields.len() as u32);
        self.call(ALLOC_FUNC.to_string(), 1);
    }

    fn label(&mut self, label: String) {
        self.i(Instruction::Label(label));
    }

    fn goto(&mut self, label: String) {
        self.i(Instruction::Goto(label));
    }

    fn if_goto(&mut self, label: String) {
        self.i(Instruction::IfGoto(label));
    }

    fn gen_string(&mut self, label: usize, len: usize, ty: &Type) {
        self.new_object(ty);
        self.call(format!("String.{}.{}", self.cls_name, label), 0);
        self.push(Segment::Constant, len as u32);
        self.call("String.new".to_string(), 3)
    }

    fn try_eval_expr(&mut self, expr: &Node) -> Option<u32> {
        match &expr.kind {
            NodeKind::Int(v) => Some(*v),
            NodeKind::Unary(op, expr) => self.try_eval_expr(expr).map(|v| match op {
                Token::Minus => v.wrapping_neg(),
                Token::Tilde => !v,
                _ => unreachable!(),
            }),
            NodeKind::Binary(op, lhs, rhs) => self
                .try_eval_expr(lhs)
                .and_then(|lhs| self.try_eval_expr(rhs).map(|rhs| (lhs, rhs)))
                .map(|(lhs, rhs)| match op {
                    Token::Plus => lhs.wrapping_add(rhs),
                    Token::Minus => lhs.wrapping_sub(rhs),
                    Token::Asterisk => lhs.wrapping_mul(rhs),
                    Token::Slash => lhs.wrapping_div(rhs),
                    Token::And => lhs & rhs,
                    Token::Or => lhs | rhs,
                    _ => unreachable!(),
                }),
            _ => None,
        }
    }

    fn gen_call(&mut self, sub: &Node, args: &[Box<Node>]) {
        let (obj, sub) = match &sub.kind {
            NodeKind::Subroutine(obj, sub) => (obj, sub),
            _ => unreachable!(),
        };
        let fn_name = format!("{}.{}", obj.ty.as_class().name, sub.name);
        let arg_base = match sub.kind {
            SubKind::Func => 0,
            SubKind::Ctor => {
                self.new_object(&obj.ty);
                1
            }
            SubKind::Method => {
                match &obj.kind {
                    NodeKind::VarRef(var) if var.kind == VarKind::This => {
                        self.push(Segment::Pointer, 0)
                    }
                    _ => self.push_expr(obj),
                }
                1
            }
        };
        for arg in args {
            self.push_expr(arg);
        }
        self.call(fn_name, arg_base + args.len());
    }

    fn push_expr(&mut self, expr: &Node) {
        match &expr.kind {
            NodeKind::Int(v) => self.push(Segment::Constant, *v),
            NodeKind::String(label, len) => self.gen_string(*label, *len, &expr.ty),
            NodeKind::VarRef(var) => match var.kind {
                VarKind::Const => self.push(Segment::Constant, var.value),
                VarKind::This => self.push(Segment::Pointer, 0),
                VarKind::Static => self.push(Segment::Static, var.value),
                VarKind::Field => self.push(Segment::This, var.value),
                VarKind::Arg => self.push(Segment::Argument, var.value),
                VarKind::Local => self.push(Segment::Local, var.value),
                _ => unreachable!(),
            },
            NodeKind::Field(primary, field) => match &primary.kind {
                NodeKind::StaticRef => self.push(Segment::Static, field.value),
                NodeKind::VarRef(var) if var.kind == VarKind::This => {
                    self.push(Segment::This, field.value)
                }
                _ => {
                    self.push_expr(primary);
                    self.pop(Segment::Pointer, 1);
                    self.push(Segment::That, field.value);
                }
            },
            NodeKind::Index(primary, index) => {
                self.push_expr(primary);
                if let Some(idx) = self.try_eval_expr(index) {
                    self.pop(Segment::Pointer, 1);
                    self.push(Segment::That, idx);
                } else {
                    self.push_expr(index);
                    self.i(Instruction::Add);
                    self.pop(Segment::Pointer, 1);
                    self.push(Segment::That, 0);
                }
            }
            NodeKind::Unary(op, expr) => {
                self.push_expr(expr);
                match op {
                    Token::Minus => self.i(Instruction::Neg),
                    Token::Tilde => self.i(Instruction::Not),
                    _ => unreachable!(),
                }
            }
            NodeKind::Binary(op, lhs, rhs) => {
                self.push_expr(lhs);
                self.push_expr(rhs);
                match op {
                    Token::Plus => self.i(Instruction::Add),
                    Token::Minus => self.i(Instruction::Sub),
                    Token::Asterisk => self.i(Instruction::Call("Math.mul".to_string(), 2)),
                    Token::Slash => self.i(Instruction::Call("Math.div".to_string(), 2)),
                    Token::And => self.i(Instruction::And),
                    Token::Or => self.i(Instruction::Or),
                    Token::Equal => self.i(Instruction::Eq),
                    Token::Gt => self.i(Instruction::Gt),
                    Token::Lt => self.i(Instruction::Lt),
                    _ => unreachable!(),
                }
            }
            NodeKind::Call(primary, args) => self.gen_call(primary, args),
            _ => unreachable!(),
        }
    }

    fn gen_assign(&mut self, lvalue: &Node) {
        match &lvalue.kind {
            NodeKind::VarRef(var) => self.pop(
                match var.kind {
                    VarKind::Static => Segment::Static,
                    VarKind::Field => Segment::This,
                    VarKind::Arg => Segment::Argument,
                    VarKind::Local => Segment::Local,
                    _ => unreachable!(),
                },
                var.value,
            ),
            NodeKind::Field(primary, field) => match &primary.kind {
                NodeKind::StaticRef => self.pop(Segment::Static, field.value),
                NodeKind::VarRef(var) if var.kind == VarKind::This => {
                    self.pop(Segment::This, field.value)
                }
                _ => {
                    self.push_expr(primary);
                    self.pop(Segment::Pointer, 1);
                    self.pop(Segment::That, field.value);
                }
            },
            NodeKind::Index(primary, index) => {
                self.push_expr(primary);
                if let Some(idx) = self.try_eval_expr(index) {
                    self.pop(Segment::Pointer, 1);
                    self.pop(Segment::That, idx);
                } else {
                    self.push_expr(index);
                    self.i(Instruction::Add);
                    self.pop(Segment::Pointer, 1);
                    self.pop(Segment::That, 0);
                }
            }
            _ => unreachable!(),
        }
    }

    fn gen_stmt(&mut self, node: &Node) {
        match &node.kind {
            NodeKind::Nop => {}
            NodeKind::ExprStmt(expr) => {
                self.push_expr(expr);
                self.pop_discard();
            }
            NodeKind::Assign(lhs, rhs) => {
                self.push_expr(rhs);
                self.gen_assign(lhs);
            }
            NodeKind::Block(stmts) => {
                for stmt in stmts {
                    self.gen_stmt(stmt);
                }
            }
            NodeKind::If(cond, then, els, label) => {
                let then_label = format!("L{}.then", label);
                let end_label = format!("L{}.end", label);
                self.push_expr(cond);
                self.if_goto(then_label.clone());
                if let Some(els) = els {
                    self.gen_stmt(els);
                    self.goto(end_label.clone());
                }
                self.label(then_label);
                self.gen_stmt(&then);
                self.label(end_label);
            }
            NodeKind::Loop(cond, body, label) => {
                let begin_label = format!("L{}", label);
                let end_label = format!("L{}.end", label);
                self.label(begin_label.clone());
                self.push_expr(cond);
                self.i(Instruction::Not);
                self.if_goto(end_label.clone());
                self.gen_stmt(body);
                self.goto(begin_label);
                self.label(end_label);
            }
            NodeKind::Goto(label) => self.goto(label.clone()),
            NodeKind::Return(ret) => {
                if let Some(ret) = ret {
                    self.push_expr(ret);
                } else {
                    self.push(Segment::Constant, 0);
                }
                self.i(Instruction::Goto("RETURN".to_string()));
            }
            _ => unreachable!(),
        }
    }

    pub fn gen_subroutine(&mut self, sub: &Subroutine) {
        self.i(Instruction::Function(
            format!("{}.{}", self.cls_name, sub.prototype.name),
            sub.frame_size as u32,
        ));
        if sub.prototype.kind != SubKind::Func {
            self.push(Segment::Pointer, 0);
            self.push(Segment::Argument, 0);
            self.pop(Segment::Pointer, 0);
            self.pop(Segment::Argument, 0);
        }
        self.gen_stmt(&sub.body);
        self.push(Segment::Constant, 0);
        self.label("RETURN".to_string());
        if sub.prototype.kind != SubKind::Func {
            self.push(Segment::Argument, 0);
            self.pop(Segment::Pointer, 0);
        }
        self.i(Instruction::Return)
    }

    fn gen_string_lit(&mut self, label: usize, s: String) {
        self.i(Instruction::Function(
            format!("String.{}.{}", self.cls_name, label),
            0,
        ));
        self.push(Segment::Constant, s.len() as u32);
        self.call(ALLOC_FUNC.to_string(), 1);
        self.pop(Segment::Pointer, 1);
        for (i, &c) in s.as_bytes().iter().enumerate() {
            self.push(Segment::Constant, c as u32);
            self.pop(Segment::That, i as u32);
        }
        self.push(Segment::Pointer, 1);
        self.i(Instruction::Return);
    }

    pub fn gen_class(&mut self, cls: Class) {
        self.cls_name = cls.ty.as_class().name.clone();
        for sub in cls.subs {
            self.gen_subroutine(&sub);
        }
        for (label, string_lit) in cls.string_lits {
            self.gen_string_lit(label, string_lit);
        }
    }

    pub fn instructions(&self) -> &[Instruction] {
        &self.instrs
    }
}
