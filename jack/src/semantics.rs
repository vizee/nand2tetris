use std::collections::HashMap;
use std::mem;
use std::rc::Rc;

use crate::parser::*;

const TRUE: u32 = u32::MAX;
const FALSE: u32 = 0;
const ZERO: u32 = 0;

#[derive(PartialEq, Eq, Clone, Copy)]
pub enum SubKind {
    Func,
    Ctor,
    Method,
}

pub struct SubPrototype {
    pub name: String,
    pub kind: SubKind,
    pub ret: Type,
    pub args: Vec<Rc<Var>>,
}

pub struct ClassType {
    pub name: String,
    pub fields: HashMap<String, Rc<Var>>,
    pub subs: HashMap<String, Rc<SubPrototype>>,
}

#[derive(Clone)]
pub enum Type {
    Void,
    Int,
    Char,
    Boolean,
    Null,
    Class(Rc<ClassType>),
    Subroutine(Rc<SubPrototype>),
}

impl Type {
    fn as_str(&self) -> &str {
        match self {
            Type::Void => "void",
            Type::Int => "int",
            Type::Char => "char",
            Type::Boolean => "boolean",
            Type::Null => "null",
            Type::Subroutine(sub) => match sub.kind {
                SubKind::Func => "function",
                SubKind::Ctor => "constructor",
                SubKind::Method => "method",
            },
            Type::Class(cls) => cls.name.as_str(),
        }
    }

    pub fn as_class(&self) -> &Rc<ClassType> {
        if let Type::Class(cls_ty) = self {
            cls_ty
        } else {
            fatal!("type {} is not a class", self.as_str())
        }
    }

    pub fn as_sub(&self) -> &Rc<SubPrototype> {
        if let Type::Subroutine(sub) = self {
            sub
        } else {
            fatal!("type {} is not a class", self.as_str())
        }
    }

    fn is_bool(&self) -> bool {
        matches!(self, Type::Boolean)
    }

    fn is_int(&self) -> bool {
        matches!(self, Type::Int)
    }

    fn is_comparable(&self) -> bool {
        matches!(
            self,
            Type::Int | Type::Char | Type::Boolean | Type::Class(_)
        )
    }

    fn is_ordered(&self) -> bool {
        matches!(self, Type::Int | Type::Char)
    }

    fn is_match(&self, other: &Type) -> bool {
        match (self, other) {
            (Type::Int, Type::Int) | (Type::Char, Type::Char) | (Type::Boolean, Type::Boolean) => {
                true
            }
            (Type::Class(a), Type::Class(b)) => a.name == b.name,
            _ => false,
        }
    }
}

fn type_ref_name(ty: &TypeRef) -> &str {
    match ty {
        TypeRef::Primitive(tok) => tok.as_str(),
        TypeRef::Named(name) => &name,
    }
}

pub enum NodeKind {
    Nop,
    StaticRef,
    Int(u32),
    String(usize, usize),
    Subroutine(Box<Node>, Rc<SubPrototype>),
    Field(Box<Node>, Rc<Var>),
    VarRef(Rc<Var>),
    Index(Box<Node>, Box<Node>),
    Unary(Token, Box<Node>),
    Binary(Token, Box<Node>, Box<Node>),
    Call(Box<Node>, Vec<Box<Node>>),
    ExprStmt(Box<Node>),
    Assign(Box<Node>, Box<Node>),
    Block(Vec<Box<Node>>),
    If(Box<Node>, Box<Node>, Option<Box<Node>>, usize),
    Loop(Box<Node>, Box<Node>, usize),
    Goto(String),
    Return(Option<Box<Node>>),
}

pub struct Node {
    pub kind: NodeKind,
    pub ty: Type,
}

impl Node {
    fn stmt(kind: NodeKind) -> Self {
        Self {
            kind,
            ty: Type::Void,
        }
    }

    fn expr(kind: NodeKind, ty: Type) -> Self {
        Self { kind, ty }
    }

    fn var(var: Rc<Var>) -> Self {
        let ty = var.ty.clone();
        Self {
            kind: NodeKind::VarRef(var),
            ty,
        }
    }

    fn field(primary: Box<Node>, member: Rc<Var>) -> Self {
        let ty = member.ty.clone();
        Self {
            kind: NodeKind::Field(primary, member),
            ty,
        }
    }

    fn subroutine(primary: Box<Node>, sub: Rc<SubPrototype>) -> Self {
        let ty = Type::Subroutine(sub.clone());
        Self {
            kind: NodeKind::Subroutine(primary, sub),
            ty,
        }
    }
}

#[derive(PartialEq, Eq)]
pub enum VarKind {
    Const,
    This,
    Subroutine,
    Static,
    Field,
    Arg,
    Local,
}

pub struct Var {
    pub name: String,
    pub kind: VarKind,
    pub ty: Type,
    pub value: u32,
}

pub struct Subroutine {
    pub prototype: Rc<SubPrototype>,
    pub frame_size: usize,
    pub body: Node,
}

pub struct Class {
    pub ty: Type,
    pub statics: Vec<Rc<Var>>,
    pub fields: Vec<Rc<Var>>,
    pub subs: Vec<Rc<Subroutine>>,
    pub string_lits: Vec<(usize, String)>,
}

struct Scope {
    vars: HashMap<String, Rc<Var>>,
    outer: Option<Box<Scope>>,
}

impl Scope {
    fn new(outer: Option<Box<Scope>>) -> Self {
        Self {
            vars: HashMap::new(),
            outer,
        }
    }

    fn define(&mut self, var: Rc<Var>) {
        if self.vars.contains_key(&var.name) {
            fatal!("{} defined", var.name);
        }
        let is_subroutine = self
            .find(&var.name)
            .map(|v| matches!(v.ty, Type::Subroutine(_)))
            .unwrap_or(false);
        if is_subroutine {
            fatal!("{} subroutine defined", var.name);
        }
        self.vars.insert(var.name.clone(), var);
    }

    fn find(&self, name: &str) -> Option<&Rc<Var>> {
        self.vars
            .get(name)
            .or_else(|| self.outer.as_ref().and_then(|outer| outer.find(name)))
    }
}

pub struct Analyzer {
    types: HashMap<String, Type>,
    scope: Box<Scope>,
    labels: usize,
    string_lits: Vec<(usize, String)>,
    this: Option<Rc<Var>>,
    cur_sub: Option<Rc<SubPrototype>>,
    loop_label: Option<usize>,
    frame_size: usize,
}

impl Analyzer {
    pub fn new() -> Self {
        let types = {
            let mut t = HashMap::new();
            t.insert("void".to_string(), Type::Void);
            t.insert("int".to_string(), Type::Int);
            t.insert("char".to_string(), Type::Char);
            t.insert("boolean".to_string(), Type::Boolean);
            t
        };
        let scope = {
            let mut s = Box::new(Scope::new(None));
            s.define(Rc::new(Var {
                name: "true".to_string(),
                kind: VarKind::Const,
                ty: Type::Boolean,
                value: TRUE,
            }));
            s.define(Rc::new(Var {
                name: "false".to_string(),
                kind: VarKind::Const,
                ty: Type::Boolean,
                value: FALSE,
            }));
            s.define(Rc::new(Var {
                name: "null".to_string(),
                kind: VarKind::Const,
                ty: Type::Null,
                value: ZERO,
            }));
            s
        };

        Self {
            types,
            scope,
            labels: 0,
            string_lits: Vec::new(),
            this: None,
            cur_sub: None,
            loop_label: None,
            frame_size: 0,
        }
    }

    fn get_type(&self, name: &str) -> Type {
        if let Some(ty) = self.types.get(name) {
            ty.clone()
        } else {
            fatal!("type {} undeclared", name)
        }
    }

    fn var_def(&self, var_def: &VarDef, kind: VarKind, offset: usize) -> Var {
        if matches!(var_def.ty, TypeRef::Primitive(Token::Void)) {
            fatal!("cannot declare variable {} of void", var_def.name);
        }
        Var {
            name: var_def.name.clone(),
            kind,
            ty: self.get_type(type_ref_name(&var_def.ty)),
            value: offset as u32,
        }
    }

    fn prototype(&self, sub_decl: &SubroutineDecl) -> SubPrototype {
        let (kind, arg_base) = match sub_decl.lead {
            Token::Function => (SubKind::Func, 0),
            Token::Constructor => (SubKind::Ctor, 1),
            Token::Method => (SubKind::Method, 1),
            _ => unreachable!(),
        };
        SubPrototype {
            name: sub_decl.name.clone(),
            kind,
            ret: self.get_type(type_ref_name(&sub_decl.ret_ty)),
            args: sub_decl
                .args
                .iter()
                .enumerate()
                .map(|(idx, arg)| Rc::new(self.var_def(arg, VarKind::Arg, arg_base + idx)))
                .collect(),
        }
    }

    fn gen_label(&mut self) -> usize {
        self.labels += 1;
        self.labels
    }

    fn enter_scope(&mut self) {
        let outer = mem::replace(&mut self.scope, Box::new(Scope::new(None)));
        self.scope.outer = Some(outer);
    }

    fn leave_scope(&mut self) {
        let outer = mem::replace(&mut self.scope.outer, None);
        self.scope = outer.unwrap();
    }

    fn add_local_var(&mut self, var: VarDef, offset: usize) {
        let var = Rc::new(self.var_def(&var, VarKind::Local, offset));
        self.scope.define(var);
    }

    fn check_assign(&self, rvalue: &Node, lty: &Type) -> bool {
        rvalue.ty.is_match(lty)
            || matches!((&rvalue.ty, lty), (Type::Null, Type::Class(_)))
            || matches!((&rvalue.ty, lty), (Type::Int, Type::Class(_)))
    }

    fn check_lvalue(&self, lvalue: &Node) {
        let allow = if let NodeKind::VarRef(ref var) = lvalue.kind {
            matches!(
                var.kind,
                VarKind::Static | VarKind::Field | VarKind::Arg | VarKind::Local
            )
        } else {
            matches!(lvalue.kind, NodeKind::Index(_, _) | NodeKind::Field(_, _))
        };
        if !allow {
            fatal!("illegal left value");
        }
    }

    fn check_cond(&self, cond: &Node) {
        if !cond.ty.is_bool() && !cond.ty.is_int() {
            fatal!("unexpected condition expression type");
        }
    }

    fn check_access_var(&self, var: &Var) {
        let cur_sub = self.cur_sub.as_ref().unwrap();
        let allow = if cur_sub.kind == SubKind::Func {
            match var.kind {
                VarKind::This => false,
                VarKind::Field => false,
                VarKind::Subroutine => var.ty.as_sub().kind != SubKind::Method,
                _ => true,
            }
        } else {
            true
        };
        if !allow {
            fatal!("cannot access {} in function {}", var.name, cur_sub.name);
        }
    }

    fn check_member(&self, member: &Node) {
        let allow = match &member.kind {
            NodeKind::Field(primary, field) => {
                if matches!(primary.kind, NodeKind::StaticRef) {
                    Rc::ptr_eq(
                        primary.ty.as_class(),
                        self.this.as_ref().unwrap().ty.as_class(),
                    ) && field.kind == VarKind::Static
                } else {
                    field.kind == VarKind::Field
                }
            }
            NodeKind::Subroutine(primary, sub) => {
                if matches!(primary.kind, NodeKind::StaticRef) {
                    sub.kind != SubKind::Method
                } else {
                    sub.kind == SubKind::Method
                }
            }
            _ => unreachable!(),
        };

        if !allow {
            let (cls, member) = match &member.kind {
                NodeKind::Subroutine(cls, sub) => (&cls.ty.as_class().name, &sub.name),
                NodeKind::Field(cls, var) => (&cls.ty.as_class().name, &var.name),
                _ => unreachable!(),
            };
            fatal!("cannot access member {} of {}", member, cls);
        }
    }

    fn walk_var_ref(&self, name: &str) -> Node {
        let var = self
            .scope
            .find(&name)
            .unwrap_or_else(|| fatal!("variable {} undefined", name));
        self.check_access_var(var);
        match var.kind {
            VarKind::Subroutine => {
                let this = self.this.as_ref().unwrap();
                let sub = var.ty.as_sub();
                let obj = match sub.kind {
                    SubKind::Func => Box::new(Node::expr(NodeKind::StaticRef, this.ty.clone())),
                    SubKind::Method => Box::new(Node::var(this.clone())),
                    _ => unreachable!(),
                };
                Node::subroutine(obj, sub.clone())
            }
            _ => Node::var(var.to_owned()),
        }
    }

    fn walk_unary(&mut self, op: Token, expr: Expr) -> Node {
        let expr = Box::new(self.walk_expr(expr));
        let ty = &expr.ty;
        let allow = match op {
            Token::Minus => ty.is_int(),
            Token::Tilde => ty.is_bool() || ty.is_int(),
            _ => unreachable!(),
        };
        if !allow {
            fatal!("illegal unary expression {} {}", op.as_str(), ty.as_str());
        }

        let ty = ty.clone();
        Node::expr(NodeKind::Unary(op, expr), ty)
    }

    fn walk_binary(&mut self, op: Token, lhs: Expr, rhs: Expr) -> Node {
        let lhs = Box::new(self.walk_expr(lhs));
        let rhs = Box::new(self.walk_expr(rhs));
        let lhs_ty = &lhs.ty;
        let (allow, res_ty) = match op {
            Token::Plus => (lhs_ty.is_int(), lhs_ty.clone()),
            Token::Minus => (lhs_ty.is_int(), lhs_ty.clone()),
            Token::Asterisk => (lhs_ty.is_int(), lhs_ty.clone()),
            Token::Slash => (lhs_ty.is_int(), lhs_ty.clone()),
            Token::And => (lhs_ty.is_int() || lhs_ty.is_bool(), lhs_ty.clone()),
            Token::Or => (lhs_ty.is_int() || lhs_ty.is_bool(), lhs_ty.clone()),
            Token::Equal => (lhs_ty.is_comparable(), Type::Boolean),
            Token::Lt => (lhs_ty.is_ordered(), Type::Boolean),
            Token::Gt => (lhs_ty.is_ordered(), Type::Boolean),
            _ => unreachable!(),
        };
        if !allow || !lhs_ty.is_match(&rhs.ty) {
            fatal!(
                "illegal binary expression {} {} {}",
                lhs_ty.as_str(),
                op.as_str(),
                rhs.ty.as_str()
            );
        }

        Node::expr(NodeKind::Binary(op, lhs, rhs), res_ty)
    }

    fn walk_member(&mut self, primary: Expr, member: String) -> Node {
        let primary = match primary {
            Expr::VarRef(ref name) => {
                if self.scope.find(&name).is_some() {
                    Box::new(self.walk_var_ref(&name))
                } else if let Some(ty) = self.types.get(name) {
                    Box::new(Node::expr(NodeKind::StaticRef, ty.clone()))
                } else {
                    fatal!("type or variable {} undefined", name)
                }
            }
            _ => Box::new(self.walk_expr(primary)),
        };
        let pty = primary.ty.as_class();
        let member = if let Some(m) = pty.fields.get(&member) {
            let m = m.clone();
            Node::field(primary, m)
        } else if let Some(s) = pty.subs.get(&member) {
            let s = s.clone();
            Node::subroutine(primary, s)
        } else {
            fatal!("no member named {} for {}", member, pty.name)
        };

        self.check_member(&member);

        member
    }

    fn walk_index(&mut self, primary: Expr, index: Expr) -> Node {
        let primary = Box::new(self.walk_expr(primary));
        let index = Box::new(self.walk_expr(index));
        if !index.ty.is_int() {
            fatal!("index must be integer type");
        }
        match primary.ty {
            Type::Class(ref cls) => {
                if cls.name.as_str() == "Array" {
                    let ptr = cls.fields["ptr"].clone();
                    let ary_ptr = Box::new(Node::field(primary, ptr));
                    return Node::expr(NodeKind::Index(ary_ptr, index), self.get_type("int"));
                }
            }
            _ => {}
        }

        fatal!("{} cannot be indexable", primary.ty.as_str());
    }

    fn walk_expr(&mut self, expr: Expr) -> Node {
        match expr {
            Expr::Int(i) => Node::expr(NodeKind::Int(i), self.get_type("int")),
            Expr::String(s) => {
                let label = self.gen_label();
                let len = s.len();
                self.string_lits.push((label, s));
                Node::expr(NodeKind::String(label, len), self.get_type("String"))
            }
            Expr::VarRef(name) => self.walk_var_ref(&name),
            Expr::Call(sub, args) => {
                let sub = Box::new(self.walk_expr(*sub));
                if !matches!(sub.kind, NodeKind::Subroutine(_, _)) {
                    fatal!("primary expression is not a subroutine");
                }
                let sub_ty = sub.ty.as_sub();
                if sub_ty.args.len() != args.len() {
                    fatal!(
                        "expect {} arguments, have {}",
                        sub_ty.args.len(),
                        args.len()
                    );
                }
                let args = args
                    .into_iter()
                    .enumerate()
                    .map(|(idx, arg)| {
                        let arg = Box::new(self.walk_expr(arg));
                        if !self.check_assign(&arg, &sub_ty.args[idx].ty) {
                            fatal!("argument {} type mismatched", idx);
                        }
                        arg
                    })
                    .collect();
                let ret_ty = sub_ty.ret.clone();
                Node::expr(NodeKind::Call(sub, args), ret_ty)
            }
            Expr::Member(primary, member) => self.walk_member(*primary, member),
            Expr::Index(primary, index) => self.walk_index(*primary, *index),
            Expr::Unary(op, expr) => self.walk_unary(op, *expr),
            Expr::Binary(op, lhs, rhs) => self.walk_binary(op, *lhs, *rhs),
        }
    }

    fn stmt_list(&mut self, stmts: Vec<Statement>) -> Vec<Box<Node>> {
        stmts
            .into_iter()
            .filter_map(|stmt| {
                let n = self.walk_stmt(stmt);
                match n.kind {
                    NodeKind::Nop => None,
                    _ => Some(Box::new(n)),
                }
            })
            .collect()
    }

    fn walk_stmt(&mut self, stmt: Statement) -> Node {
        match stmt {
            Statement::Empty => Node::stmt(NodeKind::Nop),
            Statement::Expr(expr) => Node::stmt(NodeKind::ExprStmt(Box::new(self.walk_expr(expr)))),
            Statement::Block(stmts) => {
                self.enter_scope();
                let block = Node::stmt(NodeKind::Block(self.stmt_list(stmts)));
                self.leave_scope();
                block
            }
            Statement::If(cond, then, els) => {
                let label = self.gen_label();
                let cond = Box::new(self.walk_expr(cond));
                self.check_cond(&cond);
                let body = Box::new(self.walk_stmt(*then));
                let els = els.map(|els| Box::new(self.walk_stmt(*els)));
                Node::stmt(NodeKind::If(cond, body, els, label))
            }
            Statement::While(cond, body) => {
                let prev_label = self.loop_label;
                let label = self.gen_label();
                self.loop_label = Some(label);
                let cond = Box::new(self.walk_expr(cond));
                self.check_cond(&cond);
                let body = Box::new(self.walk_stmt(*body));
                self.loop_label = prev_label;
                Node::stmt(NodeKind::Loop(cond, body, label))
            }
            Statement::LoopFlow(tok) => {
                if let Some(label) = self.loop_label {
                    match tok {
                        Token::Continue => Node::stmt(NodeKind::Goto(format!("L{}", label))),
                        Token::Break => Node::stmt(NodeKind::Goto(format!("L{}.end", label))),
                        _ => unreachable!(),
                    }
                } else {
                    fatal!("stray {}", tok.as_str())
                }
            }
            Statement::Var(vars) => {
                for var in vars {
                    self.add_local_var(var, self.frame_size);
                    self.frame_size += 1;
                }
                Node::stmt(NodeKind::Nop)
            }
            Statement::Let(lvalue, expr) => {
                let rvalue = Box::new(self.walk_expr(expr));
                let lvalue = Box::new(self.walk_expr(lvalue));
                self.check_lvalue(&lvalue);
                if !self.check_assign(&rvalue, &lvalue.ty) {
                    fatal!("let assign type mismatched");
                }
                Node::stmt(NodeKind::Assign(lvalue, rvalue))
            }
            Statement::Do(lvalue, expr) => {
                let rvalue = Box::new(self.walk_expr(expr));
                if let Some(lvalue) = lvalue {
                    let lvalue = Box::new(self.walk_expr(lvalue));
                    self.check_lvalue(&lvalue);
                    if !self.check_assign(&rvalue, &lvalue.ty) {
                        fatal!("do assign type mismatched");
                    }
                    Node::stmt(NodeKind::Assign(lvalue, rvalue))
                } else {
                    Node::stmt(NodeKind::ExprStmt(rvalue))
                }
            }
            Statement::Return(expr) => {
                let ret = expr.map(|e| Box::new(self.walk_expr(e)));
                let cur_sub = self.cur_sub.as_ref().unwrap();
                if let Some(ref ret) = ret {
                    if !cur_sub.ret.is_match(&ret.ty) {
                        fatal!("return type mismatched");
                    }
                } else if !matches!(cur_sub.ret, Type::Void) {
                    fatal!("require a return value");
                }
                Node::stmt(NodeKind::Return(ret))
            }
        }
    }

    fn subroutine(&mut self, prototype: &Rc<SubPrototype>, sub_decl: SubroutineDecl) -> Subroutine {
        self.cur_sub = Some(prototype.clone());
        self.frame_size = 0;
        self.enter_scope();
        prototype.args.iter().for_each(|arg| {
            self.scope.define(arg.clone());
        });
        let body = self.walk_stmt(sub_decl.body);
        self.leave_scope();
        self.cur_sub = None;

        Subroutine {
            prototype: prototype.clone(),
            frame_size: self.frame_size,
            body,
        }
    }

    fn class_type(&self, class_decl: &ClassDecl) -> ClassType {
        let mut cls_ty = ClassType {
            name: class_decl.name.clone(),
            fields: HashMap::new(),
            subs: HashMap::new(),
        };

        class_decl
            .statics
            .iter()
            .enumerate()
            .for_each(|(idx, field)| {
                cls_ty.fields.insert(
                    field.name.clone(),
                    Rc::new(self.var_def(field, VarKind::Static, idx)),
                );
            });
        class_decl
            .fields
            .iter()
            .enumerate()
            .for_each(|(idx, field)| {
                cls_ty.fields.insert(
                    field.name.clone(),
                    Rc::new(self.var_def(field, VarKind::Field, idx)),
                );
            });
        class_decl.subroutines.iter().for_each(|s| {
            cls_ty
                .subs
                .insert(s.name.clone(), Rc::new(self.prototype(s)));
        });

        cls_ty
    }

    pub fn analyze_class(&mut self, class_decl: ClassDecl) -> Class {
        if self.types.contains_key(&class_decl.name) {
            fatal!("class {} existed", class_decl.name);
        }

        let mut cls = Class {
            ty: Type::Class(Rc::new(self.class_type(&class_decl))),
            statics: Vec::new(),
            fields: Vec::new(),
            subs: Vec::new(),
            string_lits: Vec::new(),
        };

        let cls_ty = cls.ty.as_class();
        self.enter_scope();

        class_decl.subroutines.iter().for_each(|s| {
            let sub_ty = cls_ty.subs.get(&s.name).unwrap();
            if sub_ty.kind == SubKind::Ctor {
                return;
            }
            self.scope.define(Rc::new(Var {
                name: s.name.clone(),
                kind: VarKind::Subroutine,
                ty: Type::Subroutine(sub_ty.clone()),
                value: 0,
            }));
        });
        class_decl.statics.iter().for_each(|field| {
            let field = cls_ty.fields.get(&field.name).unwrap().clone();
            self.scope.define(field.clone());
            cls.statics.push(field.clone());
        });
        class_decl.fields.iter().for_each(|field| {
            let field = cls_ty.fields.get(&field.name).unwrap().clone();
            self.scope.define(field.clone());
            cls.fields.push(field.clone());
        });

        let this = Rc::new(Var {
            name: "this".to_string(),
            kind: VarKind::This,
            ty: cls.ty.clone(),
            value: 0,
        });
        self.this = Some(this.clone());
        self.scope.define(this);

        class_decl.subroutines.into_iter().for_each(|s| {
            cls.subs.push(Rc::new(
                self.subroutine(cls_ty.subs.get(&s.name).unwrap(), s),
            ));
        });
        self.this = None;
        self.leave_scope();

        cls.string_lits = mem::replace(&mut self.string_lits, Vec::new());

        cls
    }
}
