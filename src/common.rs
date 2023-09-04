use std::{collections::HashMap, error::Error, fmt::Display};
pub type Num = f64;
pub type Identifier = String;

#[derive(Debug, Clone, PartialEq)]
pub enum Value {
    Primitives(Primitives),
    Table(Table),
}

#[derive(Debug, Clone, PartialEq)]
pub struct Table(pub HashMap<Identifier, Value>);

impl Table {
    pub fn get(&self, k: &Identifier) -> Option<&Value> {
        self.0.get(k)
    }

    pub fn get_mut(&mut self, k: &Identifier) -> Option<&mut Value> {
        self.0.get_mut(k)
    }
}

// TODO how to map to opcode?
#[derive(Debug, Clone, PartialEq)]
pub enum Primitives {
    Num(Num),
    String(String),
    Boolean(bool),
    Null,
}

#[derive(Debug)]
pub struct VasErr {
    pub kind: ErrKind,
    msg: String,
}

#[derive(Debug, PartialEq)]
pub enum ErrKind {
    Lexer,
    Parser,
    Interpreter,
}

pub type VasResult<T> = Result<T, VasErr>;

// TODO more or less strict ?
pub trait Falsy {
    fn is_true(&self) -> bool;
    fn is_false(&self) -> bool;
}

impl Falsy for Value {
    fn is_true(&self) -> bool {
        match self {
            Value::Primitives(p) => p.is_true(),
            Value::Table(_) => true,
        }
    }

    fn is_false(&self) -> bool {
        !self.is_true()
    }
}

impl Falsy for Primitives {
    fn is_true(&self) -> bool {
        match self {
            Primitives::Num(n) => n != &0.0,
            Primitives::String(s) => s != "",
            Primitives::Boolean(b) => *b,
            Primitives::Null => false,
        }
    }

    fn is_false(&self) -> bool {
        !self.is_true()
    }
}

impl ToString for Primitives {
    fn to_string(&self) -> Identifier {
        match self {
            Primitives::Num(n) => n.to_string(),
            Primitives::String(s) => s.clone(),
            Primitives::Boolean(b) => b.to_string(),
            Primitives::Null => String::from("null"),
        }
    }
}

impl VasErr {
    pub fn occurs_at(kind: ErrKind, pos: usize, source: &str, more: &str) -> VasErr {
        VasErr {
            kind,
            msg: format_err(pos, source, more),
        }
    }

    pub fn common(kind: ErrKind, msg: &str) -> VasErr {
        VasErr {
            kind,
            msg: msg.to_string(),
        }
    }
}

impl Error for VasErr {}

impl Display for VasErr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self.kind {
            ErrKind::Lexer => write!(f, "Lexer Err:\n{}", self.msg),
            ErrKind::Parser => write!(f, "Parser Err:\n{}", self.msg),
            ErrKind::Interpreter => write!(f, "Runtime Err:\n{}", self.msg),
        }
    }
}

fn format_err(pos: usize, source: &str, more: &str) -> String {
    let span = 5;
    let from = if pos > span { pos - span } else { 0 };
    let to = if pos + span < source.len() {
        pos + span
    } else {
        source.len() - 1
    };
    let line1 = source[from..to].to_string();
    let mut line2 = String::new();
    for _ in 0..(pos - from) {
        line2.push('-');
    }
    line2.push('^');
    for _ in 0..(to - pos) {
        line2.push('-');
    }

    format!("{}\n{}\n{}", more, line1, line2)
}
