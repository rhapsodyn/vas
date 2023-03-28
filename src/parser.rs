use std::collections::VecDeque;

use crate::{common::*, lexer::*};

#[derive(Debug, Clone)]
pub struct StmtBlock(pub Vec<Stmt>);

#[derive(Debug, Clone)]
pub enum Stmt {
    Bind(BindStmt),
    Assign(AssignStmt),
    If(IfStmt),
    For(ForStmt),
    Ret(RetStmt),
    FnDef(FnDefStmt),
    FnCall(FnCallStmt),
}

#[derive(Debug, Clone)]
pub enum BindStmt {
    /// let a;
    LetVoid(Identifier),
    /// let a = 1;
    LetAssign(AssignStmt),
}

/// a = 1;
#[derive(Debug, Clone)]
pub struct AssignStmt {
    pub ident: Identifier,
    pub expr: Expr,
}

#[derive(Debug, Clone)]
pub struct IfStmt {
    pub condition: Expr,
    pub block: StmtBlock,
    pub els: Option<Box<ElseBlock>>,
}

#[derive(Debug, Clone)]
pub enum ElseBlock {
    ElseIf(IfStmt),
    Else(StmtBlock),
}

#[derive(Debug, Clone)]
pub struct ForStmt {
    pub for_clause: ForClause,
    pub block: StmtBlock,
}

#[derive(Debug, Clone)]
pub struct ForClause {
    pub init: Option<Box<ForClauseStmt>>,
    pub condition: Option<Expr>,
    pub post: Option<Box<ForClauseStmt>>,
}

#[derive(Debug, Clone)]
pub enum ForClauseStmt {
    Bind(BindStmt),
    Assign(AssignStmt),
}

#[derive(Debug, Clone)]
pub struct RetStmt {
    pub expr: Expr,
}

#[derive(Debug, Clone)]
pub struct FnDefStmt {
    pub args: FnDefArgs,
    pub name: Identifier,
    pub block: StmtBlock,
}

#[derive(Debug, Clone)]
pub struct FnDefArgs(pub Vec<Identifier>);

///
/// `(` + arg_list + `)` or `[` + exprs `]`
///
#[derive(Debug, Clone)]
pub struct Exprs(pub Vec<Expr>);

#[derive(Debug, Clone)]
pub struct FnCallStmt(pub FnCallExpr);

#[derive(Debug, Clone)]
pub enum Expr {
    Binary(BinaryExpr),
    Unary(UnaryExpr),
    Primary(PrimaryExpr),
}

#[derive(Debug, Clone, PartialEq)]
pub enum Operator {
    Binary(BinaryOp),
    Unary(UnaryOp),
}

#[derive(Debug, Clone, PartialEq)]
pub enum BinaryOp {
    Math(MathOp),
    Compare(CompareOp),
}

#[derive(Debug, Clone, PartialEq)]
pub enum UnaryOp {
    Not,
}

#[derive(Debug, Clone, PartialEq)]
pub enum MathOp {
    Add,
    Sub,
    Mul,
    Div,
}

#[derive(Debug, Clone, PartialEq)]
pub enum CompareOp {
    And,
    Or,
    EqEq,
    NotEq,
    Gt,
    Lt,
    GtEq,
    LtEq,
}

#[derive(Debug, Clone)]
pub enum PrimaryExpr {
    KV(KVExpr),
    FnCall(FnCallExpr),
    ArrayInit(Exprs),
    ObjInit, // TODO
    Indexing(IndexingExpr),
}

///
/// [1, 2][3-2]
///
#[derive(Debug, Clone)]
pub struct IndexingExpr {
    pub lhs: Box<PrimaryExpr>,
    pub key: Box<Expr>,
}

#[derive(Debug, Clone)]
pub struct BinaryExpr {
    pub lhs: Box<Expr>,
    pub op: BinaryOp,
    pub rhs: Box<Expr>,
}

#[derive(Debug, Clone)]
pub struct UnaryExpr {
    pub op: UnaryOp,
    pub rhs: Box<Expr>,
}

#[derive(Debug, Clone)]
pub struct FnCallExpr {
    pub name: Identifier,
    pub args: Exprs,
}

#[derive(Debug, Clone)]
pub enum KVExpr {
    Key(Identifier),
    Value(Value),
}

#[derive(Debug)]
struct Parser {
    tokens: Vec<TokenInfo>,
    source: String, // TODO just to print errs
}

#[derive(PartialEq, Debug)]
enum OpOrParen {
    LeftParen,
    // not care about ) !
    // RightParen,
    Op(Operator),
}

enum OpOrExpr {
    Op(Operator),
    Expr(Expr),
}

impl Parser {
    fn new(tks: &[TokenInfo], source: &str) -> Parser {
        let mut tokens = tks.to_vec();
        tokens.reverse();
        Parser {
            tokens,
            source: source.to_string(),
        }
    }

    fn peek(&self) -> VasResult<&TokenInfo> {
        self.tokens.last().ok_or(VasErr::common(
            ErrKind::Parser,
            "token ran out, semicolon missing?",
        ))
    }

    fn peek_at(&self, n: usize) -> VasResult<&TokenInfo> {
        self.tokens
            .iter()
            .nth_back(n)
            .ok_or(VasErr::common(ErrKind::Parser, "token ran out"))
    }

    fn read(&mut self) -> VasResult<TokenInfo> {
        self.tokens
            .pop()
            .ok_or(VasErr::common(ErrKind::Parser, "token ran out"))
    }

    fn is_empty(&self) -> bool {
        self.tokens.is_empty()
    }

    // fn left_size(&self) -> usize {
    //     self.tokens.len()
    // }

    fn err(&self, more: &str, tk: &TokenInfo) -> VasErr {
        VasErr::occurs_at(ErrKind::Parser, tk.pos, &self.source, more)
    }

    // TODO opt or not
    fn read_semi_colon(&mut self) -> VasResult<()> {
        self.read_and_assert(&Token::Punctuaion(Punctuation::SemiColon))
    }

    fn read_and_assert(&mut self, expected: &Token) -> VasResult<()> {
        let next = self.read()?;
        if &next.token == expected {
            Ok(())
        } else {
            Err(self.err(&format!("expected: {:?}", expected), &next))
        }
    }

    // fn peek_and_assert(&self, expected: &Token) -> VasResult<()> {
    //     let next = self.peek()?;
    //     if &next.token == expected {
    //         Ok(())
    //     } else {
    //         Err(anyhow!(
    //             self.err(&format!("expected: {:?}", expected), &next)
    //         ))
    //     }
    // }

    fn fn_def_stmt(&mut self) -> VasResult<FnDefStmt> {
        self.read_and_assert(&Token::Keyword(Keyword::Function))?;

        let name = match self.read()?.token {
            Token::Identifier(s) => s,
            _ => return Err(VasErr::common(ErrKind::Parser, "no function name")),
        };
        let args = self.fn_def_args()?;
        let block = self.brace_stmt_block()?;

        Ok(FnDefStmt { args, name, block })
    }

    fn fn_call_expr(&mut self) -> VasResult<FnCallExpr> {
        let name = match self.read()?.token {
            Token::Identifier(s) => s,
            _ => return Err(VasErr::common(ErrKind::Parser, "no function name")),
        };
        let args = self.fn_call_args()?;

        Ok(FnCallExpr { name, args })
    }

    fn fn_def_args(&mut self) -> VasResult<FnDefArgs> {
        // (
        self.read_and_assert(&Token::Punctuaion(Punctuation::ParenLeft))?;

        let mut args = vec![];
        while !self.is_empty() {
            let arg1 = self.peek()?;
            match &arg1.token {
                Token::Punctuaion(Punctuation::ParenRight) => {
                    // meet right paren
                    break;
                }
                Token::Punctuaion(Punctuation::Comma) => {
                    // pop ,
                    self.read()?;
                    continue;
                }
                Token::Identifier(s) => {
                    args.push(s.to_string());
                    // pop ident
                    self.read()?;
                }
                _ => {
                    return Err(self.err("arg has to be a identifier", arg1));
                }
            }
        }

        // pop )
        self.read_and_assert(&Token::Punctuaion(Punctuation::ParenRight))?;

        Ok(FnDefArgs(args))
    }

    pub fn root_stmt_block(&mut self) -> VasResult<StmtBlock> {
        let mut stmts = vec![];

        while !self.is_empty() {
            stmts.push(self.stmt()?)
        }

        Ok(StmtBlock(stmts))
    }

    fn brace_stmt_block(&mut self) -> VasResult<StmtBlock> {
        // pop {
        self.read_and_assert(&Token::Punctuaion(Punctuation::BraceLeft))?;

        let mut stmts = vec![];
        loop {
            let ti = self.peek()?;
            match ti.token {
                Token::Punctuaion(Punctuation::BraceRight) => {
                    // pop }
                    self.read()?;
                    break;
                }
                _ => {
                    let stmt = self.stmt()?;
                    stmts.push(stmt);
                }
            }
        }

        Ok(StmtBlock(stmts))
    }

    fn stmt(&mut self) -> VasResult<Stmt> {
        let ti = self.peek()?;
        match &ti.token {
            Token::Keyword(rev) => match rev {
                Keyword::Var => Ok(Stmt::Bind(self.bind_stmt(true)?)),
                Keyword::Function => Ok(Stmt::FnDef(self.fn_def_stmt()?)),
                Keyword::Return => Ok(Stmt::Ret(self.ret_stmt()?)),
                Keyword::If => Ok(Stmt::If(self.if_stmt()?)),
                Keyword::Else => Err(self.err("dangling else", &ti)),
                Keyword::For => Ok(Stmt::For(self.for_stmt()?)),
                Keyword::Continue => todo!(),
                Keyword::Break => todo!(),
            },
            Token::Identifier(_) => {
                let one_more = self.peek_at(1)?;
                match one_more.token {
                    Token::Punctuaion(Punctuation::Eq) => Ok(Stmt::Assign(self.assign_stmt(true)?)),
                    Token::Punctuaion(Punctuation::ParenLeft) => {
                        Ok(Stmt::FnCall(self.fn_call_stmt()?))
                    }
                    _ => Err(self.err("unexpeted token", &ti)),
                }
            }
            Token::Literal(_) | Token::Punctuaion(_) => Err(self.err("unexpeted token", &ti)),
        }
    }

    fn bind_stmt(&mut self, have_to_semi_colon: bool) -> VasResult<BindStmt> {
        // pop var
        self.read_and_assert(&Token::Keyword(Keyword::Var))?;

        let ti = self.peek()?.clone();
        match &ti.token {
            Token::Identifier(ident) => {
                let next = self.peek_at(1)?.clone();
                let bind = match next.token {
                    // void bind
                    Token::Punctuaion(Punctuation::SemiColon) => {
                        // pop ident
                        self.read()?;
                        if have_to_semi_colon {
                            // pop ;
                            self.read_semi_colon()?;
                        }
                        BindStmt::LetVoid(ident.to_string())
                    }
                    _ => {
                        // bind to expr
                        BindStmt::LetAssign(self.assign_stmt(have_to_semi_colon)?)
                    }
                };
                Ok(bind)
            }
            _ => Err(self.err("only identifier was allowed", &ti)),
        }
    }

    fn expr(&mut self) -> VasResult<Expr> {
        self.expr_until(None)
    }

    /// non-resursive postfix parsing
    fn expr_until(&mut self, end: Option<&Token>) -> VasResult<Expr> {
        let mut op_stack: Vec<OpOrParen> = vec![];
        let mut postfix: VecDeque<OpOrExpr> = VecDeque::new();

        while !self.is_empty() {
            let ti = self.peek()?;

            if Some(&ti.token) == end {
                break;
            }

            match &ti.token {
                // operands
                Token::Literal(_)
                | Token::Identifier(_)
                | Token::Punctuaion(Punctuation::BraketLeft) => {
                    postfix.push_back(OpOrExpr::Expr(Expr::Primary(self.primary_expr()?)))
                }
                // math-ish `()`, not function-ish `()`
                Token::Punctuaion(p) => match p {
                    // (
                    Punctuation::ParenLeft => {
                        op_stack.push(OpOrParen::LeftParen);
                        // consume (
                        self.read()?;
                    }
                    // )
                    Punctuation::ParenRight => {
                        while op_stack.last() != Some(&OpOrParen::LeftParen) {
                            postfix.push_back(op1_to_op2(op_stack.pop())?);
                        }
                        // last (
                        op_stack.pop();
                        // consume )
                        self.read()?;
                    }
                    // operator
                    p => {
                        if let Some(op) = map_op(p) {
                            while !op_stack.is_empty() && operators_top_higher(&op, &op_stack) {
                                postfix.push_back(op1_to_op2(op_stack.pop())?);
                            }

                            op_stack.push(OpOrParen::Op(op));
                            // consume
                            self.read()?;
                        } else {
                            // expr end
                            break;
                        }
                    }
                },
                // expr end
                _ => break,
            }
        }

        while !op_stack.is_empty() {
            postfix.push_back(op1_to_op2(op_stack.pop())?);
        }

        if postfix.len() == 1 {
            let only = postfix.pop_front().unwrap();
            match only {
                OpOrExpr::Op(op) => {
                    return Err(VasErr::common(
                        ErrKind::Parser,
                        &format!("dangling op: {:?}", op),
                    ))
                }
                OpOrExpr::Expr(exp) => return Ok(exp),
            }
        }

        let mut eval_stack: Vec<Expr> = vec![];
        while let Some(item) = postfix.pop_front() {
            match item {
                OpOrExpr::Op(op) => match op {
                    Operator::Unary(u_op) => {
                        let rhs = eval_stack
                            .pop()
                            .ok_or(VasErr::common(ErrKind::Parser, "not enough parameters"))?;
                        let expr = UnaryExpr {
                            rhs: Box::new(rhs),
                            op: u_op,
                        };
                        eval_stack.push(Expr::Unary(expr));
                    }
                    Operator::Binary(op) => {
                        let rhs = eval_stack
                            .pop()
                            .ok_or(VasErr::common(ErrKind::Parser, "not enough parameters"))?;
                        let lhs = eval_stack
                            .pop()
                            .ok_or(VasErr::common(ErrKind::Parser, "not enough parameters"))?;
                        let expr = BinaryExpr {
                            lhs: Box::new(lhs),
                            op,
                            rhs: Box::new(rhs),
                        };
                        eval_stack.push(Expr::Binary(expr));
                    }
                },
                OpOrExpr::Expr(exp) => eval_stack.push(exp),
            }
        }

        if eval_stack.len() == 1 {
            Ok(eval_stack.pop().unwrap())
        } else {
            Err(VasErr::common(ErrKind::Parser, "illegal expression"))
        }
    }

    fn primary_expr(&mut self) -> VasResult<PrimaryExpr> {
        let base = match self.peek()?.token {
            Token::Punctuaion(Punctuation::BraketLeft) => {
                PrimaryExpr::ArrayInit(self.array_init()?)
            }
            _ => {
                match self.peek_at(1)?.token {
                    // have to check at(1) to recog `fn_name(` pattern
                    Token::Punctuaion(Punctuation::ParenLeft) => {
                        PrimaryExpr::FnCall(self.fn_call_expr()?)
                    }
                    _ => PrimaryExpr::KV(self.kv_expr()?),
                }
            }
        };

        // at least a ;
        match self.peek()?.token {
            Token::Punctuaion(Punctuation::BraketLeft) => self.indexing_chain(base),
            _ => Ok(base),
        }
    }

    fn indexing_chain(&mut self, lhs: PrimaryExpr) -> VasResult<PrimaryExpr> {
        self.read_and_assert(&Token::Punctuaion(Punctuation::BraketLeft))?;

        let key = self.expr_until(Some(&Token::Punctuaion(Punctuation::BraketRight)))?;
        dbg!(&key);

        self.read_and_assert(&Token::Punctuaion(Punctuation::BraketRight))?;

        let new_base = PrimaryExpr::Indexing(IndexingExpr {
            lhs: Box::new(lhs),
            key: Box::new(key),
        });

        // at least a ;
        match self.peek()?.token {
            Token::Punctuaion(Punctuation::BraketLeft) => self.indexing_chain(new_base),
            _ => Ok(new_base),
        }
    }

    fn kv_expr(&mut self) -> VasResult<KVExpr> {
        // pop at first
        let ti = self.read()?;
        match ti.token {
            Token::Literal(l) => {
                let value = match l {
                    Literal::String(s) => Value::Primitives(Primitives::String(s)),
                    Literal::Number(n) => Value::Primitives(Primitives::Num(n)),
                    Literal::True => Value::Primitives(Primitives::Boolean(true)),
                    Literal::False => Value::Primitives(Primitives::Boolean(false)),
                    Literal::Null => Value::Primitives(Primitives::Null),
                };
                Ok(KVExpr::Value(value))
            }
            Token::Identifier(i) => Ok(KVExpr::Key(i)),
            _ => Err(self.err("unexpected token", &ti)),
        }
    }

    fn fn_call_stmt(&mut self) -> VasResult<FnCallStmt> {
        let fn_call_expr = self.fn_call_expr()?;
        // ;
        self.read_semi_colon()?;

        Ok(FnCallStmt(fn_call_expr))
    }

    fn ret_stmt(&mut self) -> VasResult<RetStmt> {
        // return
        self.read_and_assert(&Token::Keyword(Keyword::Return))?;
        // expr
        let expr = self.expr()?;
        // ;
        self.read_semi_colon()?;

        Ok(RetStmt { expr })
    }

    fn if_stmt(&mut self) -> VasResult<IfStmt> {
        // pop if
        self.read_and_assert(&Token::Keyword(Keyword::If))?;

        // pop (
        self.read_and_assert(&Token::Punctuaion(Punctuation::ParenLeft))?;

        // expr
        let condition = self.expr_until(Some(&Token::Punctuaion(Punctuation::ParenRight)))?;

        // pop )
        self.read_and_assert(&Token::Punctuaion(Punctuation::ParenRight))?;

        // stmts
        let block = self.brace_stmt_block()?;

        // else clauses
        let els = if !self.is_empty() {
            let next = self.peek()?;
            match next.token {
                Token::Keyword(Keyword::Else) => {
                    // pop else
                    self.read()?;

                    let next_after = self.peek()?;
                    match next_after.token {
                        // else if
                        Token::Keyword(Keyword::If) => {
                            let following_if = self.if_stmt()?;
                            Some(Box::new(ElseBlock::ElseIf(following_if)))
                        }
                        // else
                        Token::Punctuaion(Punctuation::BraceLeft) => {
                            let else_block = self.brace_stmt_block()?;
                            Some(Box::new(ElseBlock::Else(else_block)))
                        }
                        _ => return Err(self.err("unexpected token", next_after)),
                    }
                }
                // no else
                _ => None,
            }
        } else {
            // eos
            None
        };

        Ok(IfStmt {
            condition,
            block,
            els,
        })
    }

    fn for_stmt(&mut self) -> VasResult<ForStmt> {
        self.read_and_assert(&Token::Keyword(Keyword::For))?;

        let for_clause = self.for_clause()?;
        let block = self.brace_stmt_block()?;

        Ok(ForStmt { for_clause, block })
    }

    fn for_clause(&mut self) -> VasResult<ForClause> {
        // (
        self.read_and_assert(&Token::Punctuaion(Punctuation::ParenLeft))?;

        // let a = 1
        let mut next = self.peek()?;
        let init = match next.token {
            Token::Punctuaion(Punctuation::SemiColon) => None,
            _ => Some(Box::new(self.for_clause_stmt()?)),
        };

        // ;
        self.read_semi_colon()?;

        // a < 10
        next = self.peek()?;
        let condition = match next.token {
            Token::Punctuaion(Punctuation::SemiColon) => None,
            _ => Some(self.expr()?),
        };

        // ;
        self.read_semi_colon()?;

        // a = a + 1
        next = self.peek()?;
        let post = match next.token {
            Token::Punctuaion(Punctuation::ParenRight) => None,
            _ => Some(Box::new(self.for_clause_stmt()?)),
        };

        // )
        self.read_and_assert(&Token::Punctuaion(Punctuation::ParenRight))?;

        Ok(ForClause {
            init,
            condition,
            post,
        })
    }

    fn for_clause_stmt(&mut self) -> VasResult<ForClauseStmt> {
        match self.peek()?.token {
            Token::Keyword(Keyword::Var) => Ok(ForClauseStmt::Bind(self.bind_stmt(false)?)),
            _ => Ok(ForClauseStmt::Assign(self.assign_stmt(false)?)),
        }
    }

    fn assign_stmt(&mut self, end_with_semi_colon: bool) -> VasResult<AssignStmt> {
        // pop ident
        let ident_tk = self.read()?;
        let ident = match ident_tk.token {
            Token::Identifier(i) => i,
            _ => return Err(self.err("unexpected token", &ident_tk)),
        };

        // pop =
        self.read_and_assert(&Token::Punctuaion(Punctuation::Eq))?;

        // rhs
        let expr = if end_with_semi_colon {
            let expr = self.expr()?;
            // pop ;
            self.read_semi_colon()?;
            expr
        } else {
            self.expr_until(Some(&Token::Punctuaion(Punctuation::ParenRight)))?
        };

        Ok(AssignStmt { ident, expr })
    }

    fn fn_call_args(&mut self) -> VasResult<Exprs> {
        // (
        self.read_and_assert(&Token::Punctuaion(Punctuation::ParenLeft))?;

        let mut args = vec![];
        while !self.is_empty() {
            let arg1 = self.peek()?;
            match arg1.token {
                Token::Punctuaion(Punctuation::ParenRight) => {
                    // meet right paren
                    break;
                }
                Token::Punctuaion(Punctuation::Comma) => {
                    // pop ,
                    self.read()?;
                    continue;
                }
                _ => {
                    let arg = self.expr_until(Some(&Token::Punctuaion(Punctuation::ParenRight)))?;
                    args.push(arg);
                }
            }
        }

        // pop )
        self.read_and_assert(&Token::Punctuaion(Punctuation::ParenRight))?;

        Ok(Exprs(args))
    }

    fn array_init(&mut self) -> VasResult<Exprs> {
        // [
        self.read_and_assert(&Token::Punctuaion(Punctuation::BraketLeft))?;

        let mut elems = vec![];
        while !self.is_empty() {
            let arg1 = self.peek()?;
            match arg1.token {
                Token::Punctuaion(Punctuation::BraketRight) => {
                    // meet right paren
                    break;
                }
                Token::Punctuaion(Punctuation::Comma) => {
                    // pop ,
                    self.read()?;
                    continue;
                }
                _ => {
                    let arg =
                        self.expr_until(Some(&Token::Punctuaion(Punctuation::BraketRight)))?;
                    elems.push(arg);
                }
            }
        }

        // ]
        self.read_and_assert(&Token::Punctuaion(Punctuation::BraketRight))?;

        Ok(Exprs(elems))
    }
}

// TODO macro ?
fn map_op(b: &Punctuation) -> Option<Operator> {
    match b {
        Punctuation::Add => Some(Operator::Binary(BinaryOp::Math(MathOp::Add))),
        Punctuation::Sub => Some(Operator::Binary(BinaryOp::Math(MathOp::Sub))),
        Punctuation::Mul => Some(Operator::Binary(BinaryOp::Math(MathOp::Mul))),
        Punctuation::Div => Some(Operator::Binary(BinaryOp::Math(MathOp::Div))),
        Punctuation::EqEq => Some(Operator::Binary(BinaryOp::Compare(CompareOp::EqEq))),
        Punctuation::Gt => Some(Operator::Binary(BinaryOp::Compare(CompareOp::Gt))),
        Punctuation::Lt => Some(Operator::Binary(BinaryOp::Compare(CompareOp::Lt))),
        Punctuation::GtEq => Some(Operator::Binary(BinaryOp::Compare(CompareOp::GtEq))),
        Punctuation::LtEq => Some(Operator::Binary(BinaryOp::Compare(CompareOp::LtEq))),
        Punctuation::Not => Some(Operator::Unary(UnaryOp::Not)),
        Punctuation::NotEq => Some(Operator::Binary(BinaryOp::Compare(CompareOp::NotEq))),
        Punctuation::Or => Some(Operator::Binary(BinaryOp::Compare(CompareOp::Or))),
        Punctuation::And => Some(Operator::Binary(BinaryOp::Compare(CompareOp::And))),
        _ => None,
    }
}

fn operators_top_higher(op: &Operator, operators: &Vec<OpOrParen>) -> bool {
    match operators.last() {
        Some(top) => match top {
            OpOrParen::Op(top_op) => precedence(top_op) >= precedence(op),
            _ => false,
        },
        None => false,
    }
}

fn precedence(op: &Operator) -> u8 {
    // high => low
    match op {
        Operator::Unary(_) => u8::MAX,
        Operator::Binary(b) => match b {
            BinaryOp::Math(m) => match m {
                MathOp::Mul | MathOp::Div => u8::MAX - 1,
                MathOp::Add | MathOp::Sub => u8::MAX - 2,
            },
            BinaryOp::Compare(c) => match c {
                CompareOp::EqEq
                | CompareOp::NotEq
                | CompareOp::Gt
                | CompareOp::Lt
                | CompareOp::GtEq
                | CompareOp::LtEq => u8::MAX - 3,
                CompareOp::And | CompareOp::Or => u8::MAX - 4,
            },
        },
    }
}

fn op1_to_op2(op1: Option<OpOrParen>) -> VasResult<OpOrExpr> {
    match op1 {
        Some(opp) => match opp {
            OpOrParen::Op(op) => Ok(OpOrExpr::Op(op)),
            _ => Err(VasErr::common(
                ErrKind::Parser,
                &format!("{:?} is not a operator", opp),
            )),
        },
        None => Err(VasErr::common(ErrKind::Parser, "no operator")),
    }
}

pub type Program = StmtBlock;

pub(crate) fn parse(tks: &[TokenInfo], source: &str) -> VasResult<Program> {
    let mut p = Parser::new(tks, source);
    p.root_stmt_block()
}
