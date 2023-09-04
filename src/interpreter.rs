use std::{collections::HashMap, ops::Neg};

use crate::{common::*, lexer::*, parser::*};

type Function = FnDefStmt;

pub struct Interpreter {
    global: Option<Scope>,
    program: Program,
}

#[derive(Default, Debug, Clone)]
pub struct Scope {
    pub variables: Variables,
    pub functions: Functions,
}

/// from global to local
/// stack[0] = global
/// stack[len - 1] = local
#[derive(Clone)]
struct Stack(Vec<Scope>);

impl Stack {
    fn new() -> Stack {
        Stack(vec![])
    }

    fn push(&mut self, value: Scope) {
        self.0.push(value)
    }

    fn pop(&mut self) -> Option<Scope> {
        self.0.pop()
    }

    fn local(&self) -> VasResult<&Scope> {
        self.0
            .last()
            .ok_or(VasErr::common(ErrKind::Interpreter, "empty stack"))
    }

    fn local_mut(&mut self) -> VasResult<&mut Scope> {
        self.0
            .last_mut()
            .ok_or(VasErr::common(ErrKind::Interpreter, "empty stack"))
    }

    fn get_var(&self, key: &Identifier, local_only: bool) -> VasResult<Option<&Value>> {
        if local_only {
            Ok(self.local()?.get_var(key))
        } else {
            for s in self.0.iter().rev() {
                if let Some(val) = s.get_var(key) {
                    return Ok(Some(val));
                }
            }

            Ok(None)
        }
    }

    fn set_var(
        &mut self,
        key: Identifier,
        val: Value,
        local_only: bool,
    ) -> VasResult<Option<Value>> {
        if local_only {
            Ok(self.local_mut()?.set_var(key, val))
        } else {
            for s in self.0.iter_mut().rev() {
                if s.get_var(&key).is_some() {
                    return Ok(s.set_var(key, val));
                }
            }

            Ok(None)
        }
    }

    fn get_func(&self, key: &Identifier, local_only: bool) -> VasResult<Option<&Function>> {
        if local_only {
            Ok(self.local()?.get_func(key))
        } else {
            for s in self.0.iter().rev() {
                if let Some(val) = s.get_func(key) {
                    return Ok(Some(val));
                }
            }

            Ok(None)
        }
    }

    fn set_func(
        &mut self,
        name: Identifier,
        func: Function,
        local_only: bool,
    ) -> VasResult<Option<Function>> {
        if local_only {
            Ok(self.local_mut()?.set_func(name, func))
        } else {
            for s in self.0.iter_mut().rev() {
                if s.get_func(&name).is_some() {
                    return Ok(s.set_func(name, func));
                }
            }

            Ok(None)
        }
    }

    fn get_var_mut(&mut self, k: &Identifier, local: bool) -> VasResult<&mut Value> {
        if local {
            Ok(self.local_mut()?.get_var_mut(k).ok_or(VasErr::common(
                ErrKind::Interpreter,
                &format!("{} undefined", k),
            ))?)
        } else {
            for s in self.0.iter_mut().rev() {
                if let Some(val) = s.get_var_mut(k) {
                    return Ok(val);
                }
            }

            Err(VasErr::common(
                ErrKind::Interpreter,
                &format!("{} undefined", k),
            ))
        }
    }
}

type Variables = HashMap<Identifier, Value>;

type Functions = HashMap<Identifier, Function>;

impl Scope {
    fn new() -> Scope {
        Scope {
            variables: HashMap::new(),
            functions: HashMap::new(),
        }
    }

    fn set_var(&mut self, key: Identifier, val: Value) -> Option<Value> {
        self.variables.insert(key, val)
    }

    fn set_func(&mut self, name: Identifier, func: Function) -> Option<Function> {
        self.functions.insert(name, func)
    }

    fn get_var(&self, key: &Identifier) -> Option<&Value> {
        self.variables.get(key)
    }

    fn get_var_mut(&mut self, key: &Identifier) -> Option<&mut Value> {
        self.variables.get_mut(key)
    }

    fn get_func(&self, name: &Identifier) -> Option<&Function> {
        self.functions.get(name)
    }

    pub fn get_pri_inspect(&self, key: &str) -> Option<&Primitives> {
        let val = self.get_var(&key.to_string());
        match val {
            Some(v) => match v {
                Value::Primitives(p) => Some(p),
                Value::Table(_) => None,
            },
            None => None,
        }
    }

    pub fn get_tab_inspect(&self, key: &str) -> Option<&Table> {
        let val = self.get_var(&key.to_string());
        match val {
            Some(v) => match v {
                Value::Primitives(_) => None,
                Value::Table(t) => Some(t),
            },
            None => None,
        }
    }

    pub fn has_func_inspect(&self, name: &str) -> bool {
        self.functions.contains_key(name)
    }
}

type EarlyReturn = (bool, Value);

impl Interpreter {
    fn main(program: Program) -> Interpreter {
        Interpreter {
            global: None,
            program,
        }
    }

    /// eval main
    fn eval(&mut self) -> VasResult<()> {
        let mut stack = Stack::new();
        stack.push(Scope::new());
        self.eval_stmts(&self.program.0.to_vec(), &mut stack)?;
        assert!(!stack.0.is_empty());
        self.global = stack.pop();
        Ok(())
    }

    /// debug only
    pub fn inspect(self) -> Scope {
        // mem::take(&mut self.scope.borrow_mut())
        self.global.unwrap()
    }

    fn err(&self, more: &str) -> VasErr {
        // TODO pos
        VasErr::occurs_at(ErrKind::Interpreter, 0, "TODO", more)
    }

    fn eval_stmts(&mut self, stmts: &Vec<Stmt>, sc: &mut Stack) -> VasResult<EarlyReturn> {
        for stmt in stmts {
            let ret = self.eval_stmt(&stmt, sc)?;
            if ret.0 {
                return Ok(ret);
            }
        }

        Ok((false, Value::Primitives(Primitives::Null)))
    }

    fn eval_stmt(&mut self, stmt: &Stmt, sc: &mut Stack) -> VasResult<EarlyReturn> {
        match stmt {
            Stmt::Bind(b) => self.eval_bind_stmt(b, sc)?,
            Stmt::Assign(a) => self.eval_assign_stmt(a, sc, false)?,
            // could return
            Stmt::If(i) => return self.eval_if(i, sc),
            // could return
            Stmt::For(f) => return self.eval_for(f, sc),
            Stmt::Ret(r) => {
                if sc.0.len() == 1 {
                    return Err(VasErr::common(ErrKind::Interpreter, "top level return!"));
                }

                let value = self.eval_expr(&r.expr, sc)?;
                return Ok((true, value));
            }
            Stmt::FnDef(fd) => {
                // no call no eval
                // TODO no clone ??
                sc.set_func(fd.name.to_string(), fd.clone(), true)?;
            }
            Stmt::FnCall(fc) => {
                // `add(1, 2)` would throw then returned value
                self.eval_fn_call(&fc.0, sc)?;
            }
        }

        Ok((false, Value::Primitives(Primitives::Null)))
    }

    fn eval_expr(&mut self, expr: &Expr, sc: &mut Stack) -> VasResult<Value> {
        match expr {
            Expr::Binary(b) => self.eval_binary(b, sc),
            Expr::Unary(u) => self.eval_unary(u, sc),
            Expr::Primary(p) => self.eval_primary(p, sc),
        }
    }

    fn eval_kv(&mut self, kv: &KVExpr, sc: &mut Stack) -> VasResult<Value> {
        match kv {
            KVExpr::Key(k) => match sc.get_var(k, false)? {
                Some(v) => Ok(v.clone()),
                None => Err(self.err(&format!("undefined variable: {}", k))),
            },
            KVExpr::Value(v) => Ok(v.clone()),
        }
    }

    fn eval_assign_stmt(&mut self, a: &AssignStmt, sc: &mut Stack, by_bind: bool) -> VasResult<()> {
        match a {
            AssignStmt::Var(v) => {
                let VarAssignStmt { ident, expr } = v;
                let new_val = self.eval_expr(expr, sc)?;
                if by_bind {
                    sc.set_var(ident.to_string(), new_val, true)?;
                } else {
                    if sc.get_var(ident, false)?.is_none() {
                        return Err(self.err(&format!("undefined variable: {}", ident)));
                    } else {
                        sc.set_var(ident.to_string(), new_val, false)?;
                    }
                }
            }
            AssignStmt::Table(t) => {
                let TableAssignStmt { lhs, expr } = t;
                let new_val = self.eval_expr(expr, sc)?;
                self.eval_indexing_lhs(lhs, sc, new_val)?;
            }
        }

        Ok(())
    }

    /// bind always to local
    fn eval_bind_stmt(&mut self, b: &BindStmt, sc: &mut Stack) -> VasResult<()> {
        match b {
            BindStmt::LetVoid(id) => {
                sc.set_var(id.to_string(), Value::Primitives(Primitives::Null), true)?;
            }
            BindStmt::LetAssign(a) => {
                self.eval_assign_stmt(a, sc, true)?;
            }
        }

        Ok(())
    }

    fn eval_if(&mut self, i: &IfStmt, sc: &mut Stack) -> VasResult<EarlyReturn> {
        let IfStmt {
            condition,
            block,
            els,
        } = i;
        let cond_result = self.eval_expr(condition, sc)?;

        if cond_result.is_true() {
            self.eval_stmts(&block.0, sc)
        } else {
            if let Some(else_block) = els {
                match else_block.as_ref() {
                    ElseBlock::ElseIf(i) => self.eval_if(i, sc),
                    ElseBlock::Else(e) => self.eval_stmts(&e.0, sc),
                }
            } else {
                Ok((false, Value::Primitives(Primitives::Null)))
            }
        }
    }

    fn eval_binary(&mut self, b: &BinaryExpr, sc: &mut Stack) -> VasResult<Value> {
        let BinaryExpr { lhs, op, rhs } = b;
        match op {
            BinaryOp::Math(m) => self.eval_math_binary(lhs, m, rhs, sc),
            BinaryOp::Compare(c) => self.eval_compare_binary(lhs, c, rhs, sc),
        }
    }

    fn eval_math_binary(
        &mut self,
        lhs: &Expr,
        op: &MathOp,
        rhs: &Expr,
        sc: &mut Stack,
    ) -> VasResult<Value> {
        let l_value = self.eval_expr(lhs, sc)?;
        let r_value = self.eval_expr(rhs, sc)?;

        match (l_value, r_value) {
            (Value::Primitives(Primitives::Num(n1)), Value::Primitives(Primitives::Num(n2))) => {
                Ok(Value::Primitives(Primitives::Num(match op {
                    MathOp::Add => n1 + n2,
                    MathOp::Sub => n1 - n2,
                    MathOp::Mul => n1 * n2,
                    MathOp::Div => n1 / n2,
                })))
            }
            (
                Value::Primitives(Primitives::String(s1)),
                Value::Primitives(Primitives::String(s2)),
            ) => match op {
                MathOp::Add => {
                    let mut result = s1.clone();
                    result.push_str(&s2);
                    Ok(Value::Primitives(Primitives::String(result)))
                }
                _ => Err(self.err("only add(+) can apply to strings")),
            },
            (l, r) => Err(self.err(&format!(
                "math expr: {:?} {:?} {:?} not supported",
                l, op, r
            ))),
        }
    }

    fn eval_compare_binary(
        &mut self,
        lhs: &Expr,
        op: &CompareOp,
        rhs: &Expr,
        sc: &mut Stack,
    ) -> VasResult<Value> {
        let l_value = self.eval_expr(lhs, sc)?;
        let r_value = self.eval_expr(rhs, sc)?;
        let err = Err(VasErr::common(
            ErrKind::Interpreter,
            &format!(
                "cmp expr: {:?} {:?} {:?} not supported",
                l_value, op, r_value
            ),
        ));
        let result: bool = match (&l_value, &r_value) {
            (Value::Primitives(Primitives::Num(n1)), Value::Primitives(Primitives::Num(n2))) => {
                match op {
                    CompareOp::EqEq => n1 == n2,
                    CompareOp::NotEq => n1 != n2,
                    CompareOp::Gt => n1 > n2,
                    CompareOp::Lt => n1 < n2,
                    CompareOp::GtEq => n1 >= n2,
                    CompareOp::LtEq => n1 <= n2,
                    _ => return err,
                }
            }
            (
                Value::Primitives(Primitives::String(s1)),
                Value::Primitives(Primitives::String(s2)),
            ) => match op {
                CompareOp::EqEq => s1 == s2,
                CompareOp::NotEq => s1 != s2,
                _ => return err,
            },
            (
                Value::Primitives(Primitives::Boolean(b1)),
                Value::Primitives(Primitives::Boolean(b2)),
            ) => match op {
                CompareOp::And => *b1 && *b2,
                CompareOp::Or => *b1 || *b2,
                CompareOp::EqEq => b1 == b2,
                CompareOp::NotEq => b1 != b2,
                _ => return err,
            },
            (Value::Primitives(Primitives::Null), r) => match op {
                CompareOp::EqEq => {
                    if let Value::Primitives(p) = r {
                        p == &Primitives::Null
                    } else {
                        false
                    }
                }
                CompareOp::NotEq => {
                    if let Value::Primitives(p) = r {
                        p != &Primitives::Null
                    } else {
                        true
                    }
                }
                _ => return err,
            },
            (l, Value::Primitives(Primitives::Null)) => match op {
                CompareOp::EqEq => {
                    if let Value::Primitives(p) = l {
                        p == &Primitives::Null
                    } else {
                        false
                    }
                }
                CompareOp::NotEq => {
                    if let Value::Primitives(p) = l {
                        p != &Primitives::Null
                    } else {
                        true
                    }
                }
                _ => return err,
            },
            _ => {
                return err;
            }
        };

        Ok(Value::Primitives(Primitives::Boolean(result)))
    }

    fn eval_primary(&mut self, p: &PrimaryExpr, sc: &mut Stack) -> VasResult<Value> {
        match p {
            PrimaryExpr::KV(kv) => self.eval_kv(kv, sc),
            PrimaryExpr::FnCall(fc) => self.eval_fn_call(fc, sc),
            PrimaryExpr::ArrayInit(arr) => self.eval_array_init(arr, sc),
            PrimaryExpr::ObjInit => todo!(),
            PrimaryExpr::Indexing(idx) => self.eval_indexing_rhs(idx, sc),
        }
    }

    fn eval_for(&mut self, f: &ForStmt, sc: &mut Stack) -> VasResult<EarlyReturn> {
        let ForStmt { for_clause, block } = f;

        // init
        self.eval_for_clause_stmt(for_clause.init.as_deref(), sc)?;

        loop {
            // cond
            if let Some(cond) = &for_clause.condition {
                let result = self.eval_expr(cond, sc)?;
                match result {
                    Value::Primitives(Primitives::Boolean(cont)) => {
                        if cont {
                            // exec block
                            let ret = self.eval_stmts(&block.0, sc)?;
                            if ret.0 {
                                return Ok(ret);
                            }

                            // post
                            self.eval_for_clause_stmt(for_clause.post.as_deref(), sc)?;
                        } else {
                            break;
                        }
                    }
                    _ => return Err(self.err("for condition has to be a boolean")),
                }
            }
        }

        Ok((false, Value::Primitives(Primitives::Null)))
    }

    fn eval_for_clause_stmt(
        &mut self,
        fc: Option<&ForClauseStmt>,
        sc: &mut Stack,
    ) -> VasResult<()> {
        if let Some(init_stmt) = fc {
            match init_stmt {
                ForClauseStmt::Bind(b) => {
                    self.eval_bind_stmt(b, sc)?;
                }
                ForClauseStmt::Assign(a) => {
                    self.eval_assign_stmt(a, sc, false)?;
                }
            }
        }

        Ok(())
    }

    fn eval_unary(&mut self, u: &UnaryExpr, sc: &mut Stack) -> VasResult<Value> {
        let UnaryExpr { op, rhs } = u;
        let r_value = self.eval_expr(&rhs, sc)?;
        let val = match (&op, &r_value) {
            (UnaryOp::Not, Value::Primitives(Primitives::Boolean(b))) => {
                Value::Primitives(Primitives::Boolean(!b))
            }
            (UnaryOp::Not, Value::Primitives(Primitives::Null)) => {
                Value::Primitives(Primitives::Boolean(true))
            }
            (UnaryOp::Neg, Value::Primitives(Primitives::Num(n))) => {
                Value::Primitives(Primitives::Num(n.neg()))
            }
            _ => return Err(self.err(&format!("unary expr: {:?} {:?} not supported", op, r_value))),
        };

        Ok(val)
    }

    fn eval_fn_call(&mut self, call: &FnCallExpr, stack: &mut Stack) -> VasResult<Value> {
        let FnCallExpr { name, args } = call;
        let def = match stack.get_func(name, false)? {
            Some(f) => f.clone(),
            None => {
                return Err(VasErr::common(
                    ErrKind::Interpreter,
                    &format!("undefined function: {}", name),
                ))
            }
        };

        if def.args.0.len() != call.args.0.len() {
            return Err(VasErr::common(
                ErrKind::Interpreter,
                "function args mismatch",
            ));
        }

        let mut local = Scope::new();
        // args eval in order
        for i in 0..def.args.0.len() {
            let key = &def.args.0[i];
            let value = self.eval_expr(&args.0[i], stack)?;
            local.set_var(key.to_string(), value);
        }
        stack.push(local);
        let ret = self.eval_stmts(&def.block.0, stack)?;
        stack.pop();

        Ok(ret.1)
    }

    fn eval_array_init(&mut self, arr: &Exprs, sc: &mut Stack) -> VasResult<Value> {
        let mut val = HashMap::new();

        for i in 0..arr.0.len() {
            // just 0 to "0"
            let expr = &arr.0[i];
            val.insert(i.to_string(), self.eval_expr(expr, sc)?);
        }

        Ok(Value::Table(Table(val)))
    }

    fn eval_indexing_rhs(&mut self, idx: &IndexingExpr, sc: &mut Stack) -> VasResult<Value> {
        let base = self.eval_primary(&idx.lhs, sc)?;
        let key = self.eval_expr(&idx.key, sc)?;
        // dbg!(&idx);
        // dbg!(&sc.0);

        match base {
            Value::Primitives(_) => Err(self.err("can not indexing to primitive")),
            Value::Table(mut t) => {
                let k = match key {
                    Value::Primitives(p) => p.to_string(),
                    Value::Table(_) => return Err(self.err("can not indexing by table")),
                };

                match t.0.get_mut(&k) {
                    Some(v) => Ok(v.clone()),
                    None => Ok(Value::Primitives(Primitives::Null)),
                }
            }
        }
    }

    fn eval_indexing_lhs(
        &mut self,
        idx: &IndexingExpr,
        sc: &mut Stack,
        val: Value,
    ) -> VasResult<()> {
        dbg!(&idx);
        let key = self.eval_expr(&idx.key, sc)?;
        let base = self.eval_primary_mut(&idx.lhs, sc)?;

        match base {
            Value::Primitives(_) => return Err(self.err("can not indexing to primitive")),
            Value::Table(t) => match key {
                Value::Table(_) => return Err(self.err("can not indexing by table")),
                Value::Primitives(p) => {
                    let k = p.to_string();
                    t.0.insert(k, val);
                }
            },
        }

        Ok(())
    }

    /// can only appear at lhs
    fn eval_primary_mut<'a, 'b>(
        &'a mut self,
        pri: &PrimaryExpr,
        sc: &'b mut Stack,
    ) -> VasResult<&'b mut Value> {
        dbg!(pri);
        match pri {
            // a[0] = 1
            PrimaryExpr::KV(kv) => match kv {
                KVExpr::Key(k) => sc.get_var_mut(k, false),
                KVExpr::Value(v) => Err(self.err(&format!("{:?} can not stand at lhs", v))),
            },
            // TODO:
            // a[0][0] = 1
            PrimaryExpr::Indexing(i) => {
                let mut cloned_sc = sc.clone();
                let v = self.eval_primary_mut(&i.lhs, sc)?;
                match v {
                    Value::Primitives(_) => Ok(v),
                    Value::Table(t) => {
                        let value_of_key = self.eval_expr(&i.key, &mut cloned_sc)?;
                        match value_of_key {
                            Value::Primitives(p) => {
                                let k = p.to_string();
                                Ok(t.get_mut(&k)
                                    .ok_or(self.err(&format!("no such key: {}", k)))?)
                            }
                            Value::Table(_) => Err(self.err("can not indexing by table")),
                        }
                    }
                }
            }
            other => Err(self.err(&format!("{:?} can not stand at lhs", other))),
        }
    }
}

///
/// TODO: copy source 3 times, may replace by some Span<usize, usize>
///
pub fn eval(source: &str) -> VasResult<Interpreter> {
    let tks = tokenize(source)?;
    // dbg!(&tks);
    let ast = parse(&tks, source)?;
    dbg!(&ast);
    let mut main = Interpreter::main(ast);
    main.eval()?;
    Ok(main)
}
