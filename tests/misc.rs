use vas::{
    common::{ErrKind, Primitives},
    interpreter::eval,
};

#[test]
fn top_return() {
    assert!(eval("return 0;").is_err());
}

#[test]
fn comment() {
    let source = r#"
        var a;
        a = 1;
        // a = 2;
    "#;
    let rt = eval(source).unwrap();
    let scope = rt.inspect();
    assert_eq!(
        scope.get_pri_inspect(&"a".to_string()),
        Some(&Primitives::Num(1.0))
    );
}

#[test]
fn errors() {
    let mut err = eval("var #;").err().unwrap();
    assert_eq!(err.kind, ErrKind::Lexer);
    err = eval("if (false) ()").err().unwrap();
    assert_eq!(err.kind, ErrKind::Parser);
    err = eval("a();").err().unwrap();
    assert_eq!(err.kind, ErrKind::Interpreter);
}

#[test]
fn neg_of_num_only() {
    assert!(eval("var a = \"abc\";var b = -a;").is_err());
    assert!(eval("var a = 1;var b = -a;").is_ok());
}
