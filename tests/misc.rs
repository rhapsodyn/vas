use vas::{common::ErrKind, interpreter::eval};

#[test]
fn top_return() {
    assert!(eval("return 0;").is_err());
}

#[test]
fn empty_source() {
    let source = r#"
        // var a;
    "#;
    assert!(eval(source).is_ok());
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
