use vas::{common::Primitives, interpreter::eval};

#[test]
fn math_precedence() {
    let source = r#"
		var a = 1 + 2 * 3;
		var b= (1+2) * 3;
	"#;

    let rt = eval(&source).unwrap();
    let globals = rt.inspect();

    let a = globals.get_pri_inspect(&"a".to_string());
    assert_eq!(a, Some(&Primitives::Num(7.0)));

    let b = globals.get_pri_inspect(&"b".to_string());
    assert_eq!(b, Some(&Primitives::Num(9.0)));
}

#[test]
fn minus_and_substract() {
    let source = r#"
        var c = -1;
        var a = -1-2;
        var b = c+(-a);
    "#;

    let i = eval(&source).unwrap();
    let globe = i.inspect();
    assert_eq!(globe.get_pri_inspect("c"), Some(&Primitives::Num(-1.0)));
    assert_eq!(globe.get_pri_inspect("a"), Some(&Primitives::Num(-3.0)));
    assert_eq!(globe.get_pri_inspect("b"), Some(&Primitives::Num(2.0)));
}

#[test]
fn string_concat() {
    let source = r#"
        var a = "str" + "1";
    "#;
    let i = eval(source).unwrap();
    let s = i.inspect();
    assert_eq!(
        s.get_pri_inspect("a"),
        Some(&Primitives::String("str1".to_string()))
    )
}

#[test]
fn complex_expr() {
    let source = r#"
        var a = 1;
        var b = true;
        var c = (((a + 1) * 2) > 3) || !b;
    "#;
    let rt = eval(&source).unwrap();
    let globals = rt.inspect();
    let c = globals.get_pri_inspect(&"c".to_string());
    assert_eq!(c, Some(&Primitives::Boolean(true)));
}
