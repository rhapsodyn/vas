use vas::{common::Primitives, interpreter::eval};

#[test]
fn wrong_syntax() {
    let no_paren = r#"
		if false {
			print(err);
		}
	"#;
    assert!(eval(&no_paren).is_err());

    let no_brace = r#"
		if (false) 
			print(err);
	"#;
    assert!(eval(&no_brace).is_err());
}

#[test]
fn simple_if() {
    let always_true = r#"
		var a;
		if (true) {
			a = 42;
		}
	"#;
    let mut rt = eval(&always_true).unwrap();
    let mut scope = rt.inspect();
    assert_eq!(
        scope.get_pri_inspect(&"a".to_string()),
        Some(&Primitives::Num(42.0))
    );

    let always_false = r#"
		var a;
		if (null) {
			a = 42;
		}
	"#;
    rt = eval(&always_false).unwrap();
    scope = rt.inspect();
    assert_eq!(
        scope.get_pri_inspect(&"a".to_string()),
        Some(&Primitives::Null)
    );
}

#[test]
fn complex_if() {
    let if_source = r#"
		var i = 3;
		var a;

		if (i > 1 + 1) {
			a = 42;
		}
	"#;
    let rt = eval(&if_source).unwrap();
    let scope = rt.inspect();
    assert_eq!(
        scope.get_pri_inspect(&"a".to_string()),
        Some(&Primitives::Num(42.0))
    );
}

#[test]
fn if_else() {
    let else_hit = r#"
		var a;
		if (false) {
			a = 1;
		} else {
			a = 2;
		}
	"#;

    let rt = eval(&else_hit).unwrap();
    let scope = rt.inspect();
    assert_eq!(
        scope.get_pri_inspect(&"a".to_string()),
        Some(&Primitives::Num(2.0))
    );
}

#[test]
fn if_else_if() {
    let else_if_hit = r#"
		var a;
		if (false) {
			a = 1;
		} else if (true) {
			a = 2;
			a = 4;
		} else {
			a = 3;
		}
	"#;

    let rt = eval(&else_if_hit).unwrap();
    let scope = rt.inspect();
    assert_eq!(
        scope.get_pri_inspect(&"a".to_string()),
        Some(&Primitives::Num(4.0))
    );
}

#[test]
fn if_return() {
    let source = r#"
		function gtZero(a) {
			if (a > 0) {
				return true;
			} else {
				return false;
			}
		}

		var b = gtZero(42);
	"#;

    let i = eval(&source).unwrap();
    let s = i.inspect();
    assert_eq!(s.get_pri_inspect(&"b"), Some(&Primitives::Boolean(true)))
}

#[test]
fn is_null() {
    let source = r#"
		var a;
		var b;

		if (a == null) {
			b = 1;
		}

		if (b != null) {
			b = 2;
		}
	"#;

    let i = eval(&source).unwrap();
    let s = i.inspect();
    assert_eq!(s.get_pri_inspect("b"), Some(&Primitives::Num(2.0)));
}
