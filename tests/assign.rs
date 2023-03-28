use vas::{
    common::{Primitives, Value},
    interpreter::eval,
};

#[test]
fn assgin_to_uninitial() {
    let no_let = r#"
		a = 1;
	"#;
    assert!(eval(&no_let).is_err());
}

#[test]
fn bind_void() {
    let bind_void = r#"
		var a;
	"#;
    let rt = eval(&bind_void).unwrap();
    let scope = rt.inspect();
    assert_eq!(
        scope.get_pri_inspect(&"a".to_string()),
        Some(&Primitives::Null)
    );
}

#[test]
fn redefined_variable() {
    let redefined = r#"
		var a = 1;
		var a;
	"#;
    let rt = eval(&redefined).unwrap();
    let scope = rt.inspect();
    assert_eq!(
        scope.get_pri_inspect(&"a".to_string()),
        Some(&Primitives::Null)
    );
}
