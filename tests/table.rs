use std::collections::HashMap;

use vas::{
    common::{Identifier, Primitives, Table, Value},
    interpreter::eval,
};

#[test]
fn array_literal_syntax() {
    let source = r#"
		var a = 42;
		var arr = [1, a, 3 - 2, [1]];
	"#;
    let i = eval(source).unwrap();
    let s = i.inspect();
    let t = s.get_tab_inspect("arr").unwrap();
    assert_eq!(t.0.len(), 4);
    assert_eq!(t.0.get("0"), Some(&Value::Primitives(Primitives::Num(1.0))));
    assert_eq!(
        t.0.get("1"),
        Some(&Value::Primitives(Primitives::Num(42.0)))
    );
    assert_eq!(t.0.get("2"), Some(&Value::Primitives(Primitives::Num(1.0))));
    let mut inner_t: HashMap<Identifier, Value> = HashMap::new();
    inner_t.insert("0".to_owned(), Value::Primitives(Primitives::Num(1.0)));
    assert_eq!(t.0.get("3"), Some(&Value::Table(Table(inner_t))));
}

#[test]
fn array_indexing() {
    let source = r#"
		// function foo(arr) {
		// 	return arr[0];
		// }

		// var a = 5;
		// var b = [[1*2, [a, 3], 4+2]][0];
		// var c = foo(b[1]);
		var d = b[1][2 - 2];
	"#;
    let i = eval(source).unwrap();
    let s = i.inspect();
    assert_eq!(s.get_pri_inspect("c"), Some(&Primitives::Num(5.0)));
    assert_eq!(s.get_pri_inspect("d"), Some(&Primitives::Num(5.0)));
}

#[test]
fn simple_array_traverse() {
    let source = r#"
		var arr = [];
		for (var i = 0; i < 5; i = i + 1) {
			arr[i] = i;
		}
		var total = 0;
		for (var j = 0; j < len(arr); j = j + 1) {
			total = total + arr[j]
		}
	"#;

    let i = eval(source).unwrap();
    let s = i.inspect();
    let mut total = 0.0;
    for i in 0..5 {
        total += i as f64;
    }
    assert_eq!(s.get_pri_inspect("total"), Some(&Primitives::Num(total)));
}
