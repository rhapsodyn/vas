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
fn array_indexing_rhs() {
    let source = r#"
		function foo(arr) {
			return arr[0];
		}

		var a = 5;
		var b = [[1*2, [a, 3], 4+2]][0];
		var c = foo(b[1]);
		var d = b[1][2 - 2];
	"#;
    let i = eval(source).unwrap();
    let s = i.inspect();
    assert_eq!(s.get_pri_inspect("c"), Some(&Primitives::Num(5.0)));
    assert_eq!(s.get_pri_inspect("d"), Some(&Primitives::Num(5.0)));
}

#[test]
fn idx_by_table_err() {
    let source = r#"
        var arr = [];
        var arr2 = [];
        var err = arr[arr2];
    "#;
    assert!(eval(&source).is_err());
}

#[test]
fn we_love_null() {
    let source = r#"
        var arr = [1,2,"3"];
        var n1 = arr[4];
        var n2 = arr["5"];
    "#;
    let i = eval(source).unwrap();
    let s = i.inspect();
    assert_eq!(s.get_pri_inspect("n1"), Some(&Primitives::Null));
    assert_eq!(s.get_pri_inspect("n2"), Some(&Primitives::Null));
}

#[test]
fn simple_array_traverse() {
    let source = r#"
		var arr = [];
		for (var i = 0; i < 5; i = i + 1) {
			arr[i] = i;
		}
		var total = 0;
		for (var j = 0; j < 5; j = j + 1) {
			total = total + arr[j];
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

#[test]
fn two_dimesional_array() {
    let source = r#"
        var d2 = [];
        for (var i = 0; i < 2; i = i + 1) {
            d2[i] = [];
        }

        d2[0][0] = 42;

        // var a = d2[0][0];
        // var arr = [];
        // arr[0] = [];
        // arr[0][0] = 1;
    "#;
    let i = eval(source).unwrap();
    let s = i.inspect();
    let t = s.get_tab_inspect("d2").unwrap();
    let e1 = i_am_a_table(t.get(&"0".to_owned()).unwrap()).unwrap();
    assert_eq!(e1.get(&"0".to_owned()), Some(&Value::Primitives(Primitives::Num(42.0))));
    i_am_a_table(t.get(&"1".to_owned()).unwrap()).unwrap();
}

fn i_am_a_table(val: &Value) -> Option<&Table> {
    match val {
        Value::Primitives(_) => None,
        Value::Table(t) => Some(t),
    }
}
