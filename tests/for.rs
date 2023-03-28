use vas::{common::Primitives, interpreter::eval};

#[test]
fn simple_loop() {
    let always_true = r#"
		var a = 0;
		for (var i = 0; i < 10; i = i + 1) {
			a = a + i;
		}
	"#;
    let rt = eval(&always_true).unwrap();
    let scope = rt.inspect();
    let mut n = 0.0;
    for i in 0..10 {
        n += i as f64;
    }
    assert_eq!(
        scope.get_pri_inspect(&"a".to_string()),
        Some(&Primitives::Num(n))
    );
}

#[test]
fn early_return() {
    let source = r#"
        function foo(n1, n2) {
            for (var i = 0; i < n1; i = i + 1) {
                if (i < n2) {
                    return 0;
                }
            }

            return 1;
        }

        var a = foo(10, 5);
    "#;
    let i = eval(&source).unwrap();
    let s = i.inspect();
    assert_eq!(
        s.get_pri_inspect("a"),
        Some(&Primitives::Num(_foo(10, 5) as f64))
    );
}

fn _foo(n1: usize, n2: usize) -> usize {
    for i in 0..n1 {
        if i < n2 {
            return 0;
        }
    }

    1
}
