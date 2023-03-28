use vas::{common::Primitives, interpreter::eval};

fn _fib(n: f64) -> f64 {
    if n == 0.0 || n == 1.0 {
        1.0
    } else {
        _fib(n - 1.0) + _fib(n - 2.0)
    }
}

#[test]
fn fib() {
    let fib_source = r#"
    	function fib(a) {
        	if (a == 1 || a == 0) {
        		return 1;
    		} else {
        	    return fib(a - 1) + fib(a - 2);
    		}
	    }
    	var a = fib(10);
    "#;
    let rt = eval(fib_source).unwrap();
    let scope = rt.inspect();
    let result = scope.get_pri_inspect("a").unwrap();
    assert_eq!(result, &Primitives::Num(_fib(10.0)));
}

#[test]
fn fn_def() {
    let source = r#"
        function add(a, b) {
            return a + b;
        }
    "#;
    let func = eval(source).unwrap();
    let scope = func.inspect();
    assert!(scope.has_func_inspect("add"));
    assert!(!scope.has_func_inspect("bad"));
}

#[test]
fn fn_call() {
    let err_source = r#"
        add(1, 2);
    "#;
    assert!(eval(err_source).is_err());

    let source = r#"
		function add(a, b) {
			return a + b;
		}
        var c = add(1, 2);
    "#;
    let func = eval(source).unwrap();
    let scope = func.inspect();
    assert_eq!(scope.get_pri_inspect("c"), Some(&Primitives::Num(3.0)));
}

#[test]
fn scope() {
    let tick_tock = r#"
        function tick(n) {
            var a = 42;
            function tock(n) {
                var a = 1;
                return n - a;
            }

            return a + tock(n);
        }

        var a = tick(1);
        var b = tick(42);
    "#;
    let rt = eval(tick_tock).unwrap();
    let scope = rt.inspect();
    assert_eq!(
        scope.get_pri_inspect("a"),
        Some(&Primitives::Num(tick(1.0)))
    );
    assert_eq!(
        scope.get_pri_inspect("b"),
        Some(&Primitives::Num(tick(42.0)))
    );
}

fn tick(n: f64) -> f64 {
    let a = 42.0;
    fn tock(n: f64) -> f64 {
        let a = 1.0;
        n - a
    }

    a + tock(n)
}

#[test]
fn scope2() {
    let mut_source = r#"
        var a = 1;
        var n = 10;

        function incr() {
            a = a + 1;
        }
        
        for (var i = 0; i < n; i = i + 1) {
            incr();
        }
    "#;

    let i = eval(&mut_source).unwrap();
    let s = i.inspect();
    let mut a = 1.0;
    let n = 10;
    for _ in 0..n {
        a += 1.0;
    }

    assert_eq!(s.get_pri_inspect("a"), Some(&Primitives::Num(a)));
}
