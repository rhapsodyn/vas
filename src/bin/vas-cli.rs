use std::{env::args, fs::read_to_string};

use vas::interpreter::eval;

fn usage() -> &'static str {
    "usage:\nvas /path/to/yourscript.js"
}

fn main() {
    match args().nth(1) {
        Some(path) => match read_to_string(&path) {
            Ok(content) => match eval(&content) {
                Ok(_) => {}
                Err(err) => eprintln!("eval err: {}", err),
            },
            Err(err) => eprintln!("read: {} err: {}", path, err),
        },
        None => eprintln!("{}", usage()),
    }
}
