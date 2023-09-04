use crate::common::*;

#[derive(Debug)]
struct Lex {
    source: String,
    cursor: usize,
}

impl Lex {
    fn new(source: &str) -> Lex {
        Lex {
            source: source.to_string(),
            cursor: 0,
        }
    }

    fn peek(&self) -> char {
        self.source.chars().nth(self.cursor).unwrap()
    }

    fn peek_more(&self, offset: usize) -> char {
        self.source.chars().nth(self.cursor + offset).unwrap()
    }

    fn read(&mut self) -> char {
        let c = self.peek();
        self.cursor += 1;
        c
    }

    fn eof(&self) -> bool {
        // self.cursor == self.source.len() - 1
        self.cursor == self.source.len()
    }

    fn read_until(&mut self, check: fn(char) -> bool) -> String {
        let mut result = String::new();

        while !self.eof() {
            let next = self.peek();
            if check(next) {
                break;
            } else {
                self.read();
                result.push(next)
            }
        }

        result
    }

    fn err(&self, more: &str) -> VasErr {
        VasErr::occurs_at(ErrKind::Lexer, self.cursor, &self.source, more)
    }
}

#[derive(Debug, Clone)]
pub struct TokenInfo {
    pub pos: usize,
    pub token: Token,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Token {
    Keyword(Keyword),
    Literal(Literal),
    Punctuaion(Punctuation),
    Identifier(Identifier),
}

#[derive(Debug, Clone, PartialEq)]
pub enum Literal {
    /// "hello"
    String(String),
    /// -42.01
    Number(Num),
    /// `true`
    True,
    /// `false`
    False,
    /// `null`
    Null,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Punctuation {
    /// `[`
    BraketLeft,
    /// `]`
    BraketRight,
    /// `{`
    BraceLeft,
    /// `}`
    BraceRight,
    /// `(`
    ParenLeft,
    /// `)`
    ParenRight,
    /// `+`
    Add,
    /// `-`
    Sub,
    /// `*`
    Mul,
    /// `/`
    Div,
    /// `=`
    Eq,
    /// `==`
    EqEq,
    /// `>`
    Gt,
    /// `<`
    Lt,
    /// `>=`
    GtEq,
    /// `<=`
    LtEq,
    /// `!`
    Not,
    /// `!=`
    NotEq,
    /// `||`
    Or,
    /// `&&`
    And,
    /// `,`
    Comma,
    /// `;`
    /// TODO is ; optional ??
    SemiColon,
}

///
/// golang's keywords as ref:
/// break        default      func         interface    select
/// case         defer        go           map          struct
/// chan         else         goto         package      switch
/// const        fallthrough  if           range        type
/// continue     for          import       return       var
///
/// es3's keywords:
/// break else new var
/// case finally return void
/// catch for switch while
/// continue function this with
/// default if throw
/// delete in try
/// do instanceof typeof
///
#[derive(Debug, Clone, PartialEq)]
pub enum Keyword {
    /// `var`
    Var,
    /// `function`
    Function,
    /// `return`
    Return,
    /// `if`
    If,
    /// `else`
    Else,
    /// `for`
    For,
    /// `continue`
    Continue,
    /// `break`
    Break,
}

type Tokens = Vec<TokenInfo>;

fn not_digit_or_dot(ch: char) -> bool {
    ch != '.' && !ch.is_ascii_digit()
}

pub fn tokenize(source: &str) -> VasResult<Tokens> {
    let mut tokens = vec![];
    let mut lex = Lex::new(source);

    while !lex.eof() {
        let pos = lex.cursor;
        let maybe_token = match lex.peek() {
            // trivial space
            ' ' | '\r' | '\t' | '\n' => {
                // just consume
                lex.read();
                None
            }
            // punctuations
            '[' => Some(Token::Punctuaion(Punctuation::BraketLeft)),
            ']' => Some(Token::Punctuaion(Punctuation::BraketRight)),
            '{' => Some(Token::Punctuaion(Punctuation::BraceLeft)),
            '}' => Some(Token::Punctuaion(Punctuation::BraceRight)),
            '(' => Some(Token::Punctuaion(Punctuation::ParenLeft)),
            ')' => Some(Token::Punctuaion(Punctuation::ParenRight)),
            '+' => Some(Token::Punctuaion(Punctuation::Add)),
            '-' => Some(Token::Punctuaion(Punctuation::Sub)),
            // '-' => {
            //     if lex.peek_more(1).is_ascii_digit() {
            //         let mut with_sign = String::from(lex.read());
            //         let only_digit = lex.read_until(not_digit_or_dot);
            //         with_sign.push_str(&only_digit);
            //         if let Ok(num) = with_sign.parse::<Num>() {
            //             tokens.push(TokenInfo {
            //                 pos,
            //                 token: Token::Literal(Literal::Number(num)),
            //             });
            //             None
            //         } else {
            //             return Err(lex.err("parse num failed"));
            //         }
            //     } else {
            //         Some(Token::Punctuaion(Punctuation::Sub))
            //     }
            // }
            '*' => Some(Token::Punctuaion(Punctuation::Mul)),
            '/' => {
                if lex.peek_more(1) == '/' {
                    // only // single line comment supported
                    lex.read_until(|ch| ch == '\r' || ch == '\n');
                    None
                } else {
                    Some(Token::Punctuaion(Punctuation::Div))
                }
            }
            '=' => {
                if lex.peek_more(1) == '=' {
                    // consume extra
                    lex.read();
                    Some(Token::Punctuaion(Punctuation::EqEq))
                } else {
                    Some(Token::Punctuaion(Punctuation::Eq))
                }
            }
            '>' => {
                if lex.peek_more(1) == '=' {
                    // consume extra
                    lex.read();
                    Some(Token::Punctuaion(Punctuation::GtEq))
                } else {
                    Some(Token::Punctuaion(Punctuation::Gt))
                }
            }
            '<' => {
                if lex.peek_more(1) == '=' {
                    // consume extra
                    lex.read();
                    Some(Token::Punctuaion(Punctuation::LtEq))
                } else {
                    Some(Token::Punctuaion(Punctuation::Lt))
                }
            }
            '!' => {
                if lex.peek_more(1) == '=' {
                    // consume extra
                    lex.read();
                    Some(Token::Punctuaion(Punctuation::NotEq))
                } else {
                    Some(Token::Punctuaion(Punctuation::Not))
                }
            }
            '|' => {
                if lex.peek_more(1) == '|' {
                    // consume extra
                    lex.read();
                    Some(Token::Punctuaion(Punctuation::Or))
                } else {
                    return Err(lex.err("bit-or not supported!"));
                }
            }
            '&' => {
                if lex.peek_more(1) == '&' {
                    // consume extra
                    lex.read();
                    Some(Token::Punctuaion(Punctuation::And))
                } else {
                    return Err(lex.err("bit-and not supported!"));
                }
            }
            ',' => Some(Token::Punctuaion(Punctuation::Comma)),
            ';' => Some(Token::Punctuaion(Punctuation::SemiColon)),
            // THE rest
            ch => {
                if ch == '"' {
                    // string
                    // consume fist "
                    lex.read();
                    let inside = lex.read_until(|ch| ch == '"');
                    // consume last "
                    lex.read();
                    tokens.push(TokenInfo {
                        token: Token::Literal(Literal::String(inside)),
                        pos,
                    });
                } else if ch.is_ascii_digit() {
                    // number
                    let d = lex.read_until(not_digit_or_dot);
                    if let Ok(num) = d.parse::<Num>() {
                        tokens.push(TokenInfo {
                            pos,
                            token: Token::Literal(Literal::Number(num)),
                        });
                    } else {
                        return Err(lex.err("parse num failed"));
                    }
                } else if ch.is_ascii_alphabetic() {
                    let id_or_rev = lex.read_until(|ch| !(ch.is_ascii_alphanumeric() || ch == '_'));
                    let token = match id_or_rev.as_str() {
                        // keyword
                        "var" => Token::Keyword(Keyword::Var),
                        "function" => Token::Keyword(Keyword::Function),
                        "return" => Token::Keyword(Keyword::Return),
                        "if" => Token::Keyword(Keyword::If),
                        "else" => Token::Keyword(Keyword::Else),
                        "for" => Token::Keyword(Keyword::For),
                        "continue" => Token::Keyword(Keyword::Continue),
                        "break" => Token::Keyword(Keyword::Break),
                        // true | false | null
                        "true" => Token::Literal(Literal::True),
                        "false" => Token::Literal(Literal::False),
                        "null" => Token::Literal(Literal::Null),
                        _ => Token::Identifier(id_or_rev),
                    };
                    tokens.push(TokenInfo { pos, token })
                } else {
                    return Err(lex.err(&format!("unexpected character: {}", ch)));
                }
                // already consumed
                None
            }
        };

        if let Some(token) = maybe_token {
            // consume one char
            lex.read();
            tokens.push(TokenInfo { pos, token });
        }
    }

    Ok(tokens)
}
