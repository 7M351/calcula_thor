use std::str::CharIndices;
use std::iter::Peekable;
use std::num::ParseFloatError;
use std::fmt;
use std::io::stdin;

fn main() {
    let mut buffer = String::new();
    stdin().read_line(&mut buffer).expect("Failed to read line.");
    let tokens = match tokenize(&buffer) {
        Ok(tokens) => tokens,
        Err((tokens, errors)) => {
            println!("Errors found during lexing:");
            for error in errors {
                println!("\t{:?} at position {}", error.kind, error.start);
            }
            println!("Continuing anyway...");

            tokens
        },
    };
    let result = parse(&tokens[..]);
    match result {
        Ok(number) => {
            for token in tokens {
                print!("{} ", token);
            }
            println!("= {}", number.0);
        },
        Err(()) => {
            println!("The expression could not be resolved.");
        }
    }
}

#[derive(Debug, Copy, Clone, PartialEq)]
enum Operator {
    Plus,
    Minus,
    Times,
    Slash,
    Power,
}

impl Operator {
    fn symbol(&self) -> char {
        match self {
            Self::Plus => '+',
            Self::Minus => '-',
            Self::Times => '*',
            Self::Slash => '/',
            Self::Power => '^',
        }
    }

    fn parse(c: char) -> Option<Self> {
        match c {
            '+' => Some(Self::Plus),
            '-' => Some(Self::Minus),
            '*' => Some(Self::Times),
            '/' => Some(Self::Slash),
            '^' => Some(Self::Power),
            _ => None,
        }
    }

    fn evaluate(&self, a: f64, b: f64) -> f64 {
        match self {
            Self::Plus => a + b,
            Self::Minus => a - b,
            Self::Times => a * b,
            Self::Slash => a / b,
            Self::Power => a.powf(b),
        }
    }
}

impl precedence::Operator for Operator {
    type Precedence = u8;

    fn precedence(&self) -> u8 {
        match self {
            Self::Plus => 0,
            Self::Minus => 0,
            Self::Times => 1,
            Self::Slash => 1,
            Self::Power => 2,
        }
    }
}

#[derive(Debug, Copy, Clone, PartialEq)]
struct Number(f64);

impl precedence::Expression<Operator> for Number {
    fn reduce(&mut self, operator: Operator, other: Self) {
        let new = operator.evaluate(self.0, other.0);

        *self = Number(new);
    }
}



fn parse_float(chars: &mut Peekable<CharIndices>) -> Result<Token, Error> {
    let mut buffer = String::new();
    let start = chars.peek()
        .map_or(0, |x| x.0);

    while let Some((_, character)) = chars.peek() {
        if character.is_ascii_digit() || *character == '.' {
            buffer.push(*character);
            chars.next();
        } else {
            break;
        }
    }

    match buffer.parse() {
        Ok(f) => Ok(Token::Number(Number(f))),
        Err(e) => Err(
            Error {
                start,
                kind: ErrorKind::FloatError(e),
            }
        ),
    }
}
#[derive(Debug, Copy, Clone, PartialEq)]
enum Token {
    Number(Number),
    Operator(Operator),
    OpeningBracket,
    ClosingBracket,
}

impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        use fmt::Write;

        match self {
            Token::Number(n) => {
                write!(f, "{}", n.0)
            },
            Token::Operator(op) => {
                f.write_char(op.symbol())
            },
            Token::OpeningBracket => {
                f.write_char('(')
            },
            Token::ClosingBracket => {
                f.write_char(')')
            },
        }
    }
}

#[derive(Debug)]
struct Error {
    start: usize,
    kind: ErrorKind,
}

#[derive(Debug)]
enum ErrorKind {
    FloatError(ParseFloatError),
    UnexpectedCharacter(char),
}

fn tokenize(query: &str) -> Result<Vec<Token>,(Vec<Token>, Vec<Error>)> {
    let mut tokens = Vec::new();
    let mut errors = Vec::new();
    let mut chars = query.char_indices().peekable();
    while let Some((_, char_ref)) = chars.peek() {
        let result = if char_ref.is_ascii_digit() || *char_ref == '.' {
            parse_float(&mut chars)
        } else {
            let (start, character) = chars.next().unwrap();
            
            if let Some(op) = Operator::parse(character) {
                Ok(Token::Operator(op))
            } else if character.is_whitespace() {
                continue;
            } else {
                match character {
                    '(' => Ok(Token::OpeningBracket),
                    ')' => Ok(Token::ClosingBracket),
                    c => Err(Error {
                        start,
                        kind: ErrorKind::UnexpectedCharacter(c),
                    }),
                }
            }
        };

        match result {
            Ok(t) => {
                tokens.push(t);
            },
            Err(e) => {
                errors.push(e);
            }
        }
    }

    if errors.is_empty() {
        Ok(tokens)
    } else {
        Err((tokens, errors))
    }
}

fn parse_left_terminal(tokens: &[Token]) -> (Result<Number, ()>, &[Token]) {
    match tokens {
        [Token::Operator(Operator::Plus), rest @ ..] => {
            parse_left_terminal(rest)
        },
        [Token::Operator(Operator::Minus), rest @ ..] => {
            let (result, rest) = parse_left_terminal(rest);
            (result.map(|n| Number(-n.0)), rest)
        },
        [Token::OpeningBracket, rest @ ..] => {
            let mut nesting_level = 1_isize;
            let mut brackets = rest
                .iter()
                .enumerate()
                .filter_map(
                    |x| {
                        let (idx, token) = x;
                        match token {
                            Token::OpeningBracket => Some((idx, 1)),
                            Token::ClosingBracket => Some((idx, -1)),
                            _ => None,
                        }
                    }
                );
            let closing_index = loop {
                match brackets.next() {
                    Some((idx, nesting_change)) => {
                        nesting_level += nesting_change;
                        if nesting_level == 0 {
                            break Some(idx);
                        }
                    },
                    None => {
                        break None;
                    }
                }
            };

            if let Some(closing_index) = closing_index {
                let contents = &rest[..closing_index];
                
                (parse(contents), &rest[(closing_index + 1)..])
            } else {
                (Err(()), &[])
            }
        },
        [Token::Number(number), rest @ ..] => {
            (Ok(*number), rest)
        },
        _ => (Err(()), &[]),
    }
}

fn parse(tokens: &[Token]) -> Result<Number, ()> {
    let (result, mut tokens) = parse_left_terminal(tokens);
    let mut stack = precedence::ExprStack::new(result?);
    
    loop {
        if let [Token::Operator(op), rest @ ..] = tokens {
            let op = *op;
            let (result, t) = parse_left_terminal(rest);
            tokens = t;
            stack.push(op, result?);
        } else {
            if tokens.is_empty() {
                break Ok(stack.finish());
            } else {
                break Err(());
            }
        }
    }
}

