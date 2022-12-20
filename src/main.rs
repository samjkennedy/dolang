use core::panic;
use std::collections::HashMap;
use std::env;
use std::fmt;
use std::fmt::Display;
use std::fs;
use std::ops::Add;
use std::ops::Div;
use std::ops::Mul;
use std::ops::Not;
use std::ops::Rem;
use std::ops::Sub;
use std::vec;

//TODO: This doesn't handle multiline spans, switch to start byte and end byte or start byte and length
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
struct Span {
    row: usize,
    col: usize,
    len: usize,
}

impl Span {
    fn empty() -> Span {
        return Span {
            row: 0,
            col: 0,
            len: 0,
        };
    }

    fn from(start: Span, end: Span) -> Span {
        return Span {
            row: start.row,
            col: start.col - 1,
            len: end.col + 2 - start.col,
        };
    }
}

impl fmt::Display for Span {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}:{}", self.row, self.col)
    }
}

#[derive(Debug, PartialEq, Clone, Copy)]
enum TokenKind {
    WhiteSpace,
    Comment,
    Identifier,
    NumberLiteral(i64),
    //StringLiteral,
    TrueLiteral,
    FalseLiteral,
    Plus,
    Minus,
    Star,
    Slash,
    Percent,
    OpenAngle,
    CloseAngle,
    Equals,
    Ampersand,
    Pipe,
    Bang,
    PrintKeyword,
    SwapKeyword,
    PopKeyword,
    DupKeyword,
    RotKeyword,
    OverKeyword,
    EndKeyword,
    RunKeyword,
    DefineKeyword,
    IfKeyword,
    WhileKeyword,
    //ImportKeyword,

    //Syntax
    OpenSquareBrace,
    CloseSquareBrace,
    OpenParenthesis,
    CloseParenthesis,
    DashDash,
    QuestionQuestionQuestion,

    //Types
    IntKeyword,
    BoolKeyword,
    AnyKeyword,
    StringKeyword,
    ListKeyword,
    OfKeyword,

    //Builtins
    MapKeyword,
    ApplyKeyword,
    FilterKeyword,
    LenKeyword,
    FoldKeyword,
    ConcatKeyword,
    HeadKeyword,
    LastKeyword,
    TailKeyword,
    InitKeyword,
    ConsKeyword,
    SortKeyword,
    ReverseKeyword,
    MaxKeyword,
    NthKeyword,
}
#[derive(Debug, Clone, Copy)]
struct Token {
    kind: TokenKind,
    span: Span,
}

#[derive(Debug)]
enum LexingError {
    UnexpectedCharacter(char, Span),
}

impl fmt::Display for LexingError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            LexingError::UnexpectedCharacter(c, span) => {
                write!(f, "Unexpected character '{}' at {}", c, span)
            }
        }
    }
}

fn lex(input: &str) -> Result<Vec<Token>, LexingError> {
    let mut tokens: Vec<Token> = vec![];

    let keywords = HashMap::from([
        //Literals
        ("true".to_string(), TokenKind::TrueLiteral),
        ("false".to_string(), TokenKind::FalseLiteral),
        //Keywords
        ("print".to_string(), TokenKind::PrintKeyword),
        ("swap".to_string(), TokenKind::SwapKeyword),
        ("pop".to_string(), TokenKind::PopKeyword),
        ("dup".to_string(), TokenKind::DupKeyword),
        ("rot".to_string(), TokenKind::RotKeyword),
        ("over".to_string(), TokenKind::OverKeyword),
        ("end".to_string(), TokenKind::EndKeyword),
        ("run".to_string(), TokenKind::RunKeyword),
        ("define".to_string(), TokenKind::DefineKeyword),
        ("if".to_string(), TokenKind::IfKeyword),
        ("while".to_string(), TokenKind::WhileKeyword),
        //("import".to_string(), TokenKind::ImportKeyword),
        //Types
        ("int".to_string(), TokenKind::IntKeyword),
        ("bool".to_string(), TokenKind::BoolKeyword),
        ("string".to_string(), TokenKind::StringKeyword),
        ("any".to_string(), TokenKind::AnyKeyword),
        ("list".to_string(), TokenKind::ListKeyword),
        ("of".to_string(), TokenKind::OfKeyword),
        //Builtins
        ("map".to_string(), TokenKind::MapKeyword),
        ("apply".to_string(), TokenKind::ApplyKeyword),
        ("filter".to_string(), TokenKind::FilterKeyword),
        ("len".to_string(), TokenKind::LenKeyword),
        ("fold".to_string(), TokenKind::FoldKeyword),
        ("concat".to_string(), TokenKind::ConcatKeyword),
        ("head".to_string(), TokenKind::HeadKeyword),
        ("last".to_string(), TokenKind::LastKeyword),
        ("tail".to_string(), TokenKind::TailKeyword),
        ("init".to_string(), TokenKind::InitKeyword),
        ("cons".to_string(), TokenKind::ConsKeyword),
        ("sort".to_string(), TokenKind::SortKeyword),
        ("reverse".to_string(), TokenKind::ReverseKeyword),
        ("max".to_string(), TokenKind::MaxKeyword),
        ("nth".to_string(), TokenKind::NthKeyword),
        //Meta
        ("???".to_string(), TokenKind::QuestionQuestionQuestion),
    ]);

    for (zero_row, line) in input.split("\n").enumerate() {
        let row = zero_row + 1;
        let mut col = 0;

        let mut chars = line.chars().peekable();

        while col < line.len() && chars.peek().is_some() {
            let c = match chars.next() {
                Some(next) => next,
                None => unreachable!(),
            };

            if c == '+' {
                tokens.push(Token {
                    kind: TokenKind::Plus,
                    span: Span {
                        row,
                        col: col + 1,
                        len: 1,
                    },
                });
            } else if c == '-' {
                if let Some(next) = chars.peek() {
                    if *next == '-' {
                        tokens.push(Token {
                            kind: TokenKind::DashDash,
                            span: Span {
                                row,
                                col: col + 1,
                                len: 2,
                            },
                        });
                        chars.next();
                        col += 1;
                    } else {
                        tokens.push(Token {
                            kind: TokenKind::Minus,
                            span: Span {
                                row,
                                col: col + 1,
                                len: 1,
                            },
                        });
                    }
                }
            } else if c == '*' {
                tokens.push(Token {
                    kind: TokenKind::Star,
                    span: Span {
                        row,
                        col: col + 1,
                        len: 1,
                    },
                });
            } else if c == '/' {
                tokens.push(Token {
                    kind: TokenKind::Slash,
                    span: Span {
                        row,
                        col: col + 1,
                        len: 1,
                    },
                });
            } else if c == '%' {
                tokens.push(Token {
                    kind: TokenKind::Percent,
                    span: Span {
                        row,
                        col: col + 1,
                        len: 1,
                    },
                });
            } else if c == '>' {
                tokens.push(Token {
                    kind: TokenKind::OpenAngle,
                    span: Span {
                        row,
                        col: col + 1,
                        len: 1,
                    },
                });
            } else if c == '<' {
                tokens.push(Token {
                    kind: TokenKind::CloseAngle,
                    span: Span {
                        row,
                        col: col + 1,
                        len: 1,
                    },
                });
            } else if c == '=' {
                tokens.push(Token {
                    kind: TokenKind::Equals,
                    span: Span {
                        row,
                        col: col + 1,
                        len: 1,
                    },
                });
            } else if c == '!' {
                tokens.push(Token {
                    kind: TokenKind::Bang,
                    span: Span {
                        row,
                        col: col + 1,
                        len: 1,
                    },
                });
            } else if c == '&' {
                tokens.push(Token {
                    kind: TokenKind::Ampersand,
                    span: Span {
                        row,
                        col: col + 1,
                        len: 1,
                    },
                });
            } else if c == '|' {
                tokens.push(Token {
                    kind: TokenKind::Pipe,
                    span: Span {
                        row,
                        col: col + 1,
                        len: 1,
                    },
                });
            } else if c == '[' {
                tokens.push(Token {
                    kind: TokenKind::OpenSquareBrace,
                    span: Span {
                        row,
                        col: col + 1,
                        len: 1,
                    },
                });
            } else if c == ']' {
                tokens.push(Token {
                    kind: TokenKind::CloseSquareBrace,
                    span: Span {
                        row,
                        col: col + 1,
                        len: 1,
                    },
                });
            } else if c == '(' {
                tokens.push(Token {
                    kind: TokenKind::OpenParenthesis,
                    span: Span {
                        row,
                        col: col + 1,
                        len: 1,
                    },
                });
            } else if c == ')' {
                tokens.push(Token {
                    kind: TokenKind::CloseParenthesis,
                    span: Span {
                        row,
                        col: col + 1,
                        len: 1,
                    },
                });
            } else if c == '#' {
                let comment = &line[col..];
                tokens.push(Token {
                    kind: TokenKind::Comment,
                    span: Span {
                        row,
                        col: col + 1,
                        len: comment.chars().count(),
                    },
                });
                col = line.chars().count();
            // } else if c == '"' {
            //     let mut s = line.chars().nth(col + 1).unwrap();
            //     col += 1;
            //     let start_col = col + 1;
            //     let mut acc = "".to_string();
            //     while s != '"' && line.chars().nth(col).is_some() {
            //         acc.push(s);
            //         col += 1;
            //         s = line.chars().nth(col).or(Some('"')).unwrap();
            //     }
            //     col += 1;
            //     tokens.push(Token {
            //         kind: TokenKind::StringLiteral,
            //         span: Span {
            //             row,
            //             col: start_col,
            //             len: acc.chars().count(),
            //         },
            //             file: file.clone(),
            //         },
            //         text: acc,
            //     });
            } else if c.is_numeric() {
                let start_col = col;
                while chars.peek().is_some() && chars.peek().unwrap().is_numeric() {
                    chars.next();
                    col += 1;
                }

                let text = &line[start_col..col + 1];
                let value = text.parse::<i64>().unwrap();

                tokens.push(Token {
                    kind: TokenKind::NumberLiteral(value),
                    span: Span {
                        row,
                        col: start_col + 1,
                        len: text.len(),
                    },
                });
            } else if c.is_whitespace() {
                let start_col = col;
                while chars.peek().is_some() && chars.peek().unwrap().is_whitespace() {
                    chars.next();
                    col += 1;
                }
                tokens.push(Token {
                    kind: TokenKind::WhiteSpace,
                    span: Span {
                        row,
                        col: start_col,
                        len: col + 1 - start_col,
                    },
                });
            } else if c.is_alphabetic() || c == '?' || c == '_' {
                let start_col = col;

                while chars.peek().is_some()
                    && !chars.peek().unwrap().is_whitespace()
                    && !['[', ']', '(', ')'].contains(&chars.peek().unwrap())
                //TODO: Have a vec of all reserved characters
                {
                    chars.next();
                    col += 1;
                }

                let text = &line[start_col..col + 1];

                //Check for keywords
                let span = Span {
                    row,
                    col: start_col + 1,
                    len: col + 1 - start_col,
                };
                if keywords.contains_key(text) {
                    tokens.push(Token {
                        kind: *keywords.get(text).unwrap(),
                        span,
                    })
                } else {
                    tokens.push(Token {
                        kind: TokenKind::Identifier,
                        span,
                    });
                }
            } else {
                return Err(LexingError::UnexpectedCharacter(
                    c,
                    Span {
                        row,
                        col: col + 1,
                        len: 1,
                    },
                ));
            }
            col += 1;
        }
    }
    Ok(tokens)
}

//Parsing

#[derive(Clone, Debug, PartialEq, Eq)]
struct Function {
    name: String,
    arguments: Vec<Type>,
    returns: Vec<Type>,
    body: Vec<Operation>,
}

#[derive(Clone, Debug, PartialEq, Eq)]
enum Operation {
    Define(Function, Span),
    // Jmp(usize),          //PC to jump to
    // CndJmp(usize, bool), //PC to jump to, jump if false
    // Label(usize),        //PC
    Pop(Span),
    Swap(Span),
    Rot(Span),
    Dup(Span),
    Over(Span),
    Print(Span),
    If(Span),
    While(Span),

    List(Vec<Operation>, Span),
    Lambda(Vec<Operation>, Span),
    Run(Span),
    IntegerLiteral(i64, Span),
    BooleanLiteral(bool, Span),
    //StringLiteral(String, Span),
    Identifier(String, Span),
    Binary(BinaryOperator, Span),
    Unary(UnaryOperator, Span),
    NoOp,
    Map(Span),
    Apply(Span),
    Filter(Span),
    Len(Span),
    Fold(Span),
    Concat(Span),
    Head(Span),
    Tail(Span),
    Last(Span),
    Init(Span),
    Cons(Span),
    //Import(Vec<Function>, Span),
    Sort(Span),
    DumpTypeStack(Span),
    Reverse(Span),
    Nth(Span),
}

impl Display for Operation {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Operation::Define(_, _) => write!(f, "{}", "define"),
            Operation::Pop(_) => write!(f, "{}", "pop"),
            Operation::Swap(_) => write!(f, "{}", "swap"),
            Operation::Rot(_) => write!(f, "{}", "rot"),
            Operation::Dup(_) => write!(f, "{}", "dup"),
            Operation::Over(_) => write!(f, "{}", "over"),
            Operation::Print(_) => write!(f, "{}", "print"),
            Operation::If(_) => write!(f, "{}", "if"),
            Operation::While(_) => write!(f, "{}", "while"),
            Operation::List(_, _) => write!(f, "{}", "list"),
            Operation::Lambda(_, _) => write!(f, "{}", "lambda"),
            Operation::Run(_) => write!(f, "{}", "run"),
            Operation::IntegerLiteral(_, _) => write!(f, "{}", "integer literal"),
            Operation::BooleanLiteral(_, _) => write!(f, "{}", "boolean literal"),
            Operation::Identifier(name, _) => write!(f, "{}", name),
            Operation::Binary(binop, _) => write!(f, "{:?}", binop),
            Operation::Unary(unop, _) => write!(f, "{:?}", unop),
            Operation::NoOp => write!(f, "{}", "nop"),
            Operation::Map(_) => write!(f, "{}", "map"),
            Operation::Apply(_) => write!(f, "{}", "apply"),
            Operation::Filter(_) => write!(f, "{}", "filter"),
            Operation::Len(_) => write!(f, "{}", "length"),
            Operation::Fold(_) => write!(f, "{}", "fold"),
            Operation::Concat(_) => write!(f, "{}", "concatenate"),
            Operation::Head(_) => write!(f, "{}", "head"),
            Operation::Tail(_) => write!(f, "{}", "tail"),
            Operation::Last(_) => write!(f, "{}", "last"),
            Operation::Init(_) => write!(f, "{}", "initial"),
            Operation::Cons(_) => write!(f, "{}", "construct"),
            Operation::Sort(_) => write!(f, "{}", "sort"),
            Operation::DumpTypeStack(_) => write!(f, "{}", "???"),
            Operation::Reverse(_) => write!(f, "{}", "reverse"),
            Operation::Nth(_) => write!(f, "{}", "nth"),
        }
    }
}

impl Ord for Operation {
    fn cmp(&self, _: &Self) -> std::cmp::Ordering {
        todo!()
    }
}

impl PartialOrd for Operation {
    fn partial_cmp(&self, _: &Self) -> Option<std::cmp::Ordering> {
        todo!()
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
enum BinaryOperator {
    Add,
    Sub,
    Mul,
    Div,
    Rem,
    Max,
    Gt,
    Lt,
    Eq,
    And,
    Or,
}

#[derive(Clone, Debug, PartialEq, Eq)]
enum UnaryOperator {
    Neg,
}

#[derive(Debug)]
enum ParseError {
    UnknownIdentifier(String, Span),
    UnexpectedToken(TokenKind, TokenKind, Span),
}
impl fmt::Display for ParseError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            ParseError::UnknownIdentifier(identifier, _) => {
                write!(f, "Unknown identifier `{}`", identifier)
            }
            ParseError::UnexpectedToken(expected, actual, _) => {
                write!(f, "Expected `{:?}` but got {:?}", expected, actual)
            }
        }
    }
}

struct Parser {
    source: String,
    cursor: usize,
    tokens: Vec<Token>,
    operations: Vec<Operation>,
    functions: HashMap<String, Function>,
}

impl Parser {
    fn new(source: String, tokens: Vec<Token>) -> Parser {
        return Parser {
            source,
            cursor: 0,
            tokens: tokens
                .into_iter()
                .filter(|tok| tok.kind != TokenKind::WhiteSpace)
                .collect(),
            operations: vec![],
            functions: HashMap::new(),
        };
    }

    fn parse_operation(&mut self) -> Result<Operation, ParseError> {
        while self.cursor < self.tokens.len() {
            let token = &self.tokens[self.cursor];
            let span = token.span;

            match token.kind {
                TokenKind::WhiteSpace | TokenKind::Comment | TokenKind::EndKeyword => {
                    return Ok(Operation::NoOp)
                }
                TokenKind::NumberLiteral(i) => {
                    return Ok(Operation::IntegerLiteral(i, span));
                }

                // TokenKind::StringLiteral => {
                //     return Ok(Operation::StringLiteral(token.text.clone(), span));
                // }
                TokenKind::TrueLiteral => return Ok(Operation::BooleanLiteral(true, span)),
                TokenKind::FalseLiteral => return Ok(Operation::BooleanLiteral(false, span)),
                TokenKind::Plus => return Ok(Operation::Binary(BinaryOperator::Add, span)),
                TokenKind::Minus => return Ok(Operation::Binary(BinaryOperator::Sub, span)),
                TokenKind::Star => return Ok(Operation::Binary(BinaryOperator::Mul, span)),
                TokenKind::Slash => return Ok(Operation::Binary(BinaryOperator::Div, span)),
                TokenKind::Percent => return Ok(Operation::Binary(BinaryOperator::Rem, span)),
                TokenKind::MaxKeyword => return Ok(Operation::Binary(BinaryOperator::Max, span)),
                TokenKind::OpenAngle => return Ok(Operation::Binary(BinaryOperator::Gt, span)),
                TokenKind::CloseAngle => return Ok(Operation::Binary(BinaryOperator::Lt, span)),
                TokenKind::Equals => return Ok(Operation::Binary(BinaryOperator::Eq, span)),
                TokenKind::Ampersand => return Ok(Operation::Binary(BinaryOperator::And, span)),
                TokenKind::Pipe => return Ok(Operation::Binary(BinaryOperator::Or, span)),
                TokenKind::Bang => return Ok(Operation::Unary(UnaryOperator::Neg, span)),
                TokenKind::OpenSquareBrace => {
                    self.cursor += 1;
                    return Ok(self.parse_list()?);
                }
                TokenKind::PrintKeyword => return Ok(Operation::Print(span)),
                TokenKind::SwapKeyword => return Ok(Operation::Swap(span)),
                TokenKind::PopKeyword => return Ok(Operation::Pop(span)),
                TokenKind::DupKeyword => return Ok(Operation::Dup(span)),
                TokenKind::RotKeyword => return Ok(Operation::Rot(span)),
                TokenKind::OverKeyword => return Ok(Operation::Over(span)),

                TokenKind::IfKeyword => return Ok(Operation::If(span)),
                TokenKind::WhileKeyword => return Ok(Operation::While(span)),

                // TokenKind::ImportKeyword => {
                //     self.cursor += 1;
                //     let import = self.match_token(TokenKind::StringLiteral)?;
                //     let file_path = ".\\core\\".to_string() + &import.text + &".do".to_string();

                //     if let Ok(contents) = fs::read_to_string(file_path.clone()) {
                //         let tokens = lex(&contents).unwrap(); //TODO: Errors from other files

                //         let mut parser = Parser::new(tokens);
                //         match parser.parse() {
                //             Ok(ops) => {
                //                 let mut functions: Vec<Function> = vec![];
                //                 for op in ops {
                //                     match op {
                //                         Operation::Define(function, _) => {
                //                             self.functions
                //                                 .insert(function.name.clone(), function.clone());
                //                             functions.push(function);
                //                         }
                //                         _ => {}
                //                     }
                //                 }
                //                 return Ok(Operation::Import(functions, span)); //TODO: Replace with Operation::Import(compilation_unit)
                //             }
                //             Err(error) => display_parse_error(error, &contents),
                //         }
                //     }
                // }
                TokenKind::CloseSquareBrace => {
                    unreachable!()
                }
                TokenKind::RunKeyword => return Ok(Operation::Run(span)),
                TokenKind::MapKeyword => return Ok(Operation::Map(span)),
                TokenKind::ApplyKeyword => return Ok(Operation::Apply(span)),
                TokenKind::FilterKeyword => return Ok(Operation::Filter(span)),
                TokenKind::LenKeyword => return Ok(Operation::Len(span)),
                TokenKind::FoldKeyword => return Ok(Operation::Fold(span)),
                TokenKind::ConcatKeyword => return Ok(Operation::Concat(span)),
                TokenKind::HeadKeyword => return Ok(Operation::Head(span)),
                TokenKind::LastKeyword => return Ok(Operation::Last(span)),
                TokenKind::TailKeyword => return Ok(Operation::Tail(span)),
                TokenKind::InitKeyword => return Ok(Operation::Init(span)),
                TokenKind::ConsKeyword => return Ok(Operation::Cons(span)),
                TokenKind::SortKeyword => return Ok(Operation::Sort(span)),
                TokenKind::ReverseKeyword => return Ok(Operation::Reverse(span)),
                TokenKind::NthKeyword => return Ok(Operation::Nth(span)),
                TokenKind::Identifier => {
                    let identifier = &self.source.lines().nth(span.row - 1).unwrap()
                        [span.col - 1..span.col - 1 + span.len]
                        .to_owned();
                    if self.functions.contains_key(identifier) {
                        return Ok(Operation::Identifier(identifier.clone(), span));
                    }
                    println!("Funcs: {:#?}", self.functions);
                    return Err(ParseError::UnknownIdentifier(
                        identifier.clone(),
                        token.span,
                    ));
                }
                TokenKind::DefineKeyword => {
                    self.cursor += 1;
                    let identifier_token = self.current();
                    let identifier_span = identifier_token.span;
                    let identifier = &self.source.lines().nth(identifier_span.row - 1).unwrap()
                        [identifier_span.col - 1..identifier_span.col - 1 + identifier_span.len]
                        .to_owned();

                    self.cursor += 1;

                    //parse signature
                    self.match_token(TokenKind::OpenParenthesis)?;
                    self.cursor += 1;
                    let mut args: Vec<Type> = vec![];
                    while self.current().kind != TokenKind::DashDash {
                        let arg_type = self.parse_type();
                        args.push(arg_type?);
                        self.cursor += 1;
                    }
                    self.match_token(TokenKind::DashDash)?;
                    self.cursor += 1;
                    let mut results: Vec<Type> = vec![];
                    while self.current().kind != TokenKind::CloseParenthesis {
                        let arg_type = self.parse_type();
                        results.push(arg_type?);
                        self.cursor += 1;
                    }
                    self.match_token(TokenKind::CloseParenthesis)?;
                    self.cursor += 1;

                    //insert dummy for recursion
                    self.functions.insert(
                        identifier.clone(),
                        Function {
                            name: identifier.clone(),
                            arguments: args.clone(),
                            returns: results.clone(),
                            body: vec![],
                        },
                    );

                    let mut body: Vec<Operation> = vec![];
                    while self.current().kind != TokenKind::EndKeyword {
                        let op = self.parse_operation()?;
                        body.push(op);
                        self.cursor += 1;
                    }
                    let function = Function {
                        name: identifier.clone(),
                        arguments: args.clone(),
                        returns: results.clone(),
                        body,
                    };
                    self.functions.insert(identifier.clone(), function.clone());
                    return Ok(Operation::Define(function, span));
                }
                TokenKind::OpenParenthesis => {
                    self.cursor += 1;
                    return Ok(self.parse_lambda()?);
                }
                TokenKind::CloseParenthesis => todo!(),
                TokenKind::DashDash => todo!(),
                TokenKind::IntKeyword => todo!(),
                TokenKind::BoolKeyword => todo!(),
                TokenKind::QuestionQuestionQuestion => return Ok(Operation::DumpTypeStack(span)),
                TokenKind::ListKeyword => todo!(),
                TokenKind::OfKeyword => todo!(),
                TokenKind::AnyKeyword => todo!(),
                TokenKind::StringKeyword => todo!(),
            }
        }
        todo!()
    }

    fn parse_type(&mut self) -> Result<Type, ParseError> {
        let arg_type = match self.current().kind {
            TokenKind::IntKeyword => Ok(Type::Int),
            TokenKind::BoolKeyword => Ok(Type::Bool),
            TokenKind::ListKeyword => {
                self.cursor += 1;
                self.match_token(TokenKind::OfKeyword)?;
                self.cursor += 1;
                Ok(Type::List(Box::new(self.parse_type()?)))
            }
            TokenKind::AnyKeyword => Ok(Type::Any),
            _ => Err(ParseError::UnexpectedToken(
                TokenKind::DashDash,
                self.current().kind,
                self.current().span,
            )),
        };
        arg_type
    }

    fn match_token(&mut self, expected: TokenKind) -> Result<&Token, ParseError> {
        let curr = self.current();
        if curr.kind == expected {
            return Ok(curr);
        }
        return Err(ParseError::UnexpectedToken(expected, curr.kind, curr.span));
    }

    fn parse(&mut self) -> Result<Vec<Operation>, ParseError> {
        while self.cursor < self.tokens.len() {
            let operation = self.parse_operation()?;
            //println!("{:?}", operation);
            self.operations.push(operation);
            self.cursor += 1;
        }
        return Ok(self.operations.clone());
    }

    fn parse_list(&mut self) -> Result<Operation, ParseError> {
        let mut operations: Vec<Operation> = vec![];

        let start = self.current().span;

        while self.current().kind != TokenKind::CloseSquareBrace {
            operations.push(self.parse_operation()?);
            self.cursor += 1;
        }
        let end = self.current().span;

        return Ok(Operation::List(operations, Span::from(start, end)));
    }

    fn parse_lambda(&mut self) -> Result<Operation, ParseError> {
        let mut operations: Vec<Operation> = vec![];

        let start = self.current().span;

        while self.current().kind != TokenKind::CloseParenthesis {
            operations.push(self.parse_operation()?);
            self.cursor += 1;
        }
        let end = self.current().span;

        return Ok(Operation::Lambda(operations, Span::from(start, end)));
    }

    fn current(&mut self) -> &Token {
        return &self.tokens[self.cursor];
    }
}

#[derive(Debug, Clone, Eq)]
enum Type {
    Int,
    Bool,
    List(Box<Type>),
    Function(Vec<Type>, Vec<Type>),
    Generic(i64),
    Any,
}

impl Display for Type {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Type::Int => write!(f, "{}", "int"),
            Type::Bool => write!(f, "{}", "bool"),
            //Type::String => write!(f, "{}", "string"),
            Type::List(el_type) => write!(f, "{} list", el_type),
            Type::Function(inputs, outputs) => write!(f, "fn ({:?} -- {:?})", inputs, outputs),
            Type::Any => write!(f, "{}", "any"),
            Type::Generic(idx) => write!(f, "generic `{}", idx),
        }
    }
}

impl PartialEq for Type {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::List(l0), Self::List(r0)) => {
                println!("Inside PartialEq for type!");
                return **l0 == Type::Any || **r0 == Type::Any || l0 == r0;
            }
            (Self::Function(l0, l1), Self::Function(r0, r1)) => l0 == r0 && l1 == r1,
            (Self::Generic(l0), Self::Generic(r0)) => l0 == r0,
            _ => core::mem::discriminant(self) == core::mem::discriminant(other),
        }
    }
}

struct Binder {
    type_stack: Vec<BoundType>,
}

struct BoundOperation {
    op: Operation,
    signature: Pair<Vec<BoundType>, Vec<BoundType>>,
}

impl Binder {
    fn new() -> Binder {
        return Binder { type_stack: vec![] };
    }

    fn bind(&mut self, ops: &Vec<Operation>) -> Result<Vec<BoundOperation>, Vec<TypeError>> {
        let mut bound_ops: Vec<BoundOperation> = vec![];
        let mut errors: Vec<TypeError> = vec![];
        for op in ops {
            match op.clone().bind(&mut self.type_stack) {
                Ok(bound_op) => bound_ops.push(bound_op),
                Err(err) => errors.push(err),
            }
        }
        if !errors.is_empty() {
            return Err(errors);
        }
        return Ok(bound_ops);
    }

    fn check(
        type_stack: &mut Vec<BoundType>,
        expected: Type,
        span: &Span,
    ) -> Result<BoundType, TypeError> {
        if type_stack.is_empty() {
            return Err(TypeError::EmptyStack(expected, *span));
        }
        let actual = type_stack.pop().unwrap();

        if actual.t == expected {
            return Ok(actual);
        }
        return Err(TypeError::TypeMismatch(expected, actual, *span));
    }
}

trait Bindable {
    fn bind(self, type_stack: &mut Vec<BoundType>) -> Result<BoundOperation, TypeError>;
}

impl Bindable for Operation {
    fn bind(self, type_stack: &mut Vec<BoundType>) -> Result<BoundOperation, TypeError> {
        match self {
            Operation::Define(_, _) => todo!(),
            Operation::Pop(span) => match type_stack.pop() {
                Some(bound_type) => {
                    return Ok(BoundOperation {
                        op: self,
                        signature: Pair {
                            left: vec![bound_type],
                            right: vec![],
                        },
                    });
                }
                None => Err(TypeError::EmptyStack(Type::Any, span)),
            },
            Operation::Swap(span) => match type_stack.pop() {
                Some(a) => match type_stack.pop() {
                    Some(b) => {
                        type_stack.push(a.clone());
                        type_stack.push(b.clone());
                        return Ok(BoundOperation {
                            op: self,
                            signature: Pair {
                                left: vec![a.clone(), b.clone()],
                                right: vec![b, a],
                            },
                        });
                    }
                    None => Err(TypeError::EmptyStack(Type::Any, span)),
                },
                None => Err(TypeError::EmptyStack(Type::Any, span)),
            },
            Operation::Rot(_) => todo!(),
            Operation::Dup(span) => match type_stack.pop() {
                Some(bound_type) => {
                    type_stack.push(bound_type.clone());
                    type_stack.push(bound_type.clone());
                    return Ok(BoundOperation {
                        op: self,
                        signature: Pair {
                            left: vec![bound_type.clone()],
                            right: vec![bound_type.clone(), bound_type],
                        },
                    });
                }
                None => Err(TypeError::EmptyStack(Type::Any, span)),
            },
            Operation::Over(_) => todo!(),
            Operation::Print(span) => match type_stack.pop() {
                Some(bound_type) => {
                    return Ok(BoundOperation {
                        op: self,
                        signature: Pair {
                            left: vec![bound_type],
                            right: vec![],
                        },
                    });
                }
                None => Err(TypeError::EmptyStack(Type::Any, span)),
            },
            Operation::If(_) => todo!(),
            Operation::While(_) => todo!(),
            Operation::List(elements, span) | Operation::Lambda(elements, span) => {
                match bind_sequence(&elements)? {
                    Type::List(element_type) => {
                        let bound_type = BoundType {
                            t: Type::List(element_type),
                            span,
                        };
                        type_stack.push(bound_type.clone());
                        return Ok(BoundOperation {
                            op: Operation::List(elements, span),
                            signature: Pair {
                                left: vec![],
                                right: vec![bound_type],
                            },
                        });
                    }
                    Type::Function(inputs, outputs) => {
                        let bound_type = BoundType {
                            t: Type::Function(inputs.clone(), outputs.clone()),
                            span,
                        };
                        type_stack.push(bound_type);
                        return Ok(BoundOperation {
                            op: Operation::Lambda(elements, span),
                            signature: Pair {
                                left: inputs
                                    .iter()
                                    .map(|t| BoundType { t: t.clone(), span })
                                    .collect(),
                                right: outputs
                                    .iter()
                                    .map(|t| BoundType { t: t.clone(), span })
                                    .collect(),
                            },
                        });
                    }
                    _ => unreachable!("Sequence pushed neither list nor function"),
                }
            }
            Operation::Run(_) => todo!(),

            //TODO: use the values of i and b to do const evaluation
            Operation::IntegerLiteral(_, span) => {
                let bound_type = BoundType { t: Type::Int, span };
                type_stack.push(bound_type.clone());
                return Ok(BoundOperation {
                    op: self,
                    signature: Pair {
                        left: vec![],
                        right: vec![bound_type],
                    },
                });
            }
            Operation::BooleanLiteral(_, span) => {
                let bound_type = BoundType {
                    t: Type::Bool,
                    span,
                };
                type_stack.push(bound_type.clone());
                return Ok(BoundOperation {
                    op: self,
                    signature: Pair {
                        left: vec![],
                        right: vec![BoundType {
                            t: Type::Bool,
                            span,
                        }],
                    },
                });
            }
            Operation::Identifier(_, _) => todo!(),
            Operation::Binary(binop, span) => match binop {
                BinaryOperator::Add
                | BinaryOperator::Sub
                | BinaryOperator::Mul
                | BinaryOperator::Div
                | BinaryOperator::Rem
                | BinaryOperator::Max => {
                    let a = Binder::check(type_stack, Type::Int, &span)?;
                    let b = Binder::check(type_stack, Type::Int, &span)?;

                    let result = BoundType { t: Type::Int, span };
                    type_stack.push(result.clone());

                    return Ok(BoundOperation {
                        op: Operation::Binary(binop, span),
                        signature: Pair {
                            left: vec![a, b],
                            right: vec![result],
                        },
                    });
                }

                BinaryOperator::Gt => todo!(),
                BinaryOperator::Lt => todo!(),
                BinaryOperator::Eq => todo!(),
                BinaryOperator::And => todo!(),
                BinaryOperator::Or => todo!(),
            },
            Operation::Unary(_, _) => todo!(),
            Operation::NoOp => {
                return Ok(BoundOperation {
                    op: Operation::NoOp,
                    signature: Pair {
                        left: vec![],
                        right: vec![],
                    },
                });
            }
            Operation::Map(span) => {
                if let Type::Function(ins, outs) = match type_stack.pop() {
                    Some(bound_type) => Ok(bound_type.t),
                    None => Err(TypeError::EmptyStack(Type::Any, span)),
                }? {
                    let mut a_span: Span = span;
                    if let Type::List(a) = match type_stack.pop() {
                        Some(bound_type) => {
                            a_span = bound_type.span;
                            Ok(bound_type.t)
                        }
                        None => Err(TypeError::EmptyStack(Type::Any, span)),
                    }? {
                        if ins.len() > 1 || !type_equals(&ins[0], &*a) {
                            // println!("{}", "---------------------");
                            // println!("ins: {:?}, a: {}", ins, a);
                            // println!("{}", "---------------------");
                            return Err(TypeError::TypeMismatch(
                                Type::List(Box::new(ins[0].clone())),
                                BoundType {
                                    t: Type::List(a.clone()),
                                    span: a_span,
                                },
                                span,
                            ));
                        }
                        if outs.len() > 1 {
                            //println!("2");
                            return Err(TypeError::TypeMismatch(
                                Type::Function(vec![*a.clone()], vec![outs[0].clone()]),
                                BoundType {
                                    t: outs[0].clone(),
                                    span,
                                },
                                span,
                            ));
                        }
                        let result = BoundType {
                            t: Type::List(Box::new(outs[0].clone())),
                            span,
                        };
                        type_stack.push(result.clone());
                        return Ok(BoundOperation {
                            op: Operation::Map(span),
                            signature: Pair {
                                left: vec![BoundType {
                                    t: Type::List(a.clone()),
                                    span,
                                }],
                                right: vec![result],
                            },
                        });
                    }
                }

                todo!();
            }
            Operation::Apply(span) => {
                if let Type::Function(ins, outs) = match type_stack.pop() {
                    Some(bound_type) => Ok(bound_type.t),
                    None => Err(TypeError::EmptyStack(Type::Any, span)),
                }? {
                    let mut a_span: Span = span;
                    match match type_stack.pop() {
                        Some(bound_type) => {
                            a_span = bound_type.span;
                            Ok(bound_type.t)
                        }
                        None => Err(TypeError::EmptyStack(Type::List(Box::new(Type::Any)), span)),
                    }? {
                        Type::List(a) => {
                            if ins.len() > 1 || !type_equals(&ins[0], &*a) {
                                return Err(TypeError::TypeMismatch(
                                    Type::List(Box::new(ins[0].clone())),
                                    BoundType {
                                        t: Type::List(a.clone()),
                                        span: a_span,
                                    },
                                    span,
                                ));
                            }
                            if outs.len() > 1 {
                                //println!("2");
                                return Err(TypeError::TypeMismatch(
                                    Type::Function(vec![*a.clone()], vec![outs[0].clone()]),
                                    BoundType {
                                        t: outs[0].clone(),
                                        span,
                                    },
                                    span,
                                ));
                            }
                            return Ok(BoundOperation {
                                op: Operation::Map(span),
                                signature: Pair {
                                    left: vec![BoundType {
                                        t: Type::List(a.clone()),
                                        span,
                                    }],
                                    right: vec![],
                                },
                            });
                        }
                        _ => unreachable!(),
                    }
                }
                unreachable!();
            }
            Operation::Filter(_) => todo!(),
            Operation::Len(_) => todo!(),
            Operation::Fold(span) => {
                let mut acc_actual: Option<BoundType> = None;
                let accumulator_type = match type_stack.pop() {
                    Some(bound_type) => {
                        acc_actual = Some(bound_type.clone());
                        Ok(bound_type.t)
                    }
                    None => Err(TypeError::EmptyStack(Type::Any, span)),
                }?;

                let mut func_actual: Option<BoundType> = None;
                if let Type::Function(ins, outs) = match type_stack.pop() {
                    Some(bound_type) => {
                        func_actual = Some(bound_type.clone());
                        Ok(bound_type.t)
                    }
                    None => Err(TypeError::EmptyStack(Type::Any, span)),
                }? {
                    let mut list_actual: Option<BoundType> = None;
                    if let Type::List(element_type) = match type_stack.pop() {
                        Some(bound_type) => {
                            list_actual = Some(bound_type.clone());
                            Ok(bound_type.t)
                        }
                        None => Err(TypeError::EmptyStack(Type::Any, span)),
                    }? {
                        if ins.len() != 2 || outs.len() != 1 {
                            return Err(TypeError::TypeMismatch(
                                Type::Function(
                                    vec![ins[0].clone(), ins[0].clone()],
                                    vec![accumulator_type],
                                ),
                                func_actual.unwrap(),
                                span,
                            ));
                        }
                        if !type_equals(&ins[0].clone(), &accumulator_type)
                            || !type_equals(&ins[1].clone(), &accumulator_type)
                            || !type_equals(&element_type, &accumulator_type)
                        {
                            return Err(TypeError::TypeMismatch(
                                *element_type,
                                acc_actual.unwrap(),
                                span,
                            ));
                        }
                        let result = BoundType {
                            t: accumulator_type.clone(),
                            span,
                        };
                        type_stack.push(result.clone());
                        return Ok(BoundOperation {
                            op: Operation::Fold(span),
                            signature: Pair {
                                left: vec![
                                    BoundType {
                                        t: accumulator_type.clone(),
                                        span,
                                    },
                                    BoundType {
                                        t: func_actual.clone().unwrap().t,
                                        span: func_actual.unwrap().span,
                                    },
                                    BoundType {
                                        t: list_actual.clone().unwrap().t,
                                        span: list_actual.unwrap().span,
                                    },
                                ],
                                right: vec![result],
                            },
                        });
                    } else {
                        return Err(TypeError::TypeMismatch(
                            Type::List(Box::new(outs[0].clone())),
                            list_actual.unwrap(),
                            span,
                        ));
                    }
                } else {
                    return Err(TypeError::TypeMismatch(
                        Type::Function(
                            vec![Type::List(Box::new(accumulator_type.clone()))],
                            vec![accumulator_type],
                        ),
                        func_actual.unwrap(),
                        span,
                    ));
                }
            }
            Operation::Concat(_) => todo!(),
            Operation::Head(_) => todo!(),
            Operation::Tail(_) => todo!(),
            Operation::Last(_) => todo!(),
            Operation::Init(_) => todo!(),
            Operation::Cons(_) => todo!(),
            Operation::Sort(_) => todo!(),
            Operation::DumpTypeStack(span) => {
                return Err(TypeError::DumpTypeStack(
                    TypeStack {
                        type_stack: type_stack.to_vec(),
                    },
                    span.clone(),
                ))
            }
            Operation::Reverse(_) => todo!(),
            Operation::Nth(_) => todo!(),
        }
    }
}

fn type_equals(a: &Type, b: &Type) -> bool {
    return a == &Type::Any || b == &Type::Any || a == b;
}

fn bind_sequence(elements: &Vec<Operation>) -> Result<Type, TypeError> {
    let mut is_list = true;
    let mut el_type: Option<Type> = None;

    for element in elements {
        match element {
            Operation::IntegerLiteral(_, _) => match el_type.clone() {
                None => el_type = Some(Type::Int),
                Some(t) => {
                    if t != Type::Int {
                        return Err(TypeError::TypeMismatch(
                            Type::Int,
                            BoundType {
                                t,
                                span: get_span(&element),
                            },
                            get_span(&element),
                        ));
                    }
                }
            },
            Operation::BooleanLiteral(_, _) => match el_type.clone() {
                None => el_type = Some(Type::Bool),
                Some(t) => {
                    if t != Type::Bool {
                        return Err(TypeError::TypeMismatch(
                            Type::Bool,
                            BoundType {
                                t,
                                span: get_span(&element),
                            },
                            get_span(&element),
                        ));
                    }
                }
            },
            _ => {
                is_list = false;
                break;
            }
        }
    }

    if !is_list {
        return bind_lambda(elements);
    }

    match el_type {
        Some(t) => return Ok(Type::List(Box::new(t))),
        None => todo!(),
    }
}

fn bind_lambda(elements: &Vec<Operation>) -> Result<Type, TypeError> {
    let mut inputs: Vec<Type> = vec![];
    let mut outputs: Vec<Type> = vec![];

    let mut generic_index = 0;

    let mut generics: HashMap<i64, Option<Type>> = HashMap::new();

    let mut infer_types = |args: Vec<Type>,
                           returns: Vec<Type>,
                           span: &Span,
                           inputs: &mut Vec<Type>,
                           outputs: &mut Vec<Type>,
                           generics: &mut HashMap<i64, Option<Type>>|
     -> Result<(), TypeError> {
        for arg in args {
            if outputs.len() > 0 {
                let output = outputs.pop().unwrap();
                if let Type::Generic(index) = output {
                    match generics.get(&index) {
                        Some(generic) => {
                            if generic.clone().unwrap() != arg {
                                return Err(TypeError::TypeMismatch(
                                    arg,
                                    BoundType {
                                        t: generic.clone().unwrap(),
                                        span: span.clone(),
                                    },
                                    span.clone(),
                                ));
                            }
                        }
                        None => {
                            generics.insert(index, Some(arg.clone()));
                            //println!("generic {} erased to {}", index, arg);
                        }
                    }
                } else {
                    if output != arg {
                        return Err(TypeError::TypeMismatch(
                            arg,
                            BoundType {
                                t: output.clone(),
                                span: span.clone(),
                            },
                            span.clone(),
                        ));
                    }
                }
            } else {
                //Else start pulling from the inputs
                let input = inputs.pop();
                match input {
                    Some(mut t) => {
                        if let Type::Generic(idx) = t {
                            match generics.get(&idx).unwrap() {
                                Some(concrete_type) => {
                                    t = concrete_type.clone();
                                }
                                None => {}
                            }
                        }
                        if t != arg {
                            return Err(TypeError::TypeMismatch(
                                arg,
                                BoundType {
                                    t: t.clone(),
                                    span: span.clone(),
                                },
                                span.clone(),
                            ));
                        }
                    }
                    None => {
                        //we need to infer
                        inputs.push(arg);
                    }
                }
            }
        }

        for ret in returns {
            outputs.push(ret);
        }
        Ok(())
    };

    for element in elements {
        // println!("ins: {:?}, outs: {:?}", inputs, outputs);
        // println!("op: {}", element);
        match element {
            Operation::Dup(span) => {
                if inputs.is_empty() && outputs.is_empty() {
                    let generic_type = Type::Generic(generic_index);
                    generic_index += 1;
                    generics.insert(generic_index, None);

                    inputs.push(generic_type.clone());

                    outputs.push(generic_type.clone());
                    outputs.push(generic_type.clone());
                } else if outputs.len() > 0 {
                    let typ = outputs.last().unwrap();
                    let args = vec![typ.clone()];
                    let returns = vec![typ.clone(), typ.clone()];

                    infer_types(
                        args,
                        returns,
                        span,
                        &mut inputs,
                        &mut outputs,
                        &mut generics,
                    )?;
                } else {
                    return Err(TypeError::EmptyStack(Type::Any, span.clone()));
                }
            }
            Operation::Binary(binop, span) => match binop {
                BinaryOperator::Add
                | BinaryOperator::Sub
                | BinaryOperator::Mul
                | BinaryOperator::Div
                | BinaryOperator::Rem
                | BinaryOperator::Max => {
                    if inputs.is_empty() && outputs.is_empty() {
                        inputs.push(Type::Int);
                        inputs.push(Type::Int);

                        outputs.push(Type::Int);
                    } else {
                        let args = vec![Type::Int, Type::Int];
                        let returns = vec![Type::Int];

                        infer_types(
                            args,
                            returns,
                            span,
                            &mut inputs,
                            &mut outputs,
                            &mut generics,
                        )?;
                    }
                }
                BinaryOperator::Eq => {
                    if inputs.is_empty() && outputs.is_empty() {
                        let generic_type = Type::Generic(generic_index);
                        generic_index += 1;
                        generics.insert(generic_index, None);

                        inputs.push(generic_type.clone());
                        inputs.push(generic_type.clone());

                        outputs.push(Type::Bool);
                    } else if outputs.len() > 0 {
                        let a = outputs.last().unwrap();
                        let b = outputs.last().unwrap();
                        let args = vec![a.clone(), b.clone()];
                        let returns = vec![Type::Bool];

                        infer_types(
                            args,
                            returns,
                            span,
                            &mut inputs,
                            &mut outputs,
                            &mut generics,
                        )?;
                    } else {
                        return Err(TypeError::EmptyStack(Type::Any, span.clone()));
                    }
                }
                BinaryOperator::Gt | BinaryOperator::Lt => {
                    if inputs.is_empty() && outputs.is_empty() {
                        inputs.push(Type::Int);
                        inputs.push(Type::Int);

                        outputs.push(Type::Bool);
                    } else {
                        let args = vec![Type::Int, Type::Int];
                        let returns = vec![Type::Bool];

                        infer_types(
                            args,
                            returns,
                            span,
                            &mut inputs,
                            &mut outputs,
                            &mut generics,
                        )?;
                    }
                }
                BinaryOperator::And | BinaryOperator::Or => {
                    if inputs.is_empty() && outputs.is_empty() {
                        inputs.push(Type::Bool);
                        inputs.push(Type::Bool);

                        outputs.push(Type::Bool);
                    } else {
                        let args = vec![Type::Bool, Type::Bool];
                        let returns = vec![Type::Bool];

                        infer_types(
                            args,
                            returns,
                            span,
                            &mut inputs,
                            &mut outputs,
                            &mut generics,
                        )?;
                    }
                }
                _ => todo!(),
            },
            Operation::Print(span) => {
                if inputs.is_empty() && outputs.is_empty() {
                    let generic_type = Type::Generic(generic_index);
                    generic_index += 1;
                    generics.insert(generic_index, None);

                    inputs.push(generic_type.clone());
                } else if outputs.len() > 0 {
                    let a = outputs.last().unwrap();
                    let args = vec![a.clone()];

                    infer_types(args, vec![], span, &mut inputs, &mut outputs, &mut generics)?;
                } else {
                    return Err(TypeError::EmptyStack(Type::Any, span.clone()));
                }
            }
            Operation::Unary(unop, span) => match unop {
                UnaryOperator::Neg => {
                    if inputs.is_empty() && outputs.is_empty() {
                        inputs.push(Type::Bool);

                        outputs.push(Type::Bool);
                    } else {
                        let args = vec![Type::Bool];
                        let returns = vec![Type::Bool];

                        infer_types(
                            args,
                            returns,
                            span,
                            &mut inputs,
                            &mut outputs,
                            &mut generics,
                        )?;
                    }
                }
            },
            Operation::Define(_, _) => todo!(),
            Operation::Pop(_) => todo!(),
            Operation::Swap(_) => todo!(),
            Operation::Rot(_) => todo!(),
            Operation::Over(_) => todo!(),
            Operation::If(_) => todo!(),
            Operation::While(_) => todo!(),
            Operation::List(_, _) => todo!(),
            Operation::Lambda(_, _) => todo!(),
            Operation::Run(_) => todo!(),
            Operation::IntegerLiteral(_, span) => {
                if inputs.is_empty() && outputs.is_empty() {
                    outputs.push(Type::Int);
                } else {
                    let args = vec![];
                    let returns = vec![Type::Int];

                    infer_types(
                        args,
                        returns,
                        span,
                        &mut inputs,
                        &mut outputs,
                        &mut generics,
                    )?;
                }
            }
            Operation::BooleanLiteral(_, span) => {
                if inputs.is_empty() && outputs.is_empty() {
                    outputs.push(Type::Bool);
                } else {
                    let args = vec![];
                    let returns = vec![Type::Bool];

                    infer_types(
                        args,
                        returns,
                        span,
                        &mut inputs,
                        &mut outputs,
                        &mut generics,
                    )?;
                }
            }
            Operation::Identifier(_, _) => todo!(),
            Operation::NoOp => todo!(),
            Operation::Map(_) => todo!(),
            Operation::Apply(_) => todo!(),
            Operation::Filter(_) => todo!(),
            Operation::Len(_) => todo!(),
            Operation::Fold(_) => todo!(),
            Operation::Concat(_) => todo!(),
            Operation::Head(_) => todo!(),
            Operation::Tail(_) => todo!(),
            Operation::Last(_) => todo!(),
            Operation::Init(_) => todo!(),
            Operation::Cons(_) => todo!(),
            Operation::Sort(_) => todo!(),
            Operation::DumpTypeStack(_) => todo!(),
            Operation::Reverse(_) => todo!(),
            Operation::Nth(_) => todo!(),
        }
        //println!("ins: {:?}, outs: {:?}", inputs, outputs);
    }

    //erase generics
    for i in 0..inputs.len() {
        if let Type::Generic(idx) = inputs[i] {
            match generics.get(&idx) {
                Some(concrete) => match concrete {
                    Some(concrete) => inputs[i] = concrete.clone(),
                    None => todo!("Generic persisted through lambda"),
                },
                None => inputs[i] = Type::Any,
            }
        }
    }
    for i in 0..outputs.len() {
        if let Type::Generic(idx) = outputs[i] {
            match generics.get(&idx).unwrap() {
                Some(concrete) => outputs[i] = concrete.clone(),
                None => todo!("Generic persisted through lambda"),
            }
        }
    }

    return Ok(Type::Function(inputs, outputs));
}

enum TypeError {
    TypeMismatch(Type, BoundType, Span),
    EmptyStack(Type, Span),
    NonEmptyStack(TypeStack),
    DumpTypeStack(TypeStack, Span),
    IfBranchInputMismatch(Vec<Type>, Vec<Type>, Span),
    IfBranchOutputMismatch(Vec<Type>, Vec<Type>, Span),
}

impl fmt::Display for TypeError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            TypeError::TypeMismatch(expected, actual, _) => {
                write!(
                    f,
                    "Type mismatch, expected '{}' but the top of the stack was `{}`",
                    expected, actual.t
                )
            }
            TypeError::EmptyStack(expected, _) => {
                write!(f, "Expected '{}' but the stack was empty", expected)
            }
            TypeError::NonEmptyStack(stack) => {
                write!(
                    f,
                    "Stack was not empty at the end of the file.\nStack: {:?}",
                    stack
                        .type_stack
                        .iter()
                        .map(|bt| bt.t.clone())
                        .collect::<Vec<Type>>()
                )
            }
            TypeError::DumpTypeStack(type_stack, _) => {
                write!(
                    f,
                    "State of the stack:\n{:?}",
                    type_stack
                        .type_stack
                        .iter()
                        .map(|bt| bt.t.clone())
                        .collect::<Vec<Type>>()
                )
            }
            TypeError::IfBranchInputMismatch(then_inputs, else_inputs, _) => {
                write!(
                    f,
                    "Both branches of the `if` expression expect different inputs.\n",
                )?;
                write!(f, " Then expects: {:?}\n", then_inputs)?;
                write!(f, " Else expects: {:?}", else_inputs)
            }
            TypeError::IfBranchOutputMismatch(then_outputs, else_outputs, _) => {
                write!(
                    f,
                    "Both branches of the `if` expression generate different outputs.\n",
                )?;
                write!(f, " Then generates: {:?}\n", then_outputs)?;
                write!(f, " Else generates: {:?}", else_outputs)
            }
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
struct BoundType {
    t: Type,
    span: Span,
}

impl Display for BoundType {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "`{}`: introduced at {}", self.t, self.span)
    }
}

#[derive(Clone)]
struct TypeChecker {
    type_stack: Vec<BoundType>,
    known_functions: HashMap<String, BoundFunction>,
    type_count: i64,
    generics: HashMap<i64, Option<Type>>,
}

#[derive(Clone)]
struct BoundFunction {
    inputs: Vec<Type>,
    outputs: Vec<Type>,
}

//For printing
struct TypeStack {
    type_stack: Vec<BoundType>,
}

impl Display for TypeStack {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        for bound_type in self.type_stack.iter().rev() {
            write!(f, "{}: introduced at {}\n", bound_type.t, bound_type.span)?;
        }
        Ok(())
    }
}

#[derive(Debug, PartialEq, Clone, Copy, Eq, PartialOrd, Ord)]
struct Pair<L, R> {
    left: L,
    right: R,
}

impl TypeChecker {
    fn next_type_id(&mut self) -> i64 {
        let next = self.type_count;
        self.type_count += 1;
        return next;
    }

    fn check_type(
        &mut self,
        expected: Type,
        actual: Option<BoundType>,
        span: Span,
    ) -> Result<(), TypeError> {
        match actual {
            Some(bound_type) => {
                if let Type::Function(inputs, outputs) = bound_type.clone().t {
                    if let Type::Function(args, returns) = expected.clone() {
                        if inputs.len() != args.len() || outputs.len() != returns.len() {
                            println!("1");
                            return Err(TypeError::TypeMismatch(expected, bound_type, span));
                        }
                        for i in 0..inputs.len() {
                            let input = inputs[i].clone();
                            let arg = args[i].clone();

                            if !(input == Type::Any || arg == Type::Any || input == arg) {
                                println!("2");
                                return Err(TypeError::TypeMismatch(expected, bound_type, span));
                            }
                        }
                        for i in 0..outputs.len() {
                            let output = outputs[i].clone();
                            let ret = returns[i].clone();

                            if !(output == Type::Any || ret == Type::Any || output == ret) {
                                println!("3");
                                return Err(TypeError::TypeMismatch(expected, bound_type, span));
                            }
                        }
                        return Ok(());
                    } else if expected != Type::Any {
                        println!("4");
                        return Err(TypeError::TypeMismatch(expected, bound_type, span));
                    }
                }

                if let Type::List(actual_el_type) = bound_type.t.clone() {
                    if let Type::List(expected_el_type) = expected.clone() {
                        //Recursively check in case of List(List(Any)) and List(List(Int))
                        if let Err(_) = self.check_type(
                            *expected_el_type,
                            Some(BoundType {
                                t: *actual_el_type,
                                span: span,
                            }),
                            span,
                        ) {
                            return Err(TypeError::TypeMismatch(expected, bound_type, span));
                        }
                        return Ok(());
                    } else if expected != Type::Any {
                        println!("5");
                        return Err(TypeError::TypeMismatch(expected, bound_type, span));
                    }
                }

                // println!("bound type: {}", bound_type.t);
                // println!("expected type: {}", expected);

                // if let Type::Generic(type_id, _, concrete_param) = expected.clone() {
                //     match concrete_param {
                //         Some(t) => {
                //             if *t != bound_type.t {
                //                 println!("7");
                //                 return Err(TypeError::TypeMismatch(expected, bound_type, span));
                //             }
                //         }
                //         None => {
                //             let generic = self
                //                 .generics
                //                 .get(&type_id)
                //                 .expect("Generic was not reported");
                //             match generic {
                //                 Some(t) => {
                //                     if *t != bound_type.t {
                //                         println!("8");
                //                         return Err(TypeError::TypeMismatch(
                //                             expected, bound_type, span,
                //                         ));
                //                     }
                //                 }
                //                 None => {
                //                     self.generics.insert(type_id, Some(bound_type.t));
                //                 }
                //             }
                //             return Ok(());
                //         }
                //     }
                // }

                // if let Type::Generic(type_id, _, concrete_param) = bound_type.clone().t {
                //     match concrete_param {
                //         Some(t) => {
                //             if *t != expected {
                //                 println!("9");
                //                 return Err(TypeError::TypeMismatch(expected, bound_type, span));
                //             }
                //         }
                //         None => {
                //             let generic = self
                //                 .generics
                //                 .get(&type_id)
                //                 .expect("Generic was not reported");
                //             match generic {
                //                 Some(t) => {
                //                     if *t != expected {
                //                         println!("10");
                //                         return Err(TypeError::TypeMismatch(
                //                             expected, bound_type, span,
                //                         ));
                //                     }
                //                 }
                //                 None => {
                //                     self.generics.insert(type_id, Some(expected));
                //                 }
                //             }
                //             return Ok(());
                //         }
                //     }
                // }

                if bound_type.t != Type::Any && expected != Type::Any && bound_type.t != expected {
                    println!("6");
                    return Err(TypeError::TypeMismatch(expected, bound_type, span));
                }
                return Ok(());
            }
            None => return Err(TypeError::EmptyStack(expected, span)),
        }
    }

    fn type_check_parsed_program(&mut self, program: &Vec<Operation>) -> Result<(), TypeError> {
        let mut cursor = 0;

        let mut type_stack_stack: Vec<Vec<BoundType>> = vec![];

        let is_in_sequence = |type_stack_stack: Vec<Vec<BoundType>>| {
            return type_stack_stack.len() > 0;
        };

        while cursor < program.len() {
            //println!("{:#?}", program);
            let op = &program[cursor];
            self.type_check_op(
                op,
                is_in_sequence(type_stack_stack.clone()),
                &mut type_stack_stack,
            )?;
            cursor += 1;
        }

        if !self.type_stack.is_empty() {
            return Err(TypeError::NonEmptyStack(TypeStack {
                type_stack: self.type_stack.clone(),
            }));
        }

        Ok(())
    }

    fn get_signature(&mut self, op: &Operation) -> Pair<Vec<Type>, Vec<Type>> {
        match op {
            Operation::Define(_, _) => todo!(),
            Operation::Pop(_) => {
                return Pair {
                    left: vec![Type::Any],
                    right: vec![],
                }
            }
            Operation::Swap(_) => todo!(),
            Operation::Rot(_) => todo!(),
            Operation::Dup(_) => {
                return Pair {
                    left: vec![Type::Any],
                    right: vec![Type::Any, Type::Any],
                }
            }
            Operation::Over(_) => {
                return Pair {
                    left: vec![Type::Any, Type::Any],
                    right: vec![Type::Any, Type::Any, Type::Any],
                }
            }
            Operation::Print(_) => {
                return Pair {
                    left: vec![Type::Any],
                    right: vec![],
                }
            }
            Operation::If(_) => {
                return Pair {
                    left: vec![
                        Type::Bool,
                        Type::Function(vec![], vec![]),
                        Type::Function(vec![], vec![]),
                    ],
                    right: vec![],
                }
            }
            Operation::While(_) => todo!(),
            Operation::List(ops, _) | Operation::Lambda(ops, _) => {
                let mut list = true;
                let mut list_type: Option<Type> = None;
                for op in ops {
                    if list {
                        match op {
                            Operation::IntegerLiteral(_, _) => match list_type.clone() {
                                Some(el_type) => {
                                    if el_type != Type::Int {
                                        list_type = Some(Type::Any)
                                    }
                                }
                                None => list_type = Some(Type::Int),
                            },
                            Operation::BooleanLiteral(_, _) => match list_type.clone() {
                                Some(el_type) => {
                                    if el_type != Type::Bool {
                                        list_type = Some(Type::Any)
                                    }
                                }
                                None => list_type = Some(Type::Bool),
                            },
                            // Operation::StringLiteral(_, _) => match list_type.clone() {
                            //     Some(el_type) => {
                            //         if el_type != Type::String {
                            //             list_type = Some(Type::Any)
                            //         }
                            //     }
                            //     None => list_type = Some(Type::String),
                            // },
                            _ => list = false,
                        }
                    }
                }
                if list {
                    match list_type {
                        Some(t) => {
                            return Pair {
                                left: vec![],
                                right: vec![Type::List(Box::new(t))],
                            }
                        }
                        None => {
                            return Pair {
                                left: vec![],
                                right: vec![Type::List(Box::new(Type::Any))], //TODO: "None", "Unit", or "Void" type, perhaps?
                            };
                        }
                    }
                }
                unreachable!()
            }
            Operation::Run(_) => todo!(),
            Operation::IntegerLiteral(_, _) => {
                return Pair {
                    left: vec![],
                    right: vec![Type::Int],
                }
            }
            Operation::BooleanLiteral(_, _) => {
                return Pair {
                    left: vec![],
                    right: vec![Type::Bool],
                }
            }
            // Operation::StringLiteral(_, _) => {
            //     return Pair {
            //         left: vec![],
            //         right: vec![Type::String],
            //     }
            // }
            Operation::Identifier(name, _) => {
                let function = self.known_functions.get(name).unwrap(); //If this blows up I didn't parse the program right

                return Pair {
                    left: function.inputs.clone(),
                    right: function.outputs.clone(),
                };
            }
            Operation::Binary(binop, _) => match binop {
                BinaryOperator::Add
                | BinaryOperator::Sub
                | BinaryOperator::Mul
                | BinaryOperator::Div
                | BinaryOperator::Rem
                | BinaryOperator::Max => {
                    return Pair {
                        left: vec![Type::Int, Type::Int],
                        right: vec![Type::Int],
                    }
                }
                BinaryOperator::Gt | BinaryOperator::Lt => {
                    return Pair {
                        left: vec![Type::Int, Type::Int],
                        right: vec![Type::Bool],
                    }
                }
                BinaryOperator::Eq => {
                    return Pair {
                        left: vec![Type::Any, Type::Any],
                        right: vec![Type::Bool],
                    }
                }
                BinaryOperator::And | BinaryOperator::Or => {
                    return Pair {
                        left: vec![Type::Bool, Type::Bool],
                        right: vec![Type::Bool],
                    }
                }
            },
            Operation::Unary(op, _) => match op {
                UnaryOperator::Neg => {
                    return Pair {
                        left: vec![Type::Bool],
                        right: vec![Type::Bool],
                    }
                }
            },
            Operation::NoOp => todo!(),
            Operation::Map(_) => {
                return Pair {
                    left: vec![
                        Type::Function(vec![Type::Any], vec![Type::Any]),
                        Type::List(Box::new(Type::Any)),
                    ],
                    right: vec![Type::List(Box::new(Type::Any))],
                }
            }
            Operation::Apply(_) => {
                return Pair {
                    left: vec![Type::Function(vec![Type::Any], vec![])],
                    right: vec![],
                }
            }
            Operation::Filter(_) => todo!(),
            Operation::Len(_) => {
                return Pair {
                    left: vec![Type::List(Box::new(Type::Any))],
                    right: vec![Type::Int],
                }
            }
            Operation::Fold(_) => {
                return Pair {
                    left: vec![
                        Type::Any,
                        Type::Function(vec![Type::Any, Type::Any], vec![Type::Any]),
                        Type::List(Box::new(Type::Any)),
                    ],
                    right: vec![Type::Any],
                };
            }
            Operation::Concat(_) => {
                //TODO: You now cannot concat two functions - may need separate keyword or a supertype Sequence
                return Pair {
                    left: vec![
                        Type::List(Box::new(Type::Any)),
                        Type::List(Box::new(Type::Any)),
                    ],
                    right: vec![Type::List(Box::new(Type::Any))],
                };
            }
            Operation::Head(_) => todo!(),
            Operation::Tail(_) => todo!(),
            Operation::Last(_) => todo!(),
            Operation::Init(_) => todo!(),
            Operation::Cons(_) => {
                return Pair {
                    left: vec![
                        Type::List(Box::new(Type::Any)),
                        Type::List(Box::new(Type::Any)),
                    ],
                    right: vec![Type::List(Box::new(Type::Any))],
                };
            }
            //Operation::Import(_, _) => todo!(),
            Operation::Sort(_) => {
                return Pair {
                    left: vec![Type::List(Box::new(Type::Any))],
                    right: vec![Type::List(Box::new(Type::Any))],
                };
            }
            Operation::Reverse(_) => {
                return Pair {
                    left: vec![Type::List(Box::new(Type::Any))],
                    right: vec![Type::List(Box::new(Type::Any))],
                };
            }
            Operation::DumpTypeStack(_) => todo!(),
            Operation::Nth(_) => {
                return Pair {
                    left: vec![Type::Int, Type::List(Box::new(Type::Any))],
                    right: vec![Type::List(Box::new(Type::Any))],
                };
            }
        }
    }

    fn type_check_op(
        &mut self,
        op: &Operation,
        is_in_sequence: bool,
        type_stack_stack: &mut Vec<Vec<BoundType>>,
    ) -> Result<(), TypeError> {
        Ok(match op {
            Operation::Define(function, span) => {
                self.known_functions.insert(
                    function.name.clone(),
                    BoundFunction {
                        inputs: function.arguments.clone(),
                        outputs: function.returns.clone(),
                    },
                );

                let current_stack = self.type_stack.clone();
                self.type_stack.clear();

                //Simulate stack
                for arg_type in function.clone().arguments {
                    self.type_stack.push(BoundType {
                        t: arg_type,
                        span: Span::empty(),
                    });
                }
                //Simulate body
                for op in function.clone().body {
                    self.type_check_op(&op, is_in_sequence, type_stack_stack)?;
                }
                //Check resulting stack
                for return_type in function.clone().returns {
                    let top = self.type_stack.pop();
                    self.check_type(return_type, top, *span)?;
                }
                if !self.type_stack.is_empty() {
                    return Err(TypeError::NonEmptyStack(TypeStack {
                        type_stack: self.type_stack.clone(),
                    }));
                }

                self.type_stack = current_stack;
            }
            Operation::Print(span) | Operation::Pop(span) => {
                self.type_check_top(is_in_sequence, Type::Any, span)?;
            }
            Operation::Swap(span) => {
                let a = self.type_check_top(is_in_sequence, Type::Any, span)?;
                let b = self.type_check_top(is_in_sequence, Type::Any, span)?;
                self.type_stack.push(BoundType {
                    t: a.clone().unwrap().t,
                    span: a.clone().unwrap().span,
                });
                self.type_stack.push(BoundType {
                    t: b.clone().unwrap().t,
                    span: b.clone().unwrap().span,
                });
            }
            Operation::Rot(span) => {
                let a = self.type_check_top(is_in_sequence, Type::Any, span)?;
                let b = self.type_check_top(is_in_sequence, Type::Any, span)?;
                let c = self.type_check_top(is_in_sequence, Type::Any, span)?;
                self.type_stack.push(BoundType {
                    t: b.clone().unwrap().t,
                    span: b.clone().unwrap().span,
                });
                self.type_stack.push(BoundType {
                    t: a.clone().unwrap().t,
                    span: a.clone().unwrap().span,
                });
                self.type_stack.push(BoundType {
                    t: c.clone().unwrap().t,
                    span: c.clone().unwrap().span,
                });
            }
            Operation::Dup(span) => {
                let top = self.type_check_top(is_in_sequence, Type::Any, span)?;
                self.type_stack.push(BoundType {
                    t: top.clone().unwrap().t,
                    span: top.clone().unwrap().span,
                });
                self.type_stack.push(BoundType {
                    t: top.clone().unwrap().t,
                    span: top.clone().unwrap().span,
                });
            }
            Operation::Over(span) => {
                let b = self.type_check_top(is_in_sequence, Type::Any, span)?;
                let a = self.type_check_top(is_in_sequence, Type::Any, span)?;
                self.type_stack.push(BoundType {
                    t: a.clone().unwrap().t,
                    span: a.clone().unwrap().span,
                });
                self.type_stack.push(BoundType {
                    t: b.clone().unwrap().t,
                    span: b.unwrap().span,
                });
                self.type_stack.push(BoundType {
                    t: a.clone().unwrap().t,
                    span: a.unwrap().span,
                });
            }
            Operation::List(ops, span) | Operation::Lambda(ops, span) => {
                type_stack_stack.push(self.type_stack.clone());
                self.type_stack.clear();

                let mut inputs: Vec<BoundType> = vec![];
                let mut outputs: Vec<BoundType> = vec![];

                let (list, list_type) = parse_seq_type(ops);

                for op in ops {
                    //self.apply(op, &mut inputs, &mut outputs)?;

                    //println!("{}", op);
                    self.type_check_op(op, true, type_stack_stack)?;
                    //println!("{:?}", self.type_stack);
                }

                self.type_stack = type_stack_stack.pop().unwrap();

                if list {
                    match list_type {
                        Some(t) => self.type_stack.push(BoundType {
                            t: Type::List(Box::new(t)),
                            span: *span,
                        }),
                        None => self.type_stack.push(BoundType {
                            t: Type::List(Box::new(Type::Any)), //TODO: also void, unit or none type?
                            span: *span,
                        }),
                    }
                } else {
                    let ins = inputs.iter().map(|t| t.clone().t).collect::<Vec<Type>>();
                    let outs = outputs.iter().map(|t| t.clone().t).collect::<Vec<Type>>();
                    self.type_stack.push(BoundType {
                        t: Type::Function(ins, outs),
                        span: *span,
                    });
                }
            }
            Operation::Run(span) => {
                let seq = self
                    .type_check_top(is_in_sequence, Type::Any, span)?
                    .unwrap()
                    .t;
                match seq {
                    Type::Function(inputs, outputs) => {
                        for input in inputs {
                            self.type_check_top(is_in_sequence, input, span)?;
                        }
                        for output in outputs {
                            self.type_stack.push(BoundType {
                                t: output,
                                span: *span,
                            });
                        }
                    }
                    // Type::List(els) => {
                    //     for el in els {
                    //         self.type_stack.push(BoundType {
                    //             t: el,
                    //             span: span,
                    //         });
                    //     }
                    // }
                    _ => panic!("Messed up type checking for run"),
                }
            }
            Operation::IntegerLiteral(_, span) => {
                self.type_stack.push(BoundType {
                    t: Type::Int,
                    span: *span,
                });
            }
            Operation::BooleanLiteral(_, span) => {
                self.type_stack.push(BoundType {
                    t: Type::Bool,
                    span: *span,
                });
            }

            // Operation::StringLiteral(_, span) => {
            //     self.type_stack.push(BoundType {
            //         t: Type::String,
            //         span: *span,
            //     });
            // }
            Operation::Identifier(identifier, span) => {
                if let Some(function) = self.clone().known_functions.get(identifier) {
                    for arg_type in function.inputs.clone() {
                        self.type_check_top(is_in_sequence, arg_type, span)?;
                    }

                    for result_type in function.outputs.clone() {
                        self.type_stack.push(BoundType {
                            t: result_type,
                            span: *span,
                        });
                    }
                } else {
                    panic!("Type checker can't find the function `{}`", identifier);
                }
            }
            Operation::Binary(op, span) => match op {
                BinaryOperator::Add
                | BinaryOperator::Sub
                | BinaryOperator::Mul
                | BinaryOperator::Div
                | BinaryOperator::Rem
                | BinaryOperator::Max => {
                    self.type_check_top(is_in_sequence, Type::Int, span)?;
                    self.type_check_top(is_in_sequence, Type::Int, span)?;
                    self.type_stack.push(BoundType {
                        t: Type::Int,
                        span: *span,
                    });
                }
                BinaryOperator::Gt | BinaryOperator::Lt => {
                    self.type_check_top(is_in_sequence, Type::Int, span)?;
                    self.type_check_top(is_in_sequence, Type::Int, span)?;
                    self.type_stack.push(BoundType {
                        t: Type::Bool,
                        span: *span,
                    });
                }
                BinaryOperator::Eq => {
                    let t = self.type_check_top(is_in_sequence, Type::Int, span)?;
                    self.type_check_top(is_in_sequence, t.unwrap().t, span)?;
                    self.type_stack.push(BoundType {
                        t: Type::Bool,
                        span: *span,
                    });
                }
                BinaryOperator::And | BinaryOperator::Or => {
                    self.type_check_top(is_in_sequence, Type::Bool, span)?;
                    self.type_check_top(is_in_sequence, Type::Bool, span)?;
                    self.type_stack.push(BoundType {
                        t: Type::Bool,
                        span: *span,
                    });
                }
            },
            Operation::Unary(op, span) => match op {
                UnaryOperator::Neg => {
                    self.type_check_top(is_in_sequence, Type::Bool, span)?;
                    self.type_stack.push(BoundType {
                        t: Type::Bool,
                        span: *span,
                    });
                }
            },
            Operation::NoOp => {}
            Operation::Filter(span) => {
                if let Type::Function(inputs, _) = self
                    .type_check_top(
                        is_in_sequence,
                        Type::Function(vec![Type::Any], vec![Type::Bool]),
                        span,
                    )?
                    .unwrap()
                    .t
                {
                    let list = self
                        .type_check_top(
                            is_in_sequence,
                            Type::List(Box::new(inputs[0].clone())),
                            span,
                        )?
                        .unwrap();
                    self.type_stack.push(BoundType {
                        t: list.t,
                        span: *span,
                    });
                } else {
                    panic!("Incorrectly parsed filter");
                }
            }
            Operation::Map(span) => {
                if let Type::Function(inputs, outputs) = self
                    .type_check_top(
                        is_in_sequence,
                        Type::Function(vec![Type::Any], vec![Type::Any]),
                        span,
                    )?
                    .unwrap()
                    .t
                {
                    self.type_check_top(
                        is_in_sequence,
                        Type::List(Box::new(inputs[0].clone())),
                        span,
                    )?
                    .unwrap();

                    self.type_stack.push(BoundType {
                        t: Type::List(Box::new(outputs[0].clone())),
                        span: *span,
                    });
                } else {
                    panic!();
                }
            }
            Operation::Apply(span) => {
                if let Type::Function(inputs, _) = self
                    .type_check_top(
                        is_in_sequence,
                        Type::Function(vec![Type::Any], vec![]),
                        span,
                    )?
                    .unwrap()
                    .t
                {
                    self.type_check_top(
                        is_in_sequence,
                        Type::List(Box::new(inputs[0].clone())),
                        span,
                    )?;
                }
            }
            Operation::Len(span) => {
                self.type_check_top(is_in_sequence, Type::List(Box::new(Type::Any)), span)?;
                self.type_stack.push(BoundType {
                    t: Type::Int,
                    span: *span,
                });
            }
            Operation::Fold(span) => {
                let acc = self
                    .type_check_top(is_in_sequence, Type::Any, span)?
                    .unwrap();

                self.type_check_top(
                    is_in_sequence,
                    Type::Function(vec![Type::Any, Type::Any], vec![acc.clone().t]),
                    span,
                )?
                .unwrap();

                self.type_check_top(is_in_sequence, Type::List(Box::new(acc.clone().t)), span)?
                    .unwrap();

                self.type_stack.push(BoundType {
                    t: acc.clone().t,
                    span: *span,
                });
            }
            Operation::Concat(span) => {
                if let Type::List(e1) = self
                    .type_check_top(is_in_sequence, Type::List(Box::new(Type::Any)), span)?
                    .unwrap()
                    .t
                {
                    if let Type::List(e2) = self
                        .type_check_top(is_in_sequence, Type::List(Box::new(Type::Any)), span)?
                        .unwrap()
                        .t
                    {
                        if e1 == e2 {
                            self.type_stack.push(BoundType {
                                t: Type::List(e1),
                                span: *span,
                            });
                        } else {
                            self.type_stack.push(BoundType {
                                t: Type::List(Box::new(Type::Any)),
                                span: *span,
                            });
                        }
                    } else {
                        panic!("Messed up type checking concat");
                    }
                } else {
                    panic!("Messed up type checking concat");
                }
            }
            Operation::Head(span) | Operation::Tail(span) => {
                if let Type::List(el) = self
                    .type_check_top(is_in_sequence, Type::List(Box::new(Type::Any)), span)?
                    .unwrap()
                    .t
                {
                    self.type_stack.push(BoundType {
                        t: *el,
                        span: *span,
                    });
                }
            }
            Operation::Last(span) | Operation::Init(span) => {
                let list = self
                    .type_check_top(is_in_sequence, Type::List(Box::new(Type::Any)), span)?
                    .unwrap()
                    .t;
                self.type_stack.push(BoundType {
                    t: list,
                    span: *span,
                });
            }
            Operation::Cons(span) => {
                let e1 = self
                    .type_check_top(is_in_sequence, Type::Any, span)?
                    .unwrap()
                    .t;
                let e2 = self
                    .type_check_top(is_in_sequence, Type::Any, span)?
                    .unwrap()
                    .t;

                if e1 == e2 {
                    self.type_stack.push(BoundType {
                        t: Type::List(Box::new(e1)),
                        span: *span,
                    });
                } else {
                    self.type_stack.push(BoundType {
                        t: Type::List(Box::new(Type::Any)),
                        span: *span,
                    });
                }
            }
            Operation::If(span) => {
                //TODO: Could pull out the then and else spans for clearer error reporting
                if let Type::Function(else_inputs, else_outputs) = self
                    .type_check_top(is_in_sequence, Type::Any, span)?
                    .unwrap()
                    .t
                {
                    if let Type::Function(then_inputs, then_outputs) = self
                        .type_check_top(is_in_sequence, Type::Any, span)?
                        .unwrap()
                        .t
                    {
                        self.type_check_top(is_in_sequence, Type::Bool, span)?;

                        if then_inputs.len() != else_inputs.len() {
                            return Err(TypeError::IfBranchInputMismatch(
                                then_inputs,
                                else_inputs,
                                *span,
                            ));
                        }
                        for i in 0..then_inputs.len() {
                            if let Err(_) = self.check_type(
                                then_inputs[i].clone(),
                                Some(BoundType {
                                    t: else_inputs[i].clone(),
                                    span: *span,
                                }),
                                *span,
                            ) {
                                return Err(TypeError::IfBranchInputMismatch(
                                    then_inputs,
                                    else_inputs,
                                    *span,
                                ));
                            }
                        }

                        if then_outputs.len() != else_outputs.len() {
                            return Err(TypeError::IfBranchOutputMismatch(
                                then_outputs,
                                else_outputs,
                                *span,
                            ));
                        }
                        for i in 0..then_outputs.len() {
                            if let Err(_) = self.check_type(
                                then_outputs[i].clone(),
                                Some(BoundType {
                                    t: else_outputs[i].clone(),
                                    span: *span,
                                }),
                                *span,
                            ) {
                                return Err(TypeError::IfBranchOutputMismatch(
                                    then_outputs,
                                    else_outputs,
                                    *span,
                                ));
                            }
                        }
                        for input in then_inputs {
                            self.type_check_top(is_in_sequence, input, span)?;
                        }
                        for output in then_outputs {
                            self.type_stack.push(BoundType {
                                t: output,
                                span: *span,
                            });
                        }
                    } else {
                        panic!("If parsed wrong");
                    }
                } else {
                    panic!("If parsed wrong");
                }
            }
            Operation::While(span) => {
                if let Type::Function(_, _) = self
                    .type_check_top(is_in_sequence, Type::Any, span)?
                    .unwrap()
                    .t
                {
                    if let Type::Function(_, _) = self
                        .type_check_top(
                            is_in_sequence,
                            Type::Function(vec![Type::Any], vec![Type::Any, Type::Bool]),
                            span,
                        )?
                        .unwrap()
                        .t
                    {
                        return Ok(());
                    } else {
                        panic!("While parsed wrong");
                    }
                } else {
                    panic!("While parsed wrong");
                }
                // self.type_check_top(is_in_sequence, Type::Seq, span)?;
            }
            Operation::Sort(span) | Operation::Reverse(span) => {
                let list = self
                    .type_check_top(is_in_sequence, Type::List(Box::new(Type::Any)), span)?
                    .unwrap();
                self.type_stack.push(BoundType {
                    t: list.t,
                    span: *span,
                });
            }
            Operation::DumpTypeStack(span) => {
                return Err(TypeError::DumpTypeStack(
                    TypeStack {
                        type_stack: self.type_stack.clone(),
                    },
                    *span,
                ));
            }
            Operation::Nth(span) => {
                self.type_check_top(is_in_sequence, Type::Int, span)?;
                if let Type::List(el_type) = self
                    .type_check_top(is_in_sequence, Type::Any, span)?
                    .unwrap()
                    .t
                {
                    self.type_stack.push(BoundType {
                        t: *el_type,
                        span: *span,
                    });
                } else {
                    panic!("nth parsed wrong");
                }
            }
        })
    }

    fn type_check_top(
        &mut self,
        is_in_sequence: bool,
        expected_type: Type,
        span: &Span,
    ) -> Result<Option<BoundType>, TypeError> {
        self.infer_type_stack(is_in_sequence, expected_type.clone());
        let top = self.type_stack.pop();
        self.check_type(expected_type.clone(), top.clone(), *span)?;
        Ok(top)
    }

    fn infer_type_stack(&mut self, is_in_sequence: bool, inferred_type: Type) {
        if is_in_sequence && self.type_stack.is_empty() {
            self.type_stack.push(BoundType {
                t: inferred_type,
                span: Span {
                    row: 0,
                    col: 0,
                    len: 0,
                },
            });
        }
    }

    fn new() -> TypeChecker {
        return TypeChecker {
            type_stack: vec![],
            known_functions: HashMap::new(),
            type_count: 0,
            generics: HashMap::new(),
        };
    }

    fn bind_signature(
        op: &Operation,
        signature: Pair<Vec<Type>, Vec<Type>>,
    ) -> Pair<Vec<BoundType>, Vec<BoundType>> {
        let mut bound_inputs: Vec<BoundType> = vec![];
        let mut bound_outputs: Vec<BoundType> = vec![];
        for input in signature.left {
            bound_inputs.push(BoundType {
                t: input,
                span: get_span(op),
            })
        }
        for output in signature.right {
            bound_outputs.push(BoundType {
                t: output,
                span: get_span(op),
            })
        }
        return Pair {
            left: bound_inputs,
            right: bound_outputs,
        };
    }

    fn apply(
        &mut self,
        op: &Operation,
        inputs: &mut Vec<BoundType>,
        outputs: &mut Vec<BoundType>,
    ) -> Result<(), TypeError> {
        let signature = TypeChecker::bind_signature(op, self.get_signature(op));

        //If it's a fresh slate then just add the inputs and outputs
        // println!("op: {}, sig: {:?}", op, signature);
        if inputs.is_empty() && outputs.is_empty() {
            *inputs = signature.left;
            *outputs = signature.right;
            // println!("inputs: {:?}", inputs);
            // println!("outputs: {:?}", outputs);
            return Ok(());
        }
        //after dup we have [any] -- [any any] // need generics to allow parameterization, we know that the two Any's are the same so if one is inferred we know the other
        //after * we want [int] -- [int]

        //otherwise we need to simulate running the operation
        for arg in signature.left.clone() {
            //If there's a type in the outputs, take it and check it's compatible
            if outputs.len() > 0 {
                let output = outputs.pop().unwrap();
                self.check_type(arg.t, Some(output), get_span(op))?;
                //If we're here then it's a match
            } else {
                //Else start pulling from the inputs
                let input = inputs.pop();
                match input {
                    Some(t) => {
                        self.check_type(arg.t, Some(t), get_span(op))?;
                    }
                    None => {
                        //we need to infer
                        inputs.push(arg);
                    }
                }
            }
        }

        //Then put the signature outputs in the outputs for the next op
        for ret in signature.right.clone() {
            outputs.push(ret);
        }
        // println!("inputs: {:?}", inputs);
        // println!("outputs: {:?}", outputs);

        return Ok(());
    }
}

fn parse_seq_type(ops: &Vec<Operation>) -> (bool, Option<Type>) {
    let mut list = true;
    let mut list_type: Option<Type> = None;
    for op in ops {
        if list {
            match op {
                Operation::IntegerLiteral(_, _) => match list_type.clone() {
                    Some(el_type) => {
                        if el_type != Type::Int {
                            list_type = Some(Type::Any)
                        }
                    }
                    None => list_type = Some(Type::Int),
                },
                Operation::BooleanLiteral(_, _) => match list_type.clone() {
                    Some(el_type) => {
                        if el_type != Type::Bool {
                            list_type = Some(Type::Any)
                        }
                    }
                    None => list_type = Some(Type::Bool),
                },
                // Operation::StringLiteral(_, _) => match list_type.clone() {
                //     Some(el_type) => {
                //         if el_type != Type::String {
                //             list_type = Some(Type::Any)
                //         }
                //     }
                //     None => list_type = Some(Type::String),
                // },
                Operation::List(ops, _) | Operation::Lambda(ops, _) => {
                    let sub_type = parse_seq_type(ops);
                    if !sub_type.0 || sub_type.1.is_none() {
                        return (false, None);
                    }
                    return (list, Some(Type::List(Box::new(sub_type.1.unwrap()))));
                }
                _ => list = false,
            }
        }
    }
    (list, list_type)
}

fn get_span(op: &Operation) -> Span {
    match op {
        Operation::Define(_, span)
        | Operation::Pop(span)
        | Operation::Swap(span)
        | Operation::Rot(span)
        | Operation::Dup(span)
        | Operation::Over(span)
        | Operation::Print(span)
        | Operation::If(span)
        | Operation::While(span)
        | Operation::List(_, span)
        | Operation::Lambda(_, span)
        | Operation::Run(span)
        | Operation::IntegerLiteral(_, span)
        | Operation::BooleanLiteral(_, span)
        | Operation::Identifier(_, span)
        | Operation::Binary(_, span)
        | Operation::Unary(_, span)
        | Operation::Map(span)
        | Operation::Apply(span)
        | Operation::Filter(span)
        | Operation::Len(span)
        | Operation::Fold(span)
        | Operation::Concat(span)
        | Operation::Head(span)
        | Operation::Tail(span)
        | Operation::Last(span)
        | Operation::Init(span)
        | Operation::Cons(span)
        | Operation::Sort(span)
        | Operation::DumpTypeStack(span)
        | Operation::Nth(span)
        | Operation::Reverse(span) => *span,

        Operation::NoOp => todo!(),
    }
}

//Interpreting

#[derive(Clone, Debug)]
enum Value {
    Integer(i64),
    Boolean(bool),
    //String(String),
    //Function(Pair<Vec<Type>, Vec<Type>>),
    Seq(Vec<Operation>),
}

impl Ord for Value {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        match (self, other) {
            (Self::Integer(l0), Self::Integer(r0)) => l0.cmp(r0),
            (Self::Boolean(l0), Self::Boolean(r0)) => l0.cmp(r0),
            _ => {
                println!("Cannot compare {} and {}", self, other);
                unreachable!()
            }
        }
    }
}

impl Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Value::Integer(i) => write!(f, "{}", i),
            Value::Boolean(b) => write!(f, "{}", b),
            // Value::Function(signature) => {
            //     write!(f, "fn ({} -- {})", signature.left, signature.right)
            // }
            Value::Seq(elements) => write!(f, "{:#?}", elements),
            //Value::String(s) => write!(f, "{}", s),
        }
    }
}
impl Add for Value {
    type Output = Value;

    fn add(self, rhs: Self) -> Value {
        if let Value::Integer(l) = self {
            match rhs {
                Value::Integer(r) => return Value::Integer(l + r),
                _ => {
                    unreachable!();
                }
            }
        // } else if let Value::String(l) = self {
        //     match rhs {
        //         Value::String(r) => return Value::String(l + &r),
        //         _ => {
        //             unreachable!();
        //         }
        //     }
        } else {
            unreachable!();
        }
    }
}
impl Sub for Value {
    type Output = Value;

    fn sub(self, rhs: Self) -> Value {
        if let Value::Integer(l) = self {
            match rhs {
                Value::Integer(r) => return Value::Integer(l - r),
                _ => {
                    unreachable!();
                }
            }
        } else {
            unreachable!();
        }
    }
}
impl Mul for Value {
    type Output = Value;

    fn mul(self, rhs: Self) -> Value {
        if let Value::Integer(l) = self {
            match rhs {
                Value::Integer(r) => return Value::Integer(l * r),
                _ => {
                    unreachable!();
                }
            }
        } else {
            unreachable!();
        }
    }
}
impl Div for Value {
    type Output = Value;

    fn div(self, rhs: Self) -> Value {
        if let Value::Integer(l) = self {
            match rhs {
                Value::Integer(r) => return Value::Integer(l / r),
                _ => {
                    unreachable!();
                }
            }
        } else {
            unreachable!();
        }
    }
}
impl Rem for Value {
    type Output = Value;
    fn rem(self, rhs: Self) -> Self::Output {
        if let Value::Integer(l) = self {
            match rhs {
                Value::Integer(r) => return Value::Integer(l % r),
                _ => {
                    unreachable!();
                }
            }
        } else {
            unreachable!();
        }
    }
}
impl PartialEq for Value {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::Integer(l0), Self::Integer(r0)) => l0 == r0,
            (Self::Boolean(l0), Self::Boolean(r0)) => l0 == r0,
            //(Self::String(l0), Self::String(r0)) => l0 == r0,
            //(Self::Function(l0), Self::Function(r0)) => l0 == r0,
            (Self::Seq(_), Self::Seq(_)) => todo!(),
            _ => {
                println!("Eq not implemented between {} and {}", self, other);
                unreachable!();
            }
        }
    }
}
impl Eq for Value {}

impl PartialOrd for Value {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        match (self, other) {
            (Self::Integer(l0), Self::Integer(r0)) => Some(l0.cmp(r0)),
            (Self::Boolean(l0), Self::Boolean(r0)) => Some(l0.cmp(r0)),
            _ => {
                println!("Cannot compare {} and {}", self, other);
                unreachable!()
            }
        }
    }
}
impl Not for Value {
    type Output = Value;
    fn not(self) -> Self::Output {
        match self {
            Value::Integer(_) => todo!(),
            Value::Boolean(b) => return Value::Boolean(!b),
            //Value::Function(_) => todo!(),
            Value::Seq(_) => todo!(),
            // Value::String(_) => todo!(),
        }
    }
}

struct Interpreter {
    pc: usize,
    program: Vec<Operation>,
    stack: Vec<Value>,
    functions: HashMap<String, Function>,
}

impl Interpreter {
    fn interpret(&mut self) {
        let operation = self.program[self.pc].clone();

        self.interpret_operation(&operation);
    }

    fn interpret_operation(&mut self, operation: &Operation) {
        // println!("{:?}", self.stack);
        // println!("{}", operation);
        match operation {
            Operation::NoOp => {}
            Operation::IntegerLiteral(i, _) => self.stack.push(Value::Integer(*i)),
            Operation::BooleanLiteral(b, _) => self.stack.push(Value::Boolean(*b)),

            //Operation::StringLiteral(s, _) => self.stack.push(Value::String(s.to_string())),
            Operation::Binary(op, _) => {
                let left_val = self.stack.pop().unwrap();
                let right_val = self.stack.pop().unwrap();

                match op {
                    BinaryOperator::Add => {
                        self.stack.push(right_val + left_val);
                    }
                    BinaryOperator::Sub => {
                        self.stack.push(right_val - left_val);
                    }
                    BinaryOperator::Mul => {
                        self.stack.push(right_val * left_val);
                    }
                    BinaryOperator::Div => {
                        self.stack.push(right_val / left_val);
                    }
                    BinaryOperator::Gt => {
                        self.stack.push(Value::Boolean(right_val > left_val));
                    }
                    BinaryOperator::Lt => {
                        self.stack.push(Value::Boolean(right_val < left_val));
                    }
                    BinaryOperator::Eq => {
                        self.stack.push(Value::Boolean(right_val == left_val));
                    }
                    BinaryOperator::Rem => {
                        self.stack.push(right_val % left_val);
                    }
                    BinaryOperator::And => {
                        match (left_val, right_val) {
                            (Value::Boolean(l), Value::Boolean(r)) => {
                                self.stack.push(Value::Boolean(r && l))
                            }
                            _ => panic!(),
                        };
                    }
                    BinaryOperator::Or => {
                        match (left_val, right_val) {
                            (Value::Boolean(l), Value::Boolean(r)) => {
                                self.stack.push(Value::Boolean(r || l))
                            }
                            _ => panic!(),
                        };
                    }
                    BinaryOperator::Max => {
                        let a = self.stack.pop().unwrap();
                        let b = self.stack.pop().unwrap();
                        if a > b {
                            self.stack.push(a);
                        } else {
                            self.stack.push(b);
                        }
                    }
                }
            }
            Operation::Unary(op, _) => {
                let operand = self.stack.pop().unwrap();
                match op {
                    UnaryOperator::Neg => self.stack.push(!operand),
                }
            }
            Operation::Pop(_) => {
                self.stack.pop();
            }
            Operation::Swap(_) => {
                let a = self.stack.pop().unwrap();
                let b = self.stack.pop().unwrap();
                self.stack.push(a);
                self.stack.push(b);
            }
            Operation::Rot(_) => {
                let a = self.stack.pop().unwrap();
                let b = self.stack.pop().unwrap();
                let c = self.stack.pop().unwrap();
                self.stack.push(b);
                self.stack.push(a);
                self.stack.push(c);
            }
            Operation::Dup(_) => {
                let val = self.stack.pop().unwrap();
                self.stack.push(val.clone());
                self.stack.push(val.clone());
            }

            Operation::Over(_) => {
                let b = self.stack.pop().unwrap();
                let a = self.stack.pop().unwrap();
                self.stack.push(a.clone());
                self.stack.push(b);
                self.stack.push(a);
            }
            Operation::Print(_) => {
                let val = self.stack.pop().unwrap();
                println!("{}", val);
            }
            Operation::List(ops, _) | Operation::Lambda(ops, _) => {
                self.stack.push(Value::Seq(ops.to_vec()))
            }
            Operation::Run(_) => {
                if let Value::Seq(ops) = self.stack.pop().unwrap() {
                    for op in ops {
                        self.interpret_operation(&op);
                    }
                }
            }
            Operation::Map(_) => {
                if let Value::Seq(operations) = self.stack.pop().unwrap() {
                    if let Value::Seq(operands) = self.stack.pop().unwrap() {
                        let mut results: Vec<Operation> = vec![];
                        for operand in operands.iter().rev() {
                            self.interpret_operation(&operand);

                            for operation in &operations {
                                self.interpret_operation(&operation)
                            }

                            let result = self.stack.pop().unwrap();
                            match result.clone() {
                                Value::Seq(ops) => {
                                    results.push(Operation::List(ops, Span::empty()));
                                }
                                Value::Integer(i) => {
                                    results.push(Operation::IntegerLiteral(i, Span::empty()));
                                }
                                Value::Boolean(b) => {
                                    results.push(Operation::BooleanLiteral(b, Span::empty()));
                                }
                            }
                        }
                        results.reverse();
                        self.stack.push(Value::Seq(results));
                    } else {
                        todo!()
                    }
                } else {
                    todo!()
                }
            }
            Operation::Apply(_) => {
                if let Value::Seq(operations) = self.stack.pop().unwrap() {
                    if let Value::Seq(operands) = self.stack.pop().unwrap() {
                        for operand in operands.iter().rev() {
                            self.interpret_operation(&operand);

                            for operation in &operations {
                                self.interpret_operation(&operation)
                            }
                        }
                    }
                }
            }
            Operation::Filter(_) => {
                if let Value::Seq(operations) = self.stack.pop().unwrap() {
                    let mut results: Vec<Operation> = vec![];
                    if let Value::Seq(operands) = self.stack.pop().unwrap() {
                        for operand in operands {
                            self.interpret_operation(&operand);

                            for operation in &operations {
                                self.interpret_operation(&operation)
                            }
                            if let Value::Boolean(b) = self.stack.pop().unwrap() {
                                if b {
                                    results.push(operand);
                                }
                            }
                        }
                        self.stack.push(Value::Seq(results));
                    }
                }
            }
            Operation::Concat(_) => {
                if let Value::Seq(right) = self.stack.pop().unwrap() {
                    let mut results: Vec<Operation> = vec![];
                    if let Value::Seq(left) = self.stack.pop().unwrap() {
                        for op in left {
                            results.push(op);
                        }
                        for op in right {
                            results.push(op);
                        }
                    }
                    self.stack.push(Value::Seq(results));
                }
            }
            Operation::Len(_) => {
                if let Value::Seq(operations) = self.stack.pop().unwrap() {
                    self.stack
                        .push(Value::Integer(operations.len().try_into().unwrap()));
                }
            }
            Operation::Fold(_) => {
                let mut accumulator = self.stack.pop().unwrap();
                if let Value::Seq(operations) = self.stack.pop().unwrap() {
                    let operands_value = self.stack.pop().unwrap();
                    match operands_value {
                        Value::Seq(operands) => {
                            for operand in operands {
                                self.interpret_operation(&operand);
                                self.stack.push(accumulator.clone());

                                for operation in &operations {
                                    self.interpret_operation(&operation)
                                }
                                accumulator = self.stack.pop().unwrap();
                                //println!("{}", accumulator);
                            }

                            self.stack.push(accumulator.clone());
                        }
                        _ => todo!("Operands were a {}", operands_value),
                    }
                } else {
                    todo!()
                }
            }
            Operation::Define(function, _) => {
                self.functions
                    .insert(function.name.clone(), function.to_owned());
            }
            Operation::Identifier(name, _) => {
                let function = self.functions.get(name).unwrap();
                for op in function.body.clone() {
                    self.interpret_operation(&op);
                }
            }
            Operation::Head(_) => {
                if let Value::Seq(operations) = self.stack.pop().unwrap() {
                    if operations.is_empty() {
                        self.stack.push(Value::Seq(vec![]));
                    } else {
                        self.interpret_operation(operations.last().unwrap());
                    }
                }
            }
            Operation::Tail(_) => {
                if let Value::Seq(operations) = self.stack.pop().unwrap() {
                    if operations.is_empty() {
                        self.stack.push(Value::Seq(vec![]));
                    } else {
                        self.stack.push(Value::Seq(operations[1..].to_vec()));
                    }
                }
            }
            Operation::Last(_) => {
                if let Value::Seq(operations) = self.stack.pop().unwrap() {
                    self.interpret_operation(operations.first().unwrap());
                }
            }
            Operation::Init(_) => {
                if let Value::Seq(operations) = self.stack.pop().unwrap() {
                    self.stack
                        .push(Value::Seq(operations[..operations.len() - 1].to_vec()));
                }
            }
            Operation::Cons(_) => {
                let top = self.stack.pop().unwrap();
                let op = match top {
                    Value::Integer(i) => Operation::IntegerLiteral(i, Span::empty()),
                    Value::Boolean(b) => Operation::BooleanLiteral(b, Span::empty()),
                    Value::Seq(ops) => Operation::List(ops, Span::empty()),
                };
                let rest = self.stack.pop().unwrap();
                match rest {
                    Value::Seq(mut operations) => {
                        operations.push(op);
                        self.stack.push(Value::Seq(operations));
                    }
                    Value::Integer(x) => {
                        let operations = vec![Operation::IntegerLiteral(x, Span::empty()), op];
                        self.stack.push(Value::Seq(operations));
                    }
                    Value::Boolean(b) => {
                        let operations = vec![Operation::BooleanLiteral(b, Span::empty()), op];
                        self.stack.push(Value::Seq(operations));
                    }
                }
            }
            Operation::If(_) => {
                if let Value::Seq(else_body) = self.stack.pop().unwrap() {
                    if let Value::Seq(then_body) = self.stack.pop().unwrap() {
                        if let Value::Boolean(condition) = self.stack.pop().unwrap() {
                            if condition {
                                for op in then_body {
                                    self.interpret_operation(&op);
                                }
                            } else {
                                for op in else_body {
                                    self.interpret_operation(&op);
                                }
                            }
                        }
                    }
                }
            }
            Operation::While(_) => {
                if let Value::Seq(body) = self.stack.pop().unwrap() {
                    if let Value::Seq(condition) = self.stack.pop().unwrap() {
                        for op in condition.clone() {
                            self.interpret_operation(&op);
                        }
                        let mut keep_looping = true;
                        while keep_looping {
                            if let Value::Boolean(result) = self.stack.pop().unwrap() {
                                keep_looping = result;
                                if result {
                                    for op in &body {
                                        self.interpret_operation(&op);
                                    }
                                }
                            } else {
                                panic!("While condition must produce a bool");
                            }
                            for op in &condition {
                                self.interpret_operation(&op);
                            }
                        }
                    }
                }
            }
            Operation::Sort(span) => {
                if let Value::Seq(ops) = self.stack.pop().unwrap() {
                    let mut values: Vec<Value> = vec![];
                    for op in ops {
                        match op {
                            Operation::IntegerLiteral(i, _) => values.push(Value::Integer(i)),
                            Operation::BooleanLiteral(b, _) => values.push(Value::Boolean(b)),
                            //Operation::StringLiteral(s, _) => values.push(Value::String(s)),
                            _ => panic!("Cannot sort {}", op),
                        }
                    }
                    values.sort();
                    let mut sorted_ops: Vec<Operation> = vec![];
                    for value in values.iter().rev() {
                        match value {
                            Value::Integer(i) => {
                                sorted_ops.push(Operation::IntegerLiteral(*i, *span))
                            }
                            Value::Boolean(b) => {
                                sorted_ops.push(Operation::BooleanLiteral(*b, *span))
                            }
                            // Value::String(s) => {
                            //     sorted_ops.push(Operation::StringLiteral(s.to_string(), *span))
                            // }
                            _ => panic!("Cannot sort {}", value),
                        }
                    }
                    self.stack.push(Value::Seq(sorted_ops));
                }
            }
            Operation::DumpTypeStack(_) => {}
            Operation::Reverse(span) => {
                if let Value::Seq(ops) = self.stack.pop().unwrap() {
                    let mut values: Vec<Value> = vec![];
                    for op in ops {
                        match op {
                            Operation::IntegerLiteral(i, _) => values.push(Value::Integer(i)),
                            Operation::BooleanLiteral(b, _) => values.push(Value::Boolean(b)),
                            //Operation::StringLiteral(s, _) => values.push(Value::String(s)),
                            _ => panic!("Cannot sort {}", op),
                        }
                    }
                    let mut sorted_ops: Vec<Operation> = vec![];
                    for value in values.iter().rev() {
                        match value {
                            Value::Integer(i) => {
                                sorted_ops.push(Operation::IntegerLiteral(*i, span.to_owned()))
                            }
                            Value::Boolean(b) => {
                                sorted_ops.push(Operation::BooleanLiteral(*b, span.to_owned()))
                            }
                            // Value::String(s) => sorted_ops
                            //     .push(Operation::StringLiteral(s.to_string(), span.to_owned())),
                            _ => panic!("Cannot reverse {}", value),
                        }
                    }
                    self.stack.push(Value::Seq(sorted_ops));
                }
            }
            Operation::Nth(_) => {
                if let Value::Integer(index) = self.stack.pop().unwrap() {
                    if let Value::Seq(els) = self.stack.pop().unwrap() {
                        let op = els.iter().rev().nth(index as usize).unwrap().clone();
                        match op {
                            Operation::IntegerLiteral(i, _) => self.stack.push(Value::Integer(i)),
                            Operation::BooleanLiteral(b, _) => self.stack.push(Value::Boolean(b)),
                            //Operation::StringLiteral(s, _) => values.push(Value::String(s)),
                            _ => panic!("Cannot nth {}", op),
                        }
                    }
                }
            }
        }
    }

    fn interpret_program(&mut self) {
        while self.pc < self.program.len().try_into().unwrap() {
            self.interpret();
            self.pc += 1;
            //println!("{:?}", self.stack);
        }
    }

    fn new(program: Vec<Operation>) -> Interpreter {
        return Interpreter {
            pc: 0,
            program,
            stack: vec![],
            functions: HashMap::new(),
        };
    }
}

//TODO: Have a map of index -> file_name
fn display_type_error(error: &TypeError, file_name: String, contents: &String) {
    if let TypeError::DumpTypeStack(_, span) = &error {
        print!("{} ", format!("{}", "Halting execution").yellow());
        println!("at {}:{}", file_name, span);
    } else {
        print!("{} ", format!("{}", "ERROR:").red());
    }
    print!("{}", error);
    match error {
        TypeError::TypeMismatch(_, actual, span) => {
            println!(" at {}:{}", file_name, span);
            print_span(span, contents, HighlightColor::RED);
            println!(
                "\n`{}` introduced at {}:{}",
                actual.t, file_name, actual.span
            );
            print_span(&actual.span, contents, HighlightColor::YELLOW);
        }
        TypeError::EmptyStack(_, span) => {
            println!(" at {}:{}", file_name, span);
            print_span(span, contents, HighlightColor::RED);
        }
        TypeError::IfBranchInputMismatch(_, _, span) => {
            println!(" at {}:{}", file_name, span);
            print_span(span, contents, HighlightColor::RED);
        }
        TypeError::IfBranchOutputMismatch(_, _, span) => {
            println!(" at {}:{}", file_name, span);
            print_span(span, contents, HighlightColor::RED);
        }
        TypeError::NonEmptyStack(type_stack) => {
            let mut stack_index = 0;
            for bound_type in type_stack.type_stack.iter().rev() {
                println!(
                    "\n[{}] {} introduced at {}:{}",
                    stack_index, bound_type.t, file_name, bound_type.span
                );
                print_span(&bound_type.span, contents, HighlightColor::RED);
                stack_index += 1;
            }
        }
        TypeError::DumpTypeStack(type_stack, _) => {
            let mut stack_index = 0;
            for bound_type in type_stack.type_stack.iter().rev() {
                println!(
                    "\n[{}] `{}` introduced at {}:{}",
                    stack_index, bound_type.t, file_name, bound_type.span
                );
                print_span(&bound_type.span, contents, HighlightColor::YELLOW);
                stack_index += 1;
            }
        }
    }
}

fn display_parse_error(error: ParseError, file_name: String, contents: &String) {
    print!("{} ", format!("{}", "ERROR:").red());
    match error {
        ParseError::UnknownIdentifier(_, span) => {
            println!("{} at {}:{}", error, file_name, span);
            print_span(&span, contents, HighlightColor::RED);
        }
        ParseError::UnexpectedToken(_, _, span) => {
            println!("{} at {}:{}", error, file_name, span);
            print_span(&span, contents, HighlightColor::RED);
        }
    }
}

enum HighlightColor {
    RED,
    YELLOW,
}

use colored::Colorize;
fn print_span(span: &Span, contents: &String, highlight_color: HighlightColor) {
    let line = contents.lines().nth(span.row - 1).unwrap();
    let pre = &line[0..span.col - 1];
    let error = &line[span.col - 1..span.col + span.len - 1];
    let post = &line[span.col + span.len - 1..];
    match highlight_color {
        HighlightColor::RED => println!(
            "{}:    {}{}{}",
            format!("{}", span.row).white(),
            format!("{}", pre).white(),
            format!("{}", error).red(),
            format!("{}", post).white(),
        ),
        HighlightColor::YELLOW => println!(
            "{}:    {}{}{}",
            format!("{}", span.row).white(),
            format!("{}", pre).white(),
            format!("{}", error).yellow(),
            format!("{}", post).white(),
        ),
    }
}

use std::time::Instant;
fn main() {
    let args: Vec<String> = env::args().collect();
    let file_path = &args[1];

    let measure = args.contains(&"-m".to_string());
    let disable_type_check = args.contains(&"-t".to_string());

    let start = Instant::now();

    let contents = fs::read_to_string(file_path).expect("Should have been able to read the file");

    let mut now = Instant::now();
    let tokens = lex(&contents);
    if measure {
        println!("Lexing took {}ms", now.elapsed().as_millis());
    }

    match tokens {
        Ok(ts) => {
            now = Instant::now();
            let mut parser = Parser::new(contents.clone(), ts);
            match parser.parse() {
                Ok(program) => {
                    if measure {
                        println!("Parsing took {}ms", now.elapsed().as_millis());
                    }
                    if !disable_type_check {
                        now = Instant::now();
                        // let mut type_checker = TypeChecker::new();
                        // if let Err(error) = type_checker.type_check_parsed_program(&program) {
                        //     if measure {
                        //         println!("Type Checking took {}ms", now.elapsed().as_millis());
                        //     }
                        //     display_type_error(error, file_path.to_string(), &contents);
                        //     return;
                        // }
                        let mut binder = Binder::new();
                        match binder.bind(&program) {
                            Ok(_) => {}
                            Err(errors) => {
                                for error in errors {
                                    display_type_error(&error, file_path.to_string(), &contents);
                                    println!();
                                }
                                return;
                            }
                        }
                        if measure {
                            println!("Type Checking took {}ms", now.elapsed().as_millis());
                        }
                    }
                    let mut interpreter = Interpreter::new(program);

                    now = Instant::now();
                    interpreter.interpret_program();
                    if measure {
                        println!("Interpreting took {}ms", now.elapsed().as_millis());
                    }
                }
                Err(error) => {
                    display_parse_error(error, file_path.to_string(), &contents);
                    return;
                }
            }
        }
        Err(err) => println!("{}", err),
    }
    if measure {
        println!(
            "Execution took a total of {}ms",
            start.elapsed().as_millis()
        );
    }
}
