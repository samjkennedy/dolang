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
        if end.col + 2 < start.col {
            //println!("TODO: Multi line spans");
            return Span {
                row: start.row,
                col: start.col - 1,
                len: 0,
            };
        }
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
    RangeKeyword,
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
    PartialKeyword,
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
        ("partial".to_string(), TokenKind::PartialKeyword),
        ("range".to_string(), TokenKind::RangeKeyword),
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
    Partial(Span),
    Range(Span),
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
            Operation::Partial(_) => write!(f, "{}", "partial"),
            Operation::Range(_) => write!(f, "{}", "range"),
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
    generics: HashMap<String, Type>,
    generic_index: i64,
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
            generics: HashMap::new(),
            generic_index: 0,
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
                TokenKind::PartialKeyword => return Ok(Operation::Partial(span)),
                TokenKind::RangeKeyword => return Ok(Operation::Range(span)),
                TokenKind::Identifier => {
                    let identifier = &self.source.lines().nth(span.row - 1).unwrap()
                        [span.col - 1..span.col - 1 + span.len]
                        .to_owned();
                    if self.functions.contains_key(identifier) {
                        return Ok(Operation::Identifier(identifier.clone(), span));
                    }
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
                        self.parse_type(&mut args, TokenKind::DashDash)?;
                    }
                    self.match_token(TokenKind::DashDash)?;
                    self.cursor += 1;
                    let mut results: Vec<Type> = vec![];
                    while self.current().kind != TokenKind::CloseParenthesis {
                        self.parse_type(&mut results, TokenKind::CloseParenthesis)?;
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

    fn parse_type(
        &mut self,
        types: &mut Vec<Type>,
        terminator: TokenKind,
    ) -> Result<(), ParseError> {
        match self.current().kind {
            TokenKind::IntKeyword => {
                types.push(Type::Int);
            }
            TokenKind::BoolKeyword => {
                types.push(Type::Bool);
            }
            TokenKind::ListKeyword => {
                if types.is_empty() {
                    return Err(ParseError::UnexpectedToken(
                        terminator,
                        self.current().kind,
                        self.current().span,
                    ));
                }
                let el_type = types.pop().unwrap();
                types.push(Type::List(Box::new(el_type)));
            }
            TokenKind::Identifier => {
                let span = self.current().span;
                let generic_name = &self.source.lines().nth(span.row - 1).unwrap()
                    [span.col - 1..span.col - 1 + span.len]
                    .to_owned();

                match self.generics.get(generic_name) {
                    Some(generic) => types.push(generic.clone()),
                    None => {
                        let generic = Type::Generic(self.next_generic());
                        self.generics
                            .insert(generic_name.to_string(), generic.clone());
                        types.push(generic);
                    }
                }
            }
            _ => {
                return Err(ParseError::UnexpectedToken(
                    terminator,
                    self.current().kind,
                    self.current().span,
                ));
            }
        }
        self.cursor += 1;
        Ok(())
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

    fn next_generic(&mut self) -> i64 {
        let idx = self.generic_index;
        self.generic_index += 1;
        return idx;
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
    functions: HashMap<String, Function>,
    generics: HashMap<i64, Option<Type>>,
}

struct BoundOperation {
    op: Operation,
    signature: Pair<Vec<BoundType>, Vec<BoundType>>,
}

impl Binder {
    fn new() -> Binder {
        return Binder {
            type_stack: vec![],
            functions: HashMap::new(),
            generics: HashMap::new(),
        };
    }

    fn bind(&mut self, ops: &Vec<Operation>) -> Result<Vec<BoundOperation>, Vec<TypeError>> {
        let mut bound_ops: Vec<BoundOperation> = vec![];
        let mut errors: Vec<TypeError> = vec![];
        for op in ops {
            match op.clone().bind(
                &mut self.type_stack,
                &mut self.functions,
                &mut self.generics,
            ) {
                Ok(bound_op) => bound_ops.push(bound_op),
                Err(err) => errors.push(err),
            }
        }
        if !errors.is_empty() {
            return Err(errors);
        }

        if !self.type_stack.is_empty() {
            return Err(vec![TypeError::NonEmptyStack(TypeStack {
                type_stack: self.type_stack.clone(),
            })]);
        }

        return Ok(bound_ops);
    }

    fn check(
        type_stack: &mut Vec<BoundType>,
        generics: &mut HashMap<i64, Option<Type>>,
        expected: Type,
        span: &Span,
    ) -> Result<BoundType, TypeError> {
        if type_stack.is_empty() {
            return Err(TypeError::EmptyStack(expected, *span));
        }
        let actual = type_stack.pop().unwrap();

        let mut final_expected = expected;

        if let Type::Generic(idx) = final_expected {
            //println!("Got a generic in expected! {} ", idx);
            if generics.contains_key(&idx) {
                match generics.get(&idx).unwrap() {
                    Some(concrete) => {
                        //println!("It had a concrete type {}", concrete);

                        final_expected = concrete.clone();
                    }
                    None => {
                        if let Type::Generic(_) = actual.t {
                        } else {
                            //println!("Set a concrete type of {}", actual.t);
                            final_expected = actual.t.clone();
                            generics.insert(idx, Some(actual.clone().t));
                        }
                    }
                }
            }
        }

        if type_equals(&actual.t, &final_expected) {
            return Ok(actual);
        }

        return Err(TypeError::TypeMismatch(final_expected, actual, *span));
    }
}

trait Bindable {
    fn bind(
        self,
        type_stack: &mut Vec<BoundType>,
        functions: &mut HashMap<String, Function>,
        generics: &mut HashMap<i64, Option<Type>>,
    ) -> Result<BoundOperation, TypeError>;
}

impl Bindable for Operation {
    fn bind(
        self,
        type_stack: &mut Vec<BoundType>,
        functions: &mut HashMap<String, Function>,
        generics: &mut HashMap<i64, Option<Type>>,
    ) -> Result<BoundOperation, TypeError> {
        match self {
            Operation::Define(function, span) => {
                let mut sub_stack: Vec<BoundType> = vec![];

                let mut bound_args: Vec<BoundType> = vec![];
                let mut bound_rets: Vec<BoundType> = vec![];

                for arg in function.clone().arguments.iter().rev() {
                    if let Type::Generic(idx) = arg {
                        if !generics.contains_key(&idx) {
                            generics.insert(*idx, None);
                        }
                    }
                    let bound_arg = BoundType {
                        t: arg.clone(),
                        span,
                    };
                    bound_args.push(bound_arg.clone());
                    sub_stack.push(bound_arg);
                }

                for op in function.clone().body {
                    op.bind(
                        &mut sub_stack,
                        &mut functions.clone(),
                        &mut generics.clone(),
                    )?;
                }
                for ret in function.clone().returns {
                    if let Type::Generic(idx) = ret {
                        if !generics.contains_key(&idx) {
                            generics.insert(idx, None);
                        }
                    }
                    let bound_ret = Binder::check(&mut sub_stack, generics, ret.clone(), &span)?;
                    bound_rets.push(bound_ret.clone());
                }
                bound_rets.reverse();

                if !sub_stack.is_empty() {
                    return Err(TypeError::NonEmptyStack(TypeStack {
                        type_stack: sub_stack.clone(),
                    }));
                }

                functions.insert(function.name.clone(), function.clone());

                return Ok(BoundOperation {
                    op: Operation::Define(function.clone(), span),
                    signature: Pair {
                        left: bound_args,
                        right: bound_rets,
                    },
                });
            }
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

            Operation::Rot(span) => match type_stack.pop() {
                Some(a) => match type_stack.pop() {
                    Some(b) => match type_stack.pop() {
                        Some(c) => {
                            type_stack.push(b.clone());
                            type_stack.push(a.clone());
                            type_stack.push(c.clone());
                            return Ok(BoundOperation {
                                op: self,
                                signature: Pair {
                                    left: vec![a.clone(), b.clone(), c.clone()],
                                    right: vec![b, a, c],
                                },
                            });
                        }
                        None => Err(TypeError::EmptyStack(Type::Any, span)),
                    },
                    None => Err(TypeError::EmptyStack(Type::Any, span)),
                },
                None => Err(TypeError::EmptyStack(Type::Any, span)),
            },
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
            Operation::Over(span) => match type_stack.pop() {
                Some(a) => match type_stack.pop() {
                    Some(b) => {
                        type_stack.push(b.clone());
                        type_stack.push(a.clone());
                        type_stack.push(BoundType {
                            t: b.t.clone(),
                            span,
                        });

                        return Ok(BoundOperation {
                            op: Operation::Over(span),
                            signature: Pair {
                                left: vec![a.clone(), b.clone()],
                                right: vec![b.clone(), a.clone(), b.clone()],
                            },
                        });
                    }
                    None => Err(TypeError::EmptyStack(Type::Any, span)),
                },
                None => Err(TypeError::EmptyStack(Type::Any, span)),
            },
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
            Operation::If(span) => match type_stack.pop() {
                Some(else_body) => {
                    if let Type::Function(else_ins, else_outs) = else_body.clone().t {
                        match type_stack.pop() {
                            Some(then_body) => {
                                if let Type::Function(then_ins, then_outs) = then_body.clone().t {
                                    if else_ins.len() != then_ins.len() {
                                        return Err(TypeError::IfBranchInputMismatch(
                                            then_ins, else_ins, span,
                                        ));
                                    }
                                    if else_outs.len() != then_outs.len() {
                                        return Err(TypeError::IfBranchOutputMismatch(
                                            then_outs, else_outs, span,
                                        ));
                                    }
                                    for i in 0..then_ins.len() {
                                        if !type_equals(&then_ins[i], &else_ins[i]) {
                                            return Err(TypeError::IfBranchInputMismatch(
                                                then_ins, else_ins, span,
                                            ));
                                        }
                                    }
                                    for i in 0..then_outs.len() {
                                        if !type_equals(&then_outs[i], &else_outs[i]) {
                                            return Err(TypeError::IfBranchOutputMismatch(
                                                then_outs, else_outs, span,
                                            ));
                                        }
                                    }
                                    match type_stack.pop() {
                                        Some(condition) => {
                                            if type_equals(&condition.t, &Type::Bool) {
                                                let mut if_ins =
                                                    vec![else_body, then_body, condition];
                                                if_ins.append(&mut bind_types(
                                                    then_ins.clone(),
                                                    span,
                                                ));

                                                let if_outs = bind_types(then_outs, span);

                                                for input in then_ins {
                                                    Binder::check(
                                                        type_stack, generics, input, &span,
                                                    )?;
                                                }

                                                for out in if_outs.clone() {
                                                    type_stack.push(out);
                                                }

                                                return Ok(BoundOperation {
                                                    op: Operation::If(span),
                                                    signature: Pair {
                                                        left: if_ins,
                                                        right: if_outs,
                                                    },
                                                });
                                            } else {
                                                return Err(TypeError::TypeMismatch(
                                                    Type::Bool,
                                                    condition.clone(),
                                                    span,
                                                ));
                                            }
                                        }
                                        None => {
                                            return Err(TypeError::EmptyStack(Type::Any, span));
                                        }
                                    }
                                } else {
                                    return Err(TypeError::TypeMismatch(
                                        Type::Function(else_ins, else_outs),
                                        then_body.clone(),
                                        span,
                                    ));
                                }
                            }
                            None => {
                                return Err(TypeError::EmptyStack(Type::Any, span));
                            }
                        }
                    } else {
                        return Err(TypeError::TypeMismatch(
                            Type::Function(vec![Type::Any], vec![Type::Any]),
                            else_body.clone(),
                            span,
                        ));
                    }
                }
                None => {
                    return Err(TypeError::EmptyStack(Type::Any, span));
                }
            },
            Operation::While(span) => match type_stack.pop() {
                Some(body) => match type_stack.pop() {
                    Some(condition) => {
                        if let Type::Function(cond_ins, cond_outs) = condition.clone().t {
                            if cond_outs.len() == 0 || cond_outs.last().unwrap() != &Type::Bool {
                                return Err(TypeError::TypeMismatch(
                                    Type::Function(vec![Type::Any], vec![Type::Bool]),
                                    condition,
                                    span,
                                ));
                            }

                            if let Type::Function(body_ins, body_outs) = body.t {
                                //TODO: ensure that the while body isn't filling up the stack
                                return Ok(BoundOperation {
                                    op: Operation::While(span),
                                    signature: Pair {
                                        left: bind_types(cond_ins, span),
                                        right: bind_types(body_outs, span),
                                    },
                                });
                            } else {
                                return Err(TypeError::TypeMismatch(
                                    Type::Function(vec![Type::Any], vec![Type::Any]),
                                    condition,
                                    span,
                                ));
                            }
                        }
                        return Err(TypeError::TypeMismatch(
                            Type::Function(vec![Type::Any], vec![Type::Bool]),
                            condition,
                            span,
                        ));
                    }
                    None => {
                        return Err(TypeError::EmptyStack(
                            Type::Function(vec![Type::Any], vec![Type::Bool]),
                            span,
                        ));
                    }
                },
                None => {
                    return Err(TypeError::EmptyStack(
                        Type::Function(vec![Type::Any], vec![Type::Any]),
                        span,
                    ));
                }
            },
            Operation::List(elements, span) | Operation::Lambda(elements, span) => {
                match bind_sequence(&elements, &functions)? {
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
            Operation::Identifier(name, span) => {
                if functions.contains_key(&name) {
                    let mut bound_args: Vec<BoundType> = vec![];
                    let mut bound_rets: Vec<BoundType> = vec![];
                    let function = functions.get(&name).unwrap();

                    for input in function.clone().arguments {
                        let bound_arg = BoundType {
                            t: input.clone(),
                            span,
                        };
                        bound_args.push(bound_arg.clone());
                        Binder::check(type_stack, generics, input, &span)?;
                    }
                    for output in function.clone().returns.iter().rev() {
                        let mut bound_ret = BoundType {
                            t: output.clone(),
                            span,
                        };
                        if let Type::Generic(idx) = bound_ret.t {
                            if generics.contains_key(&idx) {
                                match generics.get(&idx).unwrap() {
                                    Some(concrete) => {
                                        bound_ret = BoundType {
                                            t: concrete.clone(),
                                            span: bound_ret.span,
                                        }
                                    }
                                    None => todo!(),
                                }
                            }
                        }
                        bound_rets.push(bound_ret.clone());
                        type_stack.push(bound_ret);
                    }
                    return Ok(BoundOperation {
                        op: Operation::Identifier(name, span),
                        signature: Pair {
                            left: bound_args,
                            right: bound_rets,
                        },
                    });
                } else {
                    return Err(TypeError::UnknownIdentifier(name, span));
                }
            }
            Operation::Binary(binop, span) => match binop {
                BinaryOperator::Add
                | BinaryOperator::Sub
                | BinaryOperator::Mul
                | BinaryOperator::Div
                | BinaryOperator::Rem
                | BinaryOperator::Max => {
                    let a = Binder::check(type_stack, generics, Type::Int, &span)?;
                    let b = Binder::check(type_stack, generics, Type::Int, &span)?;

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

                BinaryOperator::Gt | BinaryOperator::Lt => {
                    let a = Binder::check(type_stack, generics, Type::Int, &span)?;
                    let b = Binder::check(type_stack, generics, Type::Int, &span)?;
                    let result = BoundType {
                        t: Type::Bool,
                        span,
                    };
                    type_stack.push(result.clone());

                    return Ok(BoundOperation {
                        op: Operation::Binary(binop, span),
                        signature: Pair {
                            left: vec![a, b],
                            right: vec![result],
                        },
                    });
                }
                BinaryOperator::Eq => {
                    //TODO: Generics? Must be the same type
                    let a = Binder::check(type_stack, generics, Type::Any, &span)?;
                    let b = Binder::check(type_stack, generics, Type::Any, &span)?;

                    let result = BoundType {
                        t: Type::Bool,
                        span,
                    };
                    type_stack.push(result.clone());

                    return Ok(BoundOperation {
                        op: Operation::Binary(binop, span),
                        signature: Pair {
                            left: vec![a, b],
                            right: vec![result],
                        },
                    });
                }
                BinaryOperator::And | BinaryOperator::Or => {
                    let a = Binder::check(type_stack, generics, Type::Bool, &span)?;
                    let b = Binder::check(type_stack, generics, Type::Bool, &span)?;

                    let result = BoundType {
                        t: Type::Bool,
                        span,
                    };
                    type_stack.push(result.clone());

                    return Ok(BoundOperation {
                        op: Operation::Binary(binop, span),
                        signature: Pair {
                            left: vec![a, b],
                            right: vec![result],
                        },
                    });
                }
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
                        other => {
                            return Err(TypeError::TypeMismatch(
                                Type::List(Box::new(Type::Any)),
                                BoundType {
                                    t: other,
                                    span: a_span.clone(),
                                },
                                span,
                            ))
                        }
                    }
                }
                unreachable!();
            }
            Operation::Filter(span) => match type_stack.pop() {
                Some(condition) => {
                    if let Type::Function(ins, outs) = condition.clone().t {
                        if outs.len() != 1 || !type_equals(&Type::Bool, &outs[0]) {
                            return Err(TypeError::TypeMismatch(
                                Type::Function(ins, vec![Type::Bool]),
                                condition,
                                span,
                            ));
                        }

                        match type_stack.pop() {
                            Some(list) => {
                                if let Type::List(el_type) = list.clone().t {
                                    if ins.len() != 1 || !type_equals(&el_type, &ins[0]) {
                                        return Err(TypeError::TypeMismatch(
                                            Type::Function(vec![*el_type], vec![Type::Bool]),
                                            condition,
                                            span,
                                        ));
                                    }

                                    let result = BoundType {
                                        t: Type::List(el_type),
                                        span: span.clone(),
                                    };
                                    type_stack.push(result.clone());

                                    return Ok(BoundOperation {
                                        op: Operation::Filter(span),
                                        signature: Pair {
                                            left: vec![condition, list],
                                            right: vec![result],
                                        },
                                    });
                                } else {
                                    return Err(TypeError::TypeMismatch(
                                        Type::List(Box::new(Type::Any)),
                                        list,
                                        span,
                                    ));
                                }
                            }
                            None => Err(TypeError::EmptyStack(Type::Any, span)),
                        }
                    } else {
                        return Err(TypeError::TypeMismatch(
                            Type::Function(vec![Type::Any], vec![Type::Bool]),
                            condition,
                            span,
                        ));
                    }
                }
                None => Err(TypeError::EmptyStack(Type::Any, span)),
            },
            Operation::Len(span) => match type_stack.pop() {
                Some(top) => {
                    if let Type::List(_) = top.t {
                        let result = BoundType { t: Type::Int, span };
                        type_stack.push(result.clone());

                        return Ok(BoundOperation {
                            op: Operation::Len(span),
                            signature: Pair {
                                left: vec![top],
                                right: vec![result],
                            },
                        });
                    } else {
                        Err(TypeError::TypeMismatch(
                            Type::List(Box::new(Type::Any)),
                            top,
                            span,
                        ))
                    }
                }
                None => Err(TypeError::EmptyStack(Type::Any, span)),
            },
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
                        if !type_equals(&element_type, &accumulator_type) {
                            println!("element_type {}", accumulator_type);

                            println!("{}", accumulator_type);
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
            Operation::Reverse(span) => match type_stack.pop() {
                Some(top) => {
                    if let Type::List(el_type) = top.t {
                        let result = BoundType {
                            t: Type::List(el_type),
                            span,
                        };
                        type_stack.push(result.clone());

                        return Ok(BoundOperation {
                            op: Operation::Reverse(span),
                            signature: Pair {
                                left: vec![result.clone()],
                                right: vec![result],
                            },
                        });
                    } else {
                        return Err(TypeError::EmptyStack(Type::List(Box::new(Type::Any)), span));
                    }
                }
                None => Err(TypeError::EmptyStack(Type::List(Box::new(Type::Any)), span)),
            },
            Operation::Nth(span) => {
                let index = Binder::check(type_stack, generics, Type::Int, &span)?;
                match type_stack.pop() {
                    Some(top) => {
                        if let Type::List(el_type) = top.clone().t {
                            let result = BoundType {
                                t: *el_type,
                                span: span,
                            };
                            type_stack.push(result.clone());

                            return Ok(BoundOperation {
                                op: Operation::Nth(span),
                                signature: Pair {
                                    left: vec![index, top],
                                    right: vec![result],
                                },
                            });
                        } else {
                            return Err(TypeError::TypeMismatch(
                                Type::List(Box::new(Type::Any)),
                                top,
                                span,
                            ));
                        }
                    }
                    None => {
                        return Err(TypeError::EmptyStack(Type::List(Box::new(Type::Any)), span));
                    }
                };
            }
            Operation::Partial(span) => match type_stack.pop() {
                Some(top) => {
                    if let Type::Function(ins, outs) = top.t {
                        match type_stack.pop() {
                            Some(arg) => {
                                if ins.len() > 0 && type_equals(&arg.t, &ins[0]) {
                                    type_stack.push(BoundType {
                                        t: Type::Function(ins[1..].to_vec(), outs.clone()),
                                        span,
                                    });

                                    return Ok(BoundOperation {
                                        op: Operation::Partial(span),
                                        signature: Pair {
                                            left: bind_types(ins[1..].to_vec(), span),
                                            right: bind_types(outs, span),
                                        },
                                    });
                                }
                                return Err(TypeError::TypeMismatch(ins[0].clone(), arg, span));
                            }
                            None => return Err(TypeError::EmptyStack(Type::Any, span)),
                        }
                    } else {
                        return Err(TypeError::TypeMismatch(
                            Type::Function(vec![Type::Any], vec![Type::Any]),
                            top,
                            span,
                        ));
                    }
                }
                None => {
                    return Err(TypeError::EmptyStack(
                        Type::Function(vec![Type::Any], vec![Type::Any]),
                        span,
                    ))
                }
            },
            Operation::Range(span) => {
                let a = Binder::check(type_stack, generics, Type::Int, &span)?;
                let b = Binder::check(type_stack, generics, Type::Int, &span)?;
                let result = BoundType {
                    t: Type::List(Box::new(Type::Int)),
                    span,
                };
                type_stack.push(result.clone());

                return Ok(BoundOperation {
                    op: Operation::Range(span),
                    signature: Pair {
                        left: vec![a, b],
                        right: vec![result],
                    },
                });
            }
        }
    }
}

fn type_equals(a: &Type, b: &Type) -> bool {
    //println!("a: {}, b: {}", a, b);
    return a == &Type::Any || b == &Type::Any || a == b;
}

fn bind_types(types: Vec<Type>, span: Span) -> Vec<BoundType> {
    let mut bound_types: Vec<BoundType> = vec![];

    for t in types {
        bound_types.push(BoundType { t, span })
    }
    return bound_types;
}

fn bind_sequence(
    elements: &Vec<Operation>,
    declared_functions: &HashMap<String, Function>,
) -> Result<Type, TypeError> {
    let mut is_list = true;
    let mut el_type: Option<Type> = None;

    for element in elements {
        match element {
            Operation::IntegerLiteral(_, _) => match el_type.clone() {
                None => el_type = Some(Type::Int),
                Some(t) => {
                    if t != Type::Int {
                        return Err(TypeError::TypeMismatch(
                            t,
                            BoundType {
                                t: Type::Int,
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
                            t,
                            BoundType {
                                t: Type::Bool,
                                span: get_span(&element),
                            },
                            get_span(&element),
                        ));
                    }
                }
            },
            Operation::List(ops, _) => match el_type.clone() {
                None => el_type = Some(bind_sequence(ops, declared_functions)?),
                Some(t) => {
                    if let Type::List(_) = t.clone() {
                        let actual_type = bind_sequence(ops, declared_functions)?;
                        if !type_equals(&actual_type, &t) {
                            return Err(TypeError::TypeMismatch(
                                t.clone(),
                                BoundType {
                                    t: actual_type,
                                    span: get_span(&element),
                                },
                                get_span(&element),
                            ));
                        }
                        continue;
                    } else {
                        return Err(TypeError::TypeMismatch(
                            t.clone(),
                            BoundType {
                                t: Type::List(Box::new(t)),
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
        return bind_lambda(elements, declared_functions);
    }

    match el_type {
        Some(t) => return Ok(Type::List(Box::new(t))),
        None => return Ok(Type::List(Box::new(Type::Any))),
    }
}

fn bind_lambda(
    elements: &Vec<Operation>,
    declared_functions: &HashMap<String, Function>,
) -> Result<Type, TypeError> {
    let mut inputs: Vec<BoundType> = vec![];
    let mut outputs: Vec<BoundType> = vec![];

    let mut generic_index = 0;

    let mut generics: HashMap<i64, Option<Type>> = HashMap::new();

    let mut infer_types = |args: Vec<Type>,
                           returns: Vec<BoundType>,
                           span: &Span,
                           inputs: &mut Vec<BoundType>,
                           outputs: &mut Vec<BoundType>,
                           generics: &mut HashMap<i64, Option<Type>>|
     -> Result<(), TypeError> {
        for arg in args {
            if outputs.len() > 0 {
                let output = outputs.pop().unwrap();
                if let Type::Generic(index) = output.t {
                    match generics.get(&index) {
                        Some(generic) => {
                            if let Some(gen) = generic {
                                if !type_equals(&gen, &arg) {
                                    return Err(TypeError::TypeMismatch(
                                        arg,
                                        BoundType {
                                            t: gen.clone(),
                                            span: span.clone(),
                                        },
                                        span.clone(),
                                    ));
                                }
                            }
                        }
                        None => {
                            if let Type::Generic(_) = arg {
                            } else {
                                generics.insert(index, Some(arg.clone()));
                            }
                        }
                    }
                } else {
                    if !type_equals(&output.t, &arg) {
                        return Err(TypeError::TypeMismatch(arg, output.clone(), span.clone()));
                    }
                }
            } else {
                //Else start pulling from the inputs
                let input = inputs.pop();
                match input {
                    Some(mut t) => {
                        if let Type::Generic(idx) = t.t {
                            match generics.get(&idx).unwrap() {
                                Some(concrete_type) => {
                                    t.t = concrete_type.clone();
                                }
                                None => {}
                            }
                        }
                        if !type_equals(&t.t, &arg) {
                            return Err(TypeError::TypeMismatch(arg, t, span.clone()));
                        }
                    }
                    None => {
                        //we need to infer
                        inputs.push(BoundType {
                            t: arg,
                            span: *span,
                        });
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

                    let bound_type = BoundType {
                        t: generic_type.clone(),
                        span: span.clone(),
                    };
                    inputs.push(bound_type.clone());

                    outputs.push(bound_type.clone());
                    outputs.push(bound_type.clone());
                } else if outputs.len() > 0 {
                    let typ = outputs.last().unwrap();
                    let args = vec![typ.clone().t];
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
                        let bound_type = BoundType {
                            t: Type::Int,
                            span: span.clone(),
                        };
                        inputs.push(bound_type.clone());
                        inputs.push(bound_type.clone());

                        outputs.push(bound_type);
                    } else {
                        let bound_type = BoundType {
                            t: Type::Int,
                            span: span.clone(),
                        };
                        let args = vec![Type::Int, Type::Int];
                        let returns = vec![bound_type];

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

                        let bound_type = BoundType {
                            t: generic_type.clone(),
                            span: span.clone(),
                        };

                        inputs.push(bound_type.clone());
                        inputs.push(bound_type);

                        outputs.push(BoundType {
                            t: Type::Bool,
                            span: span.clone(),
                        });
                    } else if outputs.len() > 0 {
                        let a = outputs.last().unwrap();
                        let b = outputs.last().unwrap();
                        let args = vec![a.clone().t, b.clone().t];
                        let returns = vec![BoundType {
                            t: Type::Bool,
                            span: span.clone(),
                        }];

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
                        let bound_int = BoundType {
                            t: Type::Int,
                            span: span.clone(),
                        };
                        inputs.push(bound_int.clone());
                        inputs.push(bound_int);

                        outputs.push(BoundType {
                            t: Type::Bool,
                            span: span.clone(),
                        });
                    } else {
                        let args = vec![Type::Int, Type::Int];
                        let returns = vec![BoundType {
                            t: Type::Bool,
                            span: span.clone(),
                        }];

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
                    let bound_bool = BoundType {
                        t: Type::Bool,
                        span: span.clone(),
                    };
                    if inputs.is_empty() && outputs.is_empty() {
                        inputs.push(bound_bool.clone());
                        inputs.push(bound_bool.clone());

                        outputs.push(bound_bool.clone());
                    } else {
                        let args = vec![Type::Bool, Type::Bool];
                        let returns = vec![bound_bool.clone()];

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

                    let bound_type = BoundType {
                        t: generic_type.clone(),
                        span: span.clone(),
                    };

                    inputs.push(bound_type.clone());
                } else if outputs.len() > 0 {
                    let a = outputs.last().unwrap();
                    let args = vec![a.t.clone()];

                    infer_types(args, vec![], span, &mut inputs, &mut outputs, &mut generics)?;
                } else {
                    return Err(TypeError::EmptyStack(Type::Any, span.clone()));
                }
            }
            Operation::Unary(unop, span) => match unop {
                UnaryOperator::Neg => {
                    let bound_bool = BoundType {
                        t: Type::Bool,
                        span: span.clone(),
                    };
                    if inputs.is_empty() && outputs.is_empty() {
                        inputs.push(bound_bool.clone());

                        outputs.push(bound_bool.clone());
                    } else {
                        let args = vec![Type::Bool];
                        let returns = vec![bound_bool];

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
            Operation::Pop(span) => {
                if inputs.is_empty() && outputs.is_empty() {
                    let generic_type = Type::Generic(generic_index);
                    generic_index += 1;
                    generics.insert(generic_index, None);

                    let bound_type = BoundType {
                        t: generic_type.clone(),
                        span: span.clone(),
                    };

                    inputs.push(bound_type.clone());
                } else if outputs.len() > 0 {
                    let a = outputs.last().unwrap();
                    let args = vec![a.t.clone()];

                    infer_types(args, vec![], span, &mut inputs, &mut outputs, &mut generics)?;
                } else {
                    let generic_type = Type::Generic(generic_index);
                    generic_index += 1;
                    generics.insert(generic_index, None);

                    let bound_type = BoundType {
                        t: generic_type.clone(),
                        span: span.clone(),
                    };

                    inputs.push(bound_type.clone());
                }
            }
            Operation::Swap(span) => {
                if inputs.is_empty() && outputs.is_empty() {
                    let generic_type_a = Type::Generic(generic_index);
                    generic_index += 1;
                    generics.insert(generic_index, None);
                    let generic_type_b = Type::Generic(generic_index);
                    generic_index += 1;
                    generics.insert(generic_index, None);

                    let bound_type_a = BoundType {
                        t: generic_type_a.clone(),
                        span: span.clone(),
                    };
                    let bound_type_b = BoundType {
                        t: generic_type_b.clone(),
                        span: span.clone(),
                    };
                    inputs.push(bound_type_a.clone());
                    inputs.push(bound_type_b.clone());

                    outputs.push(bound_type_b.clone());
                    outputs.push(bound_type_a.clone());
                } else if outputs.len() > 1 {
                    let a = outputs.pop().unwrap();
                    let b = outputs.pop().unwrap();

                    outputs.push(a);
                    outputs.push(b);
                } else {
                    return Err(TypeError::EmptyStack(Type::Any, span.clone()));
                }
            }
            Operation::Rot(span) => {
                if inputs.is_empty() && outputs.is_empty() {
                    let generic_type_a = Type::Generic(generic_index);
                    generic_index += 1;
                    generics.insert(generic_index, None);
                    let generic_type_b = Type::Generic(generic_index);
                    generic_index += 1;
                    generics.insert(generic_index, None);
                    let generic_type_c = Type::Generic(generic_index);
                    generic_index += 1;
                    generics.insert(generic_index, None);

                    let bound_type_a = BoundType {
                        t: generic_type_a.clone(),
                        span: span.clone(),
                    };
                    let bound_type_b = BoundType {
                        t: generic_type_b.clone(),
                        span: span.clone(),
                    };
                    let bound_type_c = BoundType {
                        t: generic_type_c.clone(),
                        span: span.clone(),
                    };

                    inputs.push(bound_type_a.clone());
                    inputs.push(bound_type_b.clone());
                    inputs.push(bound_type_c.clone());

                    outputs.push(bound_type_c.clone());
                    outputs.push(bound_type_a.clone());
                    outputs.push(bound_type_b.clone());
                } else if outputs.len() > 2 {
                    let a = outputs.last().unwrap();
                    let b = &outputs[outputs.len() - 2];
                    let c = &outputs[outputs.len() - 3];
                    let args = vec![a.t.clone(), b.t.clone(), c.t.clone()];
                    let returns = vec![c.clone(), a.clone(), b.clone()];

                    infer_types(
                        args,
                        returns,
                        span,
                        &mut inputs,
                        &mut outputs,
                        &mut generics,
                    )?;
                } else {
                    let bound_any = BoundType {
                        t: Type::Any,
                        span: span.clone(),
                    };
                    let args = vec![Type::Any, Type::Any, Type::Any];
                    let returns = vec![bound_any.clone(), bound_any.clone(), bound_any.clone()];

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
            Operation::Over(span) => {
                if inputs.is_empty() && outputs.is_empty() {
                    let generic_type_a = Type::Generic(generic_index);
                    generic_index += 1;
                    generics.insert(generic_index, None);
                    let generic_type_b = Type::Generic(generic_index);
                    generic_index += 1;
                    generics.insert(generic_index, None);

                    let bound_type_a = BoundType {
                        t: generic_type_a.clone(),
                        span: span.clone(),
                    };
                    let bound_type_b = BoundType {
                        t: generic_type_b.clone(),
                        span: span.clone(),
                    };

                    inputs.push(bound_type_a.clone());
                    inputs.push(bound_type_b.clone());

                    outputs.push(bound_type_b.clone());
                    outputs.push(bound_type_a.clone());
                    outputs.push(bound_type_b.clone());
                } else if outputs.len() > 1 {
                    let a = outputs.last().unwrap();
                    let b = &outputs[outputs.len() - 2];
                    let args = vec![a.t.clone(), b.t.clone()];
                    let returns = vec![b.clone(), a.clone(), b.clone()];

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
            Operation::If(_) => todo!(),
            Operation::While(_) => todo!(),
            Operation::List(ops, span) => {
                if ops.is_empty() {
                    let generic_type = Type::Generic(generic_index);
                    let bound_type = BoundType {
                        t: Type::List(Box::new(generic_type.clone())),
                        span: span.clone(),
                    };
                    generic_index += 1;
                    outputs.push(bound_type.clone());
                    continue;
                }
                let el_type = match ops[0] {
                    Operation::IntegerLiteral(_, _) => Type::Int,
                    Operation::BooleanLiteral(_, _) => Type::Bool,
                    Operation::List(_, _) => todo!(),
                    _ => unreachable!(),
                };
                let bound_type = BoundType {
                    t: Type::List(Box::new(el_type.clone())),
                    span: span.clone(),
                };
                if inputs.is_empty() && outputs.is_empty() {
                    outputs.push(bound_type.clone());
                } else {
                    let args = vec![];
                    let returns = vec![bound_type.clone()];

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
            Operation::Lambda(ops, span) => {
                //TODO: create a substack and then adjust the inputs/outputs
                bind_lambda(&ops, declared_functions)?;
            }
            Operation::Run(_) => todo!(),
            Operation::IntegerLiteral(_, span) => {
                let bound_int = BoundType {
                    t: Type::Int,
                    span: span.clone(),
                };
                if inputs.is_empty() && outputs.is_empty() {
                    outputs.push(bound_int);
                } else {
                    let args = vec![];
                    let returns = vec![bound_int];

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
                let bound_bool = BoundType {
                    t: Type::Bool,
                    span: span.clone(),
                };
                if inputs.is_empty() && outputs.is_empty() {
                    outputs.push(bound_bool);
                } else {
                    let args = vec![];
                    let returns = vec![bound_bool];

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
            Operation::Identifier(name, span) => {
                let function = declared_functions.get(name).unwrap();
                if inputs.is_empty() && outputs.is_empty() {
                    for arg in &function.arguments {
                        let bound_arg = BoundType {
                            t: arg.clone(),
                            span: span.clone(),
                        };
                        inputs.push(bound_arg.clone());
                    }
                    for ret in &function.returns {
                        let bound_ret = BoundType {
                            t: ret.clone(),
                            span: span.clone(),
                        };
                        outputs.push(bound_ret.clone());
                    }
                } else {
                    let args = &function.arguments;
                    let returns = &function.returns;

                    infer_types(
                        args.to_vec(),
                        bind_types(returns.to_vec(), span.clone()),
                        span,
                        &mut inputs,
                        &mut outputs,
                        &mut generics,
                    )?;
                }
            }
            Operation::NoOp => {}
            Operation::Map(_) => todo!(),
            Operation::Apply(span) => {
                if outputs.is_empty() {
                    let generic_type = Type::Generic(generic_index);
                    generic_index += 1;
                    generics.insert(generic_index, None);

                    let bound_list_type = BoundType {
                        t: Type::List(Box::new(generic_type.clone())),
                        span: span.clone(),
                    };

                    let bound_fn_type = BoundType {
                        t: Type::Function(vec![generic_type], vec![]),
                        span: span.clone(),
                    };

                    inputs.push(bound_list_type);
                    inputs.push(bound_fn_type);
                } else if outputs.len() == 1 {
                    if let Type::Function(ins, outs) = &outputs.last().unwrap().t {
                        todo!()
                    } else {
                        return Err(TypeError::TypeMismatch(
                            Type::Function(vec![Type::Any], vec![]),
                            outputs.last().unwrap().clone(),
                            span.clone(),
                        ));
                    }
                } else if outputs.len() > 1 {
                    if let Type::Function(ins, outs) = &outputs.last().unwrap().t {
                        todo!()
                    } else {
                        return Err(TypeError::TypeMismatch(
                            Type::Function(vec![Type::Any], vec![]),
                            outputs.last().unwrap().clone(),
                            span.clone(),
                        ));
                    }
                }
            }
            Operation::Filter(span) => {
                if outputs.is_empty() {
                    let generic_type = Type::Generic(generic_index);
                    generic_index += 1;
                    generics.insert(generic_index, None);

                    let bound_list_type = BoundType {
                        t: Type::List(Box::new(generic_type.clone())),
                        span: span.clone(),
                    };

                    let bound_fn_type = BoundType {
                        t: Type::Function(vec![generic_type], vec![Type::Bool]),
                        span: span.clone(),
                    };

                    inputs.push(bound_list_type.clone());
                    inputs.push(bound_fn_type);

                    outputs.push(bound_list_type);
                } else {
                    let generic_type = Type::Generic(generic_index);
                    generic_index += 1;
                    generics.insert(generic_index, None);

                    let bound_list_type = BoundType {
                        t: Type::List(Box::new(generic_type.clone())),
                        span: span.clone(),
                    };

                    let args = vec![
                        Type::Function(vec![generic_type.clone()], vec![Type::Bool]),
                        Type::List(Box::new(generic_type.clone())),
                    ];
                    let returns = vec![bound_list_type];

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
            Operation::Len(span) => {
                if outputs.is_empty() {
                    let generic_type = Type::Generic(generic_index);
                    generic_index += 1;
                    generics.insert(generic_index, None);

                    let bound_type = BoundType {
                        t: Type::List(Box::new(generic_type)),
                        span: span.clone(),
                    };

                    inputs.push(bound_type);

                    outputs.push(BoundType {
                        t: Type::Int,
                        span: span.clone(),
                    });
                } else if outputs.len() > 0 {
                    let a = outputs.last().unwrap();
                    let args = vec![a.t.clone()];
                    let returns = vec![BoundType {
                        t: Type::Int,
                        span: span.clone(),
                    }];

                    infer_types(
                        args,
                        returns,
                        span,
                        &mut inputs,
                        &mut outputs,
                        &mut generics,
                    )?;
                } else {
                    let generic_type = Type::Generic(generic_index);
                    generic_index += 1;
                    generics.insert(generic_index, None);
                    let args = vec![Type::List(Box::new(generic_type))];
                    let returns = vec![BoundType {
                        t: Type::Int,
                        span: span.clone(),
                    }];

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
            Operation::Fold(_) => todo!(),
            Operation::Concat(span) => {
                if inputs.is_empty() && outputs.is_empty() {
                    let generic_type = Type::Generic(generic_index);
                    generic_index += 1;
                    generics.insert(generic_index, None);

                    let bound_type = BoundType {
                        t: Type::List(Box::new(generic_type)),
                        span: span.clone(),
                    };

                    inputs.push(bound_type.clone());
                    inputs.push(bound_type.clone());

                    outputs.push(bound_type.clone());
                } else if outputs.len() > 0 {
                    if let Type::List(el) = &outputs.last().unwrap().t {
                        let args = vec![Type::List(el.clone()), Type::List(el.clone())];
                        let returns = vec![BoundType {
                            t: Type::List(el.clone()),
                            span: span.clone(),
                        }];
                        infer_types(
                            args,
                            returns,
                            span,
                            &mut inputs,
                            &mut outputs,
                            &mut generics,
                        )?;
                    } else {
                        return Err(TypeError::TypeMismatch(
                            Type::List(Box::new(Type::Any)),
                            outputs.last().unwrap().clone(),
                            span.clone(),
                        ));
                    }
                } else {
                    return Err(TypeError::EmptyStack(Type::Any, span.clone()));
                }
            }
            Operation::Head(_) => todo!(),
            Operation::Tail(_) => todo!(),
            Operation::Last(_) => todo!(),
            Operation::Init(_) => todo!(),
            Operation::Cons(_) => todo!(),
            Operation::Sort(_) => todo!(),
            Operation::DumpTypeStack(span) => {
                return Err(TypeError::DumpTypeStack(
                    TypeStack {
                        type_stack: outputs,
                    },
                    span.clone(),
                ));
            }
            Operation::Reverse(_) => todo!(),
            Operation::Nth(span) => {
                if outputs.is_empty() {
                    let generic_type = Type::Generic(generic_index);
                    generic_index += 1;
                    generics.insert(generic_index, None);

                    let bound_el_type = BoundType {
                        t: generic_type.clone(),
                        span: span.clone(),
                    };

                    let bound_list_type = BoundType {
                        t: Type::List(Box::new(generic_type.clone())),
                        span: span.clone(),
                    };

                    inputs.push(BoundType {
                        t: Type::Int,
                        span: span.clone(),
                    });
                    inputs.push(bound_list_type);

                    outputs.push(bound_el_type.clone());
                } else if outputs.len() > 1 {
                    if type_equals(&Type::Int, &outputs.last().unwrap().t) {
                        if let Type::List(el) = &outputs.last().unwrap().t {
                            let args = vec![Type::Int, Type::List(el.clone())];

                            let bound_type = BoundType {
                                t: *el.clone(),
                                span: span.clone(),
                            };

                            let returns = vec![bound_type];
                            infer_types(
                                args,
                                returns,
                                span,
                                &mut inputs,
                                &mut outputs,
                                &mut generics,
                            )?;
                        } else if type_equals(
                            &Type::List(Box::new(Type::Any)),
                            &outputs.last().unwrap().t,
                        ) {
                            let args = vec![Type::Int, Type::List(Box::new(Type::Any))];

                            let bound_type = BoundType {
                                t: Type::Any,
                                span: span.clone(),
                            };

                            let returns = vec![bound_type];
                            infer_types(
                                args,
                                returns,
                                span,
                                &mut inputs,
                                &mut outputs,
                                &mut generics,
                            )?;
                        } else {
                            return Err(TypeError::TypeMismatch(
                                Type::List(Box::new(Type::Any)),
                                outputs.last().unwrap().clone(),
                                span.clone(),
                            ));
                        }
                    } else {
                        return Err(TypeError::TypeMismatch(
                            Type::Int,
                            outputs.last().unwrap().clone(),
                            span.clone(),
                        ));
                    }
                } else {
                    return Err(TypeError::EmptyStack(Type::Any, span.clone()));
                }
            }
            Operation::Partial(span) => {
                if outputs.is_empty() {
                    let function = Type::Function(vec![Type::Any], vec![Type::Any]);
                    let arg = Type::Any;
                }
            }
            Operation::Range(span) => {
                if outputs.is_empty() {
                    let bound_int = BoundType {
                        t: Type::Int,
                        span: span.clone(),
                    };
                    inputs.push(bound_int.clone());
                    inputs.push(bound_int.clone());

                    outputs.push(BoundType {
                        t: Type::List(Box::new(Type::Int)),
                        span: span.clone(),
                    });
                } else if outputs.len() == 1 {
                    if !type_equals(&outputs.last().unwrap().t, &Type::Int) {
                        return Err(TypeError::TypeMismatch(
                            Type::Int,
                            outputs.last().unwrap().clone(),
                            span.clone(),
                        ));
                    }
                    let bound_int = BoundType {
                        t: Type::Int,
                        span: span.clone(),
                    };
                    inputs.push(bound_int.clone());

                    outputs.pop();
                    outputs.push(BoundType {
                        t: Type::List(Box::new(Type::Int)),
                        span: span.clone(),
                    });
                } else {
                    if !type_equals(&outputs.pop().unwrap().t, &Type::Int) {
                        return Err(TypeError::TypeMismatch(
                            Type::Int,
                            outputs.last().unwrap().clone(),
                            span.clone(),
                        ));
                    }

                    if !type_equals(&outputs.pop().unwrap().t, &Type::Int) {
                        return Err(TypeError::TypeMismatch(
                            Type::Int,
                            outputs.last().unwrap().clone(),
                            span.clone(),
                        ));
                    }
                    let bound_int = BoundType {
                        t: Type::Int,
                        span: span.clone(),
                    };
                    inputs.push(bound_int.clone());
                    inputs.push(bound_int.clone());

                    outputs.push(BoundType {
                        t: Type::List(Box::new(Type::Int)),
                        span: span.clone(),
                    });
                }
            }
        }
        //println!("ins: {:?}, outs: {:?}", inputs, outputs);
    }

    //erase generics
    for i in 0..inputs.len() {
        erase_generics(&mut inputs[i], &generics);
    }
    for i in 0..outputs.len() {
        erase_generics(&mut outputs[i], &generics);
    }

    return Ok(Type::Function(
        inputs.iter().map(|bt| bt.t.clone()).collect(),
        outputs.iter().map(|bt| bt.t.clone()).collect(),
    ));
}

fn erase_generics(bound_type: &mut BoundType, generics: &HashMap<i64, Option<Type>>) {
    //println!("erasing {}", bound_type);
    if let Type::Generic(idx) = bound_type.t {
        match generics.get(&idx) {
            Some(concrete) => match concrete {
                Some(concrete) => bound_type.t = concrete.clone(),
                None => bound_type.t = Type::Any,
            },
            None => bound_type.t = Type::Any,
        }
    } else if let Type::List(el) = &bound_type.t {
        erase_generics(
            &mut BoundType {
                t: *el.clone(),
                span: bound_type.span.clone(),
            },
            generics,
        )
    } else if let Type::Function(ins, outs) = &bound_type.t {
        for input in ins {
            erase_generics(
                &mut BoundType {
                    t: input.clone(),
                    span: bound_type.span.clone(),
                },
                generics,
            )
        }

        for output in outs {
            erase_generics(
                &mut BoundType {
                    t: output.clone(),
                    span: bound_type.span.clone(),
                },
                generics,
            )
        }
    }
}

enum TypeError {
    TypeMismatch(Type, BoundType, Span),
    EmptyStack(Type, Span),
    NonEmptyStack(TypeStack),
    DumpTypeStack(TypeStack, Span),
    IfBranchInputMismatch(Vec<Type>, Vec<Type>, Span),
    IfBranchOutputMismatch(Vec<Type>, Vec<Type>, Span),
    UnknownIdentifier(String, Span),
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
            TypeError::UnknownIdentifier(name, _) => {
                write!(f, "no such identifier `{}`", name)
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

//For printing
struct TypeStack {
    type_stack: Vec<BoundType>,
}

#[derive(Debug, PartialEq, Clone, Copy, Eq, PartialOrd, Ord)]
struct Pair<L, R> {
    left: L,
    right: R,
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
        | Operation::Partial(span)
        | Operation::Range(span)
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
        //println!("{:?}", self.stack);
        //println!("{}", operation);
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
                        match (left_val.clone(), right_val.clone()) {
                            (Value::Boolean(l), Value::Boolean(r)) => {
                                self.stack.push(Value::Boolean(r || l))
                            }
                            _ => panic!("Cannot or {} and {}", left_val, right_val),
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
                    match self.stack.pop().unwrap() {
                        Value::Seq(operands) => {
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
                        }
                        Value::Integer(i) => panic!("tried mapping with an int {}", i),
                        Value::Boolean(b) => panic!("tried mapping with a bool {}", b),
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
            Operation::Partial(span) => {
                if let Value::Seq(mut ops) = self.stack.pop().unwrap() {
                    let arg = self.stack.pop().unwrap();
                    match arg {
                        Value::Integer(i) => {
                            ops.insert(0, Operation::IntegerLiteral(i, *span));
                            self.stack.push(Value::Seq(ops));
                        }
                        Value::Boolean(b) => {
                            ops.insert(0, Operation::BooleanLiteral(b, *span));
                            self.stack.push(Value::Seq(ops));
                        }
                        Value::Seq(s) => todo!(),
                    }
                    //println!("{:#?}", self.stack);
                } else {
                    panic!("Parsed partial incorrectly")
                }
            }
            Operation::Range(span) => {
                if let Value::Integer(upper) = self.stack.pop().unwrap() {
                    if let Value::Integer(lower) = self.stack.pop().unwrap() {
                        let mut ops: Vec<Operation> = vec![];
                        for i in lower..upper {
                            ops.push(Operation::IntegerLiteral(i, *span));
                        }

                        self.stack.push(Value::Seq(ops));
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
        TypeError::UnknownIdentifier(_, span) => {
            println!(" at {}:{}", file_name, span);
            print_span(span, contents, HighlightColor::RED);
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
                            Ok(_) => {
                                if measure {
                                    println!("Type Checking took {}ms", now.elapsed().as_millis());
                                }
                            }
                            Err(errors) => {
                                if measure {
                                    println!("Type Checking took {}ms", now.elapsed().as_millis());
                                }
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
