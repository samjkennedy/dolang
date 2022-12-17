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

#[derive(Debug, Clone, PartialEq, Eq)]
struct Span {
    row: usize,
    col: usize,
    len: usize,
    file: String,
}

impl Span {
    fn empty() -> Span {
        return Span {
            row: 0,
            col: 0,
            len: 0,
            file: "".to_string(),
        };
    }

    fn from(start: Span, end: Span) -> Span {
        return Span {
            row: start.row,
            col: start.col - 1,
            len: end.col + 2 - start.col,
            file: start.file,
        };
    }
}

impl fmt::Display for Span {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}:{}:{}", self.file, self.row, self.col)
    }
}

#[derive(Debug, PartialEq, Clone, Copy)]
enum TokenKind {
    WhiteSpace,
    Comment,
    Identifier,
    NumberLiteral,
    StringLiteral,
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
    EndKeyword,
    RunKeyword,
    DefineKeyword,
    IfKeyword,
    WhileKeyword,
    ImportKeyword,

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
}
#[derive(Debug, Clone)]
struct Token {
    kind: TokenKind,
    span: Span,
    text: String,
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

fn lex(file: String, input: &str) -> Result<Vec<Token>, LexingError> {
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
        ("end".to_string(), TokenKind::EndKeyword),
        ("run".to_string(), TokenKind::RunKeyword),
        ("define".to_string(), TokenKind::DefineKeyword),
        ("if".to_string(), TokenKind::IfKeyword),
        ("while".to_string(), TokenKind::WhileKeyword),
        ("import".to_string(), TokenKind::ImportKeyword),
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
    ]);

    for (zero_row, line) in input.split("\n").enumerate() {
        let row = zero_row + 1;
        let mut col = 0;

        while col < line.chars().count() {
            let c = line.chars().nth(col).unwrap();
            if c == '+' {
                tokens.push(Token {
                    kind: TokenKind::Plus,
                    span: Span {
                        row,
                        col: col + 1,
                        len: 1,
                        file: file.clone(),
                    },
                    text: "+".to_string(),
                });
                col += 1;
            } else if c == '-' {
                if let Some(next) = line.chars().nth(col + 1) {
                    if next == '-' {
                        tokens.push(Token {
                            kind: TokenKind::DashDash,
                            span: Span {
                                row,
                                col: col + 1,
                                len: 2,
                                file: file.clone(),
                            },
                            text: "--".to_string(),
                        });
                        col += 2;
                    } else {
                        tokens.push(Token {
                            kind: TokenKind::Minus,
                            span: Span {
                                row,
                                col: col + 1,
                                len: 1,
                                file: file.clone(),
                            },
                            text: "-".to_string(),
                        });
                        col += 1;
                    }
                }
            } else if c == '*' {
                tokens.push(Token {
                    kind: TokenKind::Star,
                    span: Span {
                        row,
                        col: col + 1,
                        len: 1,
                        file: file.clone(),
                    },
                    text: "*".to_string(),
                });
                col += 1;
            } else if c == '/' {
                tokens.push(Token {
                    kind: TokenKind::Slash,
                    span: Span {
                        row,
                        col: col + 1,
                        len: 1,
                        file: file.clone(),
                    },
                    text: "/".to_string(),
                });
                col += 1;
            } else if c == '%' {
                tokens.push(Token {
                    kind: TokenKind::Percent,
                    span: Span {
                        row,
                        col: col + 1,
                        len: 1,
                        file: file.clone(),
                    },
                    text: "%".to_string(),
                });
                col += 1;
            } else if c == '>' {
                tokens.push(Token {
                    kind: TokenKind::OpenAngle,
                    span: Span {
                        row,
                        col: col + 1,
                        len: 1,
                        file: file.clone(),
                    },
                    text: ">".to_string(),
                });
                col += 1;
            } else if c == '<' {
                tokens.push(Token {
                    kind: TokenKind::CloseAngle,
                    span: Span {
                        row,
                        col: col + 1,
                        len: 1,
                        file: file.clone(),
                    },
                    text: "<".to_string(),
                });
                col += 1;
            } else if c == '=' {
                tokens.push(Token {
                    kind: TokenKind::Equals,
                    span: Span {
                        row,
                        col: col + 1,
                        len: 1,
                        file: file.clone(),
                    },
                    text: "=".to_string(),
                });
                col += 1;
            } else if c == '!' {
                tokens.push(Token {
                    kind: TokenKind::Bang,
                    span: Span {
                        row,
                        col: col + 1,
                        len: 1,
                        file: file.clone(),
                    },
                    text: "!".to_string(),
                });
                col += 1;
            } else if c == '&' {
                tokens.push(Token {
                    kind: TokenKind::Ampersand,
                    span: Span {
                        row,
                        col: col + 1,
                        len: 1,
                        file: file.clone(),
                    },
                    text: "&".to_string(),
                });
                col += 1;
            } else if c == '|' {
                tokens.push(Token {
                    kind: TokenKind::Pipe,
                    span: Span {
                        row,
                        col: col + 1,
                        len: 1,
                        file: file.clone(),
                    },
                    text: "|".to_string(),
                });
                col += 1;
            } else if c == '[' {
                tokens.push(Token {
                    kind: TokenKind::OpenSquareBrace,
                    span: Span {
                        row,
                        col: col + 1,
                        len: 1,
                        file: file.clone(),
                    },
                    text: "[".to_string(),
                });
                col += 1;
            } else if c == ']' {
                tokens.push(Token {
                    kind: TokenKind::CloseSquareBrace,
                    span: Span {
                        row,
                        col: col + 1,
                        len: 1,
                        file: file.clone(),
                    },
                    text: "]".to_string(),
                });
                col += 1;
            } else if c == '(' {
                tokens.push(Token {
                    kind: TokenKind::OpenParenthesis,
                    span: Span {
                        row,
                        col: col + 1,
                        len: 1,
                        file: file.clone(),
                    },
                    text: "(".to_string(),
                });
                col += 1;
            } else if c == ')' {
                tokens.push(Token {
                    kind: TokenKind::CloseParenthesis,
                    span: Span {
                        row,
                        col: col + 1,
                        len: 1,
                        file: file.clone(),
                    },
                    text: ")".to_string(),
                });
                col += 1;
            } else if c == '#' {
                let comment = &line[col..];
                tokens.push(Token {
                    kind: TokenKind::Comment,
                    span: Span {
                        row,
                        col: col + 1,
                        len: comment.chars().count(),
                        file: file.clone(),
                    },
                    text: comment.to_string(),
                });
                col = line.chars().count();
            } else if c == '?' {
                if let Some(next) = line.chars().nth(col + 1) {
                    if next == '?' {
                        if let Some(next) = line.chars().nth(col + 2) {
                            if next == '?' {
                                tokens.push(Token {
                                    kind: TokenKind::QuestionQuestionQuestion,
                                    span: Span {
                                        row,
                                        col: col + 1,
                                        len: 3,
                                        file: file.clone(),
                                    },
                                    text: "???".to_string(),
                                });
                                col += 3;
                            }
                        }
                    }
                }
            } else if c == '"' {
                let mut s = line.chars().nth(col + 1).unwrap();
                col += 1;
                let start_col = col + 1;
                let mut acc = "".to_string();
                while s != '"' && line.chars().nth(col).is_some() {
                    acc.push(s);
                    col += 1;
                    s = line.chars().nth(col).or(Some('"')).unwrap();
                }
                col += 1;
                tokens.push(Token {
                    kind: TokenKind::StringLiteral,
                    span: Span {
                        row,
                        col: start_col,
                        len: acc.chars().count(),
                        file: file.clone(),
                    },
                    text: acc,
                });
            } else if c.is_numeric() && line.chars().nth(col).is_some() {
                let mut n = c;
                let start_col = col + 1;
                let mut acc = "".to_string();
                while n.is_numeric() {
                    acc.push(n);
                    col += 1;
                    n = line.chars().nth(col).or(Some('\n')).unwrap();
                }
                tokens.push(Token {
                    kind: TokenKind::NumberLiteral,
                    span: Span {
                        row,
                        col: start_col,
                        len: acc.chars().count(),
                        file: file.clone(),
                    },
                    text: acc,
                });
            } else if c.is_whitespace() {
                let mut ws = c;
                let start_col = col + 1;
                let mut acc = "".to_string();
                while ws.is_whitespace() && line.chars().nth(col).is_some() {
                    acc.push(ws);
                    col += 1;
                    ws = line.chars().nth(col).or(Some('\n')).unwrap();
                }
                tokens.push(Token {
                    kind: TokenKind::WhiteSpace,
                    span: Span {
                        row,
                        col: start_col,
                        len: acc.chars().count(),
                        file: file.clone(),
                    },
                    text: acc,
                });
            } else if c.is_alphabetic() {
                let mut a = c;
                let start_col = col + 1;
                let mut acc = "".to_string();
                while !a.is_whitespace()
                    && !['[', ']', '(', ')'].contains(&a) //TODO: Have a vec of all reserved characters
                    && line.chars().nth(col).is_some()
                {
                    acc.push(a);
                    col += 1;
                    a = line.chars().nth(col).or(Some('\n')).unwrap();
                }

                //Check for keywords
                let span = Span {
                    row,
                    col: start_col,
                    len: acc.chars().count(),
                    file: file.clone(),
                };
                if keywords.contains_key(&acc) {
                    tokens.push(Token {
                        kind: *keywords.get(&acc).unwrap(),
                        span,
                        text: acc,
                    })
                } else {
                    tokens.push(Token {
                        kind: TokenKind::Identifier,
                        span,
                        text: acc,
                    });
                }
            } else {
                return Err(LexingError::UnexpectedCharacter(
                    c,
                    Span {
                        row,
                        col: col + 1,
                        len: 1,
                        file: file.clone(),
                    },
                ));
            }
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
    Print(Span),
    If(Span),
    While(Span),

    Sequence(Vec<Operation>, Span),
    Run(Span),
    IntegerLiteral(i64, Span),
    BooleanLiteral(bool, Span),
    StringLiteral(String, Span),
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
    Import(Vec<Function>, Span),
    Sort(Span),
    DumpTypeStack(Span),
    Reverse(Span),
    Max(Span),
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
            ParseError::UnknownIdentifier(identifier, span) => {
                write!(f, "Unknown identifier `{}` at {}", identifier, span)
            }
            ParseError::UnexpectedToken(expected, actual, span) => write!(
                f,
                "Expected `{:?}` but got {:?} at {}",
                expected, actual, span
            ),
        }
    }
}

struct Parser {
    cursor: usize,
    tokens: Vec<Token>,
    operations: Vec<Operation>,
    functions: HashMap<String, Function>,
}

impl Parser {
    fn new(tokens: Vec<Token>) -> Parser {
        return Parser {
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
            let span = token.span.clone();

            match token.kind {
                TokenKind::WhiteSpace | TokenKind::Comment | TokenKind::EndKeyword => {
                    return Ok(Operation::NoOp)
                }
                TokenKind::NumberLiteral => {
                    return Ok(Operation::IntegerLiteral(
                        token.text.parse::<i64>().unwrap(),
                        span,
                    ));
                }

                TokenKind::StringLiteral => {
                    return Ok(Operation::StringLiteral(token.text.clone(), span));
                }
                TokenKind::TrueLiteral => return Ok(Operation::BooleanLiteral(true, span)),
                TokenKind::FalseLiteral => return Ok(Operation::BooleanLiteral(false, span)),
                TokenKind::Plus => return Ok(Operation::Binary(BinaryOperator::Add, span)),
                TokenKind::Minus => return Ok(Operation::Binary(BinaryOperator::Sub, span)),
                TokenKind::Star => return Ok(Operation::Binary(BinaryOperator::Mul, span)),
                TokenKind::Slash => return Ok(Operation::Binary(BinaryOperator::Div, span)),
                TokenKind::Percent => return Ok(Operation::Binary(BinaryOperator::Rem, span)),
                TokenKind::OpenAngle => return Ok(Operation::Binary(BinaryOperator::Gt, span)),
                TokenKind::CloseAngle => return Ok(Operation::Binary(BinaryOperator::Lt, span)),
                TokenKind::Equals => return Ok(Operation::Binary(BinaryOperator::Eq, span)),
                TokenKind::Ampersand => return Ok(Operation::Binary(BinaryOperator::And, span)),
                TokenKind::Pipe => return Ok(Operation::Binary(BinaryOperator::Or, span)),
                TokenKind::Bang => return Ok(Operation::Unary(UnaryOperator::Neg, span)),
                TokenKind::OpenSquareBrace => {
                    self.cursor += 1;
                    return Ok(self.parse_sequence()?);
                }
                TokenKind::PrintKeyword => return Ok(Operation::Print(span)),
                TokenKind::SwapKeyword => return Ok(Operation::Swap(span)),
                TokenKind::PopKeyword => return Ok(Operation::Pop(span)),
                TokenKind::DupKeyword => return Ok(Operation::Dup(span)),
                TokenKind::RotKeyword => return Ok(Operation::Rot(span)),

                TokenKind::IfKeyword => return Ok(Operation::If(span)),
                TokenKind::WhileKeyword => return Ok(Operation::While(span)),

                TokenKind::ImportKeyword => {
                    self.cursor += 1;
                    let import = self.match_token(TokenKind::StringLiteral)?;
                    let file_path = ".\\core\\".to_string() + &import.text + &".do".to_string();

                    if let Ok(contents) = fs::read_to_string(file_path.clone()) {
                        let tokens = lex(file_path.clone(), &contents).unwrap(); //TODO: Errors from other files

                        let mut parser = Parser::new(tokens);
                        match parser.parse() {
                            Ok(ops) => {
                                let mut functions: Vec<Function> = vec![];
                                for op in ops {
                                    match op {
                                        Operation::Define(function, _) => {
                                            self.functions
                                                .insert(function.name.clone(), function.clone());
                                            functions.push(function);
                                        }
                                        _ => {}
                                    }
                                }
                                return Ok(Operation::Import(functions, span)); //TODO: Replace with Operation::Import(compilation_unit)
                            }
                            Err(error) => display_parse_error(error, &contents),
                        }
                    }
                }

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
                TokenKind::MaxKeyword => return Ok(Operation::Max(span)),
                TokenKind::Identifier => {
                    if self.functions.contains_key(&token.text) {
                        return Ok(Operation::Identifier(token.text.clone(), span));
                    }
                    return Err(ParseError::UnknownIdentifier(
                        token.text.clone(),
                        token.span.clone(),
                    ));
                }
                TokenKind::DefineKeyword => {
                    self.cursor += 1;
                    let identifier = self.current().clone();

                    let name = identifier.text.to_owned();
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
                        identifier.text.clone(),
                        Function {
                            name: name.clone(),
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
                        name: name.clone(),
                        arguments: args,
                        returns: results,
                        body,
                    };
                    self.functions.insert(name, function.clone());
                    return Ok(Operation::Define(function, identifier.span.clone()));
                }
                TokenKind::OpenParenthesis => todo!(),
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
                self.current().span.clone(),
            )),
        };
        arg_type
    }

    fn match_token(&mut self, expected: TokenKind) -> Result<&Token, ParseError> {
        let curr = self.current();
        if curr.kind == expected {
            return Ok(curr);
        }
        return Err(ParseError::UnexpectedToken(
            expected,
            curr.kind,
            curr.span.clone(),
        ));
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

    fn parse_sequence(&mut self) -> Result<Operation, ParseError> {
        let mut operations: Vec<Operation> = vec![];

        let start = self.current().span.clone();

        while self.current().kind != TokenKind::CloseSquareBrace {
            operations.push(self.parse_operation()?);
            self.cursor += 1;
        }
        let end = self.current().span.clone();

        return Ok(Operation::Sequence(operations, Span::from(start, end)));
    }

    fn current(&mut self) -> &Token {
        return &self.tokens[self.cursor];
    }
}

#[derive(Debug, PartialEq, Clone, Eq)]
enum Type {
    Int,
    Bool,
    String,
    List(Box<Type>),
    Function(Vec<Type>, Vec<Type>),
    Any,
}

impl Display for Type {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Type::Int => write!(f, "{}", "int"),
            Type::Bool => write!(f, "{}", "bool"),
            Type::String => write!(f, "{}", "string"),
            Type::List(el_type) => write!(f, "list of {}", el_type),
            Type::Function(inputs, outputs) => write!(f, "fn ({:?} -- {:?})", inputs, outputs),
            Type::Any => write!(f, "{}", "any"),
        }
    }
}

enum TypeError {
    TypeMismatch(Type, BoundType, Span),
    EmptyStack(Type, Span),
    NonEmptyStack(TypeStack, String),
    DumpTypeStack(TypeStack, Span),
    IfBranchInputMismatch(Vec<Type>, Vec<Type>, Span),
    IfBranchOutputMismatch(Vec<Type>, Vec<Type>, Span),
}

impl fmt::Display for TypeError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            TypeError::TypeMismatch(expected, actual, span) => {
                write!(
                    f,
                    "Type mismatch, expected '{:?}' but the top of the stack was `{:?}` at {}:",
                    expected, actual.t, span
                )
            }
            TypeError::EmptyStack(expected, span) => {
                write!(
                    f,
                    "Expected '{:?}' but the stack was empty at {}",
                    expected, span
                )
            }
            TypeError::NonEmptyStack(stack, file) => {
                write!(
                    f,
                    "Stack was not empty at the end of file {}\nStack: {:?}",
                    file,
                    stack
                        .type_stack
                        .iter()
                        .map(|bt| bt.t.clone())
                        .collect::<Vec<Type>>()
                )
            }
            TypeError::DumpTypeStack(type_stack, span) => {
                write!(
                    f,
                    "State of the stack at {}:\n{:?}",
                    span,
                    type_stack
                        .type_stack
                        .iter()
                        .map(|bt| bt.t.clone())
                        .collect::<Vec<Type>>()
                )
            }
            TypeError::IfBranchInputMismatch(then_inputs, else_inputs, span) => {
                write!(
                    f,
                    "Both branches of the `if` expression expect different inputs at {}\n",
                    span
                )?;
                write!(f, " Then expects: {:?}\n", then_inputs)?;
                write!(f, " Else expects: {:?}", else_inputs)
            }
            TypeError::IfBranchOutputMismatch(then_outputs, else_outputs, span) => {
                write!(
                    f,
                    "Both branches of the `if` expression generate different outputs at {}\n",
                    span
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
    fn check_type(expected: Type, actual: Option<BoundType>, span: Span) -> Result<(), TypeError> {
        match actual {
            Some(bound_type) => {
                if let Type::Function(inputs, outputs) = bound_type.clone().t {
                    if let Type::Function(args, returns) = expected.clone() {
                        if inputs.len() != args.len() || outputs.len() != returns.len() {
                            //println!("One");
                            return Err(TypeError::TypeMismatch(expected, bound_type, span));
                        }
                        for i in 0..inputs.len() {
                            let input = inputs[i].clone();
                            let arg = args[i].clone();

                            if !(input == Type::Any || arg == Type::Any || input == arg) {
                                //println!("Two");
                                return Err(TypeError::TypeMismatch(expected, bound_type, span));
                            }
                        }
                        for i in 0..outputs.len() {
                            let output = outputs[i].clone();
                            let ret = returns[i].clone();

                            if !(output == Type::Any || ret == Type::Any || output == ret) {
                                //println!("Three");
                                return Err(TypeError::TypeMismatch(expected, bound_type, span));
                            }
                        }
                        return Ok(());
                    } else if expected != Type::Any {
                        //println!("Four");
                        return Err(TypeError::TypeMismatch(expected, bound_type, span));
                    }
                }

                if let Type::List(actual_el_type) = bound_type.t.clone() {
                    if let Type::List(expected_el_type) = expected.clone() {
                        //Recursively check in case of List(List(Any)) and List(List(Int))
                        return TypeChecker::check_type(
                            *expected_el_type,
                            Some(BoundType {
                                t: *actual_el_type,
                                span: span.clone(),
                            }),
                            span.clone(),
                        );
                    } else if expected != Type::Any {
                        //println!("5");
                        return Err(TypeError::TypeMismatch(expected, bound_type, span));
                    }
                }

                if bound_type.t != Type::Any && expected != Type::Any && bound_type.t != expected {
                    //println!("6");
                    return Err(TypeError::TypeMismatch(expected, bound_type, span));
                }
                return Ok(());
            }
            None => return Err(TypeError::EmptyStack(expected, span.clone())),
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
            return Err(TypeError::NonEmptyStack(
                TypeStack {
                    type_stack: self.type_stack.clone(),
                },
                self.type_stack[0].span.file.clone(),
            ));
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
            Operation::Sequence(ops, _) => {
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
                            Operation::StringLiteral(_, _) => match list_type.clone() {
                                Some(el_type) => {
                                    if el_type != Type::String {
                                        list_type = Some(Type::Any)
                                    }
                                }
                                None => list_type = Some(Type::String),
                            },
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
                return Pair {
                    left: vec![],
                    right: vec![],
                };
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
            Operation::StringLiteral(_, _) => {
                return Pair {
                    left: vec![],
                    right: vec![Type::String],
                }
            }
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
                | BinaryOperator::Rem => {
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
            Operation::Import(_, _) => todo!(),
            Operation::Sort(_) => todo!(),
            Operation::Reverse(_) => todo!(),
            Operation::Max(_) => {
                return Pair {
                    left: vec![Type::Int, Type::Int],
                    right: vec![Type::Int],
                }
            }
            Operation::DumpTypeStack(_) => todo!(),
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
                    TypeChecker::check_type(return_type, top, span.clone())?;
                }
                if !self.type_stack.is_empty() {
                    return Err(TypeError::NonEmptyStack(
                        TypeStack {
                            type_stack: self.type_stack.clone(),
                        },
                        span.file.clone(),
                    ));
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
                    span: a.clone().unwrap().span.clone(),
                });
                self.type_stack.push(BoundType {
                    t: b.clone().unwrap().t,
                    span: b.clone().unwrap().span.clone(),
                });
            }
            Operation::Rot(span) => {
                let a = self.type_check_top(is_in_sequence, Type::Any, span)?;
                let b = self.type_check_top(is_in_sequence, Type::Any, span)?;
                let c = self.type_check_top(is_in_sequence, Type::Any, span)?;
                self.type_stack.push(BoundType {
                    t: b.clone().unwrap().t,
                    span: b.clone().unwrap().span.clone(),
                });
                self.type_stack.push(BoundType {
                    t: a.clone().unwrap().t,
                    span: a.clone().unwrap().span.clone(),
                });
                self.type_stack.push(BoundType {
                    t: c.clone().unwrap().t,
                    span: c.clone().unwrap().span.clone(),
                });
            }
            Operation::Dup(span) => {
                let top = self.type_check_top(is_in_sequence, Type::Any, span)?;
                self.type_stack.push(BoundType {
                    t: top.clone().unwrap().t,
                    span: top.clone().unwrap().span.clone(),
                });
                self.type_stack.push(BoundType {
                    t: top.clone().unwrap().t,
                    span: top.clone().unwrap().span.clone(),
                });
            }
            Operation::Sequence(ops, span) => {
                type_stack_stack.push(self.type_stack.clone());
                self.type_stack.clear();

                let mut inputs: Vec<Type> = vec![];
                let mut outputs: Vec<Type> = vec![];

                let (list, list_type) = parse_seq_type(ops);

                for op in ops {
                    let signature = self.get_signature(op);

                    apply(signature, &mut inputs, &mut outputs);

                    //println!("{:?}", op);
                    self.type_check_op(op, true, type_stack_stack)?;
                    //println!("{:?}", self.type_stack);
                }

                self.type_stack = type_stack_stack.pop().unwrap();

                if list {
                    match list_type {
                        Some(t) => self.type_stack.push(BoundType {
                            t: Type::List(Box::new(t)),
                            span: span.clone(),
                        }),
                        None => self.type_stack.push(BoundType {
                            t: Type::List(Box::new(Type::Any)), //TODO: also void, unit or none type?
                            span: span.clone(),
                        }),
                    }
                } else {
                    self.type_stack.push(BoundType {
                        t: Type::Function(inputs, outputs),
                        span: span.clone(),
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
                                span: span.clone(),
                            });
                        }
                    }
                    // Type::List(els) => {
                    //     for el in els {
                    //         self.type_stack.push(BoundType {
                    //             t: el,
                    //             span: span.clone(),
                    //         });
                    //     }
                    // }
                    _ => panic!("Messed up type checking for run"),
                }
            }
            Operation::IntegerLiteral(_, span) => {
                self.type_stack.push(BoundType {
                    t: Type::Int,
                    span: span.clone(),
                });
            }
            Operation::BooleanLiteral(_, span) => {
                self.type_stack.push(BoundType {
                    t: Type::Bool,
                    span: span.clone(),
                });
            }

            Operation::StringLiteral(_, span) => {
                self.type_stack.push(BoundType {
                    t: Type::String,
                    span: span.clone(),
                });
            }
            Operation::Identifier(identifier, span) => {
                if let Some(function) = self.clone().known_functions.get(identifier) {
                    for arg_type in function.inputs.clone() {
                        self.type_check_top(is_in_sequence, arg_type, span)?;
                    }

                    for result_type in function.outputs.clone() {
                        self.type_stack.push(BoundType {
                            t: result_type,
                            span: span.clone(),
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
                | BinaryOperator::Rem => {
                    self.type_check_top(is_in_sequence, Type::Int, span)?;
                    self.type_check_top(is_in_sequence, Type::Int, span)?;
                    self.type_stack.push(BoundType {
                        t: Type::Int,
                        span: span.clone(),
                    });
                }
                BinaryOperator::Gt | BinaryOperator::Lt => {
                    self.type_check_top(is_in_sequence, Type::Int, span)?;
                    self.type_check_top(is_in_sequence, Type::Int, span)?;
                    self.type_stack.push(BoundType {
                        t: Type::Bool,
                        span: span.clone(),
                    });
                }
                BinaryOperator::Eq => {
                    let t = self.type_check_top(is_in_sequence, Type::Int, span)?;
                    self.type_check_top(is_in_sequence, t.unwrap().t, span)?;
                    self.type_stack.push(BoundType {
                        t: Type::Bool,
                        span: span.clone(),
                    });
                }
                BinaryOperator::And | BinaryOperator::Or => {
                    self.type_check_top(is_in_sequence, Type::Bool, span)?;
                    self.type_check_top(is_in_sequence, Type::Bool, span)?;
                    self.type_stack.push(BoundType {
                        t: Type::Bool,
                        span: span.clone(),
                    });
                }
            },
            Operation::Unary(op, span) => match op {
                UnaryOperator::Neg => {
                    self.type_check_top(is_in_sequence, Type::Bool, span)?;
                    self.type_stack.push(BoundType {
                        t: Type::Bool,
                        span: span.clone(),
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
                        span: span.clone(),
                    });
                } else {
                    panic!("Incorrectly parsed filter");
                }
            }
            Operation::Map(span) => {
                self.type_check_top(
                    is_in_sequence,
                    Type::Function(vec![Type::Any], vec![Type::Any]),
                    span,
                )?;
                let list = self
                    .type_check_top(is_in_sequence, Type::List(Box::new(Type::Any)), span)?
                    .unwrap();
                self.type_stack.push(BoundType {
                    t: list.t,
                    span: span.clone(),
                });
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
                    span: span.clone(),
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
                    span: span.clone(),
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
                                span: span.clone(),
                            });
                        } else {
                            self.type_stack.push(BoundType {
                                t: Type::List(Box::new(Type::Any)),
                                span: span.clone(),
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
                        span: span.clone(),
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
                    span: span.clone(),
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
                        span: span.clone(),
                    });
                } else {
                    self.type_stack.push(BoundType {
                        t: Type::List(Box::new(Type::Any)),
                        span: span.clone(),
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
                                span.clone(),
                            ));
                        }
                        for i in 0..then_inputs.len() {
                            if then_inputs[i] != else_inputs[i] {
                                return Err(TypeError::IfBranchInputMismatch(
                                    then_inputs,
                                    else_inputs,
                                    span.clone(),
                                ));
                            }
                        }

                        if then_outputs.len() != else_outputs.len() {
                            return Err(TypeError::IfBranchOutputMismatch(
                                then_outputs,
                                else_outputs,
                                span.clone(),
                            ));
                        }
                        for i in 0..then_outputs.len() {
                            if then_outputs[i] != else_outputs[i] {
                                return Err(TypeError::IfBranchOutputMismatch(
                                    then_outputs,
                                    else_outputs,
                                    span.clone(),
                                ));
                            }
                        }
                        for input in then_inputs {
                            self.type_check_top(is_in_sequence, input, span)?;
                        }
                        for output in then_outputs {
                            self.type_stack.push(BoundType {
                                t: output,
                                span: span.clone(),
                            });
                        }
                    } else {
                        panic!("If parsed wrong");
                    }
                } else {
                    panic!("If parsed wrong");
                }
            }
            Operation::While(_) => {
                todo!();
                // self.type_check_top(is_in_sequence, Type::Seq, span)?;
                // self.type_check_top(is_in_sequence, Type::Seq, span)?;
            }
            Operation::Import(functions, _) => {
                for function in functions {
                    self.known_functions.insert(
                        function.name.clone(),
                        BoundFunction {
                            inputs: function.arguments.clone(),
                            outputs: function.returns.clone(),
                        },
                    );
                }
            }
            Operation::Sort(span) | Operation::Reverse(span) => {
                let list = self
                    .type_check_top(is_in_sequence, Type::List(Box::new(Type::Any)), span)?
                    .unwrap();
                self.type_stack.push(BoundType {
                    t: list.t,
                    span: span.clone(),
                });
            }
            Operation::DumpTypeStack(span) => {
                return Err(TypeError::DumpTypeStack(
                    TypeStack {
                        type_stack: self.type_stack.clone(),
                    },
                    span.clone(),
                ));
            }
            Operation::Max(span) => {
                self.type_check_top(is_in_sequence, Type::Int, span)?;
                self.type_check_top(is_in_sequence, Type::Int, span)?;
                self.type_stack.push(BoundType {
                    t: Type::Int,
                    span: span.clone(),
                });
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
        TypeChecker::check_type(expected_type.clone(), top.clone(), span.clone())?;
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
                    file: "".to_string(),
                },
            });
        }
    }

    fn new() -> TypeChecker {
        return TypeChecker {
            type_stack: vec![],
            known_functions: HashMap::new(),
        };
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
                Operation::StringLiteral(_, _) => match list_type.clone() {
                    Some(el_type) => {
                        if el_type != Type::String {
                            list_type = Some(Type::Any)
                        }
                    }
                    None => list_type = Some(Type::String),
                },
                Operation::Sequence(ops, _) => {
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

fn apply(signature: Pair<Vec<Type>, Vec<Type>>, inputs: &mut Vec<Type>, outputs: &mut Vec<Type>) {
    //If it's a fresh slate then just add the inputs and outputs
    //println!("sig: {:?}", signature);
    if inputs.is_empty() && outputs.is_empty() {
        *inputs = signature.left;
        *outputs = signature.right;
        //println!("inputs: {:?}", inputs);
        //println!("outputs: {:?}", outputs);
        return;
    }
    //after dup we have [any] -- [any any]
    //after * we want [int] -- [int], will settle for [any] -- [int]

    //otherwise we need to simulate running the operation
    for arg in signature.left.clone() {
        if outputs.is_empty() {
            inputs.push(arg);
        } else {
            outputs.pop();
        }
    }
    for ret in signature.right.clone() {
        outputs.push(ret);
    }
    //println!("inputs: {:?}", inputs);
    //println!("outputs: {:?}", outputs);
}

//Interpreting

#[derive(Clone, Debug)]
enum Value {
    Integer(i64),
    Boolean(bool),
    String(String),
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
            //     write!(f, "fn ({:?} -- {:?})", signature.left, signature.right)
            // }
            Value::Seq(elements) => write!(f, "{:#?}", elements),
            Value::String(s) => write!(f, "{}", s),
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
        } else if let Value::String(l) = self {
            match rhs {
                Value::String(r) => return Value::String(l + &r),
                _ => {
                    unreachable!();
                }
            }
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
            (Self::String(l0), Self::String(r0)) => l0 == r0,
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
            Value::String(_) => todo!(),
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
        // println!("{:?}", operation);
        match operation {
            Operation::NoOp => {}
            Operation::IntegerLiteral(i, _) => self.stack.push(Value::Integer(*i)),
            Operation::BooleanLiteral(b, _) => self.stack.push(Value::Boolean(*b)),

            Operation::StringLiteral(s, _) => self.stack.push(Value::String(s.to_string())),
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
            Operation::Print(_) => {
                let val = self.stack.pop().unwrap();
                println!("{}", val);
            }
            Operation::Sequence(ops, _) => self.stack.push(Value::Seq(ops.to_vec())),
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
                            match (operand, result.clone()) {
                                (Operation::Sequence(_, _), Value::Seq(ops)) => {
                                    results.push(Operation::Sequence(ops, Span::empty()));
                                }
                                (Operation::Sequence(_, _), Value::Integer(i)) => {
                                    results.push(Operation::Sequence(
                                        vec![Operation::IntegerLiteral(i, Span::empty())],
                                        Span::empty(),
                                    ));
                                }
                                (Operation::Sequence(_, _), Value::Boolean(b)) => {
                                    results.push(Operation::Sequence(
                                        vec![Operation::BooleanLiteral(b, Span::empty())],
                                        Span::empty(),
                                    ));
                                }
                                (Operation::Sequence(_, _), Value::String(s)) => {
                                    results.push(Operation::Sequence(
                                        vec![Operation::StringLiteral(s, Span::empty())],
                                        Span::empty(),
                                    ));
                                }
                                (Operation::IntegerLiteral(_, _), Value::Integer(i)) => {
                                    results.push(Operation::IntegerLiteral(i, Span::empty()));
                                }
                                (Operation::BooleanLiteral(_, _), Value::Boolean(b)) => {
                                    results.push(Operation::BooleanLiteral(b, Span::empty()));
                                }
                                (Operation::StringLiteral(_, _), Value::String(s)) => {
                                    results.push(Operation::StringLiteral(s, Span::empty()));
                                }
                                _ => panic!(
                                    "Illegal combination, operand: {:?}, result: {}",
                                    operand, &result
                                ),
                            }
                        }
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
                    Value::Seq(ops) => Operation::Sequence(ops, Span::empty()),
                    Value::String(s) => Operation::StringLiteral(s, Span::empty()),
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
                    _ => todo!(),
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
            Operation::Import(functions, _) => {
                for function in functions {
                    self.functions
                        .insert(function.name.clone(), function.clone());
                }
            }
            Operation::Sort(span) => {
                if let Value::Seq(ops) = self.stack.pop().unwrap() {
                    let mut values: Vec<Value> = vec![];
                    for op in ops {
                        match op {
                            Operation::IntegerLiteral(i, _) => values.push(Value::Integer(i)),
                            Operation::BooleanLiteral(b, _) => values.push(Value::Boolean(b)),
                            Operation::StringLiteral(s, _) => values.push(Value::String(s)),
                            _ => panic!("Cannot sort {:?}", op),
                        }
                    }
                    values.sort();
                    let mut sorted_ops: Vec<Operation> = vec![];
                    for value in values.iter().rev() {
                        match value {
                            Value::Integer(i) => {
                                sorted_ops.push(Operation::IntegerLiteral(*i, span.clone()))
                            }
                            Value::Boolean(b) => {
                                sorted_ops.push(Operation::BooleanLiteral(*b, span.clone()))
                            }
                            Value::String(s) => sorted_ops
                                .push(Operation::StringLiteral(s.to_string(), span.clone())),
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
                            Operation::StringLiteral(s, _) => values.push(Value::String(s)),
                            _ => panic!("Cannot sort {:?}", op),
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
                            Value::String(s) => sorted_ops
                                .push(Operation::StringLiteral(s.to_string(), span.to_owned())),
                            _ => panic!("Cannot sort {}", value),
                        }
                    }
                    self.stack.push(Value::Seq(sorted_ops));
                }
            }
            Operation::Max(_) => {
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

fn display_type_error(error: TypeError, contents: &String) {
    if let TypeError::DumpTypeStack(_, span) = &error {
        print!("{} ", format!("{}", "Halting execution").yellow());
        println!("at {}", span);
    } else {
        print!("{} ", format!("{}", "ERROR:").red());
    }
    println!("{}", error);
    match error {
        TypeError::TypeMismatch(_, actual, span) => {
            print_span(span.clone(), contents, HighlightColor::RED);
            println!("\n{} introduced at {}", actual.t, actual.span);
            print_span(actual.span, contents, HighlightColor::YELLOW);
        }
        TypeError::EmptyStack(_, span) => {
            print_span(span.clone(), contents, HighlightColor::RED);
        }
        TypeError::IfBranchInputMismatch(_, _, span) => {
            print_span(span.clone(), contents, HighlightColor::RED);
        }
        TypeError::IfBranchOutputMismatch(_, _, span) => {
            print_span(span.clone(), contents, HighlightColor::RED);
        }
        TypeError::NonEmptyStack(type_stack, _) => {
            let mut stack_index = 0;
            for bound_type in type_stack.type_stack.iter().rev() {
                println!(
                    "\n[{}] {} introduced at {}",
                    stack_index, bound_type.t, bound_type.span
                );
                print_span(bound_type.clone().span, contents, HighlightColor::RED);
                stack_index += 1;
            }
        }
        TypeError::DumpTypeStack(type_stack, _) => {
            let mut stack_index = 0;
            for bound_type in type_stack.type_stack.iter().rev() {
                println!(
                    "\n[{}] `{}` introduced at {}",
                    stack_index, bound_type.t, bound_type.span
                );
                print_span(bound_type.clone().span, contents, HighlightColor::YELLOW);
                stack_index += 1;
            }
        }
    }
}

fn display_parse_error(error: ParseError, contents: &String) {
    print!("{} ", format!("{}", "ERROR:").red());
    println!("{}", error);
    match error {
        ParseError::UnknownIdentifier(_, span) => {
            print_span(span.clone(), contents, HighlightColor::RED);
        }
        ParseError::UnexpectedToken(_, _, span) => {
            print_span(span.clone(), contents, HighlightColor::RED);
        }
    }
}

enum HighlightColor {
    RED,
    YELLOW,
}

use colored::Colorize;
fn print_span(span: Span, contents: &String, highlight_color: HighlightColor) {
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

fn main() {
    let args: Vec<String> = env::args().collect();
    let file_path = &args[1];

    let contents = fs::read_to_string(file_path).expect("Should have been able to read the file");

    let tokens = lex(file_path.to_owned(), &contents);

    match tokens {
        Ok(ts) => {
            let mut parser = Parser::new(ts);
            match parser.parse() {
                Ok(program) => {
                    let mut type_checker = TypeChecker::new();
                    if let Err(error) = type_checker.type_check_parsed_program(&program) {
                        display_type_error(error, &contents);
                        return;
                    }
                    let mut interpreter = Interpreter::new(program);
                    interpreter.interpret_program();
                }
                Err(error) => {
                    display_parse_error(error, &contents);
                    return;
                }
            }
        }
        Err(err) => println!("{}", err),
    }
}
