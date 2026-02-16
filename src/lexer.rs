use std::char;

pub struct Lexer {
    input: String,
    pos: usize,
    peek_pos: usize,
    char: char,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum TokenType {
    Illegal,
    Eof,
    Ident,

    // Symbols
    LCurl,
    RCurl,
    LParen,
    RParen,
    LSquare,
    RSquare,
    Assign,
    Eq,
    Neq,
    Bang,
    Plus,
    Minus,
    Div,
    Mult,
    Lt,
    Gt,
    Rarrow,
    Semi,
    Comma,
    Colon,
    Dot,
    DoubleQt,
    SingleQt,

    Digit,

    // Keywords
    Class,
    Impl,
    SelfType,
    Loop,
    With,
    While,
    Null,
    True,
    False,
    Return,
    Fn,
    If,
    Else,
    Type(VarType),
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum VarType {
    Void,
    String,
    Int,
    Float,
    Bool,
    Array(Box<VarType>),
    Custom(String),
}

#[derive(Debug, Clone, PartialEq)]
pub struct Token {
    pub token_type: TokenType,
    pub lexeme: String,
}

impl Token {
    pub fn new(token_type: TokenType, lexeme: String) -> Self {
        Self { token_type, lexeme }
    }
}

impl Lexer {
    pub fn new(input: String) -> Self {
        let mut lexer = Lexer {
            input,
            pos: 0,
            peek_pos: 0,
            char: 'a',
        };
        lexer.read_char();
        lexer
    }

    pub fn read_char(&mut self) {
        if self.peek_pos >= self.input.len() {
            self.char = '\0';
            self.pos = self.peek_pos;
            return;
        } else {
            self.char = self
                .input
                .chars()
                .nth(self.peek_pos)
                .expect("Index out of range")
        }
        self.pos = self.peek_pos;
        self.peek_pos += 1;
    }

    pub fn peek_char(&mut self) -> char {
        if self.peek_pos >= self.input.len() {
            '\0'
        } else {
            self.input
                .chars()
                .nth(self.peek_pos)
                .expect("Index out of range")
        }
    }

    fn skip_line_comment(&mut self) {
        while self.char != '\0' && self.char != '\n' {
            self.read_char();
        }
        if self.char == '\n' {
            self.read_char();
        }
    }

    fn skip_block_comment(&mut self) {
        self.read_char(); // consume the first * after /
        loop {
            if self.char == '\0' {
                break;
            } else if self.char == '*' && self.peek_char() == '/' {
                self.read_char(); // consume *
                self.read_char(); // consume /
                self.read_char(); // move to next char after comment
                break;
            } else {
                self.read_char();
            }
        }
    }

    pub fn skip_whitespace(&mut self) {
        loop {
            match self.char {
                ' ' | '\t' | '\n' | '\r' => self.read_char(),
                '/' => {
                    match self.peek_char() {
                        '/' => {
                            self.read_char(); // consume first /
                            self.read_char(); // consume second /
                            self.skip_line_comment();
                        }
                        '*' => {
                            self.read_char(); // consume first /
                            self.read_char(); // consume *
                            self.skip_block_comment();
                        }
                        _ => break,
                    }
                }
                _ => break,
            }
        }
    }

    pub fn is_letter(&mut self) -> bool {
        matches!(self.char, 'a'..='z' | 'A'..='Z' | '_')
    }

    pub fn is_digit(&mut self) -> bool {
        matches!(self.char, '0'..='9')
    }

    pub fn read_number(&mut self) -> String {
        let pos = self.pos;

        while self.is_digit() {
            self.read_char();
        }

        self.input[pos..self.pos].to_string()
    }

    pub fn read_identifier(&mut self) -> String {
        let pos = self.pos;

        while self.is_letter() {
            self.read_char();
        }

        self.input[pos..self.pos].to_string()
    }

    // pub fn read_string(&mut self) -> String {
    //     todo!("Uhh idk if I should even have it here")
    // }

    pub fn determine_token_type(&mut self, input: &str) -> TokenType {
        match input {
            "loop" => TokenType::Loop,
            "with" => TokenType::With,
            "while" => TokenType::While,
            "class" => TokenType::Class,
            "impl" => TokenType::Impl,
            "Self" => TokenType::SelfType,
            "True" => TokenType::True,
            "False" => TokenType::False,
            "fn" => TokenType::Fn,
            "return" => TokenType::Return,
            "if" => TokenType::If,
            "else" => TokenType::Else,
            "null" => TokenType::Null,
            "void" => TokenType::Type(VarType::Void),
            "string" => TokenType::Type(VarType::String),
            "int" => TokenType::Type(VarType::Int), // Maybe change to PrimInt for primitives later
            "float" => TokenType::Type(VarType::Float),
            "bool" => TokenType::Type(VarType::Bool),
            _ => TokenType::Ident,
        }
    }

    pub fn get_token(&mut self) -> Token {
        self.skip_whitespace();

        let token = match self.char {
            '{' => Token::new(TokenType::LCurl, self.char.to_string()),
            '}' => Token::new(TokenType::RCurl, self.char.to_string()),
            '(' => Token::new(TokenType::LParen, self.char.to_string()),
            ')' => Token::new(TokenType::RParen, self.char.to_string()),
            '[' => Token::new(TokenType::LSquare, self.char.to_string()),
            ']' => Token::new(TokenType::RSquare, self.char.to_string()),
            '=' => {
                if self.peek_char() == '=' {
                    let first = self.char;
                    self.read_char();
                    let second = self.char;
                    Token::new(TokenType::Eq, format!("{}{}", first, second))
                } else {
                    Token::new(TokenType::Assign, self.char.to_string())
                }
            }
            '!' => {
                if self.peek_char() == '=' {
                    let first = self.char;
                    self.read_char();
                    let second = self.char;
                    Token::new(TokenType::Neq, format!("{}{}", first, second))
                } else {
                    Token::new(TokenType::Bang, self.char.to_string())
                }
            }
            '+' => Token::new(TokenType::Plus, self.char.to_string()),
            '-' => {
                if self.peek_char() == '>' {
                    let first = self.char;
                    self.read_char();
                    let second = self.char;
                    Token::new(TokenType::Rarrow, format!("{}{}", first, second))
                } else {
                    Token::new(TokenType::Minus, self.char.to_string())
                }
            }
            '/' => Token::new(TokenType::Div, self.char.to_string()),
            '*' => Token::new(TokenType::Mult, self.char.to_string()),
            '"' => Token::new(TokenType::DoubleQt, self.char.to_string()),
            '\'' => Token::new(TokenType::SingleQt, self.char.to_string()),
            '<' => Token::new(TokenType::Lt, self.char.to_string()),
            '>' => Token::new(TokenType::Gt, self.char.to_string()),
            ':' => Token::new(TokenType::Colon, self.char.to_string()),
            ';' => Token::new(TokenType::Semi, self.char.to_string()),
            ',' => Token::new(TokenType::Comma, self.char.to_string()),
            '.' => Token::new(TokenType::Dot, self.char.to_string()),
            '\0' => Token::new(TokenType::Eof, "".to_string()),
            _ => {
                if self.is_letter() {
                    let lexeme = self.read_identifier();
                    let token_type = self.determine_token_type(&lexeme);
                    return Token::new(token_type, lexeme);
                } else if self.is_digit() {
                    let lexeme = self.read_number();
                    return Token::new(TokenType::Digit, lexeme);
                } else {
                    Token::new(TokenType::Illegal, self.char.to_string())
                }
            }
        };

        self.read_char();
        token
    }
}

