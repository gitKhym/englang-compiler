use std::char;

pub struct Lexer {
    input: String,
    pos: usize,
    peek_pos: usize,
    char: char,
}

#[derive(Debug, PartialEq)]
pub enum TokenType {
    Illegal,
    Eof,
    LCurl,
    RCurl,
    LParen,
    RParen,
    Eq,
    Bang,
    Plus,
    Minus,
    Div,
    Mult,
    Lt,
    Gt,
    Semi,
    Comma,
}

#[derive(Debug)]
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
            self.char = '0';
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

    pub fn get_token(&mut self) -> Token {
        let token: Token;
        match self.char {
            '{' => token = Token::new(TokenType::LCurl, self.char.to_string()),
            '}' => token = Token::new(TokenType::RCurl, self.char.to_string()),
            '(' => token = Token::new(TokenType::LParen, self.char.to_string()),
            ')' => token = Token::new(TokenType::RParen, self.char.to_string()),
            '=' => token = Token::new(TokenType::Eq, self.char.to_string()),
            '!' => token = Token::new(TokenType::Bang, self.char.to_string()),
            '+' => token = Token::new(TokenType::Plus, self.char.to_string()),
            '-' => token = Token::new(TokenType::Minus, self.char.to_string()),
            '/' => token = Token::new(TokenType::Div, self.char.to_string()),
            '*' => token = Token::new(TokenType::Mult, self.char.to_string()),
            '<' => token = Token::new(TokenType::Lt, self.char.to_string()),
            '>' => token = Token::new(TokenType::Gt, self.char.to_string()),
            ';' => token = Token::new(TokenType::Semi, self.char.to_string()),
            ',' => token = Token::new(TokenType::Comma, self.char.to_string()),
            '0' => token = Token::new(TokenType::Eof, String::from("")),
            _ => token = Token::new(TokenType::Illegal, self.char.to_string()),
        }
        self.read_char();
        token
    }
}
