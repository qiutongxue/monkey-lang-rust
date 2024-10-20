use std::{fmt::Display, sync::LazyLock};

static KEYWORDS: LazyLock<std::collections::HashMap<&'static str, TokenType>> =
    LazyLock::new(|| {
        let mut map = std::collections::HashMap::new();
        map.insert("fn", TokenType::Function);
        map.insert("let", TokenType::Let);
        map.insert("true", TokenType::True);
        map.insert("false", TokenType::False);
        map.insert("if", TokenType::If);
        map.insert("else", TokenType::Else);
        map.insert("return", TokenType::Return);
        map
    });

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct Token {
    pub token_type: TokenType,
    pub literal: String,
}

impl Token {
    pub fn lookup_ident(ident: &str) -> TokenType {
        match KEYWORDS.get(ident) {
            Some(token_type) => *token_type,
            None => TokenType::Identifier,
        }
    }

    pub fn from_char(token_type: TokenType, ch: u8) -> Token {
        Token {
            token_type,
            literal: (ch as char).to_string(),
        }
    }

    pub fn from_str(token_type: TokenType, literal: &str) -> Token {
        Token {
            token_type,
            literal: literal.to_string(),
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum TokenType {
    Illegal, // 非法
    EOF,     // 文件结束

    // 标识符 + 各种字面量类型
    Identifier, // 标识符
    Int,        // 整型
    String,     // 字符串

    // 操作符
    Assign,
    Plus,
    Minus,
    Bang,
    Asterisk,
    Slash,

    LT,
    GT,

    EQ,
    NotEQ,

    // 分隔符
    Comma,
    Semicolon,

    LParen,
    RParen,
    LBrace,
    RBrace,

    // 关键字
    Function,
    Let,
    True,
    False,
    If,
    Else,
    Return,
}

impl Display for TokenType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let s = match self {
            TokenType::Illegal => "ILLEGAL",
            TokenType::EOF => "EOF",
            TokenType::Identifier => "IDENT",
            TokenType::Int => "INT",
            TokenType::Assign => "=",
            TokenType::Plus => "+",
            TokenType::Minus => "-",
            TokenType::Bang => "!",
            TokenType::Asterisk => "*",
            TokenType::Slash => "/",
            TokenType::LT => "<",
            TokenType::GT => ">",
            TokenType::EQ => "==",
            TokenType::NotEQ => "!=",
            TokenType::Comma => ",",
            TokenType::Semicolon => ";",
            TokenType::LParen => "(",
            TokenType::RParen => ")",
            TokenType::LBrace => "{",
            TokenType::RBrace => "}",
            TokenType::Function => "FUNCTION",
            TokenType::Let => "LET",
            TokenType::True => "TRUE",
            TokenType::False => "FALSE",
            TokenType::If => "IF",
            TokenType::Else => "ELSE",
            TokenType::Return => "RETURN",
            TokenType::String => "STRING",
        };
        f.write_str(s)
    }
}
