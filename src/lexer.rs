use crate::token::{Token, TokenType};

pub struct Lexer {
    input: String,
    position: usize,      // 当前位置
    read_position: usize, // 下一个位置
    ch: u8,               // 当前字符
}

impl Lexer {
    pub fn new(input: String) -> Lexer {
        let mut l = Lexer {
            input,
            position: 0,
            read_position: 0,
            ch: 0,
        };
        l.read_char();
        l
    }

    pub(crate) fn next_token(&mut self) -> Token {
        let tok;

        self.skip_whitespace();

        match self.ch {
            b'=' => {
                if self.peek_char() == b'=' {
                    // ==
                    self.read_char();
                    let literal = "==";
                    tok = Token::from_str(TokenType::EQ, literal);
                } else {
                    // =
                    tok = Token::from_char(TokenType::Assign, self.ch)
                }
            }
            b'+' => tok = Token::from_char(TokenType::Plus, self.ch),
            b'-' => tok = Token::from_char(TokenType::Minus, self.ch),
            b'*' => tok = Token::from_char(TokenType::Asterisk, self.ch),
            b'/' => tok = Token::from_char(TokenType::Slash, self.ch),
            b'<' => tok = Token::from_char(TokenType::LT, self.ch),
            b'>' => tok = Token::from_char(TokenType::GT, self.ch),
            b'!' => {
                if self.peek_char() == b'=' {
                    // !=
                    self.read_char();
                    let literal = "!=";
                    tok = Token::from_str(TokenType::NotEQ, literal);
                } else {
                    // !
                    tok = Token::from_char(TokenType::Bang, self.ch)
                }
            }
            b';' => tok = Token::from_char(TokenType::Semicolon, self.ch),
            b',' => tok = Token::from_char(TokenType::Comma, self.ch),
            b'(' => tok = Token::from_char(TokenType::LParen, self.ch),
            b')' => tok = Token::from_char(TokenType::RParen, self.ch),
            b'{' => tok = Token::from_char(TokenType::LBrace, self.ch),
            b'}' => tok = Token::from_char(TokenType::RBrace, self.ch),
            0 => tok = Token::from_str(TokenType::EOF, ""),
            _ => {
                if is_letter(self.ch) {
                    let literal = self.read_identifier();
                    let token_type = Token::lookup_ident(&literal);
                    return Token::from_str(token_type, &literal);
                } else if self.ch.is_ascii_digit() {
                    let literal = self.read_number();
                    return Token::from_str(TokenType::Int, &literal);
                } else {
                    tok = Token::from_char(TokenType::Illegal, self.ch);
                }
            }
        }

        self.read_char();

        tok
    }

    fn read_char(&mut self) {
        if self.read_position >= self.input.len() {
            self.ch = 0;
        } else {
            self.ch = self.input.as_bytes()[self.read_position];
        }
        self.position = self.read_position;
        self.read_position += 1;
    }

    fn read_identifier(&mut self) -> String {
        let position = self.position;
        while is_letter(self.ch) {
            self.read_char();
        }
        self.input[position..self.position].to_string()
    }

    fn read_number(&mut self) -> String {
        let position = self.position;
        while self.ch.is_ascii_digit() {
            self.read_char();
        }
        self.input[position..self.position].to_string()
    }

    fn skip_whitespace(&mut self) {
        while self.ch.is_ascii_whitespace() {
            self.read_char();
        }
    }

    fn peek_char(&self) -> u8 {
        if self.read_position >= self.input.len() {
            0
        } else {
            self.input.as_bytes()[self.read_position]
        }
    }
}

fn is_letter(ch: u8) -> bool {
    matches!(ch, b'a'..=b'z' | b'A'..=b'Z' | b'_')
}

#[cfg(test)]
mod test {
    use crate::token::TokenType;

    #[test]
    fn test_next_token() {
        let input = "let five = 5;
        let ten = 10;
        let add = fn(x, y) {
        x + y;
        };
        let result = add(five, ten);
        !-/*5;
        5 < 10 > 5;
        
        if (5 < 10) {
            return true;
        } else {
            return false;
        }
        
        10 == 10;
        10 != 9;
        ";

        let tests = [
            (TokenType::Let, "let"),
            (TokenType::Identifier, "five"),
            (TokenType::Assign, "="),
            (TokenType::Int, "5"),
            (TokenType::Semicolon, ";"),
            (TokenType::Let, "let"),
            (TokenType::Identifier, "ten"),
            (TokenType::Assign, "="),
            (TokenType::Int, "10"),
            (TokenType::Semicolon, ";"),
            (TokenType::Let, "let"),
            (TokenType::Identifier, "add"),
            (TokenType::Assign, "="),
            (TokenType::Function, "fn"),
            (TokenType::LParen, "("),
            (TokenType::Identifier, "x"),
            (TokenType::Comma, ","),
            (TokenType::Identifier, "y"),
            (TokenType::RParen, ")"),
            (TokenType::LBrace, "{"),
            (TokenType::Identifier, "x"),
            (TokenType::Plus, "+"),
            (TokenType::Identifier, "y"),
            (TokenType::Semicolon, ";"),
            (TokenType::RBrace, "}"),
            (TokenType::Semicolon, ";"),
            (TokenType::Let, "let"),
            (TokenType::Identifier, "result"),
            (TokenType::Assign, "="),
            (TokenType::Identifier, "add"),
            (TokenType::LParen, "("),
            (TokenType::Identifier, "five"),
            (TokenType::Comma, ","),
            (TokenType::Identifier, "ten"),
            (TokenType::RParen, ")"),
            (TokenType::Semicolon, ";"),
            (TokenType::Bang, "!"),
            (TokenType::Minus, "-"),
            (TokenType::Slash, "/"),
            (TokenType::Asterisk, "*"),
            (TokenType::Int, "5"),
            (TokenType::Semicolon, ";"),
            (TokenType::Int, "5"),
            (TokenType::LT, "<"),
            (TokenType::Int, "10"),
            (TokenType::GT, ">"),
            (TokenType::Int, "5"),
            (TokenType::Semicolon, ";"),
            (TokenType::If, "if"),
            (TokenType::LParen, "("),
            (TokenType::Int, "5"),
            (TokenType::LT, "<"),
            (TokenType::Int, "10"),
            (TokenType::RParen, ")"),
            (TokenType::LBrace, "{"),
            (TokenType::Return, "return"),
            (TokenType::True, "true"),
            (TokenType::Semicolon, ";"),
            (TokenType::RBrace, "}"),
            (TokenType::Else, "else"),
            (TokenType::LBrace, "{"),
            (TokenType::Return, "return"),
            (TokenType::False, "false"),
            (TokenType::Semicolon, ";"),
            (TokenType::RBrace, "}"),
            (TokenType::Int, "10"),
            (TokenType::EQ, "=="),
            (TokenType::Int, "10"),
            (TokenType::Semicolon, ";"),
            (TokenType::Int, "10"),
            (TokenType::NotEQ, "!="),
            (TokenType::Int, "9"),
            (TokenType::Semicolon, ";"),
            (TokenType::EOF, ""),
        ];

        let mut l = super::Lexer::new(input.to_string());

        for (i, (token_type, literal)) in tests.into_iter().enumerate() {
            let tok = l.next_token();
            assert_eq!(
                tok.token_type, token_type,
                "tests[{i}] - tokentype wrong. expected={token_type:?}, got={:?}",
                tok.token_type
            );
            assert_eq!(
                tok.literal,
                literal.to_string(),
                "tests[{i}] - literal wrong. expected={literal}, got={}",
                tok.literal
            )
        }
    }
}
