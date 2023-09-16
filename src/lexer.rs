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
                    tok = Token::from_char(TokenType::ASSIGN, self.ch)
                }
            }
            b'+' => tok = Token::from_char(TokenType::PLUS, self.ch),
            b'-' => tok = Token::from_char(TokenType::MINUS, self.ch),
            b'*' => tok = Token::from_char(TokenType::ASTERISK, self.ch),
            b'/' => tok = Token::from_char(TokenType::SLASH, self.ch),
            b'<' => tok = Token::from_char(TokenType::LT, self.ch),
            b'>' => tok = Token::from_char(TokenType::GT, self.ch),
            b'!' => {
                if self.peek_char() == b'=' {
                    // !=
                    self.read_char();
                    let literal = "!=";
                    tok = Token::from_str(TokenType::NOTEQ, literal);
                } else {
                    // !
                    tok = Token::from_char(TokenType::BANG, self.ch)
                }
            }
            b';' => tok = Token::from_char(TokenType::SEMICOLON, self.ch),
            b',' => tok = Token::from_char(TokenType::COMMA, self.ch),
            b'(' => tok = Token::from_char(TokenType::LPAREN, self.ch),
            b')' => tok = Token::from_char(TokenType::RPAREN, self.ch),
            b'{' => tok = Token::from_char(TokenType::LBRACE, self.ch),
            b'}' => tok = Token::from_char(TokenType::RBRACE, self.ch),
            0 => tok = Token::from_str(TokenType::EOF, ""),
            _ => {
                if is_letter(self.ch) {
                    let literal = self.read_identifier();
                    let token_type = Token::lookup_ident(&literal);
                    return Token::from_str(token_type, &literal);
                } else if self.ch.is_ascii_digit() {
                    let literal = self.read_number();
                    return Token::from_str(TokenType::INT, &literal);
                } else {
                    tok = Token::from_char(TokenType::ILLEGAL, self.ch);
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
            (TokenType::LET, "let"),
            (TokenType::IDENT, "five"),
            (TokenType::ASSIGN, "="),
            (TokenType::INT, "5"),
            (TokenType::SEMICOLON, ";"),
            (TokenType::LET, "let"),
            (TokenType::IDENT, "ten"),
            (TokenType::ASSIGN, "="),
            (TokenType::INT, "10"),
            (TokenType::SEMICOLON, ";"),
            (TokenType::LET, "let"),
            (TokenType::IDENT, "add"),
            (TokenType::ASSIGN, "="),
            (TokenType::FUNCTION, "fn"),
            (TokenType::LPAREN, "("),
            (TokenType::IDENT, "x"),
            (TokenType::COMMA, ","),
            (TokenType::IDENT, "y"),
            (TokenType::RPAREN, ")"),
            (TokenType::LBRACE, "{"),
            (TokenType::IDENT, "x"),
            (TokenType::PLUS, "+"),
            (TokenType::IDENT, "y"),
            (TokenType::SEMICOLON, ";"),
            (TokenType::RBRACE, "}"),
            (TokenType::SEMICOLON, ";"),
            (TokenType::LET, "let"),
            (TokenType::IDENT, "result"),
            (TokenType::ASSIGN, "="),
            (TokenType::IDENT, "add"),
            (TokenType::LPAREN, "("),
            (TokenType::IDENT, "five"),
            (TokenType::COMMA, ","),
            (TokenType::IDENT, "ten"),
            (TokenType::RPAREN, ")"),
            (TokenType::SEMICOLON, ";"),
            (TokenType::BANG, "!"),
            (TokenType::MINUS, "-"),
            (TokenType::SLASH, "/"),
            (TokenType::ASTERISK, "*"),
            (TokenType::INT, "5"),
            (TokenType::SEMICOLON, ";"),
            (TokenType::INT, "5"),
            (TokenType::LT, "<"),
            (TokenType::INT, "10"),
            (TokenType::GT, ">"),
            (TokenType::INT, "5"),
            (TokenType::SEMICOLON, ";"),
            (TokenType::IF, "if"),
            (TokenType::LPAREN, "("),
            (TokenType::INT, "5"),
            (TokenType::LT, "<"),
            (TokenType::INT, "10"),
            (TokenType::RPAREN, ")"),
            (TokenType::LBRACE, "{"),
            (TokenType::RETURN, "return"),
            (TokenType::TRUE, "true"),
            (TokenType::SEMICOLON, ";"),
            (TokenType::RBRACE, "}"),
            (TokenType::ELSE, "else"),
            (TokenType::LBRACE, "{"),
            (TokenType::RETURN, "return"),
            (TokenType::FALSE, "false"),
            (TokenType::SEMICOLON, ";"),
            (TokenType::RBRACE, "}"),
            (TokenType::INT, "10"),
            (TokenType::EQ, "=="),
            (TokenType::INT, "10"),
            (TokenType::SEMICOLON, ";"),
            (TokenType::INT, "10"),
            (TokenType::NOTEQ, "!="),
            (TokenType::INT, "9"),
            (TokenType::SEMICOLON, ";"),
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
