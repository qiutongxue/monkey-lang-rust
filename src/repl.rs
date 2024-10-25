const MONKEY_FACE: &str = r#"
            __,__
   .--.  .-"     "-.  .--.
  / .. \/  .-. .-.  \/ .. \
 | |  '|  /   Y   \  |'  | |
 | \   \  \ 0 | 0 /  /   / |
  \ '- ,\.-"""""""-./, -' /
   ''-' /_   ^ ^   _\ '-''
       |  \._   _./  |
       \   \ '~' /   /
        '._ '-=-' _.'
           '-----'
"#;

const PROMPT: &str = ">> ";

use std::io::Write;

use crate::{evaluator, lexer, object::Environment, parser};

pub fn start(stdin: std::io::Stdin, mut out: std::io::Stdout) {
    println!("{MONKEY_FACE}");
    let env = Environment::new().to_rc();

    loop {
        out.write_all(PROMPT.as_bytes()).unwrap();
        out.flush().unwrap();

        let mut line = String::new();
        stdin.read_line(&mut line).expect("Failed to read line");

        let lexer = lexer::Lexer::new(line);
        let mut parser = parser::Parser::new(lexer);

        match parser.parse_program() {
            Ok(program) => {
                let evaluated = evaluator::eval(&program, env.clone());
                println!("{}", evaluated.inspect());
            }
            Err(error) => eprintln!("{}", error),
        }
    }
}
