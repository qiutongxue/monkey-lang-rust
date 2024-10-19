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

use crate::{ast::NodeEnum, evaluator, lexer, object::Environment, parser};

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

        if let Ok(program) = parser.parse_program() {
            if parser.errors().len() > 0 {
                print_parse_errors(parser.errors());
                continue;
            }

            let evaluated = evaluator::eval(NodeEnum::Program(program), env.clone());

            println!("{}", evaluated.inspect()); // println!("{}", program.to_string());
        }
    }
}

fn print_parse_errors(errors: &Vec<String>) {
    eprintln!("Woops! We ran into some monkey business here!\n");
    eprintln!(" parser errors:");
    for error in errors {
        eprintln!("\t{}", error);
    }
}
