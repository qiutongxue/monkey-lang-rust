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

use crate::{compiler::Compiler, evaluator, lexer, object::Environment, parser, vm};

pub enum Engine {
    Eval,
    VM,
}

pub fn start(stdin: std::io::Stdin, mut out: std::io::Stdout, engine: Engine) {
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
            Ok(program) => match engine {
                Engine::Eval => {
                    let evaluated = evaluator::eval(&program, env.clone());
                    println!("{}", evaluated.inspect());
                }
                Engine::VM => {
                    let mut comp = Compiler::new();
                    comp.compile(&program).expect("Failed to compile program");
                    let mut vm = vm::new(comp.byte_code());
                    vm.run().expect("execute bytecode failed");

                    let stack_top = vm.stack_top().unwrap();
                    println!("{}", stack_top.inspect())
                }
            },
            Err(error) => eprintln!("{}", error),
        }
    }
}
