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

use crate::{
    ast::Program,
    compiler::Compiler,
    evaluator, lexer,
    object::Environment,
    parser::{self, ParseError},
    vm,
};

pub enum Engine {
    Eval,
    VM,
}

pub fn start(stdin: std::io::Stdin, out: std::io::Stdout, engine: Engine) {
    println!("{MONKEY_FACE}");
    match engine {
        Engine::Eval => start_eval(stdin, out),
        Engine::VM => start_vm(stdin, out),
    }
}

fn start_eval(stdin: std::io::Stdin, mut out: std::io::Stdout) {
    let env = Environment::new().to_rc();

    loop {
        match read_and_parse(&stdin, &mut out) {
            Ok(program) => {
                let evaluated = evaluator::eval(&program, env.clone());
                println!("{}", evaluated.inspect());
            }
            Err(error) => eprintln!("{}", error),
        }
    }
}

fn start_vm(stdin: std::io::Stdin, mut out: std::io::Stdout) {
    loop {
        match read_and_parse(&stdin, &mut out) {
            Ok(program) => {
                // FIXME: 全局变量/环境需要绑定
                let mut comp = Compiler::new();
                comp.compile(&program).expect("Failed to compile program");
                let mut vm = vm::new(comp.byte_code());
                vm.run().expect("execute bytecode failed");

                let stack_top = vm.last_popped_elem().unwrap();
                println!("{}", stack_top.inspect())
            }
            Err(error) => eprintln!("{}", error),
        }
    }
}

fn read_and_parse(
    stdin: &std::io::Stdin,
    out: &mut std::io::Stdout,
) -> Result<Program, ParseError> {
    out.write_all(PROMPT.as_bytes()).unwrap();
    out.flush().unwrap();

    let mut line = String::new();
    stdin.read_line(&mut line).expect("Failed to read line");

    let lexer = lexer::Lexer::new(line);
    let mut parser = parser::Parser::new(lexer);
    parser.parse_program()
}
