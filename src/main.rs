use monkey_interpreter_rust::repl::start;

use std::io;

fn main() {
    start(
        io::stdin(),
        io::stdout(),
        monkey_interpreter_rust::repl::Engine::VM,
    );
}
