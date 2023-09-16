use monkey_interpreter_rust::repl::start;

use std::io;
fn main() {
    println!("Hello, world!");
    start(io::stdin());
}
