use monkey_lang_rust::repl::{start, Engine};

use std::io;

fn main() {
    start(io::stdin(), io::stdout(), Engine::VM);
}
