use std::error::Error;

use crate::{
    code::{read_u16, Instructions, Opcode},
    compiler::Bytecode,
    object::Object,
};

const STACK_SIZE: usize = 2048;

#[derive(Debug)]
pub enum RuntimeError {
    StackOverflow,
    MismachedTypes,
    InvalidOperation,
    InnerError(Box<dyn Error>),
}

#[derive(Debug)]
pub struct VM {
    constants: Vec<Object>,
    instructions: Instructions,

    stack: Vec<Object>,
}

impl VM {
    pub fn stack_top(&self) -> Option<&Object> {
        self.stack.last()
    }

    pub fn run(&mut self) -> Result<(), RuntimeError> {
        let mut ip = 0;
        while ip < self.instructions.0.len() {
            let op = self.instructions.0[ip]
                .try_into()
                .map_err(|e| RuntimeError::InnerError(Box::new(e)))?;
            match op {
                Opcode::Constant => {
                    let const_index = read_u16(&self.instructions.0[ip + 1..]);
                    ip += 2;
                    self.push(self.constants[const_index as usize].clone())?;
                }
                Opcode::Add => {
                    let b = self.pop().unwrap();
                    let a = self.pop().unwrap();
                    let result = match (a, b) {
                        (Object::Integer(a), Object::Integer(b)) => Object::Integer(a + b),
                        _ => return Err(RuntimeError::MismachedTypes),
                    };
                    self.push(result)?;
                }
            }
            ip += 1;
        }
        Ok(())
    }

    pub fn push(&mut self, obj: Object) -> Result<(), RuntimeError> {
        if self.stack.len() > STACK_SIZE {
            return Err(RuntimeError::StackOverflow);
        }
        self.stack.push(obj);
        Ok(())
    }

    pub fn pop(&mut self) -> Option<Object> {
        self.stack.pop()
    }
}

pub fn new(bytecode: Bytecode) -> VM {
    VM {
        constants: bytecode.constants,
        instructions: bytecode.instructions,
        stack: Vec::with_capacity(STACK_SIZE),
    }
}

#[cfg(test)]
mod tests {
    use crate::{
        compiler::Compiler,
        test_utils::{object::test_object, Value},
    };

    use super::new;

    struct VMTestCase {
        input: String,
        expected: Value,
    }

    impl From<(&str, i64)> for VMTestCase {
        fn from(t: (&str, i64)) -> Self {
            Self {
                input: t.0.to_string(),
                expected: Value::Integer(t.1),
            }
        }
    }

    fn run_vm_tests(tests: &[VMTestCase]) {
        for VMTestCase { input, expected } in tests {
            let program = crate::test_utils::parse_program(&input);

            let mut comp = Compiler::new();
            comp.compile(&program).expect("compile error");
            let mut vm = new(comp.byte_code());
            vm.run().expect("run error");

            let stack_elem = vm.stack_top().expect("empty stack");

            test_object(stack_elem, expected);
        }
    }

    #[test]
    fn test_integer_arithmetic() {
        let tests = [("1", 1), ("2", 2), ("1 + 2", 3)]
            .into_iter()
            .map(VMTestCase::from)
            .collect::<Vec<_>>();
        run_vm_tests(&tests);
    }
}
