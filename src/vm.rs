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
    stack: Vec<Option<Object>>,
    sp: usize,
}

impl VM {
    pub fn stack_top(&self) -> Option<&Object> {
        self.stack.last().and_then(|x| x.as_ref())
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
                Opcode::Add | Opcode::Sub | Opcode::Mul | Opcode::Div => {
                    self.execute_binary_operation(op)?
                }
                Opcode::Pop => {
                    self.pop();
                }
            }
            ip += 1;
        }
        Ok(())
    }

    fn execute_binary_operation(&mut self, op: Opcode) -> Result<(), RuntimeError> {
        let b = self.pop().unwrap();
        let a = self.pop().unwrap();

        match (a, b) {
            (Object::Integer(a), Object::Integer(b)) => {
                self.execute_binary_integer_operation(op, a, b)
            }
            _ => Err(RuntimeError::MismachedTypes),
        }
    }

    fn execute_binary_integer_operation(
        &mut self,
        op: Opcode,
        left: i64,
        right: i64,
    ) -> Result<(), RuntimeError> {
        let result = match op {
            Opcode::Add => left + right,
            Opcode::Sub => left - right,
            Opcode::Mul => left * right,
            Opcode::Div => left / right,
            _ => return Err(RuntimeError::InvalidOperation),
        };
        self.push(Object::Integer(result))?;
        Ok(())
    }

    pub fn push(&mut self, obj: Object) -> Result<(), RuntimeError> {
        if self.sp >= STACK_SIZE {
            return Err(RuntimeError::StackOverflow);
        }
        self.stack[self.sp] = Some(obj);
        self.sp += 1;
        Ok(())
    }

    pub fn pop(&mut self) -> Option<Object> {
        if self.sp == 0 {
            return None;
        }
        self.sp -= 1;
        self.stack[self.sp].clone()
    }

    pub fn last_popped_elem(&self) -> Option<&Object> {
        self.stack[self.sp].as_ref()
    }
}

pub fn new(bytecode: Bytecode) -> VM {
    VM {
        constants: bytecode.constants,
        instructions: bytecode.instructions,
        stack: vec![None; STACK_SIZE],
        sp: 0,
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

            let stack_elem = vm.last_popped_elem().expect("empty stack");

            test_object(stack_elem, expected);
        }
    }

    #[test]
    fn test_integer_arithmetic() {
        let tests = [
            ("1", 1),
            ("2", 2),
            ("1 + 2", 3),
            ("1 - 2", -1),
            ("2 * 3", 6),
            ("4 / 2", 2),
            ("50 / 2 * 2 + 10 - 5", 55),
            ("5 + 5 + 5 + 5 - 10", 10),
            ("2 * 3 + 4 * 5", 26),
            ("5 / 2 * 2 + 1", 5),
            ("5 * 2 + 10 / 2", 15),
            ("5 * (2 + 10) / 2", 30),
        ]
        .into_iter()
        .map(VMTestCase::from)
        .collect::<Vec<_>>();
        run_vm_tests(&tests);
    }
}
