use std::{collections::HashMap, error::Error};

use crate::{
    code::{read_u16, Instructions, Opcode},
    compiler::Bytecode,
    object::{Object, FALSE, NULL, TRUE},
};

const STACK_SIZE: usize = 1 << 11;
const GLOBAL_SIZE: usize = 1 << 16;

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
    globals: Vec<Object>,
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
                    self.pop_cloned();
                }
                Opcode::True => {
                    self.push(TRUE)?;
                }
                Opcode::False => {
                    self.push(FALSE)?;
                }
                Opcode::Null => {
                    self.push(NULL)?;
                }
                Opcode::Equal | Opcode::NotEqual | Opcode::GreaterThan => {
                    self.execute_comparison(op)?
                }
                Opcode::Bang => {
                    let operand = self.pop().unwrap();
                    let result = Object::from(!operand.is_truthy());
                    self.push(result)?;
                }
                Opcode::Minus => match self.pop().unwrap() {
                    Object::Integer(n) => self.push(Object::from(-n))?,
                    _ => return Err(RuntimeError::InvalidOperation),
                },
                Opcode::JumpNotTruthy => {
                    let is_truthy = self.pop().map_or(false, |obj| obj.is_truthy());
                    if !is_truthy {
                        let jump_offset = read_u16(&self.instructions.0[ip + 1..]);
                        ip = jump_offset as usize - 1;
                    } else {
                        ip += 2;
                    }
                }
                Opcode::Jump => {
                    let jump_offset = read_u16(&self.instructions.0[ip + 1..]);
                    ip = jump_offset as usize - 1;
                }
                Opcode::SetGlobal => {
                    let global_index = read_u16(&self.instructions.0[ip + 1..]);
                    ip += 2;
                    let value = self.pop().unwrap();
                    self.globals[global_index as usize] = value;
                }
                Opcode::GetGlobal => {
                    let global_index = read_u16(&self.instructions.0[ip + 1..]);
                    ip += 2;
                    let value = self.globals[global_index as usize].clone();
                    self.push(value)?;
                }
                Opcode::Array => {
                    let elements_count = read_u16(&self.instructions.0[ip + 1..]) as usize;
                    ip += 2;
                    // 前面的 elements_count 个元素就是数组元素
                    let mut array = vec![NULL; elements_count];
                    for i in 0..elements_count {
                        array[elements_count - 1 - i] = self.pop().unwrap();
                    }
                    self.push(Object::Array(array))?;
                }
                Opcode::Dictionary => {
                    let elements_count = read_u16(&self.instructions.0[ip + 1..]) as usize;
                    ip += 2;
                    let mut dict = HashMap::new();
                    for _ in 0..elements_count / 2 {
                        let value = self.pop().unwrap();
                        let key = self.pop().unwrap();
                        dict.insert(key, value);
                    }
                    self.push(Object::Dict(dict))?;
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
            (Object::String(a), Object::String(b)) => {
                self.execute_binary_string_operation(op, a, b)
            }
            _ => Err(RuntimeError::MismachedTypes),
        }
    }

    fn execute_comparison(&mut self, op: Opcode) -> Result<(), RuntimeError> {
        let right = self.pop().unwrap();
        let left = self.pop().unwrap();
        match (left, right) {
            (Object::Integer(left), Object::Integer(right)) => {
                let result = match op {
                    Opcode::Equal => left == right,
                    Opcode::NotEqual => left != right,
                    Opcode::GreaterThan => left > right,
                    _ => return Err(RuntimeError::InvalidOperation),
                };
                self.push(Object::from(result))
            }
            (Object::Boolean(left), Object::Boolean(right)) => match op {
                Opcode::Equal => self.push(Object::from(left == right)),
                Opcode::NotEqual => self.push(Object::from(left != right)),
                _ => Err(RuntimeError::InvalidOperation),
            },
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
        self.push(result.into())?;
        Ok(())
    }

    fn execute_binary_string_operation(
        &mut self,
        op: Opcode,
        left: String,
        right: String,
    ) -> Result<(), RuntimeError> {
        match op {
            Opcode::Add => self.push(Object::from(left + &right)),
            _ => Err(RuntimeError::InvalidOperation),
        }
    }

    pub fn push(&mut self, obj: Object) -> Result<(), RuntimeError> {
        if self.sp >= STACK_SIZE {
            return Err(RuntimeError::StackOverflow);
        }
        self.stack[self.sp] = Some(obj);
        self.sp += 1;
        Ok(())
    }

    pub fn pop_cloned(&mut self) -> Option<Object> {
        if self.sp == 0 {
            return None;
        }
        self.sp -= 1;
        self.stack[self.sp].clone()
    }

    pub fn pop(&mut self) -> Option<Object> {
        if self.sp == 0 {
            return None;
        }
        self.sp -= 1;
        self.stack[self.sp].take()
    }

    pub fn last_popped_elem(&self) -> Option<&Object> {
        self.stack[self.sp].as_ref()
    }

    pub fn read_bytecode(&mut self, bytecode: Bytecode) {
        self.constants = bytecode.constants;
        self.instructions = bytecode.instructions;
    }

    pub fn new() -> Self {
        VM {
            constants: vec![],
            instructions: Instructions(vec![]),
            stack: vec![None; STACK_SIZE],
            sp: 0,
            globals: vec![Object::Null; GLOBAL_SIZE],
        }
    }
}

#[cfg(test)]
mod tests {
    use std::collections::HashMap;

    use crate::{
        compiler::Compiler,
        test_utils::{object::test_object, Value},
    };

    use super::VM;

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

    impl From<(&str, bool)> for VMTestCase {
        fn from(t: (&str, bool)) -> Self {
            Self {
                input: t.0.to_string(),
                expected: Value::Boolean(t.1),
            }
        }
    }

    impl From<(&str, Value)> for VMTestCase {
        fn from(t: (&str, Value)) -> Self {
            Self {
                input: t.0.to_string(),
                expected: t.1,
            }
        }
    }

    fn run_vm_tests(tests: &[VMTestCase]) {
        for VMTestCase { input, expected } in tests {
            let program = crate::test_utils::parse_program(&input);

            let mut comp = Compiler::new();
            comp.compile(&program).expect("compile error");
            let mut vm = VM::new();
            vm.read_bytecode(comp.byte_code());
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
            ("-5", -5),
            ("-10 + 15", 5),
            ("-50 + 100 + -50", 0),
            ("5 * -5", -25),
            ("(5 + 10 * 2 + 15 / 3) * 2 + -10", 50),
        ]
        .into_iter()
        .map(VMTestCase::from)
        .collect::<Vec<_>>();
        run_vm_tests(&tests);
    }

    #[test]
    fn test_boolean_expression() {
        let tests = [
            ("true", true),
            ("false", false),
            ("1 < 2", true),
            ("1 > 2", false),
            ("1 < 1", false),
            ("1 > 1", false),
            ("1 == 1", true),
            ("1 != 1", false),
            ("1 == 2", false),
            ("1 != 2", true),
            ("true == true", true),
            ("false == false", true),
            ("true == false", false),
            ("(1 < 2) == true", true),
            ("(1 < 2) == false", false),
            ("(1 > 2) == true", false),
            ("(1 > 2) == false", true),
            ("!true", false),
            ("!false", true),
            ("!5", false),
            ("!!true", true),
            ("!!false", false),
            ("!!5", true),
            ("!(if (false) { 10; })", true),
            // ("true && true", true),
            // ("true && false", false),
        ]
        .into_iter()
        .map(VMTestCase::from)
        .collect::<Vec<_>>();
        run_vm_tests(&tests);
    }

    #[test]
    fn test_conditionals() {
        let tests = [
            ("if (true) { 10 }", 10.into()),
            ("if (true) { 10 } else { 20 }", 10.into()),
            ("if (false) { 10 } else { 20 }", 20.into()),
            ("if (1) { 10 }", 10.into()),
            ("if (1 < 2) { 10 }", 10.into()),
            ("if (1 < 2) { 10 } else { 20 }", 10.into()),
            ("if (1 > 2) { 10 } else { 20 }", 20.into()),
            ("if (1 > 2) { 10 }", Value::Null),
            ("if (false) { 10 }", Value::Null),
            ("if ((if (false) { 10 })) { 10 } else { 20 }", 20.into()),
        ]
        .into_iter()
        .map(VMTestCase::from)
        .collect::<Vec<_>>();
        run_vm_tests(&tests);
    }

    #[test]
    fn test_global_let_statements() {
        let tests = [
            ("let one = 1; one", 1),
            ("let one = 1; let two = 2; one + two", 3),
            ("let one = 1; let two = one + one; one + two", 3),
        ]
        .into_iter()
        .map(VMTestCase::from)
        .collect::<Vec<_>>();

        run_vm_tests(&tests);
    }

    #[test]
    fn test_string_expressions() {
        let tests = [
            ("\"monkey\"", Value::from("monkey")),
            ("\"mon\" + \"key\"", "monkey".into()),
            ("\"mon\" + \"key\" + \"banana\"", "monkeybanana".into()),
        ]
        .into_iter()
        .map(VMTestCase::from)
        .collect::<Vec<_>>();

        run_vm_tests(&tests);
    }

    #[test]
    fn test_array_literals() {
        let tests = [
            ("[]", Value::Array(vec![])),
            (
                "[1, 2, 3]",
                Value::Array(vec![1.into(), 2.into(), 3.into()]),
            ),
            (
                "[1 + 2, 3 * 4, 5 + 6]",
                Value::Array(vec![3.into(), 12.into(), 11.into()]),
            ),
        ]
        .into_iter()
        .map(VMTestCase::from)
        .collect::<Vec<_>>();

        run_vm_tests(&tests);
    }

    #[test]
    fn test_dict_literals() {
        let tests = [
            ("{}", Value::Dict(HashMap::new())),
            (
                "{1: 2, 2: 3}",
                Value::Dict(HashMap::from([(1.into(), 2.into()), (2.into(), 3.into())])),
            ),
            (
                "{1 + 1: 2 * 2, 3 + 3: 4 * 4}",
                Value::Dict(HashMap::from([(2.into(), 4.into()), (6.into(), 16.into())])),
            ),
        ]
        .into_iter()
        .map(VMTestCase::from)
        .collect::<Vec<_>>();
        run_vm_tests(&tests);
    }
}
