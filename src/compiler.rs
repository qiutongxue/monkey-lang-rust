use std::fmt::Display;

use crate::{
    ast::{ExpressionEnum, Program, StatementEnum},
    code::{make, Instructions, Opcode},
    object::Object,
};

#[derive(Debug)]
pub enum CompileError {
    UnexpectedError,
    UnknownOperator(String),
}

impl Display for CompileError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            CompileError::UnexpectedError => write!(f, "unexpected error"),
            CompileError::UnknownOperator(op) => write!(f, "unknown operator: {}", op),
        }
    }
}

#[derive(Debug)]
pub struct Compiler {
    instructions: Instructions,
    constants: Vec<Object>,
}

impl Compiler {
    pub fn new() -> Self {
        Self {
            instructions: Instructions(vec![]),
            constants: vec![],
        }
    }

    pub fn compile(&mut self, program: &Program) -> Result<(), CompileError> {
        for statement in &program.statements {
            self.complie_stmt(statement)?;
        }
        Ok(())
    }

    /// 添加到常量池
    ///
    /// 返回该对象在常量池的索引
    pub fn add_constant(&mut self, obj: Object) -> i32 {
        self.constants.push(obj);
        self.constants.len() as i32 - 1
    }

    fn emit(&mut self, op: Opcode, operands: &[i32]) -> i32 {
        let ins = make(op, operands);
        let pos = self.add_instruction(ins);
        pos
    }

    fn add_instruction(&mut self, ins: Vec<u8>) -> i32 {
        let pos_new_instruction = self.instructions.0.len();
        self.instructions.0.extend(ins);
        pos_new_instruction as _
    }

    fn complie_stmt(&mut self, stmt: &StatementEnum) -> Result<(), CompileError> {
        match stmt {
            StatementEnum::LetStatement(_) => todo!(),
            StatementEnum::ReturnStatement(_) => todo!(),
            StatementEnum::ExpressionStatement(expr_stmt) => {
                self.compile_expr(&expr_stmt.expression)?;
                // 每次用完表达式，把值弹出栈（因为已经没用了）
                self.emit(Opcode::Pop, &[]);
                Ok(())
            }
        }
    }

    fn compile_expr(&mut self, expr: &ExpressionEnum) -> Result<(), CompileError> {
        match expr {
            ExpressionEnum::IntegerLiteral(expr) => {
                let integer = Object::Integer(expr.value);
                // 存 integer 在常量池的地址，而不是值
                let index = self.add_constant(integer);
                self.emit(Opcode::Constant, &[index]);
                Ok(())
            }
            ExpressionEnum::InfixExpression(expr) => {
                self.compile_expr(&expr.left)?;
                self.compile_expr(&expr.right)?;
                match expr.operator.as_str() {
                    "+" => {
                        self.emit(Opcode::Add, &[]);
                    }
                    _ => return Err(CompileError::UnknownOperator(expr.operator.clone())),
                }
                Ok(())
            }
            ExpressionEnum::Identifier(_) => todo!(),
            ExpressionEnum::PrefixExpression(_) => todo!(),
            _ => unreachable!(),
        }
    }

    pub fn byte_code(self) -> Bytecode {
        Bytecode {
            instructions: self.instructions,
            constants: self.constants,
        }
    }
}

impl Default for Compiler {
    fn default() -> Self {
        Self::new()
    }
}

#[derive(Debug)]
pub struct Bytecode {
    pub instructions: Instructions,
    pub constants: Vec<Object>,
}

#[cfg(test)]
mod tests {
    use crate::{
        code::{make, Instructions, Opcode},
        object::Object,
        test_utils::{object::test_object, parse_program, Value},
    };

    use super::Compiler;

    struct CompilerTestCase {
        input: &'static str,
        expected_constants: Vec<Value>,
        expected_instructions: Vec<Instructions>,
    }

    impl From<(&'static str, Vec<Value>, Vec<Vec<u8>>)> for CompilerTestCase {
        fn from(value: (&'static str, Vec<Value>, Vec<Vec<u8>>)) -> Self {
            Self {
                input: value.0,
                expected_constants: value.1,
                expected_instructions: value.2.into_iter().map(|s| s.into()).collect(),
            }
        }
    }

    fn run_compiler_test(tests: &[CompilerTestCase]) {
        for CompilerTestCase {
            input,
            expected_constants,
            expected_instructions,
        } in tests
        {
            let program = parse_program(input);
            let mut compiler = Compiler::new();
            compiler.compile(&program).unwrap();

            let bytecode = compiler.byte_code();
            test_instructions(expected_instructions, &bytecode.instructions);
            test_constants(expected_constants, &bytecode.constants);
        }
    }

    fn test_instructions(expected: &[Instructions], actual: &Instructions) {
        let expected = expected
            .iter()
            .flat_map(|s| &s.0)
            .copied()
            .collect::<Vec<_>>();
        assert_eq!(expected, actual.0);
    }

    fn test_constants(expected: &[Value], actual: &Vec<Object>) {
        assert_eq!(
            expected.len(),
            actual.len(),
            "wrong number of constants. got={}, want={}",
            actual.len(),
            expected.len()
        );
        for (expected, actual) in expected.iter().zip(actual.iter()) {
            test_object(actual, expected)
        }
    }

    #[test]
    fn test_integer_arithmetic() {
        let tests: Vec<CompilerTestCase> = vec![
            (
                "1 + 2",
                vec![1.into(), 2.into()],
                vec![
                    make(Opcode::Constant, &[0]),
                    make(Opcode::Constant, &[1]),
                    make(Opcode::Add, &[]),
                    make(Opcode::Pop, &[]),
                ],
            ),
            (
                "1; 2",
                vec![1.into(), 2.into()],
                vec![
                    make(Opcode::Constant, &[0]),
                    make(Opcode::Pop, &[]),
                    make(Opcode::Constant, &[1]),
                    make(Opcode::Pop, &[]),
                ],
            ),
        ]
        .into_iter()
        .map(|t| t.into())
        .collect();

        run_compiler_test(&tests);
    }

    #[test]
    fn test_instructions_string() {
        let instructions = [
            make(Opcode::Add, &[]),
            make(Opcode::Constant, &[2]),
            make(Opcode::Constant, &[65535]),
        ];

        let expected = r#"0000 Add
0001 Constant 2
0004 Constant 65535
"#;

        let instructions = Instructions(instructions.iter().flatten().copied().collect::<Vec<_>>());
        assert_eq!(expected, format!("{}", instructions));
    }
}
