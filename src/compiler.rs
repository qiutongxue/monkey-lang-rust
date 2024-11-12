use std::fmt::Display;

use crate::{
    ast::{BlockStatement, ExpressionEnum, Program, StatementEnum},
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
    last_instruction: Option<EmittedInstruction>,
    previous_instruction: Option<EmittedInstruction>,
}

impl Compiler {
    pub fn new() -> Self {
        Self {
            instructions: Instructions(vec![]),
            constants: vec![],
            last_instruction: None,
            previous_instruction: None,
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

    fn emit(&mut self, op: Opcode, operands: &[i32]) -> usize {
        let ins = make(op, operands);
        let pos = self.add_instruction(ins);

        self.set_last_instruction(op, pos);
        pos
    }

    fn set_last_instruction(&mut self, op: Opcode, pos: usize) {
        let previous = self.last_instruction.take();
        let last = EmittedInstruction { opcode: op, pos };
        self.previous_instruction = previous;
        self.last_instruction = Some(last);
    }

    fn add_instruction(&mut self, ins: Vec<u8>) -> usize {
        let pos_new_instruction = self.instructions.0.len();
        self.instructions.0.extend(ins);
        pos_new_instruction
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
            ExpressionEnum::PrefixExpression(expr) => {
                self.compile_expr(&expr.right)?;
                match expr.operator.as_str() {
                    "!" => {
                        self.emit(Opcode::Bang, &[]);
                    }
                    "-" => {
                        self.emit(Opcode::Minus, &[]);
                    }
                    _ => return Err(CompileError::UnknownOperator(expr.operator.clone())),
                }
                Ok(())
            }
            ExpressionEnum::InfixExpression(expr) => {
                // 翻转
                if expr.operator.as_str() == "<" {
                    self.compile_expr(&expr.right)?;
                    self.compile_expr(&expr.left)?;
                    self.emit(Opcode::GreaterThan, &[]);
                    return Ok(());
                }
                self.compile_expr(&expr.left)?;
                self.compile_expr(&expr.right)?;
                match expr.operator.as_str() {
                    "+" => {
                        self.emit(Opcode::Add, &[]);
                    }
                    "-" => {
                        self.emit(Opcode::Sub, &[]);
                    }
                    "*" => {
                        self.emit(Opcode::Mul, &[]);
                    }
                    "/" => {
                        self.emit(Opcode::Div, &[]);
                    }
                    "==" => {
                        self.emit(Opcode::Equal, &[]);
                    }
                    "!=" => {
                        self.emit(Opcode::NotEqual, &[]);
                    }
                    ">" => {
                        self.emit(Opcode::GreaterThan, &[]);
                    }
                    _ => return Err(CompileError::UnknownOperator(expr.operator.clone())),
                }
                Ok(())
            }
            ExpressionEnum::Boolean(b) => {
                if b.value {
                    self.emit(Opcode::True, &[]);
                } else {
                    self.emit(Opcode::False, &[]);
                }
                Ok(())
            }
            ExpressionEnum::IfExpression(ie) => {
                self.compile_expr(&ie.condition)?;
                let jump_not_truthy_pos = self.emit(Opcode::JumpNotTruthy, &[9999]);
                self.compile_block_stmt(&ie.consequence)?;
                if self.last_instruction_is(Opcode::Pop) {
                    self.remove_last_pop();
                }
                // 这是 consequence 里的 jump，要跳过 alternative
                let jump_pos = self.emit(Opcode::Jump, &[9999]);
                // 这里是 alternative/Null 的起始位置，即 jump_not_truthy 要跳到的地方
                let after_consequence_pos = self.instructions.0.len();
                self.change_operand(jump_not_truthy_pos, after_consequence_pos as i32);

                if let Some(alternative) = &ie.alternative {
                    self.compile_block_stmt(alternative)?;
                    if self.last_instruction_is(Opcode::Pop) {
                        self.remove_last_pop();
                    }
                } else {
                    self.emit(Opcode::Null, &[]);
                }
                // 最后 jump 要跳到 alternative 结束的位置
                let after_alternative_pos = self.instructions.0.len();
                self.change_operand(jump_pos, after_alternative_pos as i32);

                Ok(())
            }
            ExpressionEnum::Identifier(_) => todo!(),
            _ => unreachable!(),
        }
    }

    fn compile_block_stmt(&mut self, block_stmt: &BlockStatement) -> Result<(), CompileError> {
        for stmt in &block_stmt.statements {
            self.complie_stmt(stmt)?;
        }
        Ok(())
    }

    fn last_instruction_is(&self, op: Opcode) -> bool {
        self.last_instruction.as_ref().map(|i| i.opcode) == Some(op)
    }

    fn remove_last_pop(&mut self) {
        let last_instruction = self.last_instruction.take().unwrap();
        self.instructions.0.truncate(last_instruction.pos);
        self.last_instruction = self.previous_instruction;
    }

    fn change_operand(&mut self, pos: usize, operand: i32) {
        let opcode = self.instructions.0[pos];
        let new_ins = make(opcode.try_into().unwrap(), &[operand]);
        self.instructions.0[pos..pos + new_ins.len()].copy_from_slice(&new_ins);
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

#[derive(Debug, Clone, Copy)]
struct EmittedInstruction {
    opcode: Opcode,
    pos: usize,
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
        let expected: Instructions = expected
            .into_iter()
            .flat_map(|s| &s.0)
            .copied()
            .collect::<Vec<_>>()
            .into();
        assert_eq!(expected.to_string(), actual.to_string());
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
            (
                "1 - 2",
                vec![1.into(), 2.into()],
                vec![
                    make(Opcode::Constant, &[0]),
                    make(Opcode::Constant, &[1]),
                    make(Opcode::Sub, &[]),
                    make(Opcode::Pop, &[]),
                ],
            ),
            (
                "1 * 2",
                vec![1.into(), 2.into()],
                vec![
                    make(Opcode::Constant, &[0]),
                    make(Opcode::Constant, &[1]),
                    make(Opcode::Mul, &[]),
                    make(Opcode::Pop, &[]),
                ],
            ),
            (
                "2 / 1",
                vec![2.into(), 1.into()],
                vec![
                    make(Opcode::Constant, &[0]),
                    make(Opcode::Constant, &[1]),
                    make(Opcode::Div, &[]),
                    make(Opcode::Pop, &[]),
                ],
            ),
            (
                "-1",
                vec![1.into()],
                vec![
                    make(Opcode::Constant, &[0]),
                    make(Opcode::Minus, &[]),
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
            make(Opcode::Sub, &[]),
            make(Opcode::Pop, &[]),
            make(Opcode::Mul, &[]),
            make(Opcode::Div, &[]),
        ];

        let expected = r#"0000 Add
0001 Constant 2
0004 Constant 65535
0007 Sub
0008 Pop
0009 Mul
0010 Div
"#;

        let instructions = Instructions(instructions.iter().flatten().copied().collect::<Vec<_>>());
        assert_eq!(expected, format!("{}", instructions));
    }

    #[test]
    fn test_boolean_expression() {
        let tests: Vec<CompilerTestCase> = vec![
            (
                "true",
                vec![],
                vec![make(Opcode::True, &[]), make(Opcode::Pop, &[])],
            ),
            (
                "false",
                vec![],
                vec![make(Opcode::False, &[]), make(Opcode::Pop, &[])],
            ),
            (
                "1 > 2",
                vec![1.into(), 2.into()],
                vec![
                    make(Opcode::Constant, &[0]),
                    make(Opcode::Constant, &[1]),
                    make(Opcode::GreaterThan, &[]),
                    make(Opcode::Pop, &[]),
                ],
            ),
            (
                "1 == 2",
                vec![1.into(), 2.into()],
                vec![
                    make(Opcode::Constant, &[0]),
                    make(Opcode::Constant, &[1]),
                    make(Opcode::Equal, &[]),
                    make(Opcode::Pop, &[]),
                ],
            ),
            (
                "1 != 2",
                vec![1.into(), 2.into()],
                vec![
                    make(Opcode::Constant, &[0]),
                    make(Opcode::Constant, &[1]),
                    make(Opcode::NotEqual, &[]),
                    make(Opcode::Pop, &[]),
                ],
            ),
            (
                "1 < 2",
                vec![2.into(), 1.into()],
                vec![
                    make(Opcode::Constant, &[0]),
                    make(Opcode::Constant, &[1]),
                    make(Opcode::GreaterThan, &[]),
                    make(Opcode::Pop, &[]),
                ],
            ),
            (
                "true == false",
                vec![],
                vec![
                    make(Opcode::True, &[]),
                    make(Opcode::False, &[]),
                    make(Opcode::Equal, &[]),
                    make(Opcode::Pop, &[]),
                ],
            ),
            (
                "true != false",
                vec![],
                vec![
                    make(Opcode::True, &[]),
                    make(Opcode::False, &[]),
                    make(Opcode::NotEqual, &[]),
                    make(Opcode::Pop, &[]),
                ],
            ),
            (
                "!true",
                vec![],
                vec![
                    make(Opcode::True, &[]),
                    make(Opcode::Bang, &[]),
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
    fn test_conditionals() {
        let tests: Vec<CompilerTestCase> = vec![
            (
                "if (true) { 10 }; 3333;",
                vec![10.into(), 3333.into()],
                vec![
                    make(Opcode::True, &[]),            // 0000
                    make(Opcode::JumpNotTruthy, &[10]), // 0001
                    make(Opcode::Constant, &[0]),       // 0004
                    make(Opcode::Jump, &[11]),          // 0007
                    make(Opcode::Null, &[]),            // 0010 alternative 用 null 替换
                    make(Opcode::Pop, &[]),             // 0011 表达式有返回值，语句结束之后要清空
                    make(Opcode::Constant, &[1]),       // 0012
                    make(Opcode::Pop, &[]),             // 0015
                ],
            ),
            (
                "if (true) { 10 } else { 20 }; 3333;",
                vec![10.into(), 20.into(), 3333.into()],
                vec![
                    make(Opcode::True, &[]),            // 0000
                    make(Opcode::JumpNotTruthy, &[10]), // 0001
                    make(Opcode::Constant, &[0]),       // 0004
                    make(Opcode::Jump, &[13]),          // 0007
                    make(Opcode::Constant, &[1]),       // 0010
                    make(Opcode::Pop, &[]), // 0013 if else 整体是一个表达式，只需要在最后 pop 一次
                    make(Opcode::Constant, &[2]), // 0014
                    make(Opcode::Pop, &[]), // 0017
                ],
            ),
        ]
        .into_iter()
        .map(|t| t.into())
        .collect();
        run_compiler_test(&tests);
    }
}
