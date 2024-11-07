use std::{collections::HashMap, error::Error, fmt::Display, sync::LazyLock};

#[derive(Debug, Clone)]
pub struct Instructions(pub Vec<u8>);

impl Display for Instructions {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        // <address> <instruction> <operands>
        // 0006 OpConstant 65535
        let mut i = 0;
        while i < self.0.len() {
            match lookup(self.0[i]) {
                Ok(def) => {
                    let (operands, read) = read_operands(def, &self.0[i + 1..]);
                    writeln!(f, "{:04} {}", i, fmt_instruction(def, &operands))?;
                    i += read + 1;
                }
                Err(e) => {
                    return writeln!(f, "{:04} ERROR: {}", i, e);
                }
            }
        }
        Ok(())
    }
}

fn fmt_instruction(def: &Definition, operands: &[i32]) -> String {
    let operand_count = def.operand_width.len();

    if operands.len() != operand_count {
        return format!(
            "ERROR: operand len {} does not match defined {}\n",
            operands.len(),
            operand_count
        );
    }

    match operand_count {
        0 => def.name.to_string(),
        1 => format!("{} {}", def.name, operands[0]),
        _ => format!("ERROR: unhandled operand count for {}\n", def.name),
    }
}

impl From<Vec<u8>> for Instructions {
    fn from(v: Vec<u8>) -> Self {
        Self(v)
    }
}

macro_rules! define_opcode {
    ($(
        $opcode:ident = $value:expr, $name:expr, $width:expr,    )*) => {
        #[repr(u8)]
        #[derive(Debug, PartialEq, Eq, Hash, Clone, Copy)]
        pub enum Opcode {
            $($opcode = $value,)*
        }

        impl TryFrom<u8> for Opcode {
            type Error = OpcodeError;

            fn try_from(value: u8) -> Result<Self, Self::Error> {
                match value {
                    $($value => Ok(Opcode::$opcode),)*
                    _ => Err(OpcodeError::InvalidOpcode(value)),
                }
            }
        }

        static DEFINITIONS: LazyLock<HashMap<Opcode, Definition>> = LazyLock::new(|| {
            HashMap::from([
                $((
                    Opcode::$opcode,
                    Definition {
                        name: $name,
                        operand_width: $width,
                    },
                ),)*
            ])
        });
    };
}

define_opcode! {
    Constant = 0, "Constant", vec![2],
    Add = 1, "Add", vec![],
    Pop = 2, "Pop", vec![],
    Sub = 3, "Sub", vec![],
    Mul = 4, "Mul", vec![],
    Div = 5, "Div", vec![],
    True = 6, "True", vec![],
    False = 7, "False", vec![],
    Equal = 8, "Equal", vec![],
    NotEqual = 9, "NotEqual", vec![],
    GreaterThan = 10, "GreaterThan", vec![],
    Minus = 11, "Minus", vec![],
    Bang   = 12, "Bang", vec![],
}

#[derive(Debug)]
pub enum OpcodeError {
    InvalidOpcode(u8),
    UnexpectedError,
}

impl Display for OpcodeError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            OpcodeError::InvalidOpcode(op) => write!(f, "Invalid opcode: {}", op),
            OpcodeError::UnexpectedError => write!(f, "Unexpected error"),
        }
    }
}

impl Error for OpcodeError {}

#[derive(Debug, Clone)]
pub struct Definition {
    name: &'static str,
    /// 操作数的长度，为什么是 Vec 呢，因为操作数可能不止一个
    operand_width: Vec<usize>,
}

pub fn lookup<'a>(op: u8) -> Result<&'a Definition, OpcodeError> {
    let opcode = op.try_into()?;
    DEFINITIONS.get(&opcode).ok_or(OpcodeError::UnexpectedError)
}

/// 生成指令
pub fn make(op: Opcode, operands: &[i32]) -> Vec<u8> {
    DEFINITIONS
        .get(&op)
        .map(|def| {
            let mut instruction_length = 1; // 初始操作码长度为 1
            for &w in &def.operand_width {
                instruction_length += w; // 操作数长度加上操作码长度
            }
            let mut instruction = vec![0; instruction_length];
            instruction[0] = op as u8;

            let mut offset = 1;
            for (i, o) in operands.iter().enumerate() {
                let width = def.operand_width[i];
                match width {
                    2 => write_u16(&mut instruction[offset..], *o as u16),
                    _ => unimplemented!(),
                }
                offset += width;
            }
            instruction
        })
        .expect("missing definition for opcode")
}

pub fn read_operands(def: &Definition, instruction: &[u8]) -> (Vec<i32>, usize) {
    let mut offset = 0;
    let mut operands = vec![0; def.operand_width.len()];
    for (i, width) in def.operand_width.iter().enumerate() {
        match width {
            2 => {
                operands[i] = read_u16(&instruction[offset..]) as i32;
            }
            _ => unimplemented!(),
        }
        offset += width;
    }

    (operands, offset)
}

fn write_u16(buf: &mut [u8], value: u16) {
    buf[..2].copy_from_slice(&value.to_be_bytes());
}

pub fn read_u16(buf: &[u8]) -> u16 {
    u16::from_be_bytes(buf[..2].try_into().unwrap())
}

#[cfg(test)]
mod tests {
    use super::{lookup, make, read_operands, Opcode};

    #[test]
    fn test_make() {
        let tests = [
            (
                Opcode::Constant,
                vec![65534],
                vec![Opcode::Constant as u8, 0xff, 0xfe],
            ),
            (Opcode::Add, vec![], vec![Opcode::Add as u8]),
            (Opcode::Pop, vec![], vec![Opcode::Pop as u8]),
            (Opcode::Sub, vec![], vec![Opcode::Sub as u8]),
            (Opcode::Mul, vec![], vec![Opcode::Mul as u8]),
            (Opcode::Div, vec![], vec![Opcode::Div as u8]),
        ];
        for (op, operands, expected) in tests {
            let instruction = make(op, &operands);
            assert_eq!(
                instruction.len(),
                expected.len(),
                "instruction has wrong length. want={}, got={}",
                expected.len(),
                instruction.len()
            );

            for (i, b) in expected.iter().enumerate() {
                assert_eq!(
                    *b, expected[i],
                    "instruction has wrong byte at index {}. want={}, got={}",
                    i, b, instruction[i]
                );
            }
        }
    }

    #[test]
    fn test_read_operands() {
        let tests = [(Opcode::Constant, vec![65534], 2)];

        for (op, operands, bytes_read) in tests {
            let instruction = make(op, &operands);
            match lookup(instruction[0]) {
                Ok(def) => {
                    let (operands_read, n) = read_operands(def, &instruction[1..]);
                    assert_eq!(n, bytes_read, "n wrong. got={}, want={}", n, bytes_read);
                    for (i, want) in operands.iter().enumerate() {
                        assert_eq!(
                            operands_read[i], *want,
                            "operand wrong. got={}, want={}",
                            operands_read[i], *want
                        )
                    }
                }
                Err(e) => panic!("{}", e),
            }
        }
    }
}
