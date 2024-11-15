use std::collections::HashMap;

use super::CompileError;

const GLOBAL_SCOPE: &str = "GLOBAL";

type SymbolScope = String;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Symbol {
    name: String,
    scope: SymbolScope,
    pub index: i32,
}

#[derive(Debug)]
pub struct SymbolTable {
    store: HashMap<String, Symbol>,
    num_definitions: i32,
}

impl SymbolTable {
    pub fn new() -> Self {
        Self {
            store: HashMap::new(),
            num_definitions: 0,
        }
    }

    pub fn define(&mut self, name: &str) -> &Symbol {
        let symbol = Symbol {
            name: name.to_string(),
            scope: GLOBAL_SCOPE.to_string(),
            index: self.num_definitions,
        };
        self.store.insert(name.to_string(), symbol);
        self.num_definitions += 1;
        self.store.get(name).unwrap()
    }

    pub fn resolve(&self, name: &str) -> Result<&Symbol, CompileError> {
        match self.store.get(name) {
            Some(symbol) => Ok(symbol),
            None => Err(CompileError::IdentifierNotDefined(name.to_string())),
        }
    }
}

#[cfg(test)]
mod tests {
    use std::collections::HashMap;

    use super::{Symbol, SymbolTable, GLOBAL_SCOPE};

    #[test]
    fn test_define() {
        let expected = HashMap::from([
            (
                "a".to_string(),
                Symbol {
                    name: "a".to_string(),
                    scope: GLOBAL_SCOPE.to_string(),
                    index: 0,
                },
            ),
            (
                "b".to_string(),
                Symbol {
                    name: "b".to_string(),
                    scope: GLOBAL_SCOPE.to_string(),
                    index: 1,
                },
            ),
        ]);
        let mut global = SymbolTable::new();
        let a = global.define("a");
        assert_eq!(
            a,
            expected.get("a").unwrap(),
            "expected a={:?}, got={:?}",
            expected.get("a").unwrap(),
            a
        );
        let b = global.define("b");
        assert_eq!(
            b,
            expected.get("b").unwrap(),
            "expected b={:?}, got={:?}",
            expected.get("b").unwrap(),
            b
        );
    }

    #[test]
    fn test_resolve_global() {
        let mut global = SymbolTable::new();
        global.define("a");
        global.define("b");

        let expected = vec![
            Symbol {
                name: "a".to_string(),
                scope: GLOBAL_SCOPE.to_string(),
                index: 0,
            },
            Symbol {
                name: "b".to_string(),
                scope: GLOBAL_SCOPE.to_string(),
                index: 1,
            },
        ];
        for sym in expected {
            let result = global.resolve(&sym.name).expect("symbol not found");
            assert_eq!(result, &sym, "expected {:?}, got {:?}", sym, result);
        }
    }
}
