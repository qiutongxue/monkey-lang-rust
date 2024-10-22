use std::{collections::HashMap, fmt::Display};

use crate::{ast::Node, token::Token};

use super::ExpressionEnum;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct DictLiteral {
    pub token: Token,
    pub pairs: HashMap<ExpressionEnum, ExpressionEnum>,
}

impl std::hash::Hash for DictLiteral {
    fn hash<H: std::hash::Hasher>(&self, hasher: &mut H) {
        self.token.hash(hasher);
        for (key, value) in self.pairs.iter() {
            key.hash(hasher);
            value.hash(hasher);
        }
    }
}

impl Node for DictLiteral {
    fn token_literal(&self) -> String {
        self.token.literal.clone()
    }
}

impl Display for DictLiteral {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut pairs = Vec::with_capacity(self.pairs.len());
        for (key, value) in self.pairs.iter() {
            pairs.push(format!("{}: {}", key, value));
        }
        write!(f, "{{ {} }}", pairs.join(", "))
    }
}
