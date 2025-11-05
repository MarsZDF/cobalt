use crate::literal::Literal;
use crate::span::Spanned;

/// COBOL expression.
#[derive(Debug, Clone, PartialEq)]
#[cfg_attr(feature = "serialize", derive(serde::Serialize, serde::Deserialize))]
pub enum Expression {
    /// Literal value
    Literal(Literal),
    
    /// Identifier (variable reference)
    Identifier(String),
    
    /// Unary expression (-x, +x, NOT x)
    Unary {
        op: UnaryOp,
        operand: Box<Spanned<Expression>>,
    },
    
    /// Binary expression (x + y, x AND y, etc.)
    Binary {
        op: BinaryOp,
        left: Box<Spanned<Expression>>,
        right: Box<Spanned<Expression>>,
    },
    
    /// Function call
    FunctionCall {
        name: String,
        arguments: Vec<Spanned<Expression>>,
    },
    
    /// Subscripted reference (array access)
    Subscripted {
        identifier: String,
        subscripts: Vec<Spanned<Expression>>,
    },
    
    /// Qualified reference (identifier IN/OF identifier)
    Qualified {
        identifier: String,
        qualifier: String,
    },
}

/// Unary operators.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[cfg_attr(feature = "serialize", derive(serde::Serialize, serde::Deserialize))]
pub enum UnaryOp {
    Negate,    // -
    Positive,  // +
    Not,       // NOT
}

/// Binary operators.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[cfg_attr(feature = "serialize", derive(serde::Serialize, serde::Deserialize))]
pub enum BinaryOp {
    // Arithmetic
    Add,      // +
    Subtract, // -
    Multiply, // *
    Divide,   // /
    Power,    // **
    
    // Comparison
    Equal,          // =
    NotEqual,       // <>
    LessThan,       // <
    GreaterThan,    // >
    LessOrEqual,    // <=
    GreaterOrEqual, // >=
    
    // Logical
    And, // AND
    Or,  // OR
    
    // String comparison
    EqualToString,      // equal to (string)
    NotEqualToString,   // not equal to (string)
    LessThanString,     // less than (string)
    GreaterThanString,  // greater than (string)
}

impl BinaryOp {
    /// Check if this is an arithmetic operator.
    pub fn is_arithmetic(self) -> bool {
        matches!(
            self,
            BinaryOp::Add | BinaryOp::Subtract | BinaryOp::Multiply | BinaryOp::Divide | BinaryOp::Power
        )
    }
    
    /// Check if this is a comparison operator.
    pub fn is_comparison(self) -> bool {
        matches!(
            self,
            BinaryOp::Equal
                | BinaryOp::NotEqual
                | BinaryOp::LessThan
                | BinaryOp::GreaterThan
                | BinaryOp::LessOrEqual
                | BinaryOp::GreaterOrEqual
        )
    }
    
    /// Check if this is a logical operator.
    pub fn is_logical(self) -> bool {
        matches!(self, BinaryOp::And | BinaryOp::Or)
    }
    
    /// Check if this is a string comparison operator.
    pub fn is_string_comparison(self) -> bool {
        matches!(
            self,
            BinaryOp::EqualToString
                | BinaryOp::NotEqualToString
                | BinaryOp::LessThanString
                | BinaryOp::GreaterThanString
        )
    }
}
