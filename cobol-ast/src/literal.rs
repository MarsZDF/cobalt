/// COBOL literal values.
#[derive(Debug, Clone, PartialEq)]
#[cfg_attr(feature = "serialize", derive(serde::Serialize, serde::Deserialize))]
pub enum Literal {
    /// String literal (quoted)
    String(String),

    /// Numeric literal (integer or decimal)
    Numeric(NumericLiteral),

    /// Boolean literal
    Boolean(bool),
}

/// Numeric literal value.
#[derive(Debug, Clone, PartialEq)]
#[cfg_attr(feature = "serialize", derive(serde::Serialize, serde::Deserialize))]
pub struct NumericLiteral {
    pub value: f64,
    pub is_integer: bool,
}

impl NumericLiteral {
    pub fn integer(value: i64) -> Self {
        Self {
            value: value as f64,
            is_integer: true,
        }
    }

    pub fn float(value: f64) -> Self {
        Self {
            value,
            is_integer: false,
        }
    }
}

impl From<i64> for NumericLiteral {
    fn from(value: i64) -> Self {
        Self::integer(value)
    }
}

impl From<f64> for NumericLiteral {
    fn from(value: f64) -> Self {
        Self::float(value)
    }
}

impl From<i32> for NumericLiteral {
    fn from(value: i32) -> Self {
        Self::integer(value as i64)
    }
}

impl From<u64> for NumericLiteral {
    fn from(value: u64) -> Self {
        Self::integer(value as i64)
    }
}
