use crate::literal::Literal;
use crate::span::Spanned;

/// Data division item.
#[derive(Debug, Clone, PartialEq)]
#[cfg_attr(feature = "serialize", derive(serde::Serialize, serde::Deserialize))]
pub struct DataItem {
    pub level: u8,
    pub name: Spanned<String>,
    pub picture: Option<Spanned<Picture>>,
    pub value: Option<Spanned<InitialValue>>,
    pub occurs: Option<Spanned<OccursClause>>,
    pub redefines: Option<Spanned<String>>, // Name of item being redefined
    pub usage: Option<Spanned<Usage>>,
    pub children: Vec<Spanned<DataItem>>,
}

/// PICTURE clause (character string describing data format).
#[derive(Debug, Clone, PartialEq, Eq)]
#[cfg_attr(feature = "serialize", derive(serde::Serialize, serde::Deserialize))]
pub struct Picture {
    pub string: String,
}

impl Picture {
    pub fn new(string: String) -> Self {
        Self { string }
    }
}

/// Initial value clause.
#[derive(Debug, Clone, PartialEq)]
#[cfg_attr(feature = "serialize", derive(serde::Serialize, serde::Deserialize))]
pub enum InitialValue {
    Literal(Literal),
    FigurativeConstant(FigurativeConstant),
}

/// Figurative constants (ZERO, SPACE, HIGH-VALUE, etc.).
#[derive(Debug, Clone, PartialEq, Eq)]
#[cfg_attr(feature = "serialize", derive(serde::Serialize, serde::Deserialize))]
pub enum FigurativeConstant {
    Zero,
    Zeros,
    Zeroes,
    Space,
    Spaces,
    HighValue,
    HighValues,
    LowValue,
    LowValues,
    Quote,
    Quotes,
    All(String), // ALL literal
}

/// OCCURS clause for arrays/tables.
#[derive(Debug, Clone, PartialEq, Eq)]
#[cfg_attr(feature = "serialize", derive(serde::Serialize, serde::Deserialize))]
pub struct OccursClause {
    pub count: OccursCount,
    pub ascending: Option<bool>, // true for ASCENDING, false for DESCENDING
    pub key: Option<Vec<Spanned<String>>>, // KEY data names
    pub indexed: Option<Vec<Spanned<String>>>, // INDEXED BY data names
}

/// Count in OCCURS clause (fixed or variable).
#[derive(Debug, Clone, PartialEq, Eq)]
#[cfg_attr(feature = "serialize", derive(serde::Serialize, serde::Deserialize))]
pub enum OccursCount {
    Fixed(u64),
    Variable(String),                             // Data name or integer data item
    VariableRange { min: u64, max: Option<u64> }, // OCCURS n TO m
}

/// USAGE clause (COMPUTATIONAL, DISPLAY, etc.).
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[cfg_attr(feature = "serialize", derive(serde::Serialize, serde::Deserialize))]
pub enum Usage {
    Binary,
    BinaryShort,
    BinaryLong,
    BinaryDouble,
    BinaryExtended,
    PackedDecimal,
    Computational,
    Computational1,
    Computational2,
    Computational3,
    Computational4,
    Computational5,
    Display,
    Float,
    FloatShort,
    FloatLong,
    FloatExtended,
    Index,
    National,
    ObjectReference,
    Pointer,
    ProcedurePointer,
    FunctionPointer,
}
