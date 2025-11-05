/// Source code location information.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
#[cfg_attr(feature = "serialize", derive(serde::Serialize, serde::Deserialize))]
pub struct Span {
    /// Starting line number (1-indexed)
    pub start_line: usize,
    
    /// Starting column number (1-indexed)
    pub start_column: usize,
    
    /// Ending line number (1-indexed)
    pub end_line: usize,
    
    /// Ending column number (1-indexed)
    pub end_column: usize,
    
    /// Byte offset of start (0-indexed)
    pub start_byte: usize,
    
    /// Byte offset of end (0-indexed)
    pub end_byte: usize,
}

impl Span {
    /// Create a new span.
    pub fn new(
        start_line: usize,
        start_column: usize,
        end_line: usize,
        end_column: usize,
        start_byte: usize,
        end_byte: usize,
    ) -> Self {
        Self {
            start_line,
            start_column,
            end_line,
            end_column,
            start_byte,
            end_byte,
        }
    }
    
    /// Create a span from token positions (start and end tokens).
    pub fn from_tokens(start_pos: (usize, usize, usize), end_pos: (usize, usize, usize)) -> Self {
        Self {
            start_line: start_pos.0,
            start_column: start_pos.1,
            end_line: end_pos.0,
            end_column: end_pos.1,
            start_byte: start_pos.2,
            end_byte: end_pos.2,
        }
    }
    
    /// Merge two spans (creates a span that encompasses both).
    pub fn merge(self, other: Span) -> Self {
        Self {
            start_line: self.start_line.min(other.start_line),
            start_column: if self.start_line < other.start_line {
                self.start_column
            } else if self.start_line == other.start_line {
                self.start_column.min(other.start_column)
            } else {
                other.start_column
            },
            end_line: self.end_line.max(other.end_line),
            end_column: if self.end_line > other.end_line {
                self.end_column
            } else if self.end_line == other.end_line {
                self.end_column.max(other.end_column)
            } else {
                other.end_column
            },
            start_byte: self.start_byte.min(other.start_byte),
            end_byte: self.end_byte.max(other.end_byte),
        }
    }
}

/// A node with source location information.
#[derive(Debug, Clone, PartialEq, Eq)]
#[cfg_attr(feature = "serialize", derive(serde::Serialize, serde::Deserialize))]
pub struct Spanned<T> {
    pub node: T,
    pub span: Span,
}

impl<T> Spanned<T> {
    pub fn new(node: T, span: Span) -> Self {
        Self { node, span }
    }
}
