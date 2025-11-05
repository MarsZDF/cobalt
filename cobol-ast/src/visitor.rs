use crate::data::DataItem;
use crate::expression::Expression;
use crate::program::Program;
use crate::span::Spanned;
use crate::statement::Statement;

/// Visitor trait for traversing the AST.
pub trait Visitor<T = ()> {
    /// Visit a complete program.
    fn visit_program(&mut self, program: &Spanned<Program>) -> T;

    /// Visit a statement.
    fn visit_statement(&mut self, statement: &Spanned<Statement>) -> T;

    /// Visit an expression.
    fn visit_expression(&mut self, expression: &Spanned<Expression>) -> T;

    /// Visit a data item.
    fn visit_data_item(&mut self, data_item: &Spanned<DataItem>) -> T;
}
