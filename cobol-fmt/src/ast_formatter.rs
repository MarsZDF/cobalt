use crate::config::FormatConfig;
use cobol_ast::{DataDivision, DataItem, ProcedureDivision, Program, Statement};

/// AST-based formatter for better code formatting.
pub struct AstFormatter {
    config: FormatConfig,
}

impl AstFormatter {
    pub fn new(config: FormatConfig) -> Self {
        Self { config }
    }

    /// Format a COBOL program using AST information.
    pub fn format_program(&self, program: &cobol_ast::Spanned<Program>) -> String {
        let mut output = String::new();

        // Format IDENTIFICATION DIVISION
        output.push_str(&self.format_identification_division(&program.node.identification));
        output.push('\n');

        // Format ENVIRONMENT DIVISION (if present)
        if let Some(env) = &program.node.environment {
            output.push_str(&self.format_environment_division(env));
            output.push('\n');
        }

        // Format DATA DIVISION (if present)
        if let Some(data) = &program.node.data {
            output.push_str(&self.format_data_division(data));
            output.push('\n');
        }

        // Format PROCEDURE DIVISION
        output.push_str(&self.format_procedure_division(&program.node.procedure));

        output
    }

    fn format_identification_division(
        &self,
        id: &cobol_ast::Spanned<cobol_ast::IdentificationDivision>,
    ) -> String {
        let mut output = String::new();
        output.push_str("IDENTIFICATION DIVISION.\n");

        if let Some(prog_id) = &id.node.program_id {
            output.push_str(&format!(
                "PROGRAM-ID. {}.\n",
                self.format_identifier(prog_id)
            ));
        }

        if let Some(author) = &id.node.author {
            output.push_str(&format!("AUTHOR. {}.\n", author));
        }

        if let Some(date_written) = &id.node.date_written {
            output.push_str(&format!("DATE-WRITTEN. {}.\n", date_written));
        }

        output
    }

    fn format_environment_division(
        &self,
        _env: &cobol_ast::Spanned<cobol_ast::EnvironmentDivision>,
    ) -> String {
        let mut output = String::new();
        output.push_str("ENVIRONMENT DIVISION.\n");
        // TODO: Format environment division details
        output
    }

    fn format_data_division(&self, data: &cobol_ast::Spanned<DataDivision>) -> String {
        let mut output = String::new();
        output.push_str("DATA DIVISION.\n");

        // Format WORKING-STORAGE SECTION
        if let Some(ws) = &data.node.working_storage_section {
            output.push_str("WORKING-STORAGE SECTION.\n");
            for item in &ws.node.data_items {
                output.push_str(&self.format_data_item(item, 0));
            }
        }

        // Format LOCAL-STORAGE SECTION
        if let Some(ls) = &data.node.local_storage_section {
            output.push_str("LOCAL-STORAGE SECTION.\n");
            for item in &ls.node.data_items {
                output.push_str(&self.format_data_item(item, 0));
            }
        }

        // Format LINKAGE SECTION
        if let Some(link) = &data.node.linkage_section {
            output.push_str("LINKAGE SECTION.\n");
            for item in &link.node.data_items {
                output.push_str(&self.format_data_item(item, 0));
            }
        }

        output
    }

    fn format_data_item(&self, item: &cobol_ast::Spanned<DataItem>, indent: usize) -> String {
        let mut output = String::new();
        let indent_str = self.indent_string(indent);

        // Format level number (aligned)
        let level_str = format!("{:02}", item.node.level);
        output.push_str(&format!("{}{} ", indent_str, level_str));

        // Format name
        output.push_str(&self.format_identifier(&item.node.name.node));

        // Format PICTURE clause (aligned)
        if let Some(pic) = &item.node.picture {
            let pic_str = self.format_picture(&pic.node.string);
            // Align PICTURE clause to column 32 (typical COBOL convention)
            let name_len = item.node.name.node.len();
            let padding = if name_len < 24 { 24 - name_len } else { 1 };
            output.push_str(&" ".repeat(padding));
            output.push_str(&format!("PIC {}", pic_str));
        }

        // Format VALUE clause
        if let Some(value) = &item.node.value {
            output.push_str(" VALUE ");
            output.push_str(&self.format_initial_value(value));
        }

        // Format OCCURS clause
        if let Some(occurs) = &item.node.occurs {
            output.push_str(" OCCURS ");
            output.push_str(&self.format_occurs(occurs));
        }

        // Format REDEFINES
        if let Some(redefines) = &item.node.redefines {
            output.push_str(&format!(
                " REDEFINES {}",
                self.format_identifier(&redefines.node)
            ));
        }

        output.push_str(".\n");

        // Format children (nested data items)
        for child in &item.node.children {
            output.push_str(&self.format_data_item(child, indent + 1));
        }

        output
    }

    fn format_picture(&self, pic: &str) -> String {
        // Format PICTURE clause with proper spacing
        // Example: PIC 9(5) -> PIC 9(5)
        // Example: PIC X(10) -> PIC X(10)
        // Example: PIC 999V99 -> PIC 999V99
        pic.to_string()
    }

    fn format_initial_value(&self, value: &cobol_ast::Spanned<cobol_ast::InitialValue>) -> String {
        match &value.node {
            cobol_ast::InitialValue::Literal(lit) => match lit {
                cobol_ast::Literal::String(s) => format!("\"{}\"", s),
                cobol_ast::Literal::Numeric(n) => format!("{}", n.value),
                cobol_ast::Literal::Boolean(b) => format!("{}", if *b { "TRUE" } else { "FALSE" }),
            },
            cobol_ast::InitialValue::FigurativeConstant(fc) => match fc {
                cobol_ast::FigurativeConstant::Zero => "ZERO".to_string(),
                cobol_ast::FigurativeConstant::Zeros => "ZEROS".to_string(),
                cobol_ast::FigurativeConstant::Zeroes => "ZEROES".to_string(),
                cobol_ast::FigurativeConstant::Space => "SPACE".to_string(),
                cobol_ast::FigurativeConstant::Spaces => "SPACES".to_string(),
                cobol_ast::FigurativeConstant::HighValue => "HIGH-VALUE".to_string(),
                cobol_ast::FigurativeConstant::HighValues => "HIGH-VALUES".to_string(),
                cobol_ast::FigurativeConstant::LowValue => "LOW-VALUE".to_string(),
                cobol_ast::FigurativeConstant::LowValues => "LOW-VALUES".to_string(),
                cobol_ast::FigurativeConstant::Quote => "QUOTE".to_string(),
                cobol_ast::FigurativeConstant::Quotes => "QUOTES".to_string(),
                cobol_ast::FigurativeConstant::All(s) => format!("ALL \"{}\"", s),
            },
        }
    }

    fn format_occurs(&self, occurs: &cobol_ast::Spanned<cobol_ast::OccursClause>) -> String {
        match &occurs.node.count {
            cobol_ast::OccursCount::Fixed(n) => n.to_string(),
            cobol_ast::OccursCount::Variable(name) => name.clone(),
            cobol_ast::OccursCount::VariableRange { min, max } => {
                if let Some(max_val) = max {
                    format!("{} TO {}", min, max_val)
                } else {
                    min.to_string()
                }
            }
        }
    }

    fn format_procedure_division(&self, proc: &cobol_ast::Spanned<ProcedureDivision>) -> String {
        let mut output = String::new();
        output.push_str("PROCEDURE DIVISION");

        if let Some(using) = &proc.node.using {
            output.push_str(" USING");
            for (i, param) in using.iter().enumerate() {
                if i > 0 {
                    output.push_str(",");
                }
                output.push_str(&format!(" {}", self.format_identifier(&param.node)));
            }
        }

        if let Some(returning) = &proc.node.returning {
            output.push_str(&format!(" RETURNING {}", self.format_identifier(returning)));
        }

        output.push_str(".\n");

        // Format statements with proper indentation
        for stmt in &proc.node.statements {
            output.push_str(&self.format_statement(stmt, 1));
        }

        output
    }

    fn format_statement(&self, stmt: &cobol_ast::Spanned<Statement>, indent: usize) -> String {
        let indent_str = self.indent_string(indent);
        let mut output = String::new();

        match &stmt.node {
            Statement::Display(display) => {
                output.push_str(&format!("{}DISPLAY", indent_str));
                for (i, operand) in display.node.operands.iter().enumerate() {
                    if i > 0 {
                        output.push_str(",");
                    }
                    match &operand.node {
                        cobol_ast::DisplayOperand::Literal(lit) => match lit {
                            cobol_ast::Literal::String(s) => {
                                output.push_str(&format!(" \"{}\"", s))
                            }
                            cobol_ast::Literal::Numeric(n) => {
                                output.push_str(&format!(" {}", n.value))
                            }
                            cobol_ast::Literal::Boolean(b) => {
                                output.push_str(&format!(" {}", if *b { "TRUE" } else { "FALSE" }))
                            }
                        },
                        cobol_ast::DisplayOperand::Identifier(id) => {
                            output.push_str(&format!(" {}", self.format_identifier(id)));
                        }
                    }
                }
                output.push_str(".\n");
            }
            Statement::Move(move_stmt) => {
                output.push_str(&format!("{}MOVE", indent_str));
                match &move_stmt.node.from.node {
                    cobol_ast::MoveSource::Literal(lit) => match lit {
                        cobol_ast::Literal::String(s) => output.push_str(&format!(" \"{}\"", s)),
                        cobol_ast::Literal::Numeric(n) => output.push_str(&format!(" {}", n.value)),
                        cobol_ast::Literal::Boolean(b) => {
                            output.push_str(&format!(" {}", if *b { "TRUE" } else { "FALSE" }))
                        }
                    },
                    cobol_ast::MoveSource::Identifier(id) => {
                        output.push_str(&format!(" {}", self.format_identifier(id)));
                    }
                    cobol_ast::MoveSource::FigurativeConstant(fc) => {
                        output.push_str(&format!(" {}", self.format_figurative_constant(fc)));
                    }
                }
                output.push_str(" TO");
                for (i, to) in move_stmt.node.to.iter().enumerate() {
                    if i > 0 {
                        output.push_str(",");
                    }
                    output.push_str(&format!(" {}", self.format_identifier(&to.node)));
                }
                output.push_str(".\n");
            }
            Statement::If(if_stmt) => {
                output.push_str(&format!("{}IF ", indent_str));
                output.push_str(&self.format_expression(&if_stmt.node.condition));
                output.push_str(" THEN\n");

                for then_stmt in &if_stmt.node.then_statements {
                    output.push_str(&self.format_statement(then_stmt, indent + 1));
                }

                if let Some(else_stmts) = &if_stmt.node.else_statements {
                    output.push_str(&format!("{}ELSE\n", indent_str));
                    for else_stmt in else_stmts {
                        output.push_str(&self.format_statement(else_stmt, indent + 1));
                    }
                }

                output.push_str(&format!("{}END-IF.\n", indent_str));
            }
            Statement::Stop(stop) => match stop.node {
                cobol_ast::StopStatement::Run => {
                    output.push_str(&format!("{}STOP RUN.\n", indent_str));
                }
                cobol_ast::StopStatement::Literal(_) => {
                    output.push_str(&format!("{}STOP.\n", indent_str));
                }
            },
            _ => {
                // Fallback for other statement types
                output.push_str(&format!("{}<statement>\n", indent_str));
            }
        }

        output
    }

    fn format_expression(&self, _expr: &cobol_ast::Spanned<cobol_ast::Expression>) -> String {
        // Simplified expression formatting
        // TODO: Implement full expression formatting
        "<expression>".to_string()
    }

    fn format_identifier(&self, id: &str) -> String {
        match self.config.identifier_case {
            crate::config::IdentifierCase::Upper => id.to_uppercase(),
            crate::config::IdentifierCase::Lower => id.to_lowercase(),
            crate::config::IdentifierCase::Preserve => id.to_string(),
        }
    }

    fn format_figurative_constant(&self, fc: &cobol_ast::FigurativeConstant) -> String {
        match fc {
            cobol_ast::FigurativeConstant::Zero => "ZERO".to_string(),
            cobol_ast::FigurativeConstant::Zeros => "ZEROS".to_string(),
            cobol_ast::FigurativeConstant::Zeroes => "ZEROES".to_string(),
            cobol_ast::FigurativeConstant::Space => "SPACE".to_string(),
            cobol_ast::FigurativeConstant::Spaces => "SPACES".to_string(),
            cobol_ast::FigurativeConstant::HighValue => "HIGH-VALUE".to_string(),
            cobol_ast::FigurativeConstant::HighValues => "HIGH-VALUES".to_string(),
            cobol_ast::FigurativeConstant::LowValue => "LOW-VALUE".to_string(),
            cobol_ast::FigurativeConstant::LowValues => "LOW-VALUES".to_string(),
            cobol_ast::FigurativeConstant::Quote => "QUOTE".to_string(),
            cobol_ast::FigurativeConstant::Quotes => "QUOTES".to_string(),
            cobol_ast::FigurativeConstant::All(s) => format!("ALL \"{}\"", s),
        }
    }

    fn indent_string(&self, level: usize) -> String {
        if self.config.use_spaces {
            " ".repeat(level * self.config.indent_width)
        } else {
            "\t".repeat(level)
        }
    }
}
