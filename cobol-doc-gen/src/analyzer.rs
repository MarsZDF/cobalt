use crate::models::*;
use cobol_ast::{Program, DataItem, Statement};
use std::collections::HashMap;
use anyhow::Result;

/// Analyzes COBOL AST to extract documentation information.
pub struct CobolAnalyzer {
    config: AnalyzerConfig,
}

#[derive(Debug, Clone)]
pub struct AnalyzerConfig {
    pub extract_business_rules: bool,
    pub calculate_complexity: bool,
    pub generate_cross_references: bool,
    pub infer_descriptions: bool,
}

impl Default for AnalyzerConfig {
    fn default() -> Self {
        Self {
            extract_business_rules: true,
            calculate_complexity: true,
            generate_cross_references: true,
            infer_descriptions: true,
        }
    }
}

impl CobolAnalyzer {
    pub fn new(config: AnalyzerConfig) -> Self {
        Self { config }
    }

    /// Analyze a COBOL program and extract documentation information.
    pub fn analyze(&self, program: &Program) -> Result<Documentation> {
        let program_summary = self.extract_program_summary(program)?;
        let data_structures = self.extract_data_structures(program)?;
        let business_rules = if self.config.extract_business_rules {
            self.extract_business_rules(program)?
        } else {
            Vec::new()
        };
        let procedure_flow = self.extract_procedure_flow(program)?;
        let cross_references = if self.config.generate_cross_references {
            self.generate_cross_references(program)?
        } else {
            CrossReferences {
                variable_usage: HashMap::new(),
                paragraph_calls: HashMap::new(),
                copybook_includes: Vec::new(),
            }
        };
        let complexity_metrics = if self.config.calculate_complexity {
            self.calculate_complexity_metrics(program)?
        } else {
            ComplexityMetrics {
                cyclomatic_complexity: 0,
                nesting_depth: 0,
                lines_of_code: 0,
                comment_ratio: 0.0,
                maintainability_index: 0.0,
                technical_debt_minutes: 0.0,
            }
        };

        Ok(Documentation {
            program_summary,
            data_structures,
            business_rules,
            procedure_flow,
            cross_references,
            complexity_metrics,
        })
    }

    fn extract_program_summary(&self, program: &Program) -> Result<ProgramSummary> {
        // Extract from identification division
        let identification = &program.identification.node;
        Ok(ProgramSummary {
            program_id: identification.program_id.clone().unwrap_or_else(|| "UNKNOWN".to_string()),
            author: identification.author.clone(),
            date_written: identification.date_written.clone(),
            date_compiled: identification.date_compiled.clone(),
            purpose: self.infer_program_purpose(program),
            remarks: identification.remarks.clone(),
            total_lines: self.count_lines(program),
            divisions: self.extract_divisions(program),
            called_programs: self.extract_called_programs(program),
            input_files: self.extract_input_files(program),
            output_files: self.extract_output_files(program),
        })
    }

    fn extract_data_structures(&self, program: &Program) -> Result<Vec<DataStructure>> {
        let mut structures = Vec::new();
        
        if let Some(ref data_division) = program.data {
            if let Some(ref working_storage) = data_division.node.working_storage_section {
                for item in &working_storage.node.data_items {
                    structures.push(self.convert_data_item(&item.node)?);
                }
            }
            
            if let Some(ref file_section) = data_division.node.file_section {
                for file_desc in &file_section.node.file_descriptions {
                    for item in &file_desc.node.data_items {
                        structures.push(self.convert_data_item(&item.node)?);
                    }
                }
            }
            
            if let Some(ref linkage_section) = data_division.node.linkage_section {
                for item in &linkage_section.node.data_items {
                    structures.push(self.convert_data_item(&item.node)?);
                }
            }
        }
        
        Ok(structures)
    }

    fn convert_data_item(&self, item: &DataItem) -> Result<DataStructure> {
        let description = if self.config.infer_descriptions {
            self.infer_data_description(&item.name.node)
        } else {
            None
        };

        let children = item.children.iter()
            .map(|child| self.convert_data_item(&child.node))
            .collect::<Result<Vec<_>>>()?;

        Ok(DataStructure {
            name: item.name.node.clone(),
            level: item.level,
            picture: item.picture.as_ref().map(|p| p.node.string.clone()),
            usage: item.usage.as_ref().map(|u| format!("{:?}", u.node)),
            description,
            length: self.calculate_field_length(item),
            initial_value: item.value.as_ref().map(|v| format!("{:?}", v.node)),
            occurs: item.occurs.as_ref().map(|o| format!("{:?}", o.node)),
            children,
            line_number: Some(item.name.span.start_line),
        })
    }

    fn extract_business_rules(&self, program: &Program) -> Result<Vec<BusinessRule>> {
        let mut rules = Vec::new();
        let mut rule_counter = 1;

        for statement in &program.procedure.node.statements {
            if let Some(rule) = self.extract_rule_from_statement(&statement.node, &mut rule_counter)? {
                rules.push(rule);
            }
        }

        Ok(rules)
    }

    fn extract_rule_from_statement(&self, statement: &Statement, counter: &mut usize) -> Result<Option<BusinessRule>> {
        match statement {
            Statement::If(if_stmt) => {
                let rule = BusinessRule {
                    id: format!("BR-{:03}", counter),
                    description: self.describe_if_statement(&if_stmt.node),
                    rule_type: BusinessRuleType::Validation,
                    location: SourceLocation {
                        paragraph: None, // TODO: Track current paragraph
                        section: None,   // TODO: Track current section
                        line_start: if_stmt.span.start_line,
                        line_end: if_stmt.span.end_line,
                    },
                    conditions: vec![format!("{:?}", if_stmt.node.condition.node)],
                    actions: if_stmt.node.then_statements.iter()
                        .map(|s| format!("{:?}", s.node))
                        .collect(),
                    complexity: self.assess_rule_complexity(&if_stmt.node),
                };
                *counter += 1;
                Ok(Some(rule))
            }
            Statement::Compute(compute_stmt) => {
                let rule = BusinessRule {
                    id: format!("BR-{:03}", counter),
                    description: self.describe_compute_statement(&compute_stmt.node),
                    rule_type: BusinessRuleType::Calculation,
                    location: SourceLocation {
                        paragraph: None,
                        section: None,
                        line_start: compute_stmt.span.start_line,
                        line_end: compute_stmt.span.end_line,
                    },
                    conditions: Vec::new(),
                    actions: vec![format!("{:?}", compute_stmt.node.expression.node)],
                    complexity: RuleComplexity::Simple,
                };
                *counter += 1;
                Ok(Some(rule))
            }
            _ => Ok(None),
        }
    }

    fn extract_procedure_flow(&self, program: &Program) -> Result<Vec<ProcedureBlock>> {
        let mut blocks = Vec::new();

        // Extract paragraphs and sections
        for statement in &program.procedure.node.statements {
            if let Statement::Paragraph(para) = &statement.node {
                blocks.push(ProcedureBlock {
                    name: para.node.name.clone(),
                    block_type: ProcedureBlockType::Paragraph,
                    purpose: self.infer_paragraph_purpose(&para.node.name),
                    statements: Vec::new(), // TODO: Collect statements in this paragraph
                    calls_to: Vec::new(),   // TODO: Analyze PERFORM statements
                    called_by: Vec::new(),  // TODO: Reverse reference
                    cyclomatic_complexity: 1, // TODO: Calculate properly
                });
            }
        }

        Ok(blocks)
    }

    fn generate_cross_references(&self, program: &Program) -> Result<CrossReferences> {
        Ok(CrossReferences {
            variable_usage: HashMap::new(), // TODO: Implement variable tracking
            paragraph_calls: HashMap::new(), // TODO: Implement call tracking
            copybook_includes: Vec::new(),   // TODO: Extract COPY statements
        })
    }

    fn calculate_complexity_metrics(&self, program: &Program) -> Result<ComplexityMetrics> {
        Ok(ComplexityMetrics {
            cyclomatic_complexity: self.calculate_cyclomatic_complexity(program),
            nesting_depth: self.calculate_max_nesting_depth(program),
            lines_of_code: self.count_lines(program),
            comment_ratio: self.calculate_comment_ratio(program),
            maintainability_index: self.calculate_maintainability_index(program),
            technical_debt_minutes: self.estimate_technical_debt(program),
        })
    }

    // Helper methods for inference and calculation
    fn infer_program_purpose(&self, program: &Program) -> Option<String> {
        // Basic heuristics based on program structure and statements
        let has_file_operations = program.procedure.node.statements.iter().any(|s| {
            matches!(s.node, Statement::Return(_))
        });
        
        let has_calculations = program.procedure.node.statements.iter().any(|s| {
            matches!(s.node, Statement::Compute(_))
        });

        if has_file_operations && has_calculations {
            Some("Data processing and calculation program".to_string())
        } else if has_file_operations {
            Some("File processing program".to_string())
        } else if has_calculations {
            Some("Calculation program".to_string())
        } else {
            Some("General purpose program".to_string())
        }
    }

    fn infer_data_description(&self, name: &str) -> Option<String> {
        // Simple heuristics based on common COBOL naming conventions
        let name_lower = name.to_lowercase();
        
        if name_lower.contains("date") {
            Some("Date field".to_string())
        } else if name_lower.contains("amount") || name_lower.contains("amt") {
            Some("Monetary amount".to_string())
        } else if name_lower.contains("count") || name_lower.contains("cnt") {
            Some("Counter or quantity".to_string())
        } else if name_lower.contains("flag") || name_lower.contains("flg") {
            Some("Boolean flag".to_string())
        } else if name_lower.contains("name") {
            Some("Name field".to_string())
        } else if name_lower.contains("address") || name_lower.contains("addr") {
            Some("Address information".to_string())
        } else {
            None
        }
    }

    fn calculate_field_length(&self, item: &DataItem) -> Option<usize> {
        item.picture.as_ref().and_then(|pic| {
            // Simple PICTURE clause length calculation
            let pic_str = &pic.node.string;
            if pic_str.starts_with('X') {
                pic_str.chars().filter(|c| c.is_ascii_digit()).collect::<String>()
                    .parse().ok()
            } else if pic_str.starts_with('9') {
                pic_str.chars().filter(|c| *c == '9').count().into()
            } else {
                None
            }
        })
    }

    fn describe_if_statement(&self, if_stmt: &cobol_ast::statement::IfStatement) -> String {
        format!("Conditional logic: IF {} THEN ...", format!("{:?}", if_stmt.condition.node))
    }

    fn describe_compute_statement(&self, compute_stmt: &cobol_ast::statement::ComputeStatement) -> String {
        format!("Mathematical calculation: {} = {}", 
                compute_stmt.targets.iter().map(|t| &t.node).collect::<Vec<_>>().join(", "),
                format!("{:?}", compute_stmt.expression.node))
    }

    fn assess_rule_complexity(&self, if_stmt: &cobol_ast::statement::IfStatement) -> RuleComplexity {
        let has_else = if_stmt.else_statements.is_some();
        let then_count = if_stmt.then_statements.len();
        
        if then_count <= 2 && !has_else {
            RuleComplexity::Simple
        } else if then_count <= 5 {
            RuleComplexity::Moderate
        } else {
            RuleComplexity::Complex
        }
    }

    fn infer_paragraph_purpose(&self, name: &str) -> Option<String> {
        let name_lower = name.to_lowercase();
        
        if name_lower.contains("init") {
            Some("Initialization".to_string())
        } else if name_lower.contains("process") {
            Some("Main processing logic".to_string())
        } else if name_lower.contains("cleanup") || name_lower.contains("clean") {
            Some("Cleanup operations".to_string())
        } else if name_lower.contains("error") {
            Some("Error handling".to_string())
        } else if name_lower.contains("validate") {
            Some("Data validation".to_string())
        } else {
            None
        }
    }

    // Placeholder implementations for complex calculations
    fn count_lines(&self, _program: &Program) -> usize { 100 } // TODO: Implement
    fn extract_divisions(&self, _program: &Program) -> Vec<String> { 
        vec!["IDENTIFICATION".to_string(), "DATA".to_string(), "PROCEDURE".to_string()]
    }
    fn extract_called_programs(&self, _program: &Program) -> Vec<String> { Vec::new() }
    fn extract_input_files(&self, _program: &Program) -> Vec<String> { Vec::new() }
    fn extract_output_files(&self, _program: &Program) -> Vec<String> { Vec::new() }
    fn calculate_cyclomatic_complexity(&self, _program: &Program) -> u32 { 1 }
    fn calculate_max_nesting_depth(&self, _program: &Program) -> u32 { 1 }
    fn calculate_comment_ratio(&self, _program: &Program) -> f64 { 0.1 }
    fn calculate_maintainability_index(&self, _program: &Program) -> f64 { 85.0 }
    fn estimate_technical_debt(&self, _program: &Program) -> f64 { 30.0 }
}