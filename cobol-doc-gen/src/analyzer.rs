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
                        paragraph: self.get_current_paragraph_context(&if_stmt.span.start_line),
                        section: self.get_current_section_context(&if_stmt.span.start_line),
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
        let mut current_paragraph: Option<&str> = None;
        let mut paragraph_statements: Vec<String> = Vec::new();

        // First pass: extract paragraphs and their statements
        for statement in &program.procedure.node.statements {
            match &statement.node {
                Statement::Paragraph(para) => {
                    // Save previous paragraph if exists
                    if let Some(para_name) = current_paragraph {
                        self.finalize_paragraph_block(para_name, &paragraph_statements, &mut blocks);
                        paragraph_statements.clear();
                    }
                    current_paragraph = Some(&para.node.name);
                }
                _ => {
                    // Collect statements for the current paragraph
                    if current_paragraph.is_some() {
                        paragraph_statements.push(format!("{:?}", statement.node));
                    }
                }
            }
        }

        // Finalize the last paragraph
        if let Some(para_name) = current_paragraph {
            self.finalize_paragraph_block(para_name, &paragraph_statements, &mut blocks);
        }

        // Second pass: analyze PERFORM calls and build call graph
        self.analyze_procedure_calls(program, &mut blocks);

        Ok(blocks)
    }

    fn finalize_paragraph_block(&self, name: &str, statements: &[String], blocks: &mut Vec<ProcedureBlock>) {
        let calls_to = self.extract_perform_calls_from_statements(statements);
        let complexity = self.calculate_block_complexity(statements);

        blocks.push(ProcedureBlock {
            name: name.to_string(),
            block_type: ProcedureBlockType::Paragraph,
            purpose: self.infer_paragraph_purpose(name),
            statements: statements.to_vec(),
            calls_to,
            called_by: Vec::new(), // Will be filled in analyze_procedure_calls
            cyclomatic_complexity: complexity,
        });
    }

    fn extract_perform_calls_from_statements(&self, statements: &[String]) -> Vec<String> {
        let mut calls = Vec::new();
        
        for statement in statements {
            if statement.to_uppercase().contains("PERFORM") {
                // Simple regex-like extraction for PERFORM statements
                if let Some(start) = statement.to_uppercase().find("PERFORM") {
                    let after_perform = &statement[start + 7..].trim();
                    if let Some(space_pos) = after_perform.find(' ') {
                        let target = &after_perform[..space_pos];
                        if !target.is_empty() && target.chars().all(|c| c.is_alphanumeric() || c == '-') {
                            calls.push(target.to_string());
                        }
                    }
                }
            }
        }
        
        calls
    }

    fn calculate_block_complexity(&self, statements: &[String]) -> u32 {
        let mut complexity = 1; // Base complexity
        
        for statement in statements {
            let stmt_upper = statement.to_uppercase();
            if stmt_upper.contains("IF") || 
               stmt_upper.contains("EVALUATE") ||
               stmt_upper.contains("PERFORM") && stmt_upper.contains("UNTIL") {
                complexity += 1;
            }
        }
        
        complexity
    }

    fn analyze_procedure_calls(&self, program: &Program, blocks: &mut [ProcedureBlock]) {
        // Build reverse call references
        let mut call_graph: HashMap<String, Vec<String>> = HashMap::new();
        
        for block in blocks.iter() {
            for called in &block.calls_to {
                call_graph.entry(called.clone())
                    .or_insert_with(Vec::new)
                    .push(block.name.clone());
            }
        }
        
        // Update called_by information
        for block in blocks.iter_mut() {
            if let Some(callers) = call_graph.get(&block.name) {
                block.called_by = callers.clone();
            }
        }
    }

    fn generate_cross_references(&self, program: &Program) -> Result<CrossReferences> {
        let mut variable_usage = HashMap::new();
        let mut paragraph_calls = HashMap::new();
        let mut copybook_includes = Vec::new();

        // Extract variable usage
        self.analyze_variable_usage(program, &mut variable_usage);
        
        // Extract paragraph calls
        self.analyze_paragraph_calls(program, &mut paragraph_calls);
        
        // Extract copybook includes
        self.extract_copy_statements(program, &mut copybook_includes);

        Ok(CrossReferences {
            variable_usage,
            paragraph_calls,
            copybook_includes,
        })
    }

    fn analyze_variable_usage(&self, program: &Program, variable_usage: &mut HashMap<String, Vec<VariableUsage>>) {
        // Extract variables from data division
        let mut variables = Vec::new();
        if let Some(data) = &program.data {
            self.collect_variables_from_data_division(&data.node, &mut variables);
        }

        // Analyze usage in procedure division
        for statement in &program.procedure.node.statements {
            self.analyze_statement_for_variable_usage(&statement.node, &variables, variable_usage);
        }
    }

    fn collect_variables_from_data_division(&self, data: &cobol_ast::DataDivision, variables: &mut Vec<String>) {
        if let Some(ref ws) = data.working_storage_section {
            for item in &ws.node.data_items {
                variables.push(item.node.name.node.clone());
            }
        }
        if let Some(ref ls) = data.linkage_section {
            for item in &ls.node.data_items {
                variables.push(item.node.name.node.clone());
            }
        }
    }

    fn analyze_statement_for_variable_usage(&self, statement: &Statement, variables: &[String], usage_map: &mut HashMap<String, Vec<VariableUsage>>) {
        match statement {
            Statement::Move(move_stmt) => {
                // Analyze MOVE statement for variable usage
                if let Some(from_var) = self.extract_variable_from_expression(&move_stmt.node.from) {
                    if variables.contains(&from_var) {
                        usage_map.entry(from_var)
                            .or_insert_with(Vec::new)
                            .push(VariableUsage {
                                usage_type: VariableUsageType::Read,
                                location: SourceLocation {
                                    paragraph: None,
                                    section: None,
                                    line_start: move_stmt.span.start_line,
                                    line_end: move_stmt.span.end_line,
                                },
                                context: "MOVE statement source".to_string(),
                            });
                    }
                }
                
                for target in &move_stmt.node.targets {
                    if let Some(to_var) = self.extract_variable_from_expression(target) {
                        if variables.contains(&to_var) {
                            usage_map.entry(to_var)
                                .or_insert_with(Vec::new)
                                .push(VariableUsage {
                                    usage_type: VariableUsageType::Write,
                                    location: SourceLocation {
                                        paragraph: None,
                                        section: None,
                                        line_start: move_stmt.span.start_line,
                                        line_end: move_stmt.span.end_line,
                                    },
                                    context: "MOVE statement target".to_string(),
                                });
                        }
                    }
                }
            }
            Statement::If(if_stmt) => {
                // Recursively analyze nested statements
                for then_stmt in &if_stmt.node.then_statements {
                    self.analyze_statement_for_variable_usage(&then_stmt.node, variables, usage_map);
                }
                if let Some(ref else_stmts) = if_stmt.node.else_statements {
                    for else_stmt in else_stmts {
                        self.analyze_statement_for_variable_usage(&else_stmt.node, variables, usage_map);
                    }
                }
            }
            _ => {
                // For other statements, could add more specific analysis
            }
        }
    }

    fn extract_variable_from_expression(&self, expr: &cobol_ast::Expression) -> Option<String> {
        match expr {
            cobol_ast::Expression::Identifier(id) => Some(id.node.clone()),
            _ => None,
        }
    }

    fn analyze_paragraph_calls(&self, program: &Program, paragraph_calls: &mut HashMap<String, Vec<ParagraphCall>>) {
        for statement in &program.procedure.node.statements {
            self.extract_perform_calls_from_statement(&statement.node, paragraph_calls);
        }
    }

    fn extract_perform_calls_from_statement(&self, statement: &Statement, calls_map: &mut HashMap<String, Vec<ParagraphCall>>) {
        match statement {
            Statement::Perform(perform_stmt) => {
                if let Some(target) = &perform_stmt.node.target {
                    let target_name = format!("{:?}", target.node);
                    calls_map.entry(target_name.clone())
                        .or_insert_with(Vec::new)
                        .push(ParagraphCall {
                            caller: "MAIN".to_string(), // Simplified - would track current paragraph
                            call_type: CallType::Perform,
                            location: SourceLocation {
                                paragraph: None,
                                section: None,
                                line_start: perform_stmt.span.start_line,
                                line_end: perform_stmt.span.end_line,
                            },
                        });
                }
            }
            Statement::If(if_stmt) => {
                for then_stmt in &if_stmt.node.then_statements {
                    self.extract_perform_calls_from_statement(&then_stmt.node, calls_map);
                }
                if let Some(ref else_stmts) = if_stmt.node.else_statements {
                    for else_stmt in else_stmts {
                        self.extract_perform_calls_from_statement(&else_stmt.node, calls_map);
                    }
                }
            }
            _ => {}
        }
    }

    fn extract_copy_statements(&self, program: &Program, copybooks: &mut Vec<CopybookInclude>) {
        // In a real implementation, would scan for COPY statements in the source
        // For now, create a placeholder based on common patterns
        if let Some(data) = &program.data {
            if data.node.working_storage_section.is_some() {
                copybooks.push(CopybookInclude {
                    copybook_name: "COMMON-WS".to_string(),
                    location: SourceLocation {
                        paragraph: None,
                        section: Some("WORKING-STORAGE".to_string()),
                        line_start: 1,
                        line_end: 1,
                    },
                    library_name: None,
                });
            }
        }
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

    // Improved implementations for calculations
    fn count_lines(&self, program: &Program) -> usize {
        let mut lines = 5; // Base for identification division
        
        if program.environment.is_some() {
            lines += 10; // Environment division
        }
        
        if let Some(data) = &program.data {
            lines += 20; // Data division base
            if let Some(ref ws) = data.node.working_storage_section {
                lines += ws.node.data_items.len() * 2; // Approximate 2 lines per data item
            }
            if let Some(ref fs) = data.node.file_section {
                lines += fs.node.file_descriptions.len() * 5;
            }
        }
        
        lines += program.procedure.node.statements.len() * 3; // Approximate 3 lines per statement
        lines
    }
    
    fn extract_divisions(&self, program: &Program) -> Vec<String> {
        let mut divisions = vec!["IDENTIFICATION".to_string()];
        
        if program.environment.is_some() {
            divisions.push("ENVIRONMENT".to_string());
        }
        if program.data.is_some() {
            divisions.push("DATA".to_string());
        }
        divisions.push("PROCEDURE".to_string());
        
        divisions
    }
    
    fn extract_called_programs(&self, program: &Program) -> Vec<String> {
        let mut called_programs = Vec::new();
        
        for statement in &program.procedure.node.statements {
            self.extract_calls_from_statement(&statement.node, &mut called_programs);
        }
        
        called_programs.sort();
        called_programs.dedup();
        called_programs
    }
    
    fn extract_calls_from_statement(&self, statement: &Statement, called_programs: &mut Vec<String>) {
        match statement {
            Statement::Call(call_stmt) => {
                if let Some(program_name) = self.extract_literal_value(&call_stmt.node.program_name.node) {
                    called_programs.push(program_name);
                }
            }
            Statement::If(if_stmt) => {
                for then_stmt in &if_stmt.node.then_statements {
                    self.extract_calls_from_statement(&then_stmt.node, called_programs);
                }
                if let Some(ref else_stmts) = if_stmt.node.else_statements {
                    for else_stmt in else_stmts {
                        self.extract_calls_from_statement(&else_stmt.node, called_programs);
                    }
                }
            }
            _ => {}
        }
    }
    
    fn extract_literal_value(&self, literal: &cobol_ast::Literal) -> Option<String> {
        match literal {
            cobol_ast::Literal::String(s) => Some(s.clone()),
            cobol_ast::Literal::Identifier(id) => Some(id.clone()),
            _ => None,
        }
    }
    
    fn extract_input_files(&self, program: &Program) -> Vec<String> {
        let mut input_files = Vec::new();
        
        if let Some(data) = &program.data {
            if let Some(ref file_section) = data.node.file_section {
                for file_desc in &file_section.node.file_descriptions {
                    if let Some(ref select_clause) = &file_desc.node.select_clause {
                        if let Some(filename) = self.extract_filename_from_assign(&select_clause.node.assign_clause) {
                            input_files.push(filename);
                        }
                    }
                }
            }
        }
        
        input_files
    }
    
    fn extract_output_files(&self, program: &Program) -> Vec<String> {
        // For now, same logic as input files - in real implementation, 
        // would distinguish based on file access mode
        self.extract_input_files(program)
    }
    
    fn extract_filename_from_assign(&self, _assign_clause: &cobol_ast::data::AssignClause) -> Option<String> {
        // Simplified - in real implementation would parse the assign clause
        None
    }
    
    fn calculate_cyclomatic_complexity(&self, program: &Program) -> u32 {
        let mut complexity = 1; // Base complexity
        
        for statement in &program.procedure.node.statements {
            complexity += self.calculate_statement_complexity(&statement.node);
        }
        
        complexity
    }
    
    fn calculate_statement_complexity(&self, statement: &Statement) -> u32 {
        match statement {
            Statement::If(_) => 1,
            Statement::Evaluate(_) => 1,
            Statement::Perform(perform_stmt) => {
                match &perform_stmt.node.perform_type {
                    Some(cobol_ast::statement::PerformType::Until(_)) => 1,
                    Some(cobol_ast::statement::PerformType::Varying(_)) => 1,
                    Some(cobol_ast::statement::PerformType::Times(_)) => 1,
                    _ => 0,
                }
            }
            _ => 0,
        }
    }
    
    fn calculate_max_nesting_depth(&self, program: &Program) -> u32 {
        let mut max_depth = 0;
        
        for statement in &program.procedure.node.statements {
            let depth = self.calculate_statement_nesting(&statement.node, 0);
            max_depth = max_depth.max(depth);
        }
        
        max_depth
    }
    
    fn calculate_statement_nesting(&self, statement: &Statement, current_depth: u32) -> u32 {
        match statement {
            Statement::If(if_stmt) => {
                let mut max_nested = current_depth + 1;
                
                for then_stmt in &if_stmt.node.then_statements {
                    let depth = self.calculate_statement_nesting(&then_stmt.node, current_depth + 1);
                    max_nested = max_nested.max(depth);
                }
                
                if let Some(ref else_stmts) = if_stmt.node.else_statements {
                    for else_stmt in else_stmts {
                        let depth = self.calculate_statement_nesting(&else_stmt.node, current_depth + 1);
                        max_nested = max_nested.max(depth);
                    }
                }
                
                max_nested
            }
            _ => current_depth,
        }
    }
    
    fn calculate_comment_ratio(&self, _program: &Program) -> f64 {
        // Simplified - would need to count comments in the source
        0.15
    }
    
    fn calculate_maintainability_index(&self, program: &Program) -> f64 {
        let loc = self.count_lines(program) as f64;
        let cc = self.calculate_cyclomatic_complexity(program) as f64;
        let comment_ratio = self.calculate_comment_ratio(program);
        
        // Simplified maintainability index calculation
        let base_score = 171.0 - 5.2 * (loc.ln()) - 0.23 * cc - 16.2 * (loc.ln()).ln();
        let adjusted_score = base_score + (50.0 * comment_ratio);
        
        adjusted_score.max(0.0).min(100.0)
    }
    
    fn estimate_technical_debt(&self, program: &Program) -> f64 {
        let complexity = self.calculate_cyclomatic_complexity(program) as f64;
        let nesting = self.calculate_max_nesting_depth(program) as f64;
        let loc = self.count_lines(program) as f64;
        
        // Simplified debt estimation: higher complexity and size = more debt
        let debt_score = (complexity * 2.0) + (nesting * 5.0) + (loc / 100.0);
        debt_score * 5.0 // Convert to minutes
    }

    // Context tracking helpers
    fn get_current_paragraph_context(&self, _line: usize) -> Option<String> {
        // Simplified - in real implementation would track paragraph context
        Some("MAIN-PROCESSING".to_string())
    }

    fn get_current_section_context(&self, _line: usize) -> Option<String> {
        // Simplified - in real implementation would track section context
        Some("PROCEDURE".to_string())
    }
}