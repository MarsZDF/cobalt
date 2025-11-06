use crate::cfg::ControlFlowGraph;
use cobol_ast::{DataItem, ProcedureDivision, Program, Statement};
use std::collections::HashSet;

/// Dead code analysis results.
#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
pub struct DeadCodeReport {
    pub unused_variables: Vec<UnusedVariable>,
    pub unused_paragraphs: Vec<String>,
    pub unused_sections: Vec<String>,
    pub unreachable_statements: Vec<UnreachableStatement>,
}

#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
pub struct UnusedVariable {
    pub name: String,
    pub level: u8,
    pub location: String,
}

#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
pub struct UnreachableStatement {
    pub statement_type: String,
    pub location: String,
}

/// Dead code analyzer.
pub struct DeadCodeAnalyzer {
    program: Program,
    cfg: ControlFlowGraph,
}

impl DeadCodeAnalyzer {
    pub fn new(program: Program) -> Self {
        let cfg = ControlFlowGraph::from_program(&program);
        Self { program, cfg }
    }

    /// Analyze the program for dead code.
    pub fn analyze(&self) -> DeadCodeReport {
        DeadCodeReport {
            unused_variables: self.find_unused_variables(),
            unused_paragraphs: self.find_unused_paragraphs(),
            unused_sections: self.find_unused_sections(),
            unreachable_statements: self.find_unreachable_statements(),
        }
    }

    /// Find unused variables in DATA DIVISION.
    fn find_unused_variables(&self) -> Vec<UnusedVariable> {
        let mut unused = Vec::new();
        let mut used_variables = HashSet::new();

        // Collect all variable references from PROCEDURE DIVISION
        self.collect_variable_uses(&self.program.procedure.node, &mut used_variables);

        // Check DATA DIVISION for unused variables
        if let Some(data) = &self.program.data {
            if let Some(ws) = &data.node.working_storage_section {
                for item in &ws.node.data_items {
                    self.check_data_item_usage(item, &used_variables, &mut unused);
                }
            }
        }

        unused
    }

    fn check_data_item_usage(
        &self,
        item: &cobol_ast::Spanned<DataItem>,
        used_variables: &HashSet<String>,
        unused: &mut Vec<UnusedVariable>,
    ) {
        let name = &item.node.name.node;
        let name_upper = name.to_uppercase();

        // Check if variable is used
        if !used_variables.contains(&name_upper) && !used_variables.contains(name) {
            unused.push(UnusedVariable {
                name: name.clone(),
                level: item.node.level,
                location: format!("DATA DIVISION, level {}", item.node.level),
            });
        }

        // Check children
        for child in &item.node.children {
            self.check_data_item_usage(child, used_variables, unused);
        }
    }

    fn collect_variable_uses(&self, proc: &ProcedureDivision, used: &mut HashSet<String>) {
        // Collect from statements (including Statement::Paragraph which only has name)
        for stmt in &proc.statements {
            // Skip Paragraph statements themselves (they only have names, no variable uses)
            if !matches!(stmt.node, Statement::Paragraph(_)) {
                self.collect_statement_uses(stmt, used);
            }
        }

        // Collect from paragraphs
        for para in &proc.paragraphs {
            for stmt in &para.node.statements {
                self.collect_statement_uses(stmt, used);
            }
        }

        // Collect from sections
        for section in &proc.sections {
            for para in &section.node.paragraphs {
                for stmt in &para.node.statements {
                    self.collect_statement_uses(stmt, used);
                }
            }
        }
    }

    fn collect_statement_uses(
        &self,
        stmt: &cobol_ast::Spanned<Statement>,
        used: &mut HashSet<String>,
    ) {
        match &stmt.node {
            Statement::Display(display) => {
                for operand in &display.node.operands {
                    if let cobol_ast::DisplayOperand::Identifier(id) = &operand.node {
                        used.insert(id.clone());
                        used.insert(id.to_uppercase());
                    }
                }
            }
            Statement::Move(move_stmt) => {
                if let cobol_ast::MoveSource::Identifier(id) = &move_stmt.node.from.node {
                    used.insert(id.clone());
                    used.insert(id.to_uppercase());
                }
                for to in &move_stmt.node.to {
                    used.insert(to.node.clone());
                    used.insert(to.node.to_uppercase());
                }
            }
            Statement::Compute(compute) => {
                for target in &compute.node.targets {
                    used.insert(target.node.clone());
                    used.insert(target.node.to_uppercase());
                }
                // TODO: Collect from expression
            }
            Statement::If(if_stmt) => {
                // TODO: Collect from condition expression
                for then_stmt in &if_stmt.node.then_statements {
                    self.collect_statement_uses(then_stmt, used);
                }
                if let Some(else_stmts) = &if_stmt.node.else_statements {
                    for else_stmt in else_stmts {
                        self.collect_statement_uses(else_stmt, used);
                    }
                }
            }
            Statement::Perform(perform) => match &perform.node {
                cobol_ast::PerformStatement::Simple { paragraph, .. } => {
                    used.insert(paragraph.clone());
                    used.insert(paragraph.to_uppercase());
                }
                _ => {}
            },
            Statement::GoTo(goto) => {
                if let Some(paragraph) = &goto.node.paragraph {
                    used.insert(paragraph.clone());
                    used.insert(paragraph.to_uppercase());
                }
            }
            _ => {}
        }
    }

    /// Find unused paragraphs.
    fn find_unused_paragraphs(&self) -> Vec<String> {
        let mut unused = Vec::new();
        let mut used_paragraphs = HashSet::new();
        let mut defined_paragraphs = Vec::new();

        // Collect paragraph references from PERFORM and GO TO
        self.collect_paragraph_references(&self.program.procedure.node, &mut used_paragraphs);

        // Collect paragraphs from paragraphs vector
        for (idx, para) in self.program.procedure.node.paragraphs.iter().enumerate() {
            defined_paragraphs.push((idx, para.node.name.clone()));
        }

        // Also collect paragraphs from Statement::Paragraph in statements
        for stmt in &self.program.procedure.node.statements {
            if let Statement::Paragraph(para_stmt) = &stmt.node {
                defined_paragraphs.push((usize::MAX, para_stmt.node.name.clone()));
            }
        }

        // Check which paragraphs are defined but not used
        for (idx, para_name) in defined_paragraphs {
            let name_upper = para_name.to_uppercase();
            if !used_paragraphs.contains(&para_name) && !used_paragraphs.contains(&name_upper) {
                // Check if it's an entry point (first paragraph)
                if idx == 0 {
                    continue; // Entry point is always "used"
                }
                unused.push(para_name);
            }
        }

        unused
    }

    /// Find unused sections.
    fn find_unused_sections(&self) -> Vec<String> {
        let mut unused = Vec::new();
        let mut used_sections = HashSet::new();

        // Collect section references
        // Sections are typically called via PERFORM
        self.collect_section_references(&self.program.procedure.node, &mut used_sections);

        // Check which sections are defined but not used
        for (idx, section) in self.program.procedure.node.sections.iter().enumerate() {
            let name_upper = section.node.name.to_uppercase();
            if !used_sections.contains(&section.node.name) && !used_sections.contains(&name_upper) {
                // Check if it's an entry point (first section)
                if idx == 0 {
                    continue; // Entry point is always "used"
                }
                unused.push(section.node.name.clone());
            }
        }

        unused
    }

    fn collect_paragraph_references(&self, proc: &ProcedureDivision, used: &mut HashSet<String>) {
        // Collect from top-level statements
        for stmt in &proc.statements {
            self.collect_paragraph_refs_from_stmt(stmt, used);
        }

        // Collect from paragraphs vector (recurse into their statements)
        for para in &proc.paragraphs {
            for stmt in &para.node.statements {
                self.collect_paragraph_refs_from_stmt(stmt, used);
            }
        }

        // Collect from sections
        for section in &proc.sections {
            for para in &section.node.paragraphs {
                for stmt in &para.node.statements {
                    self.collect_paragraph_refs_from_stmt(stmt, used);
                }
            }
        }

        // Also collect from Statement::Paragraph nodes (recurse into following statements)
        // Note: ParagraphStatement only contains the name, statements follow in the statements vector
        // We need to collect references from statements that follow a paragraph statement
        let mut i = 0;
        while i < proc.statements.len() {
            if let Statement::Paragraph(_) = &proc.statements[i].node {
                // Collect from statements following this paragraph
                i += 1;
                while i < proc.statements.len() {
                    if let Statement::Paragraph(_) = &proc.statements[i].node {
                        break; // Hit next paragraph, stop
                    }
                    self.collect_paragraph_refs_from_stmt(&proc.statements[i], used);
                    i += 1;
                }
            } else {
                i += 1;
            }
        }
    }

    fn collect_paragraph_refs_from_stmt(
        &self,
        stmt: &cobol_ast::Spanned<Statement>,
        used: &mut HashSet<String>,
    ) {
        match &stmt.node {
            Statement::Perform(perform) => match &perform.node {
                cobol_ast::PerformStatement::Simple { paragraph, through } => {
                    used.insert(paragraph.clone());
                    used.insert(paragraph.to_uppercase());
                    if let Some(thru) = through {
                        used.insert(thru.clone());
                        used.insert(thru.to_uppercase());
                    }
                }
                _ => {}
            },
            Statement::GoTo(goto) => {
                if let Some(paragraph) = &goto.node.paragraph {
                    used.insert(paragraph.clone());
                    used.insert(paragraph.to_uppercase());
                }
            }
            Statement::If(if_stmt) => {
                for then_stmt in &if_stmt.node.then_statements {
                    self.collect_paragraph_refs_from_stmt(then_stmt, used);
                }
                if let Some(else_stmts) = &if_stmt.node.else_statements {
                    for else_stmt in else_stmts {
                        self.collect_paragraph_refs_from_stmt(else_stmt, used);
                    }
                }
            }
            _ => {}
        }
    }

    fn collect_section_references(&self, proc: &ProcedureDivision, used: &mut HashSet<String>) {
        // Similar to paragraph references, but for sections
        // Sections are less commonly referenced directly in COBOL
        // This is a simplified implementation
        self.collect_paragraph_references(proc, used);
    }

    /// Find unreachable statements using CFG.
    fn find_unreachable_statements(&self) -> Vec<UnreachableStatement> {
        let mut unreachable = Vec::new();
        let unreachable_nodes = self.cfg.find_unreachable_nodes();

        for node in unreachable_nodes {
            match node {
                crate::cfg::CfgNode::Statement(idx) => {
                    if idx < self.program.procedure.node.statements.len() {
                        unreachable.push(UnreachableStatement {
                            statement_type: format!(
                                "{:?}",
                                self.program.procedure.node.statements[idx].node
                            ),
                            location: format!("Statement index {}", idx),
                        });
                    }
                }
                crate::cfg::CfgNode::Paragraph(name) => {
                    unreachable.push(UnreachableStatement {
                        statement_type: "Paragraph".to_string(),
                        location: format!("Paragraph: {}", name),
                    });
                }
                crate::cfg::CfgNode::Section(name) => {
                    unreachable.push(UnreachableStatement {
                        statement_type: "Section".to_string(),
                        location: format!("Section: {}", name),
                    });
                }
                _ => {}
            }
        }

        unreachable
    }
}
