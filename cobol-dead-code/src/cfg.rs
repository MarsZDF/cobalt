use cobol_ast::{ProcedureDivision, Program, Statement};
use std::collections::{HashMap, HashSet};

/// Control Flow Graph node.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum CfgNode {
    Entry,
    Statement(usize), // Index into statements vector
    Paragraph(String),
    Section(String),
    Exit,
}

/// Control Flow Graph edge.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct CfgEdge {
    pub from: CfgNode,
    pub to: CfgNode,
    pub edge_type: EdgeType,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum EdgeType {
    Sequential,
    Conditional, // IF/ELSE branches
    Jump,        // GO TO
    Call,        // PERFORM
    Return,      // EXIT, RETURN, STOP RUN
}

/// Control Flow Graph for a COBOL program.
pub struct ControlFlowGraph {
    pub nodes: Vec<CfgNode>,
    pub edges: Vec<CfgEdge>,
    pub entry_points: HashSet<CfgNode>,
    pub paragraph_map: HashMap<String, CfgNode>,
    pub section_map: HashMap<String, CfgNode>,
}

impl ControlFlowGraph {
    pub fn new() -> Self {
        Self {
            nodes: Vec::new(),
            edges: Vec::new(),
            entry_points: HashSet::new(),
            paragraph_map: HashMap::new(),
            section_map: HashMap::new(),
        }
    }

    /// Build CFG from a COBOL program.
    pub fn from_program(program: &Program) -> Self {
        let mut cfg = Self::new();

        // Add entry point
        let entry = CfgNode::Entry;
        cfg.nodes.push(entry.clone());
        cfg.entry_points.insert(entry.clone());

        // Build CFG for PROCEDURE DIVISION
        cfg.build_procedure_cfg(&program.procedure.node, &entry);

        cfg
    }

    fn build_procedure_cfg(&mut self, proc: &ProcedureDivision, entry: &CfgNode) {
        // Process paragraphs and sections first
        for (idx, para) in proc.paragraphs.iter().enumerate() {
            let para_node = CfgNode::Paragraph(para.node.name.clone());
            if !self.nodes.contains(&para_node) {
                self.nodes.push(para_node.clone());
            }
            self.paragraph_map
                .insert(para.node.name.clone(), para_node.clone());

            // Entry point for first paragraph
            if idx == 0 {
                self.entry_points.insert(para_node.clone());
                self.edges.push(CfgEdge {
                    from: entry.clone(),
                    to: para_node.clone(),
                    edge_type: EdgeType::Sequential,
                });
            }
        }

        for (idx, section) in proc.sections.iter().enumerate() {
            let section_node = CfgNode::Section(section.node.name.clone());
            if !self.nodes.contains(&section_node) {
                self.nodes.push(section_node.clone());
            }
            self.section_map
                .insert(section.node.name.clone(), section_node.clone());

            // Entry point for first section
            if idx == 0 && proc.paragraphs.is_empty() {
                self.entry_points.insert(section_node.clone());
                self.edges.push(CfgEdge {
                    from: entry.clone(),
                    to: section_node.clone(),
                    edge_type: EdgeType::Sequential,
                });
            }
        }

        // Process top-level statements
        if proc.paragraphs.is_empty() && proc.sections.is_empty() {
            self.build_statement_cfg(&proc.statements, entry);
        }
    }

    fn build_statement_cfg(
        &mut self,
        statements: &[cobol_ast::Spanned<Statement>],
        start: &CfgNode,
    ) {
        let mut current = start.clone();

        for (idx, stmt) in statements.iter().enumerate() {
            let stmt_node = CfgNode::Statement(idx);
            if !self.nodes.contains(&stmt_node) {
                self.nodes.push(stmt_node.clone());
            }

            // Add edge from current to this statement
            self.edges.push(CfgEdge {
                from: current.clone(),
                to: stmt_node.clone(),
                edge_type: EdgeType::Sequential,
            });

            // Handle control flow statements
            match &stmt.node {
                Statement::If(if_stmt) => {
                    // Then branch
                    let then_node = CfgNode::Statement(idx + 1000); // Use offset to avoid conflicts
                    self.nodes.push(then_node.clone());
                    self.edges.push(CfgEdge {
                        from: stmt_node.clone(),
                        to: then_node.clone(),
                        edge_type: EdgeType::Conditional,
                    });
                    self.build_statement_cfg(&if_stmt.node.then_statements, &then_node);

                    // Else branch (if present)
                    if let Some(else_stmts) = &if_stmt.node.else_statements {
                        let else_node = CfgNode::Statement(idx + 2000);
                        self.nodes.push(else_node.clone());
                        self.edges.push(CfgEdge {
                            from: stmt_node.clone(),
                            to: else_node.clone(),
                            edge_type: EdgeType::Conditional,
                        });
                        self.build_statement_cfg(else_stmts, &else_node);
                    }
                }
                Statement::Perform(perform) => {
                    match &perform.node {
                        cobol_ast::PerformStatement::Simple { paragraph, .. } => {
                            // PERFORM paragraph-name
                            if let Some(para_node) = self.paragraph_map.get(paragraph) {
                                self.edges.push(CfgEdge {
                                    from: stmt_node.clone(),
                                    to: para_node.clone(),
                                    edge_type: EdgeType::Call,
                                });
                            }
                        }
                        _ => {
                            // Other PERFORM types - handle later
                        }
                    }
                }
                Statement::GoTo(goto) => {
                    // GO TO paragraph-name
                    if let Some(paragraph) = &goto.node.paragraph {
                        if let Some(target) = self.paragraph_map.get(paragraph) {
                            self.edges.push(CfgEdge {
                                from: stmt_node.clone(),
                                to: target.clone(),
                                edge_type: EdgeType::Jump,
                            });
                        }
                    }
                }
                Statement::Stop(_) | Statement::Return(_) | Statement::Exit(_) => {
                    // Terminal statements
                    let exit = CfgNode::Exit;
                    if !self.nodes.contains(&exit) {
                        self.nodes.push(exit.clone());
                    }
                    self.edges.push(CfgEdge {
                        from: stmt_node.clone(),
                        to: exit,
                        edge_type: EdgeType::Return,
                    });
                    return; // Don't continue after terminal statement
                }
                _ => {}
            }

            current = stmt_node;
        }
    }

    /// Find all reachable nodes from entry points.
    pub fn find_reachable_nodes(&self) -> HashSet<CfgNode> {
        let mut reachable = HashSet::new();
        let mut to_visit: Vec<CfgNode> = self.entry_points.iter().cloned().collect();

        while let Some(node) = to_visit.pop() {
            if reachable.contains(&node) {
                continue;
            }

            reachable.insert(node.clone());

            // Find all edges from this node
            for edge in &self.edges {
                if edge.from == node {
                    to_visit.push(edge.to.clone());
                }
            }
        }

        reachable
    }

    /// Find unreachable nodes (dead code).
    pub fn find_unreachable_nodes(&self) -> HashSet<CfgNode> {
        let reachable = self.find_reachable_nodes();
        let all_nodes: HashSet<CfgNode> = self.nodes.iter().cloned().collect();

        all_nodes.difference(&reachable).cloned().collect()
    }
}

impl Default for ControlFlowGraph {
    fn default() -> Self {
        Self::new()
    }
}
