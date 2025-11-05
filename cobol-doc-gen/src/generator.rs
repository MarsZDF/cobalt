use crate::analyzer::{CobolAnalyzer, AnalyzerConfig};
use crate::formats::OutputFormat;
use crate::models::Documentation;
use crate::templates::TemplateEngine;
use anyhow::Result;
use cobol_ast::Program;

/// Configuration for documentation generation.
#[derive(Debug, Clone)]
pub struct GeneratorConfig {
    pub analyzer_config: AnalyzerConfig,
    pub template_dir: Option<String>,
    pub custom_css: Option<String>,
    pub include_source_code: bool,
    pub include_complexity_metrics: bool,
    pub include_cross_references: bool,
}

impl Default for GeneratorConfig {
    fn default() -> Self {
        Self {
            analyzer_config: AnalyzerConfig::default(),
            template_dir: None,
            custom_css: None,
            include_source_code: false,
            include_complexity_metrics: true,
            include_cross_references: true,
        }
    }
}

/// Main documentation generator.
pub struct DocumentGenerator {
    config: GeneratorConfig,
    analyzer: CobolAnalyzer,
    template_engine: TemplateEngine,
}

impl DocumentGenerator {
    /// Create a new document generator with the given configuration.
    pub fn new(config: GeneratorConfig) -> Self {
        let analyzer = CobolAnalyzer::new(config.analyzer_config.clone());
        let template_engine = TemplateEngine::new(config.template_dir.clone());
        
        Self {
            config,
            analyzer,
            template_engine,
        }
    }

    /// Generate documentation for a COBOL program in the specified format.
    pub fn generate(&self, program: &Program, format: OutputFormat) -> Result<String> {
        // Step 1: Analyze the program
        let documentation = self.analyzer.analyze(program)?;

        // Step 2: Generate output in the requested format
        match format {
            OutputFormat::Html => self.generate_html(&documentation),
            OutputFormat::Markdown => self.generate_markdown(&documentation),
            OutputFormat::Json => self.generate_json(&documentation),
            OutputFormat::Pdf => self.generate_pdf(&documentation),
            OutputFormat::Word => self.generate_word(&documentation),
        }
    }

    /// Generate documentation for multiple programs (e.g., a complete system).
    pub fn generate_system_documentation(
        &self,
        programs: &[Program],
        format: OutputFormat,
    ) -> Result<String> {
        let mut all_docs = Vec::new();
        
        for program in programs {
            let doc = self.analyzer.analyze(program)?;
            all_docs.push(doc);
        }

        match format {
            OutputFormat::Html => self.generate_system_html(&all_docs),
            OutputFormat::Markdown => self.generate_system_markdown(&all_docs),
            OutputFormat::Json => self.generate_system_json(&all_docs),
            _ => Err(anyhow::anyhow!("System documentation not supported for {}", format)),
        }
    }

    fn generate_html(&self, documentation: &Documentation) -> Result<String> {
        let context = self.create_template_context(documentation)?;
        self.template_engine.render("program.html", &context)
    }

    fn generate_markdown(&self, documentation: &Documentation) -> Result<String> {
        let context = self.create_template_context(documentation)?;
        self.template_engine.render("program.md", &context)
    }

    fn generate_json(&self, documentation: &Documentation) -> Result<String> {
        serde_json::to_string_pretty(documentation)
            .map_err(|e| anyhow::anyhow!("Failed to serialize documentation: {}", e))
    }

    fn generate_pdf(&self, documentation: &Documentation) -> Result<String> {
        // For PDF generation, we would typically:
        // 1. Generate HTML first
        // 2. Use a PDF library (like wkhtmltopdf or headless Chrome) to convert
        // For now, return HTML with a note
        let html = self.generate_html(documentation)?;
        Ok(format!("<!-- PDF generation not yet implemented. This is HTML content: -->\n{}", html))
    }

    fn generate_word(&self, documentation: &Documentation) -> Result<String> {
        // For Word generation, we would typically use a library like docx-rs
        // For now, return Markdown with a note
        let markdown = self.generate_markdown(documentation)?;
        Ok(format!("<!-- Word generation not yet implemented. This is Markdown content: -->\n{}", markdown))
    }

    fn generate_system_html(&self, docs: &[Documentation]) -> Result<String> {
        let context = serde_json::json!({
            "programs": docs,
            "system_summary": self.create_system_summary(docs),
            "include_complexity_metrics": self.config.include_complexity_metrics,
            "include_cross_references": self.config.include_cross_references,
        });
        self.template_engine.render("system.html", &context)
    }

    fn generate_system_markdown(&self, docs: &[Documentation]) -> Result<String> {
        let context = serde_json::json!({
            "programs": docs,
            "system_summary": self.create_system_summary(docs),
        });
        self.template_engine.render("system.md", &context)
    }

    fn generate_system_json(&self, docs: &[Documentation]) -> Result<String> {
        let system_doc = serde_json::json!({
            "programs": docs,
            "system_summary": self.create_system_summary(docs),
            "generated_at": chrono::Utc::now().to_rfc3339(),
        });
        serde_json::to_string_pretty(&system_doc)
            .map_err(|e| anyhow::anyhow!("Failed to serialize system documentation: {}", e))
    }

    fn create_template_context(&self, documentation: &Documentation) -> Result<serde_json::Value> {
        Ok(serde_json::json!({
            "program": documentation,
            "config": {
                "include_source_code": self.config.include_source_code,
                "include_complexity_metrics": self.config.include_complexity_metrics,
                "include_cross_references": self.config.include_cross_references,
                "custom_css": self.config.custom_css,
            },
            "generated_at": chrono::Utc::now().to_rfc3339(),
            "generator_version": env!("CARGO_PKG_VERSION"),
        }))
    }

    fn create_system_summary(&self, docs: &[Documentation]) -> serde_json::Value {
        let total_programs = docs.len();
        let total_lines: usize = docs.iter().map(|d| d.program_summary.total_lines).sum();
        let total_data_items: usize = docs.iter().map(|d| d.data_structures.len()).sum();
        let total_business_rules: usize = docs.iter().map(|d| d.business_rules.len()).sum();
        let avg_complexity: f64 = docs.iter()
            .map(|d| d.complexity_metrics.cyclomatic_complexity as f64)
            .sum::<f64>() / docs.len() as f64;

        serde_json::json!({
            "total_programs": total_programs,
            "total_lines_of_code": total_lines,
            "total_data_items": total_data_items,
            "total_business_rules": total_business_rules,
            "average_complexity": avg_complexity,
            "most_complex_program": docs.iter()
                .max_by_key(|d| d.complexity_metrics.cyclomatic_complexity)
                .map(|d| &d.program_summary.program_id),
        })
    }
}