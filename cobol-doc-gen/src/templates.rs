use handlebars::{Handlebars, RenderError};
use serde_json::Value;
use std::collections::HashMap;

/// Template engine for generating documentation in various formats.
pub struct TemplateEngine {
    handlebars: Handlebars<'static>,
    template_dir: Option<String>,
}

impl TemplateEngine {
    pub fn new(template_dir: Option<String>) -> Self {
        let mut handlebars = Handlebars::new();
        
        // Register built-in templates
        if let Err(e) = handlebars.register_template_string("program.html", PROGRAM_HTML_TEMPLATE) {
            eprintln!("Warning: Failed to register HTML template: {}", e);
        }
        if let Err(e) = handlebars.register_template_string("program.md", PROGRAM_MARKDOWN_TEMPLATE) {
            eprintln!("Warning: Failed to register Markdown template: {}", e);
        }
        if let Err(e) = handlebars.register_template_string("system.html", SYSTEM_HTML_TEMPLATE) {
            eprintln!("Warning: Failed to register system HTML template: {}", e);
        }
        if let Err(e) = handlebars.register_template_string("system.md", SYSTEM_MARKDOWN_TEMPLATE) {
            eprintln!("Warning: Failed to register system Markdown template: {}", e);
        }

        // Register custom helpers
        handlebars.register_helper("format_complexity", Box::new(format_complexity_helper));
        handlebars.register_helper("format_percentage", Box::new(format_percentage_helper));
        handlebars.register_helper("format_date", Box::new(format_date_helper));

        Self {
            handlebars,
            template_dir,
        }
    }

    pub fn render(&self, template_name: &str, context: &Value) -> Result<String, RenderError> {
        self.handlebars.render(template_name, context)
    }

    pub fn load_custom_templates(&mut self, templates: HashMap<String, String>) -> Result<(), RenderError> {
        for (name, content) in templates {
            self.handlebars.register_template_string(&name, &content)?;
        }
        Ok(())
    }
}

// Helper functions for templates
fn format_complexity_helper(
    h: &handlebars::Helper,
    _: &Handlebars,
    _: &handlebars::Context,
    _: &mut handlebars::RenderContext,
    out: &mut dyn handlebars::Output,
) -> handlebars::HelperResult {
    let complexity = h.param(0)
        .and_then(|v| v.value().as_u64())
        .unwrap_or(0);
    
    let level = match complexity {
        1..=5 => "Low",
        6..=10 => "Medium",
        11..=20 => "High",
        _ => "Very High",
    };
    
    out.write(&format!("{} ({})", complexity, level))?;
    Ok(())
}

fn format_percentage_helper(
    h: &handlebars::Helper,
    _: &Handlebars,
    _: &handlebars::Context,
    _: &mut handlebars::RenderContext,
    out: &mut dyn handlebars::Output,
) -> handlebars::HelperResult {
    let value = h.param(0)
        .and_then(|v| v.value().as_f64())
        .unwrap_or(0.0);
    
    out.write(&format!("{:.1}%", value * 100.0))?;
    Ok(())
}

fn format_date_helper(
    h: &handlebars::Helper,
    _: &Handlebars,
    _: &handlebars::Context,
    _: &mut handlebars::RenderContext,
    out: &mut dyn handlebars::Output,
) -> handlebars::HelperResult {
    let date_str = h.param(0)
        .and_then(|v| v.value().as_str())
        .unwrap_or("");
    
    // Simple date formatting - in practice, you'd use chrono for proper parsing
    out.write(date_str)?;
    Ok(())
}

// Built-in template definitions
const PROGRAM_HTML_TEMPLATE: &str = r#"
<!DOCTYPE html>
<html lang="en">
<head>
    <meta charset="UTF-8">
    <meta name="viewport" content="width=device-width, initial-scale=1.0">
    <title>COBOL Program Documentation - {{program.program_summary.program_id}}</title>
    <style>
        body { font-family: Arial, sans-serif; margin: 40px; line-height: 1.6; }
        .header { border-bottom: 2px solid #333; padding-bottom: 20px; margin-bottom: 30px; }
        .section { margin-bottom: 40px; }
        .section h2 { color: #333; border-bottom: 1px solid #ccc; padding-bottom: 10px; }
        .data-structure { margin: 10px 0; padding: 10px; background: #f9f9f9; border-left: 4px solid #007acc; }
        .business-rule { margin: 10px 0; padding: 15px; background: #fff3cd; border: 1px solid #ffeaa7; border-radius: 4px; }
        .complexity-high { color: #e74c3c; font-weight: bold; }
        .complexity-medium { color: #f39c12; font-weight: bold; }
        .complexity-low { color: #27ae60; font-weight: bold; }
        table { width: 100%; border-collapse: collapse; margin: 20px 0; }
        th, td { border: 1px solid #ddd; padding: 12px; text-align: left; }
        th { background-color: #f2f2f2; }
        {{#if config.custom_css}}
        {{config.custom_css}}
        {{/if}}
    </style>
</head>
<body>
    <div class="header">
        <h1>COBOL Program Documentation</h1>
        <h2>{{program.program_summary.program_id}}</h2>
        {{#if program.program_summary.purpose}}
        <p><strong>Purpose:</strong> {{program.program_summary.purpose}}</p>
        {{/if}}
        <p><strong>Generated:</strong> {{generated_at}}</p>
    </div>

    <div class="section">
        <h2>Program Summary</h2>
        <table>
            <tr><td><strong>Program ID</strong></td><td>{{program.program_summary.program_id}}</td></tr>
            {{#if program.program_summary.author}}
            <tr><td><strong>Author</strong></td><td>{{program.program_summary.author}}</td></tr>
            {{/if}}
            {{#if program.program_summary.date_written}}
            <tr><td><strong>Date Written</strong></td><td>{{format_date program.program_summary.date_written}}</td></tr>
            {{/if}}
            <tr><td><strong>Lines of Code</strong></td><td>{{program.program_summary.total_lines}}</td></tr>
            <tr><td><strong>Divisions</strong></td><td>{{#each program.program_summary.divisions}}{{this}}{{#unless @last}}, {{/unless}}{{/each}}</td></tr>
        </table>
    </div>

    {{#if program.data_structures}}
    <div class="section">
        <h2>Data Structures</h2>
        {{#each program.data_structures}}
        <div class="data-structure">
            <h4>{{name}} (Level {{level}})</h4>
            {{#if description}}<p>{{description}}</p>{{/if}}
            {{#if picture}}<p><strong>Picture:</strong> {{picture}}</p>{{/if}}
            {{#if usage}}<p><strong>Usage:</strong> {{usage}}</p>{{/if}}
            {{#if initial_value}}<p><strong>Initial Value:</strong> {{initial_value}}</p>{{/if}}
        </div>
        {{/each}}
    </div>
    {{/if}}

    {{#if program.business_rules}}
    <div class="section">
        <h2>Business Rules</h2>
        {{#each program.business_rules}}
        <div class="business-rule">
            <h4>{{id}}: {{description}}</h4>
            <p><strong>Type:</strong> {{rule_type}}</p>
            <p><strong>Complexity:</strong> <span class="complexity-{{complexity}}">{{complexity}}</span></p>
            {{#if conditions}}
            <p><strong>Conditions:</strong></p>
            <ul>{{#each conditions}}<li>{{this}}</li>{{/each}}</ul>
            {{/if}}
            {{#if actions}}
            <p><strong>Actions:</strong></p>
            <ul>{{#each actions}}<li>{{this}}</li>{{/each}}</ul>
            {{/if}}
        </div>
        {{/each}}
    </div>
    {{/if}}

    {{#if config.include_complexity_metrics}}
    <div class="section">
        <h2>Complexity Metrics</h2>
        <table>
            <tr><td><strong>Cyclomatic Complexity</strong></td><td>{{format_complexity program.complexity_metrics.cyclomatic_complexity}}</td></tr>
            <tr><td><strong>Max Nesting Depth</strong></td><td>{{program.complexity_metrics.nesting_depth}}</td></tr>
            <tr><td><strong>Comment Ratio</strong></td><td>{{format_percentage program.complexity_metrics.comment_ratio}}</td></tr>
            <tr><td><strong>Maintainability Index</strong></td><td>{{program.complexity_metrics.maintainability_index}}</td></tr>
            <tr><td><strong>Technical Debt</strong></td><td>{{program.complexity_metrics.technical_debt_minutes}} minutes</td></tr>
        </table>
    </div>
    {{/if}}
</body>
</html>
"#;

const PROGRAM_MARKDOWN_TEMPLATE: &str = r#"
# COBOL Program Documentation: {{program.program_summary.program_id}}

{{#if program.program_summary.purpose}}
**Purpose:** {{program.program_summary.purpose}}
{{/if}}

**Generated:** {{generated_at}}

## Program Summary

| Property | Value |
|----------|-------|
| Program ID | {{program.program_summary.program_id}} |
{{#if program.program_summary.author}}
| Author | {{program.program_summary.author}} |
{{/if}}
{{#if program.program_summary.date_written}}
| Date Written | {{program.program_summary.date_written}} |
{{/if}}
| Lines of Code | {{program.program_summary.total_lines}} |
| Divisions | {{#each program.program_summary.divisions}}{{this}}{{#unless @last}}, {{/unless}}{{/each}} |

{{#if program.data_structures}}
## Data Structures

{{#each program.data_structures}}
### {{name}} (Level {{level}})

{{#if description}}
{{description}}
{{/if}}

{{#if picture}}
- **Picture:** `{{picture}}`
{{/if}}
{{#if usage}}
- **Usage:** {{usage}}
{{/if}}
{{#if initial_value}}
- **Initial Value:** {{initial_value}}
{{/if}}

{{/each}}
{{/if}}

{{#if program.business_rules}}
## Business Rules

{{#each program.business_rules}}
### {{id}}: {{description}}

- **Type:** {{rule_type}}
- **Complexity:** {{complexity}}

{{#if conditions}}
**Conditions:**
{{#each conditions}}
- {{this}}
{{/each}}
{{/if}}

{{#if actions}}
**Actions:**
{{#each actions}}
- {{this}}
{{/each}}
{{/if}}

---

{{/each}}
{{/if}}

{{#if config.include_complexity_metrics}}
## Complexity Metrics

| Metric | Value |
|--------|-------|
| Cyclomatic Complexity | {{program.complexity_metrics.cyclomatic_complexity}} |
| Max Nesting Depth | {{program.complexity_metrics.nesting_depth}} |
| Comment Ratio | {{format_percentage program.complexity_metrics.comment_ratio}} |
| Maintainability Index | {{program.complexity_metrics.maintainability_index}} |
| Technical Debt | {{program.complexity_metrics.technical_debt_minutes}} minutes |
{{/if}}
"#;

const SYSTEM_HTML_TEMPLATE: &str = r#"
<!DOCTYPE html>
<html lang="en">
<head>
    <meta charset="UTF-8">
    <meta name="viewport" content="width=device-width, initial-scale=1.0">
    <title>COBOL System Documentation</title>
    <style>
        body { font-family: Arial, sans-serif; margin: 40px; line-height: 1.6; }
        .header { border-bottom: 2px solid #333; padding-bottom: 20px; margin-bottom: 30px; }
        .program-card { margin: 20px 0; padding: 20px; border: 1px solid #ddd; border-radius: 8px; }
        .system-stats { display: grid; grid-template-columns: repeat(auto-fit, minmax(200px, 1fr)); gap: 20px; }
        .stat-card { padding: 20px; background: #f8f9fa; border-radius: 8px; text-align: center; }
    </style>
</head>
<body>
    <div class="header">
        <h1>COBOL System Documentation</h1>
        <p>Complete system overview and program documentation</p>
    </div>

    <div class="system-stats">
        <div class="stat-card">
            <h3>{{system_summary.total_programs}}</h3>
            <p>Programs</p>
        </div>
        <div class="stat-card">
            <h3>{{system_summary.total_lines_of_code}}</h3>
            <p>Lines of Code</p>
        </div>
        <div class="stat-card">
            <h3>{{system_summary.total_business_rules}}</h3>
            <p>Business Rules</p>
        </div>
        <div class="stat-card">
            <h3>{{system_summary.average_complexity}}</h3>
            <p>Avg Complexity</p>
        </div>
    </div>

    <h2>Programs</h2>
    {{#each programs}}
    <div class="program-card">
        <h3>{{program_summary.program_id}}</h3>
        {{#if program_summary.purpose}}
        <p>{{program_summary.purpose}}</p>
        {{/if}}
        <p><strong>Lines:</strong> {{program_summary.total_lines}} | <strong>Complexity:</strong> {{complexity_metrics.cyclomatic_complexity}}</p>
    </div>
    {{/each}}
</body>
</html>
"#;

const SYSTEM_MARKDOWN_TEMPLATE: &str = r#"
# COBOL System Documentation

## System Overview

| Metric | Value |
|--------|-------|
| Total Programs | {{system_summary.total_programs}} |
| Total Lines of Code | {{system_summary.total_lines_of_code}} |
| Total Business Rules | {{system_summary.total_business_rules}} |
| Average Complexity | {{system_summary.average_complexity}} |

## Programs

{{#each programs}}
### {{program_summary.program_id}}

{{#if program_summary.purpose}}
{{program_summary.purpose}}
{{/if}}

- **Lines of Code:** {{program_summary.total_lines}}
- **Complexity:** {{complexity_metrics.cyclomatic_complexity}}
- **Business Rules:** {{business_rules.length}}

---

{{/each}}
"#;