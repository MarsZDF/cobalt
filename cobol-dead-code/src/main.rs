use clap::Parser;
use std::fs;
use std::path::PathBuf;
use anyhow::{Result, Context};
use cobol_dead_code::analyze_program;
use cobol_parser::parse_source;
use cobol_lexer::detect_format;

#[derive(Parser)]
#[command(name = "cobol-dead-code")]
#[command(about = "Dead code detector for COBOL programs", long_about = None)]
struct Cli {
    /// Input COBOL file(s) to analyze
    #[arg(required = true)]
    files: Vec<PathBuf>,
    
    /// Output format (text, json)
    #[arg(short, long, default_value = "text")]
    format: String,
    
    /// Show only unused variables
    #[arg(long)]
    variables_only: bool,
    
    /// Show only unused paragraphs/sections
    #[arg(long)]
    procedures_only: bool,
    
    /// Show only unreachable code
    #[arg(long)]
    unreachable_only: bool,
}

fn main() -> Result<()> {
    let cli = Cli::parse();
    
    for file in &cli.files {
        match analyze_file(file, &cli) {
            Ok(_) => {}
            Err(e) => {
                eprintln!("Error analyzing {}: {}", file.display(), e);
            }
        }
    }
    
    Ok(())
}

fn analyze_file(file: &PathBuf, cli: &Cli) -> Result<()> {
    let source = fs::read_to_string(file)
        .with_context(|| format!("Failed to read file: {}", file.display()))?;
    
    let format = detect_format(&source);
    let program = parse_source(&source, format)
        .with_context(|| format!("Failed to parse file: {}", file.display()))?;
    
    let report = analyze_program(program.node);
    
    if cli.format == "json" {
        print_json_report(&report, file)?;
    } else {
        print_text_report(&report, file, cli)?;
    }
    
    Ok(())
}

fn print_text_report(report: &cobol_dead_code::DeadCodeReport, file: &PathBuf, cli: &Cli) -> Result<()> {
    println!("Dead Code Analysis: {}\n", file.display());
    println!("{}", "=".repeat(60));
    
    if !cli.procedures_only && !cli.unreachable_only {
        println!("\n📊 Unused Variables: {}", report.unused_variables.len());
        if !report.unused_variables.is_empty() {
            for var in &report.unused_variables {
                println!("  - {} (level {}) - {}", var.name, var.level, var.location);
            }
        }
    }
    
    if !cli.variables_only && !cli.unreachable_only {
        println!("\n📋 Unused Paragraphs: {}", report.unused_paragraphs.len());
        if !report.unused_paragraphs.is_empty() {
            for para in &report.unused_paragraphs {
                println!("  - {}", para);
            }
        }
        
        println!("\n📋 Unused Sections: {}", report.unused_sections.len());
        if !report.unused_sections.is_empty() {
            for section in &report.unused_sections {
                println!("  - {}", section);
            }
        }
    }
    
    if !cli.variables_only && !cli.procedures_only {
        println!("\n🚫 Unreachable Statements: {}", report.unreachable_statements.len());
        if !report.unreachable_statements.is_empty() {
            for stmt in &report.unreachable_statements {
                println!("  - {} - {}", stmt.statement_type, stmt.location);
            }
        }
    }
    
    println!("\n{}", "=".repeat(60));
    
    Ok(())
}

fn print_json_report(report: &cobol_dead_code::DeadCodeReport, file: &PathBuf) -> Result<()> {
    let json = serde_json::json!({
        "file": file.display().to_string(),
        "unused_variables": report.unused_variables,
        "unused_paragraphs": report.unused_paragraphs,
        "unused_sections": report.unused_sections,
        "unreachable_statements": report.unreachable_statements,
    });
    
    println!("{}", serde_json::to_string_pretty(&json)?);
    Ok(())
}

