use clap::{Parser, ValueEnum};
use cobol_doc_gen::{DocumentGenerator, GeneratorConfig, OutputFormat};
use std::fs;
use std::path::PathBuf;
use anyhow::Result;

#[derive(Parser)]
#[command(name = "cobol-doc")]
#[command(about = "Generate documentation from COBOL programs")]
#[command(version = env!("CARGO_PKG_VERSION"))]
struct Cli {
    /// Input COBOL file or directory
    #[arg(short, long)]
    input: PathBuf,

    /// Output file (if not specified, writes to stdout)
    #[arg(short, long)]
    output: Option<PathBuf>,

    /// Output format
    #[arg(short, long, default_value = "html")]
    format: OutputFormat,

    /// Custom template directory
    #[arg(short, long)]
    template_dir: Option<String>,

    /// Include source code in documentation
    #[arg(long)]
    include_source: bool,

    /// Include complexity metrics
    #[arg(long, default_value = "true")]
    include_metrics: bool,

    /// Include cross-references
    #[arg(long, default_value = "true")]
    include_references: bool,

    /// Generate system documentation for multiple programs
    #[arg(long)]
    system_mode: bool,
}

fn main() -> Result<()> {
    let cli = Cli::parse();

    // Configure the generator
    let config = GeneratorConfig {
        template_dir: cli.template_dir,
        include_source_code: cli.include_source,
        include_complexity_metrics: cli.include_metrics,
        include_cross_references: cli.include_references,
        ..Default::default()
    };

    let generator = DocumentGenerator::new(config);

    if cli.system_mode {
        generate_system_documentation(&generator, &cli)?;
    } else {
        generate_single_program_documentation(&generator, &cli)?;
    }

    Ok(())
}

fn generate_single_program_documentation(generator: &DocumentGenerator, cli: &Cli) -> Result<()> {
    // For now, we'll create a mock program since we don't have a parser yet
    let program = create_mock_program(&cli.input)?;
    
    let documentation = generator.generate(&program, cli.format)?;

    match &cli.output {
        Some(output_path) => {
            fs::write(output_path, documentation)?;
            println!("Documentation written to {}", output_path.display());
        }
        None => {
            println!("{}", documentation);
        }
    }

    Ok(())
}

fn generate_system_documentation(generator: &DocumentGenerator, cli: &Cli) -> Result<()> {
    // For system mode, we'd typically scan a directory for COBOL files
    // For now, create a mock system with multiple programs
    let programs = vec![
        create_mock_program(&cli.input)?,
        create_mock_program(&cli.input)?, // Duplicate for demo
    ];
    
    let documentation = generator.generate_system_documentation(&programs, cli.format)?;

    match &cli.output {
        Some(output_path) => {
            fs::write(output_path, documentation)?;
            println!("System documentation written to {}", output_path.display());
        }
        None => {
            println!("{}", documentation);
        }
    }

    Ok(())
}

// Mock program creation - in reality, this would use the cobol-parser
fn create_mock_program(_input_path: &PathBuf) -> Result<cobol_ast::Program> {
    use cobol_ast::*;

    // Create a mock program structure
    // This is just for demonstration - real implementation would parse the COBOL file
    Ok(Program {
        program_id: Some("SAMPLE-PROGRAM".to_string()),
        author: Some("COBOL Developer".to_string()),
        date_written: Some("2024-01-15".to_string()),
        remarks: Some("Sample program for documentation generation".to_string()),
        identification_division: None,
        environment_division: None,
        data_division: None,
        procedure_division: None,
    })
}

// Add this to the cobol_ast module for the mock
mod cobol_ast {
    use serde::{Deserialize, Serialize};

    #[derive(Debug, Clone, Serialize, Deserialize)]
    pub struct Program {
        pub program_id: Option<String>,
        pub author: Option<String>,
        pub date_written: Option<String>,
        pub remarks: Option<String>,
        pub identification_division: Option<()>,
        pub environment_division: Option<()>,
        pub data_division: Option<DataDivision>,
        pub procedure_division: Option<ProcedureDivision>,
    }

    #[derive(Debug, Clone, Serialize, Deserialize)]
    pub struct DataDivision {
        pub working_storage: Vec<()>,
        pub file_section: Vec<()>,
        pub linkage_section: Vec<()>,
    }

    #[derive(Debug, Clone, Serialize, Deserialize)]
    pub struct ProcedureDivision {
        pub statements: Vec<()>,
    }
}