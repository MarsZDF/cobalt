use clap::{Parser, ValueEnum};
use std::fs;
use std::path::PathBuf;
use anyhow::{Result, Context};
use cobol_fmt::{format_source, FormatConfig, KeywordCase, IdentifierCase};
use cobol_lexer::detect_format;

#[derive(Parser)]
#[command(name = "cobol-fmt")]
#[command(about = "Auto-formatter for COBOL source code", long_about = None)]
struct Cli {
    /// Input COBOL file(s) to format
    #[arg(required = true)]
    files: Vec<PathBuf>,
    
    /// Write formatted output back to files (instead of stdout)
    #[arg(short, long)]
    write: bool,
    
    /// Check if files are formatted (exit with non-zero if not)
    #[arg(long)]
    check: bool,
    
    /// Indentation width in spaces
    #[arg(long, default_value = "4")]
    indent_width: usize,
    
    /// Use tabs instead of spaces
    #[arg(long)]
    use_tabs: bool,
    
    /// Keyword case style
    #[arg(long, value_enum, default_value = "upper")]
    keyword_case: CaseStyle,
    
    /// Identifier case style
    #[arg(long, value_enum, default_value = "preserve")]
    identifier_case: CaseStyle,
    
    /// Maximum line length
    #[arg(long, default_value = "80")]
    max_line_length: usize,
    
    /// Use traditional COBOL style (uppercase keywords, 4-space indent)
    #[arg(long)]
    traditional: bool,
    
    /// Use modern COBOL style (lowercase keywords, 2-space indent)
    #[arg(long)]
    modern: bool,
    
    /// Use fixed-format COBOL style (72-character line length)
    #[arg(long)]
    fixed_format: bool,
}

#[derive(Clone, ValueEnum)]
enum CaseStyle {
    Upper,
    Lower,
    Preserve,
}

impl From<CaseStyle> for KeywordCase {
    fn from(style: CaseStyle) -> Self {
        match style {
            CaseStyle::Upper => KeywordCase::Upper,
            CaseStyle::Lower => KeywordCase::Lower,
            CaseStyle::Preserve => KeywordCase::Preserve,
        }
    }
}

impl From<CaseStyle> for IdentifierCase {
    fn from(style: CaseStyle) -> Self {
        match style {
            CaseStyle::Upper => IdentifierCase::Upper,
            CaseStyle::Lower => IdentifierCase::Lower,
            CaseStyle::Preserve => IdentifierCase::Preserve,
        }
    }
}

fn main() -> Result<()> {
    let cli = Cli::parse();
    
    // Determine configuration
    let config = if cli.fixed_format {
        FormatConfig::fixed_format()
    } else if cli.traditional {
        FormatConfig::traditional()
    } else if cli.modern {
        FormatConfig::modern()
    } else {
        FormatConfig {
            indent_width: cli.indent_width,
            use_spaces: !cli.use_tabs,
            keyword_case: cli.keyword_case.clone().into(),
            identifier_case: cli.identifier_case.clone().into(),
            max_line_length: cli.max_line_length,
            ..Default::default()
        }
    };
    
    let mut has_errors = false;
    
    for file in &cli.files {
        match format_file(file, &config, cli.write, cli.check) {
            Ok(changed) => {
                if cli.check && changed {
                    eprintln!("❌ {} is not properly formatted", file.display());
                    has_errors = true;
                } else if cli.write && changed {
                    println!("✅ Formatted {}", file.display());
                } else if !cli.write {
                    // Print to stdout
                    let source = fs::read_to_string(file)
                        .with_context(|| format!("Failed to read file: {}", file.display()))?;
                    let format = detect_format(&source);
                    let formatted = format_source(&source, format, config.clone())
                        .with_context(|| format!("Failed to format file: {}", file.display()))?;
                    print!("{}", formatted);
                }
            }
            Err(e) => {
                eprintln!("Error formatting {}: {}", file.display(), e);
                has_errors = true;
            }
        }
    }
    
    if has_errors {
        std::process::exit(1);
    }
    
    Ok(())
}

fn format_file(
    file: &PathBuf,
    config: &FormatConfig,
    write: bool,
    check: bool,
) -> Result<bool> {
    let source = fs::read_to_string(file)
        .with_context(|| format!("Failed to read file: {}", file.display()))?;
    
    let format = detect_format(&source);
    let formatted = format_source(&source, format, config.clone())
        .with_context(|| format!("Failed to format file: {}", file.display()))?;
    
    let changed = source != formatted;
    
    if check {
        Ok(changed)
    } else if write && changed {
        fs::write(file, formatted)
            .with_context(|| format!("Failed to write file: {}", file.display()))?;
        Ok(true)
    } else {
        Ok(changed)
    }
}

