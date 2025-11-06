use anyhow::{Context, Result};
use cobol_ast::{Program, Spanned};
use cobol_lexer::{tokenize, Format};
use cobol_parser::parse_source;
use rustyline::error::ReadlineError;
use rustyline::DefaultEditor;
use std::collections::HashMap;

fn main() -> Result<()> {
    println!("Welcome to COBOL REPL!");
    println!("Type 'help' for commands, 'exit' to quit.\n");

    let mut rl = DefaultEditor::new()?;
    let mut loaded_programs: HashMap<String, Spanned<Program>> = HashMap::new();

    loop {
        let readline = rl.readline("cobol> ");
        match readline {
            Ok(line) => {
                rl.add_history_entry(line.as_str())?;
                let line = line.trim();

                if line.is_empty() {
                    continue;
                }

                if line == "exit" || line == "quit" {
                    break;
                }

                if line == "help" {
                    print_help();
                    continue;
                }

                #[allow(clippy::manual_strip)]
                if line.starts_with("parse ") {
                    let code = &line[6..];
                    handle_parse(&mut loaded_programs, code)?;
                    continue;
                }

                #[allow(clippy::manual_strip)]
                if line.starts_with("load ") {
                    let filename = &line[5..];
                    handle_load(&mut loaded_programs, filename)?;
                    continue;
                }

                if line.starts_with("list") {
                    handle_list(&loaded_programs);
                    continue;
                }

                #[allow(clippy::manual_strip)]
                if line.starts_with("show ") {
                    let name = &line[5..];
                    handle_show(&loaded_programs, name)?;
                    continue;
                }

                #[allow(clippy::manual_strip)]
                if line.starts_with("ast ") {
                    let name = &line[4..];
                    handle_ast(&loaded_programs, name)?;
                    continue;
                }

                #[allow(clippy::manual_strip)]
                if line.starts_with("tokens ") {
                    let code = &line[7..];
                    handle_tokens(code)?;
                    continue;
                }

                if line.starts_with("clear") {
                    loaded_programs.clear();
                    println!("Cleared all loaded programs.");
                    continue;
                }

                println!("Unknown command: {}. Type 'help' for available commands.", line);
            }
            Err(ReadlineError::Interrupted) => {
                println!("CTRL-C");
                break;
            }
            Err(ReadlineError::Eof) => {
                println!("CTRL-D");
                break;
            }
            Err(err) => {
                eprintln!("Error: {:?}", err);
                break;
            }
        }
    }

    Ok(())
}

fn print_help() {
    println!("\nCOBOL REPL Commands:");
    println!("  parse <code>     - Parse COBOL code and store it");
    println!("  load <file>      - Load and parse a COBOL file");
    println!("  list             - List all loaded programs");
    println!("  show <name>      - Show information about a loaded program");
    println!("  ast <name>       - Show AST of a loaded program");
    println!("  tokens <code>    - Tokenize COBOL code");
    println!("  clear            - Clear all loaded programs");
    println!("  help             - Show this help message");
    println!("  exit/quit        - Exit the REPL");
    println!();
}

fn handle_parse(
    loaded_programs: &mut HashMap<String, Spanned<Program>>,
    code: &str,
) -> Result<()> {
    let format = cobol_lexer::detect_format(code);
    let program = parse_source(code, format)
        .with_context(|| "Failed to parse COBOL code")?;

    let program_id = program
        .node
        .identification
        .node
        .program_id
        .clone()
        .unwrap_or_else(|| "UNNAMED".to_string());

    loaded_programs.insert(program_id.clone(), program);
    println!("✓ Parsed and stored program: {}", program_id);
    Ok(())
}

fn handle_load(
    loaded_programs: &mut HashMap<String, Spanned<Program>>,
    filename: &str,
) -> Result<()> {
    use std::fs;

    let contents = fs::read_to_string(filename)
        .with_context(|| format!("Failed to read file: {}", filename))?;

    let format = cobol_lexer::detect_format(&contents);
    let program = parse_source(&contents, format)
        .with_context(|| format!("Failed to parse file: {}", filename))?;

    let program_id = program
        .node
        .identification
        .node
        .program_id
        .clone()
        .unwrap_or_else(|| "UNNAMED".to_string());

    loaded_programs.insert(program_id.clone(), program);
    println!("✓ Loaded and stored program: {} from {}", program_id, filename);
    Ok(())
}

fn handle_list(loaded_programs: &HashMap<String, Spanned<Program>>) {
    if loaded_programs.is_empty() {
        println!("No programs loaded.");
        return;
    }

    println!("\nLoaded programs:");
    for (name, _) in loaded_programs {
        println!("  - {}", name);
    }
    println!();
}

fn handle_show(
    loaded_programs: &HashMap<String, Spanned<Program>>,
    name: &str,
) -> Result<()> {
    let program = loaded_programs
        .get(name)
        .ok_or_else(|| anyhow::anyhow!("Program not found: {}", name))?;

    println!("\nProgram: {}", name);
    println!("  Identification Division: ✓");
    println!(
        "    Program ID: {:?}",
        program.node.identification.node.program_id
    );
    println!("    Author: {:?}", program.node.identification.node.author);

    if let Some(env) = &program.node.environment {
        println!("  Environment Division: ✓");
        if let Some(config) = &env.node.configuration_section {
            println!("    Configuration Section: ✓");
            if let Some(source) = &config.node.source_computer {
                println!("      Source Computer: {}", source);
            }
            if let Some(object) = &config.node.object_computer {
                println!("      Object Computer: {}", object);
            }
        }
    }

    if let Some(data) = &program.node.data {
        println!("  Data Division: ✓");
        if let Some(ws) = &data.node.working_storage_section {
            println!(
                "    Working Storage Section: {} data items",
                ws.node.data_items.len()
            );
        }
        if let Some(ls) = &data.node.local_storage_section {
            println!(
                "    Local Storage Section: {} data items",
                ls.node.data_items.len()
            );
        }
        if let Some(link) = &data.node.linkage_section {
            println!(
                "    Linkage Section: {} data items",
                link.node.data_items.len()
            );
        }
    }

    println!("  Procedure Division: ✓");
    println!(
        "    Statements: {}",
        program.node.procedure.node.statements.len()
    );
    println!(
        "    Paragraphs: {}",
        program.node.procedure.node.paragraphs.len()
    );
    println!(
        "    Sections: {}",
        program.node.procedure.node.sections.len()
    );
    println!();

    Ok(())
}

fn handle_ast(
    loaded_programs: &HashMap<String, Spanned<Program>>,
    name: &str,
) -> Result<()> {
    let program = loaded_programs
        .get(name)
        .ok_or_else(|| anyhow::anyhow!("Program not found: {}", name))?;

    println!("\nAST for program: {}", name);
    println!("{:#?}", program.node);
    println!();

    Ok(())
}

fn handle_tokens(code: &str) -> Result<()> {
    let format = cobol_lexer::detect_format(code);
    let tokens = tokenize(code, format)?;

    println!("\nTokens:");
    for token in tokens {
        if !matches!(token.token_type, cobol_lexer::TokenType::Whitespace(_)) {
            println!(
                "  {:?} at line {}:{} - '{}'",
                token.token_type, token.line, token.column, token.lexeme
            );
        }
    }
    println!();

    Ok(())
}
