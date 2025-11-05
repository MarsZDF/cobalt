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
                    println!("Goodbye!");
                    break;
                }

                if line == "help" {
                    print_help();
                    continue;
                }

                if line.starts_with("parse ") {
                    let code = &line[6..];
                    handle_parse(&mut loaded_programs, code)?;
                    continue;
                }

                if line.starts_with("load ") {
                    let filename = &line[5..];
                    handle_load(&mut loaded_programs, filename)?;
                    continue;
                }

                if line.starts_with("list") {
                    handle_list(&loaded_programs);
                    continue;
                }

                if line.starts_with("show ") {
                    let name = &line[5..];
                    handle_show(&loaded_programs, name)?;
                    continue;
                }

                if line.starts_with("ast ") {
                    let name = &line[4..];
                    handle_ast(&loaded_programs, name)?;
                    continue;
                }

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

                // Try to parse as COBOL code directly
                handle_parse(&mut loaded_programs, line)?;
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
    println!("  help                  - Show this help message");
    println!("  parse <code>          - Parse COBOL code");
    println!("  load <file>           - Load COBOL program from file");
    println!("  list                  - List all loaded programs");
    println!("  show <name>           - Show program structure");
    println!("  ast <name>            - Show AST for program");
    println!("  tokens <code>         - Tokenize COBOL code");
    println!("  clear                 - Clear all loaded programs");
    println!("  exit / quit           - Exit REPL");
    println!("\nYou can also type COBOL code directly to parse it.\n");
}

fn handle_parse(programs: &mut HashMap<String, Spanned<Program>>, code: &str) -> Result<()> {
    match parse_source(code, Format::FreeFormat) {
        Ok(program) => {
            let program_id = program
                .node
                .identification
                .node
                .program_id
                .clone()
                .unwrap_or_else(|| "UNKNOWN".to_string());

            programs.insert(program_id.clone(), program);
            println!("✓ Parsed successfully! Program ID: {}", program_id);
            println!(
                "  Loaded as '{}' (use 'show {}' to inspect)",
                program_id, program_id
            );
        }
        Err(e) => {
            eprintln!("✗ Parse error: {}", e);
        }
    }
    Ok(())
}

fn handle_load(programs: &mut HashMap<String, Spanned<Program>>, filename: &str) -> Result<()> {
    let contents = std::fs::read_to_string(filename)
        .with_context(|| format!("Failed to read file: {}", filename))?;

    match parse_source(&contents, Format::FreeFormat) {
        Ok(program) => {
            let program_id = program
                .node
                .identification
                .node
                .program_id
                .clone()
                .unwrap_or_else(|| format!("file_{}", filename.replace('/', "_")));

            programs.insert(program_id.clone(), program);
            println!("✓ Loaded program from '{}' as '{}'", filename, program_id);
        }
        Err(e) => {
            eprintln!("✗ Failed to parse file: {}", e);
        }
    }
    Ok(())
}

fn handle_list(programs: &HashMap<String, Spanned<Program>>) {
    if programs.is_empty() {
        println!("No programs loaded.");
        return;
    }

    println!("\nLoaded programs:");
    for (name, program) in programs {
        let stmt_count = program.node.procedure.node.statements.len();
        let has_data = program.node.data.is_some();
        let has_env = program.node.environment.is_some();

        println!("  {} - {} statements", name, stmt_count);
        if has_data {
            println!("    - Has Data Division");
        }
        if has_env {
            println!("    - Has Environment Division");
        }
    }
    println!();
}

fn handle_show(programs: &HashMap<String, Spanned<Program>>, name: &str) -> Result<()> {
    let program = programs
        .get(name)
        .ok_or_else(|| anyhow::anyhow!("Program '{}' not found", name))?;

    println!("\nProgram: {}", name);
    println!("  Identification Division:");
    println!(
        "    Program ID: {:?}",
        program.node.identification.node.program_id
    );
    println!("    Author: {:?}", program.node.identification.node.author);

    if let Some(env) = &program.node.environment {
        println!("  Environment Division: ✓");
        if let Some(config) = &env.node.configuration_section {
            println!("    Configuration Section: ✓");
            if let Some(source) = &config.source_computer {
                println!("      Source Computer: {}", source);
            }
            if let Some(object) = &config.object_computer {
                println!("      Object Computer: {}", object);
            }
        }
    }

    if let Some(data) = &program.node.data {
        println!("  Data Division: ✓");
        if let Some(ws) = &data.node.working_storage_section {
            println!(
                "    Working Storage Section: {} items",
                ws.node.data_items.len()
            );
        }
        if let Some(ls) = &data.node.local_storage_section {
            println!(
                "    Local Storage Section: {} items",
                ls.node.data_items.len()
            );
        }
        if let Some(linkage) = &data.node.linkage_section {
            println!(
                "    Linkage Section: {} items",
                linkage.node.data_items.len()
            );
        }
    }

    println!("  Procedure Division:");
    println!(
        "    Statements: {}",
        program.node.procedure.node.statements.len()
    );

    if !program.node.procedure.node.statements.is_empty() {
        println!("\n    First few statements:");
        for (i, stmt) in program
            .node
            .procedure
            .node
            .statements
            .iter()
            .take(5)
            .enumerate()
        {
            println!("      {}. {:?}", i + 1, stmt.node);
        }
        if program.node.procedure.node.statements.len() > 5 {
            println!(
                "      ... ({} more)",
                program.node.procedure.node.statements.len() - 5
            );
        }
    }

    println!();
    Ok(())
}

fn handle_ast(programs: &HashMap<String, Spanned<Program>>, name: &str) -> Result<()> {
    let program = programs
        .get(name)
        .ok_or_else(|| anyhow::anyhow!("Program '{}' not found", name))?;

    println!("\nAST for program '{}':", name);
    println!("{:#?}\n", program);
    Ok(())
}

fn handle_tokens(code: &str) -> Result<()> {
    match tokenize(code, Format::FreeFormat) {
        Ok(tokens) => {
            println!("\nTokens ({} total):", tokens.len());
            for (i, token) in tokens.iter().enumerate() {
                if !token.is_trivial() {
                    println!(
                        "  {}: {:?} at line {}:{}",
                        i, token.token_type, token.line, token.column
                    );
                }
            }
            println!();
        }
        Err(e) => {
            eprintln!("✗ Tokenization error: {}", e);
        }
    }
    Ok(())
}
