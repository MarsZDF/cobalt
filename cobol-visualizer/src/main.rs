use anyhow::{Context, Result};
use clap::{Arg, Command};
use cobol_ast::{Program, Spanned, Statement};
use cobol_lexer::Format;
use cobol_parser::parse_source;
use std::fs;
use std::path::PathBuf;

fn main() -> Result<()> {
    let matches = Command::new("cobol-visualizer")
        .about("Generate visualizations for COBOL programs")
        .version("0.1.0")
        .arg(Arg::new("input").help("Input COBOL file").required(true))
        .arg(
            Arg::new("output")
                .short('o')
                .long("output")
                .help("Output file (default: input.svg)"),
        )
        .arg(
            Arg::new("format")
                .short('f')
                .long("format")
                .help("Output format (svg, mermaid, dot)")
                .default_value("svg"),
        )
        .arg(
            Arg::new("type")
                .short('t')
                .long("type")
                .help("Visualization type (ast, flow, dependencies)")
                .default_value("ast"),
        )
        .get_matches();

    let input_file = matches.get_one::<String>("input").unwrap();
    let format = matches.get_one::<String>("format").unwrap();
    let vis_type = matches.get_one::<String>("type").unwrap();

    let output_file = matches
        .get_one::<String>("output")
        .map(|s| PathBuf::from(s))
        .unwrap_or_else(|| {
            let mut path = PathBuf::from(input_file);
            path.set_extension(format);
            path
        });

    let contents = fs::read_to_string(input_file)
        .with_context(|| format!("Failed to read file: {}", input_file))?;

    let program = parse_source(&contents, Format::FreeFormat)
        .with_context(|| format!("Failed to parse COBOL file: {}", input_file))?;

    let output = match vis_type.as_str() {
        "ast" => generate_ast_diagram(&program, format)?,
        "flow" => generate_flow_diagram(&program, format)?,
        "dependencies" => generate_dependency_diagram(&program, format)?,
        _ => return Err(anyhow::anyhow!("Unknown visualization type: {}", vis_type)),
    };

    fs::write(&output_file, output)
        .with_context(|| format!("Failed to write output file: {:?}", output_file))?;

    println!("✓ Generated visualization: {:?}", output_file);
    Ok(())
}

fn generate_ast_diagram(program: &Spanned<Program>, format: &str) -> Result<String> {
    match format {
        "svg" => generate_ast_svg(program),
        "mermaid" => generate_ast_mermaid(program),
        "dot" => generate_ast_dot(program),
        _ => Err(anyhow::anyhow!("Unsupported format: {}", format)),
    }
}

fn generate_flow_diagram(program: &Spanned<Program>, format: &str) -> Result<String> {
    match format {
        "svg" => generate_flow_svg(program),
        "mermaid" => generate_flow_mermaid(program),
        "dot" => generate_flow_dot(program),
        _ => Err(anyhow::anyhow!("Unsupported format: {}", format)),
    }
}

fn generate_dependency_diagram(program: &Spanned<Program>, format: &str) -> Result<String> {
    match format {
        "svg" => generate_dependency_svg(program),
        "mermaid" => generate_dependency_mermaid(program),
        "dot" => generate_dependency_dot(program),
        _ => Err(anyhow::anyhow!("Unsupported format: {}", format)),
    }
}

// SVG generation functions
fn generate_ast_svg(program: &Spanned<Program>) -> Result<String> {
    let mut svg = String::new();
    svg.push_str("<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n");
    svg.push_str("<svg xmlns=\"http://www.w3.org/2000/svg\" width=\"800\" height=\"600\">\n");
    svg.push_str("<rect width=\"800\" height=\"600\" fill=\"white\"/>\n");

    let program_id = program
        .node
        .identification
        .node
        .program_id
        .as_ref()
        .map(|s| s.as_str())
        .unwrap_or("UNKNOWN");

    svg.push_str(&format!("<text x=\"400\" y=\"30\" text-anchor=\"middle\" font-size=\"20\" font-weight=\"bold\">{}</text>\n", program_id));

    let mut y = 80;
    svg.push_str(&format!(
        "<text x=\"50\" y=\"{}\" font-size=\"14\">Identification Division</text>\n",
        y
    ));
    y += 30;

    if let Some(data) = &program.node.data {
        svg.push_str(&format!(
            "<text x=\"50\" y=\"{}\" font-size=\"14\">Data Division</text>\n",
            y
        ));
        y += 30;
        if let Some(ws) = &data.node.working_storage_section {
            svg.push_str(&format!("<text x=\"70\" y=\"{}\" font-size=\"12\" fill=\"gray\">Working Storage: {} items</text>\n", y, ws.node.data_items.len()));
            y += 25;
        }
    }

    svg.push_str(&format!(
        "<text x=\"50\" y=\"{}\" font-size=\"14\">Procedure Division</text>\n",
        y
    ));
    y += 30;
    svg.push_str(&format!(
        "<text x=\"70\" y=\"{}\" font-size=\"12\" fill=\"gray\">{} statements</text>\n",
        y,
        program.node.procedure.node.statements.len()
    ));

    svg.push_str("</svg>\n");
    Ok(svg)
}

fn generate_flow_svg(program: &Spanned<Program>) -> Result<String> {
    let mut svg = String::new();
    svg.push_str("<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n");
    svg.push_str("<svg xmlns=\"http://www.w3.org/2000/svg\" width=\"800\" height=\"600\">\n");
    svg.push_str("<rect width=\"800\" height=\"600\" fill=\"white\"/>\n");

    let mut y = 50;

    for (i, stmt) in program.node.procedure.node.statements.iter().enumerate() {
        let stmt_type = match &stmt.node {
            Statement::Display { .. } => "DISPLAY",
            Statement::Move { .. } => "MOVE",
            Statement::Compute { .. } => "COMPUTE",
            Statement::If { .. } => "IF",
            Statement::Perform { .. } => "PERFORM",
            Statement::Stop { .. } => "STOP",
            _ => "STMT",
        };

        svg.push_str(&format!(
            "<rect x=\"50\" y=\"{}\" width=\"200\" height=\"30\" fill=\"lightblue\" stroke=\"black\"/>\n",
            y
        ));
        svg.push_str(&format!(
            "<text x=\"150\" y=\"{}\" text-anchor=\"middle\" font-size=\"12\">{}. {}</text>\n",
            y + 20,
            i + 1,
            stmt_type
        ));

        if i < program.node.procedure.node.statements.len() - 1 {
            svg.push_str(&format!(
                "<line x1=\"150\" y1=\"{}\" x2=\"150\" y2=\"{}\" stroke=\"black\" stroke-width=\"2\" marker-end=\"url(#arrowhead)\"/>\n",
                y + 30, y + 50
            ));
        }

        y += 50;
        if y > 550 {
            break; // Limit for visualization
        }
    }

    // Arrow marker definition
    svg.push_str("<defs>\n");
    svg.push_str("<marker id=\"arrowhead\" markerWidth=\"10\" markerHeight=\"10\" refX=\"5\" refY=\"3\" orient=\"auto\">\n");
    svg.push_str("<polygon points=\"0 0, 10 3, 0 6\" fill=\"black\"/>\n");
    svg.push_str("</marker>\n");
    svg.push_str("</defs>\n");

    svg.push_str("</svg>\n");
    Ok(svg)
}

fn generate_dependency_svg(_program: &Spanned<Program>) -> Result<String> {
    // Simplified dependency diagram
    let mut svg = String::new();
    svg.push_str("<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n");
    svg.push_str("<svg xmlns=\"http://www.w3.org/2000/svg\" width=\"400\" height=\"400\">\n");
    svg.push_str("<rect width=\"400\" height=\"400\" fill=\"white\"/>\n");
    svg.push_str("<text x=\"200\" y=\"200\" text-anchor=\"middle\" font-size=\"16\">Dependency Graph</text>\n");
    svg.push_str("<text x=\"200\" y=\"230\" text-anchor=\"middle\" font-size=\"12\" fill=\"gray\">(Not yet implemented)</text>\n");
    svg.push_str("</svg>\n");
    Ok(svg)
}

// Mermaid generation functions
fn generate_ast_mermaid(program: &Spanned<Program>) -> Result<String> {
    let program_id = program
        .node
        .identification
        .node
        .program_id
        .as_ref()
        .map(|s| s.as_str())
        .unwrap_or("UNKNOWN");

    let mut mermaid = String::new();
    mermaid.push_str("graph TD\n");
    mermaid.push_str(&format!("  Root[{}]\n", program_id));
    mermaid.push_str("  Root --> ID[Identification Division]\n");

    if program.node.data.is_some() {
        mermaid.push_str("  Root --> Data[Data Division]\n");
        if let Some(data) = &program.node.data {
            if let Some(ws) = &data.node.working_storage_section {
                mermaid.push_str(&format!(
                    "  Data --> WS[Working Storage: {} items]\n",
                    ws.node.data_items.len()
                ));
            }
        }
    }

    mermaid.push_str(&format!(
        "  Root --> Proc[Procedure Division: {} statements]\n",
        program.node.procedure.node.statements.len()
    ));

    Ok(mermaid)
}

fn generate_flow_mermaid(program: &Spanned<Program>) -> Result<String> {
    let mut mermaid = String::new();
    mermaid.push_str("flowchart TD\n");

    for (i, stmt) in program.node.procedure.node.statements.iter().enumerate() {
        let stmt_type = match &stmt.node {
            Statement::Display { .. } => "DISPLAY",
            Statement::Move { .. } => "MOVE",
            Statement::Compute { .. } => "COMPUTE",
            Statement::If { .. } => "IF",
            Statement::Perform { .. } => "PERFORM",
            Statement::Stop { .. } => "STOP",
            _ => "STMT",
        };

        let node_id = format!("S{}", i);
        mermaid.push_str(&format!("  {}[{}]\n", node_id, stmt_type));

        if i > 0 {
            let prev_id = format!("S{}", i - 1);
            mermaid.push_str(&format!("  {} --> {}\n", prev_id, node_id));
        }

        if i > 20 {
            break; // Limit for readability
        }
    }

    Ok(mermaid)
}

fn generate_dependency_mermaid(_program: &Spanned<Program>) -> Result<String> {
    Ok("graph LR\n  A[Program]\n  B[Dependencies]\n  A --> B\n".to_string())
}

// Graphviz DOT generation functions
fn generate_ast_dot(program: &Spanned<Program>) -> Result<String> {
    let program_id = program
        .node
        .identification
        .node
        .program_id
        .as_ref()
        .map(|s| s.as_str())
        .unwrap_or("UNKNOWN");

    let mut dot = String::new();
    dot.push_str("digraph AST {\n");
    dot.push_str("  node [shape=box];\n");
    dot.push_str(&format!("  Root [label=\"{}\"];\n", program_id));
    dot.push_str("  Root -> ID [label=\"Identification\"];\n");

    if program.node.data.is_some() {
        dot.push_str("  Root -> Data [label=\"Data\"];\n");
    }

    dot.push_str(&format!(
        "  Root -> Proc [label=\"Procedure: {} stmts\"];\n",
        program.node.procedure.node.statements.len()
    ));
    dot.push_str("}\n");

    Ok(dot)
}

fn generate_flow_dot(program: &Spanned<Program>) -> Result<String> {
    let mut dot = String::new();
    dot.push_str("digraph Flow {\n");
    dot.push_str("  node [shape=box];\n");

    for (i, stmt) in program.node.procedure.node.statements.iter().enumerate() {
        let stmt_type = match &stmt.node {
            Statement::Display { .. } => "DISPLAY",
            Statement::Move { .. } => "MOVE",
            Statement::Compute { .. } => "COMPUTE",
            Statement::If { .. } => "IF",
            Statement::Perform { .. } => "PERFORM",
            Statement::Stop { .. } => "STOP",
            _ => "STMT",
        };

        let node_id = format!("S{}", i);
        dot.push_str(&format!("  {} [label=\"{}\"];\n", node_id, stmt_type));

        if i > 0 {
            let prev_id = format!("S{}", i - 1);
            dot.push_str(&format!("  {} -> {};\n", prev_id, node_id));
        }

        if i > 20 {
            break;
        }
    }

    dot.push_str("}\n");
    Ok(dot)
}

fn generate_dependency_dot(_program: &Spanned<Program>) -> Result<String> {
    Ok("digraph Dependencies {\n  node [shape=box];\n  A -> B;\n}\n".to_string())
}
