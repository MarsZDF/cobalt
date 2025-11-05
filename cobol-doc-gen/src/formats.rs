use serde::{Deserialize, Serialize};

/// Output format for generated documentation.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum OutputFormat {
    Html,
    Markdown,
    Json,
    Pdf,
    Word,
}

impl OutputFormat {
    pub fn file_extension(&self) -> &'static str {
        match self {
            OutputFormat::Html => "html",
            OutputFormat::Markdown => "md",
            OutputFormat::Json => "json",
            OutputFormat::Pdf => "pdf",
            OutputFormat::Word => "docx",
        }
    }

    pub fn mime_type(&self) -> &'static str {
        match self {
            OutputFormat::Html => "text/html",
            OutputFormat::Markdown => "text/markdown",
            OutputFormat::Json => "application/json",
            OutputFormat::Pdf => "application/pdf",
            OutputFormat::Word => "application/vnd.openxmlformats-officedocument.wordprocessingml.document",
        }
    }
}

impl std::fmt::Display for OutputFormat {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            OutputFormat::Html => write!(f, "HTML"),
            OutputFormat::Markdown => write!(f, "Markdown"),
            OutputFormat::Json => write!(f, "JSON"),
            OutputFormat::Pdf => write!(f, "PDF"),
            OutputFormat::Word => write!(f, "Word"),
        }
    }
}

impl std::str::FromStr for OutputFormat {
    type Err = String;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s.to_lowercase().as_str() {
            "html" | "htm" => Ok(OutputFormat::Html),
            "markdown" | "md" => Ok(OutputFormat::Markdown),
            "json" => Ok(OutputFormat::Json),
            "pdf" => Ok(OutputFormat::Pdf),
            "word" | "docx" => Ok(OutputFormat::Word),
            _ => Err(format!("Unknown output format: {}", s)),
        }
    }
}