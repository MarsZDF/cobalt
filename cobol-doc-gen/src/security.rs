use anyhow::{bail, Result};
use std::path::{Path, PathBuf};

/// Security utilities for safe file operations and input validation.

/// Maximum file size allowed for input files (10MB)
pub const MAX_INPUT_FILE_SIZE: u64 = 10 * 1024 * 1024;

/// Maximum line length to prevent memory exhaustion attacks
pub const MAX_LINE_LENGTH: usize = 10_000;

/// Maximum number of lines in a file
pub const MAX_LINES_COUNT: usize = 100_000;

/// Validate and sanitize file path to prevent path traversal attacks.
pub fn validate_file_path(path: &Path) -> Result<PathBuf> {
    // Convert to absolute path to resolve any relative components
    let canonical_path = path
        .canonicalize()
        .map_err(|_| anyhow::anyhow!("Invalid file path: {}", path.display()))?;

    // Check for common path traversal patterns
    let path_str = canonical_path.to_string_lossy();
    if path_str.contains("..") || path_str.contains("~") {
        bail!("Path traversal attempt detected: {}", path.display());
    }

    // Ensure it's a regular file, not a device or special file
    if !canonical_path.is_file() {
        bail!("Path is not a regular file: {}", path.display());
    }

    Ok(canonical_path)
}

/// Validate file size before processing.
pub fn validate_file_size(path: &Path) -> Result<()> {
    let metadata =
        std::fs::metadata(path).map_err(|e| anyhow::anyhow!("Cannot read file metadata: {}", e))?;

    let size = metadata.len();
    if size > MAX_INPUT_FILE_SIZE {
        bail!(
            "File too large: {} bytes (max: {} bytes)",
            size,
            MAX_INPUT_FILE_SIZE
        );
    }

    if size == 0 {
        bail!("File is empty: {}", path.display());
    }

    Ok(())
}

/// Safely read file content with size and content validation.
pub fn safe_read_file(path: &Path) -> Result<String> {
    let validated_path = validate_file_path(path)?;
    validate_file_size(&validated_path)?;

    let content = std::fs::read_to_string(&validated_path)
        .map_err(|e| anyhow::anyhow!("Failed to read file: {}", e))?;

    // Validate content
    validate_file_content(&content)?;

    Ok(content)
}

/// Validate file content for potential security issues.
pub fn validate_file_content(content: &str) -> Result<()> {
    let lines: Vec<&str> = content.lines().collect();

    // Check line count
    if lines.len() > MAX_LINES_COUNT {
        bail!(
            "Too many lines in file: {} (max: {})",
            lines.len(),
            MAX_LINES_COUNT
        );
    }

    // Check individual line lengths
    for (line_num, line) in lines.iter().enumerate() {
        if line.len() > MAX_LINE_LENGTH {
            bail!(
                "Line {} too long: {} characters (max: {})",
                line_num + 1,
                line.len(),
                MAX_LINE_LENGTH
            );
        }
    }

    // Check for null bytes (potential binary file)
    if content.contains('\0') {
        bail!("File contains null bytes - may be a binary file");
    }

    Ok(())
}

/// Safely write file with path validation.
pub fn safe_write_file(path: &Path, content: &str) -> Result<()> {
    // Validate output path (but don't require it to exist yet)
    let parent_dir = path
        .parent()
        .ok_or_else(|| anyhow::anyhow!("Invalid output path: {}", path.display()))?;

    if !parent_dir.exists() {
        bail!("Output directory does not exist: {}", parent_dir.display());
    }

    // Check for suspicious extensions
    if let Some(extension) = path.extension() {
        let ext_str = extension.to_string_lossy().to_lowercase();
        match ext_str.as_str() {
            "exe" | "bat" | "cmd" | "com" | "scr" | "pif" | "sh" | "ps1" => {
                bail!("Potentially dangerous file extension: {}", ext_str);
            }
            _ => {}
        }
    }

    // Validate content size
    if content.len() > MAX_INPUT_FILE_SIZE as usize {
        bail!("Output content too large: {} bytes", content.len());
    }

    std::fs::write(path, content).map_err(|e| anyhow::anyhow!("Failed to write file: {}", e))?;

    Ok(())
}

/// Sanitize string for safe display in error messages.
pub fn sanitize_for_display(input: &str) -> String {
    input
        .chars()
        .take(100) // Limit length
        .filter(|c| c.is_ascii_graphic() || c.is_ascii_whitespace())
        .collect::<String>()
        .replace('\n', "\\n")
        .replace('\r', "\\r")
        .replace('\t', "\\t")
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::io::Write;
    use tempfile::NamedTempFile;

    #[test]
    fn test_validate_file_content() {
        // Valid content
        assert!(validate_file_content("IDENTIFICATION DIVISION.\nPROGRAM-ID. TEST.").is_ok());

        // Too many lines
        let long_content = "line\n".repeat(MAX_LINES_COUNT + 1);
        assert!(validate_file_content(&long_content).is_err());

        // Line too long
        let long_line = "A".repeat(MAX_LINE_LENGTH + 1);
        assert!(validate_file_content(&long_line).is_err());

        // Null bytes
        assert!(validate_file_content("test\0content").is_err());
    }

    #[test]
    fn test_safe_file_operations() {
        let mut temp_file = NamedTempFile::new().unwrap();
        writeln!(temp_file, "IDENTIFICATION DIVISION.").unwrap();
        writeln!(temp_file, "PROGRAM-ID. TEST.").unwrap();
        temp_file.flush().unwrap();

        // Should successfully read valid file
        assert!(safe_read_file(temp_file.path()).is_ok());
    }

    #[test]
    fn test_sanitize_for_display() {
        assert_eq!(sanitize_for_display("normal text"), "normal text");
        assert_eq!(sanitize_for_display("line1\nline2"), "line1\\nline2");
        assert_eq!(sanitize_for_display("tab\there"), "tab\\there");

        // Should truncate long strings
        let long_string = "A".repeat(200);
        let sanitized = sanitize_for_display(&long_string);
        assert!(sanitized.len() <= 100);
    }
}
