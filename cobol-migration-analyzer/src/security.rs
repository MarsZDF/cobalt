use anyhow::{bail, Result};
use std::path::{Path, PathBuf};

/// Security utilities for migration analyzer.

/// Maximum file size allowed for input files (10MB)
pub const MAX_INPUT_FILE_SIZE: u64 = 10 * 1024 * 1024;

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
