use crate::span::{Span, Spanned};
use crate::data::DataItem;
use crate::statement::Statement;

/// Complete COBOL program.
#[derive(Debug, Clone, PartialEq, Eq)]
#[cfg_attr(feature = "serialize", derive(serde::Serialize, serde::Deserialize))]
pub struct Program {
    pub identification: Spanned<IdentificationDivision>,
    pub environment: Option<Spanned<EnvironmentDivision>>,
    pub data: Option<Spanned<DataDivision>>,
    pub procedure: Spanned<ProcedureDivision>,
}

/// IDENTIFICATION DIVISION.
#[derive(Debug, Clone, PartialEq, Eq)]
#[cfg_attr(feature = "serialize", derive(serde::Serialize, serde::Deserialize))]
pub struct IdentificationDivision {
    pub program_id: Option<String>,
    pub author: Option<String>,
    pub installation: Option<String>,
    pub date_written: Option<String>,
    pub date_compiled: Option<String>,
    pub security: Option<String>,
    pub remarks: Option<String>,
}

/// ENVIRONMENT DIVISION.
#[derive(Debug, Clone, PartialEq, Eq)]
#[cfg_attr(feature = "serialize", derive(serde::Serialize, serde::Deserialize))]
pub struct EnvironmentDivision {
    pub configuration_section: Option<Spanned<ConfigurationSection>>,
    pub input_output_section: Option<Spanned<InputOutputSection>>,
}

/// CONFIGURATION SECTION.
#[derive(Debug, Clone, PartialEq, Eq)]
#[cfg_attr(feature = "serialize", derive(serde::Serialize, serde::Deserialize))]
pub struct ConfigurationSection {
    pub source_computer: Option<String>,
    pub object_computer: Option<String>,
    pub special_names: Option<Vec<Spanned<SpecialName>>>,
}

/// SPECIAL-NAMES entry.
#[derive(Debug, Clone, PartialEq, Eq)]
#[cfg_attr(feature = "serialize", derive(serde::Serialize, serde::Deserialize))]
pub struct SpecialName {
    pub system_name: String,
    pub mnemonic_name: Option<String>,
}

/// INPUT-OUTPUT SECTION.
#[derive(Debug, Clone, PartialEq, Eq)]
#[cfg_attr(feature = "serialize", derive(serde::Serialize, serde::Deserialize))]
pub struct InputOutputSection {
    pub file_control: Option<Spanned<FileControl>>,
    pub io_control: Option<Spanned<IoControl>>,
}

/// FILE-CONTROL paragraph.
#[derive(Debug, Clone, PartialEq, Eq)]
#[cfg_attr(feature = "serialize", derive(serde::Serialize, serde::Deserialize))]
pub struct FileControl {
    pub file_entries: Vec<Spanned<FileEntry>>,
}

/// FILE entry in FILE-CONTROL.
#[derive(Debug, Clone, PartialEq, Eq)]
#[cfg_attr(feature = "serialize", derive(serde::Serialize, serde::Deserialize))]
pub struct FileEntry {
    pub file_name: String,
    pub organization: Option<String>, // SEQUENTIAL, INDEXED, RELATIVE
    pub access_mode: Option<String>,  // SEQUENTIAL, RANDOM, DYNAMIC
    pub assign: Option<String>,       // ASSIGN TO clause
}

/// I-O-CONTROL paragraph.
#[derive(Debug, Clone, PartialEq, Eq)]
#[cfg_attr(feature = "serialize", derive(serde::Serialize, serde::Deserialize))]
pub struct IoControl {
    // I-O-CONTROL details (simplified for now)
}

/// DATA DIVISION.
#[derive(Debug, Clone, PartialEq, Eq)]
#[cfg_attr(feature = "serialize", derive(serde::Serialize, serde::Deserialize))]
pub struct DataDivision {
    pub file_section: Option<Spanned<FileSection>>,
    pub working_storage_section: Option<Spanned<WorkingStorageSection>>,
    pub local_storage_section: Option<Spanned<LocalStorageSection>>,
    pub linkage_section: Option<Spanned<LinkageSection>>,
}

/// FILE SECTION.
#[derive(Debug, Clone, PartialEq, Eq)]
#[cfg_attr(feature = "serialize", derive(serde::Serialize, serde::Deserialize))]
pub struct FileSection {
    pub file_descriptions: Vec<Spanned<FileDescription>>,
}

/// File description (FD).
#[derive(Debug, Clone, PartialEq, Eq)]
#[cfg_attr(feature = "serialize", derive(serde::Serialize, serde::Deserialize))]
pub struct FileDescription {
    pub file_name: String,
    pub record_blocking: Option<String>,
    pub record_key: Option<Vec<String>>,
    pub data_items: Vec<Spanned<DataItem>>,
}

/// WORKING-STORAGE SECTION.
#[derive(Debug, Clone, PartialEq, Eq)]
#[cfg_attr(feature = "serialize", derive(serde::Serialize, serde::Deserialize))]
pub struct WorkingStorageSection {
    pub data_items: Vec<Spanned<DataItem>>,
}

/// LOCAL-STORAGE SECTION.
#[derive(Debug, Clone, PartialEq, Eq)]
#[cfg_attr(feature = "serialize", derive(serde::Serialize, serde::Deserialize))]
pub struct LocalStorageSection {
    pub data_items: Vec<Spanned<DataItem>>,
}

/// LINKAGE SECTION.
#[derive(Debug, Clone, PartialEq, Eq)]
#[cfg_attr(feature = "serialize", derive(serde::Serialize, serde::Deserialize))]
pub struct LinkageSection {
    pub data_items: Vec<Spanned<DataItem>>,
}

/// PROCEDURE DIVISION.
#[derive(Debug, Clone, PartialEq, Eq)]
#[cfg_attr(feature = "serialize", derive(serde::Serialize, serde::Deserialize))]
pub struct ProcedureDivision {
    pub using: Option<Vec<Spanned<String>>>, // USING clause (parameters)
    pub returning: Option<String>,           // RETURNING clause
    pub sections: Vec<Spanned<Section>>,
    pub paragraphs: Vec<Spanned<Paragraph>>,
    pub statements: Vec<Spanned<Statement>>, // Top-level statements (if no sections/paragraphs)
}

/// Procedure section.
#[derive(Debug, Clone, PartialEq, Eq)]
#[cfg_attr(feature = "serialize", derive(serde::Serialize, serde::Deserialize))]
pub struct Section {
    pub name: String,
    pub paragraphs: Vec<Spanned<Paragraph>>,
}

/// Procedure paragraph.
#[derive(Debug, Clone, PartialEq, Eq)]
#[cfg_attr(feature = "serialize", derive(serde::Serialize, serde::Deserialize))]
pub struct Paragraph {
    pub name: String,
    pub statements: Vec<Spanned<Statement>>,
}
