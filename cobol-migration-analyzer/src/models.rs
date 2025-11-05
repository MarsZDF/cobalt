use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use chrono::{DateTime, Utc};

/// Complete migration assessment for a COBOL system.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct MigrationAssessment {
    pub system_overview: SystemOverview,
    pub cloud_readiness: CloudReadinessScore,
    pub microservices_analysis: MicroservicesAnalysis,
    pub effort_estimation: EffortEstimation,
    pub risk_assessment: RiskAssessment,
    pub recommendations: Vec<Recommendation>,
    pub migration_roadmap: MigrationRoadmap,
    pub technical_debt: TechnicalDebtAnalysis,
    pub generated_at: DateTime<Utc>,
}

/// High-level system overview.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct SystemOverview {
    pub total_programs: usize,
    pub total_lines_of_code: usize,
    pub programming_patterns: Vec<ProgrammingPattern>,
    pub data_dependencies: Vec<DataDependency>,
    pub external_interfaces: Vec<ExternalInterface>,
    pub business_domains: Vec<BusinessDomain>,
}

/// Cloud readiness assessment score.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct CloudReadinessScore {
    pub overall_score: f64, // 0-100
    pub categories: CloudReadinessCategories,
    pub blockers: Vec<CloudMigrationBlocker>,
    pub enablers: Vec<CloudMigrationEnabler>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct CloudReadinessCategories {
    pub architecture: f64,
    pub data_management: f64,
    pub security: f64,
    pub scalability: f64,
    pub monitoring: f64,
    pub deployment: f64,
}

/// Microservices boundary analysis.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct MicroservicesAnalysis {
    pub recommended_services: Vec<MicroserviceCandidate>,
    pub service_dependencies: Vec<ServiceDependency>,
    pub data_consistency_challenges: Vec<DataConsistencyChallenge>,
    pub api_design_recommendations: Vec<ApiRecommendation>,
}

/// Effort estimation for migration.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct EffortEstimation {
    pub total_effort_months: f64,
    pub phases: Vec<MigrationPhase>,
    pub resource_requirements: ResourceRequirements,
    pub cost_estimation: CostEstimation,
    pub timeline: MigrationTimeline,
}

/// Risk assessment for migration.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct RiskAssessment {
    pub overall_risk: RiskLevel,
    pub technical_risks: Vec<TechnicalRisk>,
    pub business_risks: Vec<BusinessRisk>,
    pub mitigation_strategies: Vec<MitigationStrategy>,
}

/// Migration recommendation.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Recommendation {
    pub id: String,
    pub title: String,
    pub description: String,
    pub category: RecommendationCategory,
    pub priority: Priority,
    pub effort_level: EffortLevel,
    pub impact: Impact,
    pub prerequisites: Vec<String>,
}

/// Migration roadmap with phases.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct MigrationRoadmap {
    pub phases: Vec<RoadmapPhase>,
    pub dependencies: Vec<PhaseDependency>,
    pub milestones: Vec<Milestone>,
    pub success_criteria: Vec<SuccessCriterion>,
}

/// Technical debt analysis.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct TechnicalDebtAnalysis {
    pub debt_score: f64, // 0-100, higher is worse
    pub debt_categories: Vec<DebtCategory>,
    pub hotspots: Vec<TechnicalDebtHotspot>,
    pub refactoring_opportunities: Vec<RefactoringOpportunity>,
}

// Supporting types
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ProgrammingPattern {
    pub pattern_type: String,
    pub frequency: usize,
    pub modernization_difficulty: DifficultyLevel,
    pub cloud_compatibility: CompatibilityLevel,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct DataDependency {
    pub source: String,
    pub target: String,
    pub dependency_type: DependencyType,
    pub strength: DependencyStrength,
    pub migration_impact: Impact,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ExternalInterface {
    pub name: String,
    pub interface_type: InterfaceType,
    pub modernization_approach: ModernizationApproach,
    pub migration_complexity: DifficultyLevel,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct BusinessDomain {
    pub name: String,
    pub programs: Vec<String>,
    pub cohesion_score: f64,
    pub coupling_score: f64,
    pub microservice_candidate: bool,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct CloudMigrationBlocker {
    pub description: String,
    pub severity: Severity,
    pub resolution_effort: EffortLevel,
    pub workarounds: Vec<String>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct CloudMigrationEnabler {
    pub description: String,
    pub benefit: Impact,
    pub implementation_effort: EffortLevel,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct MicroserviceCandidate {
    pub name: String,
    pub programs: Vec<String>,
    pub business_capability: String,
    pub data_entities: Vec<String>,
    pub estimated_size: ServiceSize,
    pub migration_complexity: DifficultyLevel,
    pub dependencies: Vec<String>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ServiceDependency {
    pub from_service: String,
    pub to_service: String,
    pub dependency_type: ServiceDependencyType,
    pub communication_pattern: CommunicationPattern,
    pub data_consistency_requirement: ConsistencyRequirement,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct DataConsistencyChallenge {
    pub description: String,
    pub affected_services: Vec<String>,
    pub consistency_pattern: ConsistencyPattern,
    pub implementation_complexity: DifficultyLevel,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ApiRecommendation {
    pub service: String,
    pub api_style: ApiStyle,
    pub endpoints: Vec<ApiEndpoint>,
    pub data_formats: Vec<DataFormat>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct MigrationPhase {
    pub name: String,
    pub description: String,
    pub duration_months: f64,
    pub deliverables: Vec<String>,
    pub success_criteria: Vec<String>,
    pub risks: Vec<String>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ResourceRequirements {
    pub developers: usize,
    pub architects: usize,
    pub devops_engineers: usize,
    pub business_analysts: usize,
    pub testers: usize,
    pub specialized_skills: Vec<String>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct CostEstimation {
    pub development_cost: f64,
    pub infrastructure_cost: f64,
    pub training_cost: f64,
    pub operational_cost_change: f64,
    pub total_cost: f64,
    pub roi_timeline_months: usize,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct MigrationTimeline {
    pub phases: Vec<TimelinePhase>,
    pub critical_path: Vec<String>,
    pub parallel_workstreams: Vec<Workstream>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct TechnicalRisk {
    pub description: String,
    pub probability: Probability,
    pub impact: Impact,
    pub risk_score: f64,
    pub mitigation_actions: Vec<String>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct BusinessRisk {
    pub description: String,
    pub probability: Probability,
    pub business_impact: Impact,
    pub risk_score: f64,
    pub mitigation_actions: Vec<String>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct MitigationStrategy {
    pub risk_category: String,
    pub strategy: String,
    pub implementation_cost: EffortLevel,
    pub effectiveness: f64,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct RoadmapPhase {
    pub phase_number: usize,
    pub name: String,
    pub description: String,
    pub start_month: usize,
    pub duration_months: usize,
    pub deliverables: Vec<String>,
    pub success_metrics: Vec<String>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct PhaseDependency {
    pub prerequisite_phase: usize,
    pub dependent_phase: usize,
    pub dependency_type: String,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Milestone {
    pub name: String,
    pub month: usize,
    pub description: String,
    pub deliverables: Vec<String>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct SuccessCriterion {
    pub metric: String,
    pub target_value: String,
    pub measurement_method: String,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct DebtCategory {
    pub category: String,
    pub debt_amount: f64,
    pub programs_affected: usize,
    pub remediation_effort: EffortLevel,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct TechnicalDebtHotspot {
    pub program_name: String,
    pub debt_score: f64,
    pub issues: Vec<String>,
    pub refactoring_priority: Priority,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct RefactoringOpportunity {
    pub description: String,
    pub programs_affected: Vec<String>,
    pub effort_estimate: f64,
    pub benefits: Vec<String>,
}

// Enums for categorical data
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum RiskLevel {
    Low,
    Medium,
    High,
    Critical,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum Priority {
    Low,
    Medium,
    High,
    Critical,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum EffortLevel {
    Low,
    Medium,
    High,
    VeryHigh,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum Impact {
    Low,
    Medium,
    High,
    VeryHigh,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum DifficultyLevel {
    Low,
    Medium,
    High,
    VeryHigh,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum CompatibilityLevel {
    FullyCompatible,
    MostlyCompatible,
    PartiallyCompatible,
    Incompatible,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum DependencyType {
    Data,
    Control,
    Functional,
    Temporal,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum DependencyStrength {
    Weak,
    Moderate,
    Strong,
    VeryStrong,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum InterfaceType {
    FileSystem,
    Database,
    MessageQueue,
    WebService,
    LegacyApi,
    Batch,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum ModernizationApproach {
    DirectMigration,
    ApiWrapper,
    EventBridge,
    DataSync,
    Replacement,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum Severity {
    Low,
    Medium,
    High,
    Critical,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum ServiceSize {
    Small,
    Medium,
    Large,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum ServiceDependencyType {
    Synchronous,
    Asynchronous,
    Data,
    Event,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum CommunicationPattern {
    RequestResponse,
    EventDriven,
    DataStreaming,
    SharedDatabase,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum ConsistencyRequirement {
    Strong,
    Eventual,
    Weak,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum ConsistencyPattern {
    Saga,
    TwoPhaseCommit,
    EventSourcing,
    CQRS,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum ApiStyle {
    Rest,
    GraphQL,
    Grpc,
    MessageBased,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ApiEndpoint {
    pub path: String,
    pub method: String,
    pub description: String,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum DataFormat {
    Json,
    Xml,
    Protobuf,
    Avro,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum RecommendationCategory {
    Architecture,
    Technology,
    Process,
    Organization,
    Security,
    Performance,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct TimelinePhase {
    pub name: String,
    pub start_month: usize,
    pub end_month: usize,
    pub milestones: Vec<String>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Workstream {
    pub name: String,
    pub phases: Vec<String>,
    pub team_size: usize,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum Probability {
    VeryLow,
    Low,
    Medium,
    High,
    VeryHigh,
}