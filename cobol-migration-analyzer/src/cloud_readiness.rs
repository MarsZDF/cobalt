use crate::analysis::AnalysisConfig;
use crate::models::*;
use anyhow::Result;
use cobol_ast::Program;

/// Analyzer for cloud migration readiness.
pub struct CloudReadinessAnalyzer {
    config: AnalysisConfig,
}

impl CloudReadinessAnalyzer {
    pub fn new(config: &AnalysisConfig) -> Self {
        Self {
            config: config.clone(),
        }
    }

    /// Analyze cloud readiness for a set of COBOL programs.
    pub fn analyze(&self, programs: &[&Program]) -> Result<CloudReadinessScore> {
        let categories = self.analyze_categories(programs)?;
        let overall_score = self.calculate_overall_score(&categories);
        let blockers = self.identify_blockers(programs)?;
        let enablers = self.identify_enablers(programs)?;

        Ok(CloudReadinessScore {
            overall_score,
            categories,
            blockers,
            enablers,
        })
    }

    fn analyze_categories(&self, programs: &[&Program]) -> Result<CloudReadinessCategories> {
        Ok(CloudReadinessCategories {
            architecture: self.analyze_architecture_readiness(programs)?,
            data_management: self.analyze_data_management_readiness(programs)?,
            security: self.analyze_security_readiness(programs)?,
            scalability: self.analyze_scalability_readiness(programs)?,
            monitoring: self.analyze_monitoring_readiness(programs)?,
            deployment: self.analyze_deployment_readiness(programs)?,
        })
    }

    fn analyze_architecture_readiness(&self, programs: &[&Program]) -> Result<f64> {
        let mut score = 80.0; // Base score for COBOL

        // Check for monolithic patterns
        if programs.len() == 1 && self.estimate_program_size(programs[0]) > 10000 {
            score -= 20.0; // Large monolithic program
        }

        // Check for tight coupling indicators
        let coupling_score = self.analyze_coupling(programs);
        score -= coupling_score * 10.0;

        // Check for cloud-friendly patterns
        if self.has_modular_structure(programs) {
            score += 10.0;
        }

        if self.has_clear_interfaces(programs) {
            score += 10.0;
        }

        Ok(score.clamp(0.0, 100.0).into())
    }

    fn analyze_data_management_readiness(&self, programs: &[&Program]) -> Result<f64> {
        let mut score = 70.0; // Base score

        // Check for file-based operations (cloud migration challenge)
        let file_operations = self.count_file_operations(programs);
        if file_operations > 10 {
            score -= 30.0;
        } else if file_operations > 5 {
            score -= 15.0;
        }

        // Check for database usage patterns
        if self.has_database_operations(programs) {
            score += 15.0;
        }

        // Check for data consistency requirements
        let consistency_complexity = self.analyze_data_consistency_requirements(programs);
        score -= consistency_complexity * 5.0;

        Ok(score.clamp(0.0, 100.0).into())
    }

    fn analyze_security_readiness(&self, programs: &[&Program]) -> Result<f64> {
        let mut score: f32 = 60.0; // Conservative base score for legacy systems

        // Check for hardcoded credentials (security risk)
        if self.has_hardcoded_credentials(programs) {
            score -= 25.0;
        }

        // Check for proper error handling
        if self.has_comprehensive_error_handling(programs) {
            score += 15.0;
        }

        // Check for input validation
        if self.has_input_validation(programs) {
            score += 10.0;
        }

        // Check for audit logging
        if self.has_audit_logging(programs) {
            score += 15.0;
        }

        Ok(score.clamp(0.0, 100.0).into())
    }

    fn analyze_scalability_readiness(&self, programs: &[&Program]) -> Result<f64> {
        let mut score: f32 = 65.0; // Base score

        // Check for stateless operations
        if self.is_mostly_stateless(programs) {
            score += 20.0;
        } else {
            score -= 15.0;
        }

        // Check for resource usage patterns
        let resource_intensity = self.analyze_resource_usage(programs);
        match resource_intensity {
            ResourceIntensity::Low => score += 10.0,
            ResourceIntensity::Medium => {}
            ResourceIntensity::High => score -= 10.0,
            ResourceIntensity::VeryHigh => score -= 25.0,
        }

        // Check for concurrent processing capability
        if self.supports_concurrent_processing(programs) {
            score += 15.0;
        }

        Ok(score.clamp(0.0, 100.0).into())
    }

    fn analyze_monitoring_readiness(&self, programs: &[&Program]) -> Result<f64> {
        let mut score = 40.0; // Low base score for legacy systems

        // Check for logging practices
        let logging_quality = self.assess_logging_quality(programs);
        score += logging_quality * 20.0;

        // Check for health check capabilities
        if self.has_health_checks(programs) {
            score += 25.0;
        }

        // Check for metrics collection
        if self.has_metrics_collection(programs) {
            score += 20.0;
        }

        Ok(score.clamp(0.0, 100.0).into())
    }

    fn analyze_deployment_readiness(&self, programs: &[&Program]) -> Result<f64> {
        let mut score = 50.0; // Base score

        // Check for deployment complexity
        let deployment_complexity = self.assess_deployment_complexity(programs);
        match deployment_complexity {
            DeploymentComplexity::Simple => score += 25.0,
            DeploymentComplexity::Moderate => score += 10.0,
            DeploymentComplexity::Complex => score -= 10.0,
            DeploymentComplexity::VeryComplex => score -= 30.0,
        }

        // Check for configuration management
        if self.has_externalized_config(programs) {
            score += 15.0;
        }

        // Check for dependency management
        let dependency_complexity = self.assess_dependency_complexity(programs);
        score -= dependency_complexity * 5.0;

        Ok(score.clamp(0.0, 100.0).into())
    }

    fn identify_blockers(&self, programs: &[&Program]) -> Result<Vec<CloudMigrationBlocker>> {
        let mut blockers = Vec::new();

        // Check for critical blockers
        if self.has_platform_specific_dependencies(programs) {
            blockers.push(CloudMigrationBlocker {
                description: "Platform-specific dependencies that cannot be migrated".to_string(),
                severity: Severity::Critical,
                resolution_effort: EffortLevel::VeryHigh,
                workarounds: vec![
                    "Create abstraction layer".to_string(),
                    "Replace with cloud-native alternatives".to_string(),
                ],
            });
        }

        if self.has_complex_file_operations(programs) {
            blockers.push(CloudMigrationBlocker {
                description: "Complex file system operations incompatible with cloud storage"
                    .to_string(),
                severity: Severity::High,
                resolution_effort: EffortLevel::High,
                workarounds: vec![
                    "Migrate to cloud storage services".to_string(),
                    "Implement file abstraction layer".to_string(),
                ],
            });
        }

        if self.has_security_vulnerabilities(programs) {
            blockers.push(CloudMigrationBlocker {
                description:
                    "Security vulnerabilities that must be addressed before cloud deployment"
                        .to_string(),
                severity: Severity::High,
                resolution_effort: EffortLevel::Medium,
                workarounds: vec![
                    "Implement security controls".to_string(),
                    "Add input validation".to_string(),
                ],
            });
        }

        Ok(blockers)
    }

    fn identify_enablers(&self, programs: &[&Program]) -> Result<Vec<CloudMigrationEnabler>> {
        let mut enablers = Vec::new();

        if self.has_modular_structure(programs) {
            enablers.push(CloudMigrationEnabler {
                description: "Modular program structure facilitates containerization".to_string(),
                benefit: Impact::High,
                implementation_effort: EffortLevel::Low,
            });
        }

        if self.has_clear_interfaces(programs) {
            enablers.push(CloudMigrationEnabler {
                description: "Well-defined interfaces enable API-based communication".to_string(),
                benefit: Impact::Medium,
                implementation_effort: EffortLevel::Medium,
            });
        }

        if self.is_mostly_stateless(programs) {
            enablers.push(CloudMigrationEnabler {
                description: "Stateless operations support horizontal scaling".to_string(),
                benefit: Impact::VeryHigh,
                implementation_effort: EffortLevel::Low,
            });
        }

        Ok(enablers)
    }

    fn calculate_overall_score(&self, categories: &CloudReadinessCategories) -> f64 {
        // Weighted average of category scores
        let weights = match self.config.target_cloud_platform {
            crate::analysis::CloudPlatform::AWS => (0.2, 0.2, 0.15, 0.2, 0.1, 0.15),
            crate::analysis::CloudPlatform::Azure => (0.2, 0.2, 0.15, 0.2, 0.1, 0.15),
            crate::analysis::CloudPlatform::GCP => (0.2, 0.2, 0.15, 0.2, 0.1, 0.15),
            crate::analysis::CloudPlatform::Hybrid => (0.25, 0.15, 0.15, 0.15, 0.15, 0.15),
            crate::analysis::CloudPlatform::OnPremiseKubernetes => (0.3, 0.1, 0.1, 0.2, 0.15, 0.15),
        };

        categories.architecture * weights.0
            + categories.data_management * weights.1
            + categories.security * weights.2
            + categories.scalability * weights.3
            + categories.monitoring * weights.4
            + categories.deployment * weights.5
    }

    // Helper methods (simplified implementations)
    fn estimate_program_size(&self, _program: &Program) -> usize {
        5000
    }
    fn analyze_coupling(&self, _programs: &[&Program]) -> f64 {
        0.3
    }
    fn has_modular_structure(&self, _programs: &[&Program]) -> bool {
        true
    }
    fn has_clear_interfaces(&self, _programs: &[&Program]) -> bool {
        false
    }
    fn count_file_operations(&self, _programs: &[&Program]) -> usize {
        5
    }
    fn has_database_operations(&self, _programs: &[&Program]) -> bool {
        false
    }
    fn analyze_data_consistency_requirements(&self, _programs: &[&Program]) -> f64 {
        2.0
    }
    fn has_hardcoded_credentials(&self, _programs: &[&Program]) -> bool {
        false
    }
    fn has_comprehensive_error_handling(&self, _programs: &[&Program]) -> bool {
        true
    }
    fn has_input_validation(&self, _programs: &[&Program]) -> bool {
        false
    }
    fn has_audit_logging(&self, _programs: &[&Program]) -> bool {
        false
    }
    fn is_mostly_stateless(&self, _programs: &[&Program]) -> bool {
        true
    }
    fn analyze_resource_usage(&self, _programs: &[&Program]) -> ResourceIntensity {
        ResourceIntensity::Medium
    }
    fn supports_concurrent_processing(&self, _programs: &[&Program]) -> bool {
        false
    }
    fn assess_logging_quality(&self, _programs: &[&Program]) -> f64 {
        0.3
    }
    fn has_health_checks(&self, _programs: &[&Program]) -> bool {
        false
    }
    fn has_metrics_collection(&self, _programs: &[&Program]) -> bool {
        false
    }
    fn assess_deployment_complexity(&self, _programs: &[&Program]) -> DeploymentComplexity {
        DeploymentComplexity::Moderate
    }
    fn has_externalized_config(&self, _programs: &[&Program]) -> bool {
        false
    }
    fn assess_dependency_complexity(&self, _programs: &[&Program]) -> f64 {
        3.0
    }
    fn has_platform_specific_dependencies(&self, _programs: &[&Program]) -> bool {
        false
    }
    fn has_complex_file_operations(&self, _programs: &[&Program]) -> bool {
        true
    }
    fn has_security_vulnerabilities(&self, _programs: &[&Program]) -> bool {
        false
    }
}

#[derive(Debug, Clone)]
enum ResourceIntensity {
    Low,
    Medium,
    High,
    VeryHigh,
}

#[derive(Debug, Clone)]
enum DeploymentComplexity {
    Simple,
    Moderate,
    Complex,
    VeryComplex,
}
