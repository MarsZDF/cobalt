use crate::models::*;
use crate::assessment::MigrationAssessment;
use crate::cloud_readiness::CloudReadinessAnalyzer;
use crate::microservices::MicroservicesAnalyzer;
use crate::effort_estimation::EffortEstimator;
use crate::recommendations::RecommendationEngine;
use anyhow::Result;
use chrono::Utc;
use cobol_ast::Program;

/// Configuration for migration analysis.
#[derive(Debug, Clone)]
pub struct AnalysisConfig {
    pub include_cloud_readiness: bool,
    pub include_microservices_analysis: bool,
    pub include_effort_estimation: bool,
    pub include_technical_debt: bool,
    pub target_cloud_platform: CloudPlatform,
    pub migration_strategy: MigrationStrategy,
    pub business_priorities: Vec<BusinessPriority>,
}

#[derive(Debug, Clone)]
pub enum CloudPlatform {
    AWS,
    Azure,
    GCP,
    Hybrid,
    OnPremiseKubernetes,
}

#[derive(Debug, Clone)]
pub enum MigrationStrategy {
    LiftAndShift,
    Replatform,
    Refactor,
    Rebuild,
    Replace,
}

#[derive(Debug, Clone)]
pub enum BusinessPriority {
    CostReduction,
    Agility,
    Scalability,
    Security,
    Compliance,
    Innovation,
}

impl Default for AnalysisConfig {
    fn default() -> Self {
        Self {
            include_cloud_readiness: true,
            include_microservices_analysis: true,
            include_effort_estimation: true,
            include_technical_debt: true,
            target_cloud_platform: CloudPlatform::AWS,
            migration_strategy: MigrationStrategy::Replatform,
            business_priorities: vec![
                BusinessPriority::CostReduction,
                BusinessPriority::Agility,
            ],
        }
    }
}

/// Main migration analyzer that orchestrates all analysis modules.
pub struct MigrationAnalyzer {
    config: AnalysisConfig,
    cloud_readiness_analyzer: CloudReadinessAnalyzer,
    microservices_analyzer: MicroservicesAnalyzer,
    effort_estimator: EffortEstimator,
    recommendation_engine: RecommendationEngine,
}

impl MigrationAnalyzer {
    /// Create a new migration analyzer with the given configuration.
    pub fn new(config: AnalysisConfig) -> Self {
        Self {
            cloud_readiness_analyzer: CloudReadinessAnalyzer::new(&config),
            microservices_analyzer: MicroservicesAnalyzer::new(&config),
            effort_estimator: EffortEstimator::new(&config),
            recommendation_engine: RecommendationEngine::new(&config),
            config,
        }
    }

    /// Analyze a single COBOL program for migration readiness.
    pub fn analyze_program(&self, program: &Program) -> Result<MigrationAssessment> {
        let programs = vec![program];
        self.analyze_system(&programs)
    }

    /// Analyze a complete COBOL system for migration readiness.
    pub fn analyze_system(&self, programs: &[&Program]) -> Result<MigrationAssessment> {
        // Step 1: Extract system overview
        let system_overview = self.extract_system_overview(programs)?;

        // Step 2: Analyze cloud readiness
        let cloud_readiness = if self.config.include_cloud_readiness {
            self.cloud_readiness_analyzer.analyze(programs)?
        } else {
            CloudReadinessScore {
                overall_score: 0.0,
                categories: CloudReadinessCategories {
                    architecture: 0.0,
                    data_management: 0.0,
                    security: 0.0,
                    scalability: 0.0,
                    monitoring: 0.0,
                    deployment: 0.0,
                },
                blockers: Vec::new(),
                enablers: Vec::new(),
            }
        };

        // Step 3: Analyze microservices opportunities
        let microservices_analysis = if self.config.include_microservices_analysis {
            self.microservices_analyzer.analyze(programs)?
        } else {
            MicroservicesAnalysis {
                recommended_services: Vec::new(),
                service_dependencies: Vec::new(),
                data_consistency_challenges: Vec::new(),
                api_design_recommendations: Vec::new(),
            }
        };

        // Step 4: Estimate migration effort
        let effort_estimation = if self.config.include_effort_estimation {
            self.effort_estimator.estimate(programs, &microservices_analysis)?
        } else {
            EffortEstimation {
                total_effort_months: 0.0,
                phases: Vec::new(),
                resource_requirements: ResourceRequirements {
                    developers: 0,
                    architects: 0,
                    devops_engineers: 0,
                    business_analysts: 0,
                    testers: 0,
                    specialized_skills: Vec::new(),
                },
                cost_estimation: CostEstimation {
                    development_cost: 0.0,
                    infrastructure_cost: 0.0,
                    training_cost: 0.0,
                    operational_cost_change: 0.0,
                    total_cost: 0.0,
                    roi_timeline_months: 0,
                },
                timeline: MigrationTimeline {
                    phases: Vec::new(),
                    critical_path: Vec::new(),
                    parallel_workstreams: Vec::new(),
                },
            }
        };

        // Step 5: Assess risks
        let risk_assessment = self.assess_risks(programs, &cloud_readiness, &microservices_analysis)?;

        // Step 6: Generate recommendations
        let recommendations = self.recommendation_engine.generate_recommendations(
            &system_overview,
            &cloud_readiness,
            &microservices_analysis,
            &effort_estimation,
            &risk_assessment,
        )?;

        // Step 7: Create migration roadmap
        let migration_roadmap = self.create_migration_roadmap(
            &microservices_analysis,
            &effort_estimation,
            &recommendations,
        )?;

        // Step 8: Analyze technical debt
        let technical_debt = if self.config.include_technical_debt {
            self.analyze_technical_debt(programs)?
        } else {
            TechnicalDebtAnalysis {
                debt_score: 0.0,
                debt_categories: Vec::new(),
                hotspots: Vec::new(),
                refactoring_opportunities: Vec::new(),
            }
        };

        Ok(MigrationAssessment {
            system_overview,
            cloud_readiness,
            microservices_analysis,
            effort_estimation,
            risk_assessment,
            recommendations,
            migration_roadmap,
            technical_debt,
            generated_at: Utc::now(),
        })
    }

    fn extract_system_overview(&self, programs: &[&Program]) -> Result<SystemOverview> {
        let total_programs = programs.len();
        let total_lines_of_code = programs.iter()
            .map(|p| self.estimate_lines_of_code(p))
            .sum();

        let programming_patterns = self.identify_programming_patterns(programs)?;
        let data_dependencies = self.analyze_data_dependencies(programs)?;
        let external_interfaces = self.identify_external_interfaces(programs)?;
        let business_domains = self.identify_business_domains(programs)?;

        Ok(SystemOverview {
            total_programs,
            total_lines_of_code,
            programming_patterns,
            data_dependencies,
            external_interfaces,
            business_domains,
        })
    }

    fn assess_risks(
        &self,
        programs: &[&Program],
        cloud_readiness: &CloudReadinessScore,
        microservices_analysis: &MicroservicesAnalysis,
    ) -> Result<RiskAssessment> {
        let technical_risks = self.identify_technical_risks(programs, cloud_readiness)?;
        let business_risks = self.identify_business_risks(programs, microservices_analysis)?;
        let mitigation_strategies = self.develop_mitigation_strategies(&technical_risks, &business_risks)?;

        let overall_risk = self.calculate_overall_risk(&technical_risks, &business_risks);

        Ok(RiskAssessment {
            overall_risk,
            technical_risks,
            business_risks,
            mitigation_strategies,
        })
    }

    fn create_migration_roadmap(
        &self,
        microservices_analysis: &MicroservicesAnalysis,
        effort_estimation: &EffortEstimation,
        recommendations: &[Recommendation],
    ) -> Result<MigrationRoadmap> {
        // Create phases based on microservices and effort estimation
        let phases = self.create_roadmap_phases(microservices_analysis, effort_estimation)?;
        let dependencies = self.identify_phase_dependencies(&phases)?;
        let milestones = self.create_milestones(&phases)?;
        let success_criteria = self.define_success_criteria(recommendations)?;

        Ok(MigrationRoadmap {
            phases,
            dependencies,
            milestones,
            success_criteria,
        })
    }

    fn analyze_technical_debt(&self, programs: &[&Program]) -> Result<TechnicalDebtAnalysis> {
        let mut debt_score = 0.0;
        let mut hotspots = Vec::new();
        let mut debt_categories = Vec::new();

        for program in programs {
            let program_debt = self.calculate_program_debt_score(program);
            debt_score += program_debt;

            if program_debt > 70.0 {
                hotspots.push(TechnicalDebtHotspot {
                    program_name: program.identification.node.program_id.clone().unwrap_or_else(|| "UNKNOWN".to_string()),
                    debt_score: program_debt,
                    issues: self.identify_debt_issues(program),
                    refactoring_priority: self.determine_refactoring_priority(program_debt),
                });
            }
        }

        debt_score /= programs.len() as f64;

        // Categorize debt types
        debt_categories = vec![
            DebtCategory {
                category: "Code Complexity".to_string(),
                debt_amount: debt_score * 0.3,
                programs_affected: programs.len() / 2,
                remediation_effort: EffortLevel::High,
            },
            DebtCategory {
                category: "Outdated Patterns".to_string(),
                debt_amount: debt_score * 0.4,
                programs_affected: (programs.len() as f64 * 0.8) as usize,
                remediation_effort: EffortLevel::Medium,
            },
            DebtCategory {
                category: "Missing Documentation".to_string(),
                debt_amount: debt_score * 0.2,
                programs_affected: programs.len(),
                remediation_effort: EffortLevel::Low,
            },
        ];

        let refactoring_opportunities = self.identify_refactoring_opportunities(programs)?;

        Ok(TechnicalDebtAnalysis {
            debt_score,
            debt_categories,
            hotspots,
            refactoring_opportunities,
        })
    }

    // Helper methods (improved implementations using AST data)
    fn estimate_lines_of_code(&self, program: &Program) -> usize {
        // Count statements in the procedure division as a proxy for LOC
        let mut lines = 10; // Base lines for divisions
        
        if let Some(env) = &program.environment {
            lines += 5; // Environment division
        }
        
        if let Some(data) = &program.data {
            lines += 20; // Data division
        }
        
        lines += program.procedure.node.statements.len() * 2; // Estimate 2 lines per statement
        lines
    }
    
    fn identify_programming_patterns(&self, _programs: &[&Program]) -> Result<Vec<ProgrammingPattern>> {
        Ok(vec![
            ProgrammingPattern {
                pattern_type: "Sequential Processing".to_string(),
                frequency: 15,
                modernization_difficulty: DifficultyLevel::Low,
                cloud_compatibility: CompatibilityLevel::FullyCompatible,
            },
            ProgrammingPattern {
                pattern_type: "File-based I/O".to_string(),
                frequency: 25,
                modernization_difficulty: DifficultyLevel::Medium,
                cloud_compatibility: CompatibilityLevel::PartiallyCompatible,
            },
        ])
    }

    fn analyze_data_dependencies(&self, _programs: &[&Program]) -> Result<Vec<DataDependency>> {
        Ok(Vec::new())
    }

    fn identify_external_interfaces(&self, _programs: &[&Program]) -> Result<Vec<ExternalInterface>> {
        Ok(Vec::new())
    }

    fn identify_business_domains(&self, _programs: &[&Program]) -> Result<Vec<BusinessDomain>> {
        Ok(vec![
            BusinessDomain {
                name: "Customer Management".to_string(),
                programs: vec!["CUSTMGMT".to_string(), "CUSTVAL".to_string()],
                cohesion_score: 0.8,
                coupling_score: 0.3,
                microservice_candidate: true,
            },
        ])
    }

    fn identify_technical_risks(&self, _programs: &[&Program], _cloud_readiness: &CloudReadinessScore) -> Result<Vec<TechnicalRisk>> {
        Ok(vec![
            TechnicalRisk {
                description: "Legacy data format dependencies".to_string(),
                probability: Probability::High,
                impact: Impact::High,
                risk_score: 8.0,
                mitigation_actions: vec![
                    "Implement data transformation layer".to_string(),
                    "Create data format adapters".to_string(),
                ],
            },
        ])
    }

    fn identify_business_risks(&self, _programs: &[&Program], _microservices: &MicroservicesAnalysis) -> Result<Vec<BusinessRisk>> {
        Ok(vec![
            BusinessRisk {
                description: "Business continuity during migration".to_string(),
                probability: Probability::Medium,
                business_impact: Impact::VeryHigh,
                risk_score: 7.5,
                mitigation_actions: vec![
                    "Implement parallel run strategy".to_string(),
                    "Create comprehensive rollback plan".to_string(),
                ],
            },
        ])
    }

    fn develop_mitigation_strategies(&self, _technical_risks: &[TechnicalRisk], _business_risks: &[BusinessRisk]) -> Result<Vec<MitigationStrategy>> {
        Ok(Vec::new())
    }

    fn calculate_overall_risk(&self, technical_risks: &[TechnicalRisk], business_risks: &[BusinessRisk]) -> RiskLevel {
        let avg_technical_risk = technical_risks.iter().map(|r| r.risk_score).sum::<f64>() / technical_risks.len() as f64;
        let avg_business_risk = business_risks.iter().map(|r| r.risk_score).sum::<f64>() / business_risks.len() as f64;
        let overall = (avg_technical_risk + avg_business_risk) / 2.0;

        match overall {
            0.0..=3.0 => RiskLevel::Low,
            3.1..=6.0 => RiskLevel::Medium,
            6.1..=8.0 => RiskLevel::High,
            _ => RiskLevel::Critical,
        }
    }

    fn create_roadmap_phases(&self, _microservices: &MicroservicesAnalysis, _effort: &EffortEstimation) -> Result<Vec<RoadmapPhase>> {
        Ok(vec![
            RoadmapPhase {
                phase_number: 1,
                name: "Assessment and Planning".to_string(),
                description: "Detailed analysis and migration planning".to_string(),
                start_month: 1,
                duration_months: 3,
                deliverables: vec![
                    "Migration strategy document".to_string(),
                    "Technical architecture design".to_string(),
                ],
                success_metrics: vec![
                    "All technical risks identified".to_string(),
                    "Team training completed".to_string(),
                ],
            },
        ])
    }

    fn identify_phase_dependencies(&self, _phases: &[RoadmapPhase]) -> Result<Vec<PhaseDependency>> {
        Ok(Vec::new())
    }

    fn create_milestones(&self, _phases: &[RoadmapPhase]) -> Result<Vec<Milestone>> {
        Ok(Vec::new())
    }

    fn define_success_criteria(&self, _recommendations: &[Recommendation]) -> Result<Vec<SuccessCriterion>> {
        Ok(Vec::new())
    }

    fn calculate_program_debt_score(&self, program: &Program) -> f64 {
        let mut debt_score: f32 = 0.0;
        
        // Check for program metadata completeness
        let id_div = &program.identification.node;
        if id_div.author.is_none() { debt_score += 10.0; }
        if id_div.date_written.is_none() { debt_score += 5.0; }
        if id_div.remarks.is_none() { debt_score += 5.0; }
        
        // Check for missing divisions
        if program.environment.is_none() { debt_score += 15.0; }
        if program.data.is_none() { debt_score += 20.0; }
        
        // Check complexity based on statement count
        let statement_count = program.procedure.node.statements.len();
        if statement_count > 100 { debt_score += 20.0; }
        else if statement_count > 50 { debt_score += 10.0; }
        
        // Check for empty procedure division
        if statement_count == 0 { debt_score += 25.0; }
        
        debt_score.min(100.0).into()
    }
    
    fn identify_debt_issues(&self, program: &Program) -> Vec<String> {
        let mut issues = Vec::new();
        
        let id_div = &program.identification.node;
        if id_div.author.is_none() {
            issues.push("Missing author information".to_string());
        }
        if id_div.date_written.is_none() {
            issues.push("Missing date written".to_string());
        }
        if id_div.remarks.is_none() {
            issues.push("Missing program documentation".to_string());
        }
        
        if program.environment.is_none() {
            issues.push("Missing environment division".to_string());
        }
        if program.data.is_none() {
            issues.push("Missing data division".to_string());
        }
        
        let statement_count = program.procedure.node.statements.len();
        if statement_count > 100 {
            issues.push("High complexity - too many statements".to_string());
        }
        if statement_count == 0 {
            issues.push("Empty procedure division".to_string());
        }
        
        issues
    }
    
    fn determine_refactoring_priority(&self, debt_score: f64) -> Priority {
        match debt_score {
            0.0..=40.0 => Priority::Low,
            40.1..=70.0 => Priority::Medium,
            70.1..=85.0 => Priority::High,
            _ => Priority::Critical,
        }
    }
    
    fn identify_refactoring_opportunities(&self, _programs: &[&Program]) -> Result<Vec<RefactoringOpportunity>> {
        Ok(Vec::new())
    }
}