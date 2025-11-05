use crate::analysis::AnalysisConfig;
use crate::models::*;
use anyhow::Result;
use cobol_ast::Program;

/// Estimator for migration effort and costs.
pub struct EffortEstimator {
    config: AnalysisConfig,
}

impl EffortEstimator {
    pub fn new(config: &AnalysisConfig) -> Self {
        Self {
            config: config.clone(),
        }
    }

    /// Estimate migration effort for a set of programs and microservices.
    pub fn estimate(
        &self,
        programs: &[&Program],
        microservices_analysis: &MicroservicesAnalysis,
    ) -> Result<EffortEstimation> {
        let phases = self.create_migration_phases(programs, microservices_analysis)?;
        let total_effort_months = phases.iter().map(|p| p.duration_months).sum();
        let resource_requirements = self.calculate_resource_requirements(&phases)?;
        let cost_estimation = self.estimate_costs(&phases, &resource_requirements)?;
        let timeline = self.create_timeline(&phases)?;

        Ok(EffortEstimation {
            total_effort_months,
            phases,
            resource_requirements,
            cost_estimation,
            timeline,
        })
    }

    fn create_migration_phases(
        &self,
        programs: &[&Program],
        microservices_analysis: &MicroservicesAnalysis,
    ) -> Result<Vec<MigrationPhase>> {
        let mut phases = Vec::new();

        // Phase 1: Assessment and Planning
        phases.push(MigrationPhase {
            name: "Assessment and Planning".to_string(),
            description: "Detailed technical assessment and migration planning".to_string(),
            duration_months: self.estimate_planning_duration(programs),
            deliverables: vec![
                "Technical assessment report".to_string(),
                "Migration strategy document".to_string(),
                "Target architecture design".to_string(),
                "Risk mitigation plan".to_string(),
                "Team training plan".to_string(),
            ],
            success_criteria: vec![
                "All technical dependencies identified".to_string(),
                "Migration strategy approved by stakeholders".to_string(),
                "Development team trained on target technologies".to_string(),
            ],
            risks: vec![
                "Incomplete understanding of legacy system dependencies".to_string(),
                "Stakeholder alignment on migration approach".to_string(),
            ],
        });

        // Phase 2: Infrastructure Setup
        phases.push(MigrationPhase {
            name: "Infrastructure Setup".to_string(),
            description: "Setup cloud infrastructure and development environment".to_string(),
            duration_months: self.estimate_infrastructure_duration(),
            deliverables: vec![
                "Cloud infrastructure provisioned".to_string(),
                "CI/CD pipelines established".to_string(),
                "Monitoring and logging systems configured".to_string(),
                "Security controls implemented".to_string(),
            ],
            success_criteria: vec![
                "Development environment fully operational".to_string(),
                "Security scan passes for infrastructure".to_string(),
                "Monitoring dashboards displaying key metrics".to_string(),
            ],
            risks: vec![
                "Cloud platform learning curve".to_string(),
                "Security compliance requirements".to_string(),
            ],
        });

        // Phase 3: Core Service Migration
        let core_services_duration = self.estimate_core_services_duration(microservices_analysis);
        phases.push(MigrationPhase {
            name: "Core Services Migration".to_string(),
            description: "Migrate core business logic to microservices".to_string(),
            duration_months: core_services_duration,
            deliverables: self.create_core_services_deliverables(microservices_analysis),
            success_criteria: vec![
                "All core services deployed and functional".to_string(),
                "API integration tests passing".to_string(),
                "Performance benchmarks met".to_string(),
            ],
            risks: vec![
                "Data consistency challenges".to_string(),
                "Service integration complexity".to_string(),
                "Performance degradation during migration".to_string(),
            ],
        });

        // Phase 4: Data Migration
        phases.push(MigrationPhase {
            name: "Data Migration".to_string(),
            description: "Migrate data and establish data synchronization".to_string(),
            duration_months: self.estimate_data_migration_duration(programs),
            deliverables: vec![
                "Data migration scripts developed and tested".to_string(),
                "Data validation and reconciliation tools".to_string(),
                "Rollback procedures documented".to_string(),
                "Data security controls implemented".to_string(),
            ],
            success_criteria: vec![
                "100% data integrity verification".to_string(),
                "Zero data loss during migration".to_string(),
                "Performance meets or exceeds legacy system".to_string(),
            ],
            risks: vec![
                "Data corruption during migration".to_string(),
                "Extended downtime requirements".to_string(),
                "Data format compatibility issues".to_string(),
            ],
        });

        // Phase 5: Testing and Validation
        phases.push(MigrationPhase {
            name: "Testing and Validation".to_string(),
            description: "Comprehensive testing and performance validation".to_string(),
            duration_months: self.estimate_testing_duration(programs),
            deliverables: vec![
                "Automated test suite covering all functionality".to_string(),
                "Performance test results".to_string(),
                "Security penetration test report".to_string(),
                "User acceptance testing completion".to_string(),
            ],
            success_criteria: vec![
                "All functional tests passing".to_string(),
                "Performance targets achieved".to_string(),
                "Security vulnerabilities addressed".to_string(),
                "User acceptance criteria met".to_string(),
            ],
            risks: vec![
                "Functional regressions discovered late".to_string(),
                "Performance issues under load".to_string(),
                "User adoption challenges".to_string(),
            ],
        });

        // Phase 6: Production Deployment
        phases.push(MigrationPhase {
            name: "Production Deployment".to_string(),
            description: "Deploy to production and transition operations".to_string(),
            duration_months: self.estimate_deployment_duration(),
            deliverables: vec![
                "Production deployment executed".to_string(),
                "Operations runbooks completed".to_string(),
                "Support team training conducted".to_string(),
                "Legacy system decommissioning plan".to_string(),
            ],
            success_criteria: vec![
                "System operational in production".to_string(),
                "All monitoring alerts configured".to_string(),
                "Support team ready for operations".to_string(),
                "Legacy system successfully decommissioned".to_string(),
            ],
            risks: vec![
                "Production deployment issues".to_string(),
                "Operations team readiness".to_string(),
                "Business impact during cutover".to_string(),
            ],
        });

        Ok(phases)
    }

    fn calculate_resource_requirements(&self, phases: &[MigrationPhase]) -> Result<ResourceRequirements> {
        // Calculate peak resource requirements across all phases
        let max_duration = phases.iter().map(|p| p.duration_months).fold(0.0, f64::max);
        let complexity_factor = self.calculate_complexity_factor(phases);

        Ok(ResourceRequirements {
            developers: self.estimate_developers_needed(max_duration, complexity_factor),
            architects: self.estimate_architects_needed(),
            devops_engineers: self.estimate_devops_engineers_needed(),
            business_analysts: self.estimate_business_analysts_needed(),
            testers: self.estimate_testers_needed(max_duration),
            specialized_skills: vec![
                "COBOL expertise".to_string(),
                "Cloud architecture".to_string(),
                "Microservices design".to_string(),
                "Data migration".to_string(),
                "DevOps automation".to_string(),
            ],
        })
    }

    fn estimate_costs(
        &self,
        phases: &[MigrationPhase],
        resources: &ResourceRequirements,
    ) -> Result<CostEstimation> {
        let total_duration = phases.iter().map(|p| p.duration_months).sum::<f64>();
        
        // Average salaries (monthly, fully loaded)
        let developer_cost_per_month = 12000.0;
        let architect_cost_per_month = 18000.0;
        let devops_cost_per_month = 14000.0;
        let analyst_cost_per_month = 10000.0;
        let tester_cost_per_month = 9000.0;

        let development_cost = total_duration * (
            resources.developers as f64 * developer_cost_per_month +
            resources.architects as f64 * architect_cost_per_month +
            resources.devops_engineers as f64 * devops_cost_per_month +
            resources.business_analysts as f64 * analyst_cost_per_month +
            resources.testers as f64 * tester_cost_per_month
        );

        let infrastructure_cost = self.estimate_infrastructure_costs(total_duration);
        let training_cost = self.estimate_training_costs(resources);
        let operational_cost_change = self.estimate_operational_cost_change();

        let total_cost = development_cost + infrastructure_cost + training_cost;

        Ok(CostEstimation {
            development_cost,
            infrastructure_cost,
            training_cost,
            operational_cost_change,
            total_cost,
            roi_timeline_months: self.estimate_roi_timeline(total_cost, operational_cost_change),
        })
    }

    fn create_timeline(&self, phases: &[MigrationPhase]) -> Result<MigrationTimeline> {
        let mut timeline_phases = Vec::new();
        let mut current_month = 1;

        for phase in phases {
            timeline_phases.push(TimelinePhase {
                name: phase.name.clone(),
                start_month: current_month,
                end_month: current_month + phase.duration_months as usize - 1,
                milestones: phase.success_criteria.clone(),
            });
            current_month += phase.duration_months as usize;
        }

        let critical_path = phases.iter().map(|p| p.name.clone()).collect();
        
        let parallel_workstreams = vec![
            Workstream {
                name: "Infrastructure Team".to_string(),
                phases: vec!["Infrastructure Setup".to_string(), "Production Deployment".to_string()],
                team_size: 3,
            },
            Workstream {
                name: "Development Team".to_string(),
                phases: vec!["Core Services Migration".to_string(), "Testing and Validation".to_string()],
                team_size: 6,
            },
            Workstream {
                name: "Data Team".to_string(),
                phases: vec!["Data Migration".to_string()],
                team_size: 2,
            },
        ];

        Ok(MigrationTimeline {
            phases: timeline_phases,
            critical_path,
            parallel_workstreams,
        })
    }

    // Helper methods for estimation
    fn estimate_planning_duration(&self, programs: &[&Program]) -> f64 {
        let base_duration = 2.0;
        let complexity_factor = (programs.len() as f64 / 10.0).min(2.0);
        base_duration + complexity_factor
    }

    fn estimate_infrastructure_duration(&self) -> f64 {
        match self.config.target_cloud_platform {
            crate::analysis::CloudPlatform::AWS => 1.5,
            crate::analysis::CloudPlatform::Azure => 1.5,
            crate::analysis::CloudPlatform::GCP => 1.5,
            crate::analysis::CloudPlatform::Hybrid => 3.0,
            crate::analysis::CloudPlatform::OnPremiseKubernetes => 2.5,
        }
    }

    fn estimate_core_services_duration(&self, microservices_analysis: &MicroservicesAnalysis) -> f64 {
        let service_count = microservices_analysis.recommended_services.len();
        let complexity_factor = microservices_analysis.service_dependencies.len() as f64 * 0.5;
        (service_count as f64 * 1.5) + complexity_factor
    }

    fn estimate_data_migration_duration(&self, programs: &[&Program]) -> f64 {
        let base_duration = 2.0;
        let program_factor = (programs.len() as f64 / 5.0).min(3.0);
        base_duration + program_factor
    }

    fn estimate_testing_duration(&self, programs: &[&Program]) -> f64 {
        let base_duration = 1.5;
        let complexity_factor = (programs.len() as f64 / 8.0).min(2.0);
        base_duration + complexity_factor
    }

    fn estimate_deployment_duration(&self) -> f64 { 1.0 }

    fn calculate_complexity_factor(&self, phases: &[MigrationPhase]) -> f64 {
        let total_risks = phases.iter().map(|p| p.risks.len()).sum::<usize>();
        (total_risks as f64 / 10.0).min(2.0)
    }

    fn estimate_developers_needed(&self, duration: f64, complexity: f64) -> usize {
        let base_developers = 4;
        let additional = ((duration / 6.0) * complexity) as usize;
        base_developers + additional
    }

    fn estimate_architects_needed(&self) -> usize { 2 }
    fn estimate_devops_engineers_needed(&self) -> usize { 2 }
    fn estimate_business_analysts_needed(&self) -> usize { 1 }
    fn estimate_testers_needed(&self, duration: f64) -> usize {
        if duration > 12.0 { 3 } else { 2 }
    }

    fn create_core_services_deliverables(&self, microservices_analysis: &MicroservicesAnalysis) -> Vec<String> {
        let mut deliverables = vec![
            "Service architecture documentation".to_string(),
            "API specifications".to_string(),
            "Database schemas".to_string(),
        ];

        for service in &microservices_analysis.recommended_services {
            deliverables.push(format!("{} service implementation", service.name));
        }

        deliverables
    }

    fn estimate_infrastructure_costs(&self, duration_months: f64) -> f64 {
        // Monthly cloud infrastructure costs
        let monthly_infrastructure_cost = 5000.0;
        duration_months * monthly_infrastructure_cost
    }

    fn estimate_training_costs(&self, resources: &ResourceRequirements) -> f64 {
        let total_people = resources.developers + resources.architects + 
                          resources.devops_engineers + resources.business_analysts + 
                          resources.testers;
        total_people as f64 * 2000.0 // $2000 per person for training
    }

    fn estimate_operational_cost_change(&self) -> f64 {
        // Estimated monthly operational cost savings after migration
        -3000.0 // Negative indicates savings
    }

    fn estimate_roi_timeline(&self, total_cost: f64, monthly_savings: f64) -> usize {
        if monthly_savings >= 0.0 {
            60 // 5 years if no savings
        } else {
            (total_cost / (-monthly_savings)) as usize
        }
    }
}