use crate::analysis::AnalysisConfig;
use crate::models::*;
use anyhow::Result;
use uuid::Uuid;

/// Engine for generating migration recommendations.
pub struct RecommendationEngine {
    config: AnalysisConfig,
}

impl RecommendationEngine {
    pub fn new(config: &AnalysisConfig) -> Self {
        Self {
            config: config.clone(),
        }
    }

    /// Generate comprehensive migration recommendations.
    pub fn generate_recommendations(
        &self,
        system_overview: &SystemOverview,
        cloud_readiness: &CloudReadinessScore,
        microservices_analysis: &MicroservicesAnalysis,
        effort_estimation: &EffortEstimation,
        risk_assessment: &RiskAssessment,
    ) -> Result<Vec<Recommendation>> {
        let mut recommendations = Vec::new();

        // Architecture recommendations
        recommendations.extend(
            self.generate_architecture_recommendations(system_overview, microservices_analysis)?,
        );

        // Cloud readiness recommendations
        recommendations.extend(self.generate_cloud_readiness_recommendations(cloud_readiness)?);

        // Risk mitigation recommendations
        recommendations.extend(self.generate_risk_mitigation_recommendations(risk_assessment)?);

        // Process and organizational recommendations
        recommendations.extend(self.generate_process_recommendations(effort_estimation)?);

        // Technology recommendations
        recommendations.extend(
            self.generate_technology_recommendations(system_overview, microservices_analysis)?,
        );

        // Security recommendations
        recommendations.extend(self.generate_security_recommendations(cloud_readiness)?);

        // Performance recommendations
        recommendations.extend(
            self.generate_performance_recommendations(system_overview, microservices_analysis)?,
        );

        // Sort by priority and impact
        recommendations.sort_by(|a, b| {
            let a_score = self.calculate_recommendation_score(a);
            let b_score = self.calculate_recommendation_score(b);
            b_score.partial_cmp(&a_score).unwrap()
        });

        Ok(recommendations)
    }

    fn generate_architecture_recommendations(
        &self,
        system_overview: &SystemOverview,
        microservices_analysis: &MicroservicesAnalysis,
    ) -> Result<Vec<Recommendation>> {
        let mut recommendations = Vec::new();

        // Microservices decomposition
        if microservices_analysis.recommended_services.len() > 1 {
            recommendations.push(Recommendation {
                id: Uuid::new_v4().to_string(),
                title: "Implement Microservices Architecture".to_string(),
                description: format!(
                    "Decompose the monolithic COBOL system into {} microservices based on business domain boundaries. This will improve maintainability, scalability, and deployment flexibility.",
                    microservices_analysis.recommended_services.len()
                ),
                category: RecommendationCategory::Architecture,
                priority: Priority::High,
                effort_level: EffortLevel::High,
                impact: Impact::VeryHigh,
                prerequisites: vec![
                    "Complete technical assessment".to_string(),
                    "Define service boundaries".to_string(),
                    "Establish API contracts".to_string(),
                ],
            });
        }

        // API Gateway implementation
        if microservices_analysis.recommended_services.len() > 2 {
            recommendations.push(Recommendation {
                id: Uuid::new_v4().to_string(),
                title: "Implement API Gateway".to_string(),
                description: "Deploy an API Gateway to manage service-to-service communication, handle authentication, rate limiting, and provide a unified entry point for external clients.".to_string(),
                category: RecommendationCategory::Architecture,
                priority: Priority::Medium,
                effort_level: EffortLevel::Medium,
                impact: Impact::High,
                prerequisites: vec![
                    "Microservices deployment".to_string(),
                    "API specifications defined".to_string(),
                ],
            });
        }

        // Event-driven architecture
        if microservices_analysis.service_dependencies.len() > 3 {
            recommendations.push(Recommendation {
                id: Uuid::new_v4().to_string(),
                title: "Adopt Event-Driven Architecture".to_string(),
                description: "Implement event-driven patterns to reduce coupling between services and improve system resilience. Use message queues or event streams for asynchronous communication.".to_string(),
                category: RecommendationCategory::Architecture,
                priority: Priority::Medium,
                effort_level: EffortLevel::High,
                impact: Impact::High,
                prerequisites: vec![
                    "Service boundaries established".to_string(),
                    "Event schema design".to_string(),
                    "Message broker selection".to_string(),
                ],
            });
        }

        // Data architecture modernization
        if system_overview
            .external_interfaces
            .iter()
            .any(|i| matches!(i.interface_type, InterfaceType::FileSystem))
        {
            recommendations.push(Recommendation {
                id: Uuid::new_v4().to_string(),
                title: "Modernize Data Architecture".to_string(),
                description: "Replace file-based data storage with modern databases and implement proper data management patterns. Consider database-per-service pattern for microservices.".to_string(),
                category: RecommendationCategory::Architecture,
                priority: Priority::High,
                effort_level: EffortLevel::High,
                impact: Impact::VeryHigh,
                prerequisites: vec![
                    "Data mapping and analysis".to_string(),
                    "Database technology selection".to_string(),
                    "Migration strategy definition".to_string(),
                ],
            });
        }

        Ok(recommendations)
    }

    fn generate_cloud_readiness_recommendations(
        &self,
        cloud_readiness: &CloudReadinessScore,
    ) -> Result<Vec<Recommendation>> {
        let mut recommendations = Vec::new();

        // Address critical blockers
        for blocker in &cloud_readiness.blockers {
            if matches!(blocker.severity, Severity::Critical | Severity::High) {
                recommendations.push(Recommendation {
                    id: Uuid::new_v4().to_string(),
                    title: format!(
                        "Address Critical Blocker: {}",
                        self.extract_blocker_title(&blocker.description)
                    ),
                    description: format!(
                        "{} Recommended workarounds: {}",
                        blocker.description,
                        blocker.workarounds.join(", ")
                    ),
                    category: RecommendationCategory::Technology,
                    priority: Priority::Critical,
                    effort_level: blocker.resolution_effort.clone(),
                    impact: Impact::VeryHigh,
                    prerequisites: vec!["Technical assessment completion".to_string()],
                });
            }
        }

        // Improve specific categories with low scores
        if cloud_readiness.categories.security < 70.0 {
            recommendations.push(Recommendation {
                id: Uuid::new_v4().to_string(),
                title: "Enhance Security Posture".to_string(),
                description: "Implement comprehensive security controls including input validation, proper authentication, authorization, and audit logging before cloud migration.".to_string(),
                category: RecommendationCategory::Security,
                priority: Priority::High,
                effort_level: EffortLevel::Medium,
                impact: Impact::High,
                prerequisites: vec![
                    "Security audit completion".to_string(),
                    "Security framework selection".to_string(),
                ],
            });
        }

        if cloud_readiness.categories.monitoring < 60.0 {
            recommendations.push(Recommendation {
                id: Uuid::new_v4().to_string(),
                title: "Implement Observability Strategy".to_string(),
                description: "Establish comprehensive monitoring, logging, and tracing capabilities to ensure system visibility in cloud environment.".to_string(),
                category: RecommendationCategory::Technology,
                priority: Priority::Medium,
                effort_level: EffortLevel::Medium,
                impact: Impact::High,
                prerequisites: vec![
                    "Observability tool selection".to_string(),
                    "Metrics definition".to_string(),
                ],
            });
        }

        if cloud_readiness.categories.scalability < 65.0 {
            recommendations.push(Recommendation {
                id: Uuid::new_v4().to_string(),
                title: "Design for Cloud Scalability".to_string(),
                description: "Refactor application to support horizontal scaling, implement stateless design patterns, and optimize resource usage for cloud environments.".to_string(),
                category: RecommendationCategory::Architecture,
                priority: Priority::Medium,
                effort_level: EffortLevel::High,
                impact: Impact::High,
                prerequisites: vec![
                    "Performance analysis".to_string(),
                    "Scalability requirements definition".to_string(),
                ],
            });
        }

        Ok(recommendations)
    }

    fn generate_risk_mitigation_recommendations(
        &self,
        risk_assessment: &RiskAssessment,
    ) -> Result<Vec<Recommendation>> {
        let mut recommendations = Vec::new();

        // Address high-risk technical issues
        for risk in &risk_assessment.technical_risks {
            if risk.risk_score > 7.0 {
                recommendations.push(Recommendation {
                    id: Uuid::new_v4().to_string(),
                    title: format!(
                        "Mitigate High Technical Risk: {}",
                        self.extract_risk_title(&risk.description)
                    ),
                    description: format!(
                        "{} Recommended actions: {}",
                        risk.description,
                        risk.mitigation_actions.join(", ")
                    ),
                    category: RecommendationCategory::Technology,
                    priority: Priority::High,
                    effort_level: self.map_risk_to_effort(&risk.impact),
                    impact: risk.impact.clone(),
                    prerequisites: vec!["Risk assessment completion".to_string()],
                });
            }
        }

        // Address high-risk business issues
        for risk in &risk_assessment.business_risks {
            if risk.risk_score > 7.0 {
                recommendations.push(Recommendation {
                    id: Uuid::new_v4().to_string(),
                    title: format!(
                        "Address Business Risk: {}",
                        self.extract_risk_title(&risk.description)
                    ),
                    description: format!(
                        "{} Recommended actions: {}",
                        risk.description,
                        risk.mitigation_actions.join(", ")
                    ),
                    category: RecommendationCategory::Process,
                    priority: Priority::High,
                    effort_level: self.map_risk_to_effort(&risk.business_impact),
                    impact: risk.business_impact.clone(),
                    prerequisites: vec!["Stakeholder alignment".to_string()],
                });
            }
        }

        // Overall risk management
        if matches!(
            risk_assessment.overall_risk,
            RiskLevel::High | RiskLevel::Critical
        ) {
            recommendations.push(Recommendation {
                id: Uuid::new_v4().to_string(),
                title: "Implement Comprehensive Risk Management".to_string(),
                description: "Establish a formal risk management process with regular assessment, monitoring, and mitigation activities throughout the migration project.".to_string(),
                category: RecommendationCategory::Process,
                priority: Priority::Critical,
                effort_level: EffortLevel::Medium,
                impact: Impact::VeryHigh,
                prerequisites: vec![
                    "Risk management framework selection".to_string(),
                    "Risk register establishment".to_string(),
                ],
            });
        }

        Ok(recommendations)
    }

    fn generate_process_recommendations(
        &self,
        effort_estimation: &EffortEstimation,
    ) -> Result<Vec<Recommendation>> {
        let mut recommendations = Vec::new();

        // Phased migration approach
        if effort_estimation.total_effort_months > 12.0 {
            recommendations.push(Recommendation {
                id: Uuid::new_v4().to_string(),
                title: "Implement Phased Migration Strategy".to_string(),
                description: "Execute migration in phases to reduce risk, enable early value delivery, and allow for learning and adjustment throughout the process.".to_string(),
                category: RecommendationCategory::Process,
                priority: Priority::High,
                effort_level: EffortLevel::Medium,
                impact: Impact::High,
                prerequisites: vec![
                    "Phase definition".to_string(),
                    "Success criteria establishment".to_string(),
                ],
            });
        }

        // Team training and skill development
        recommendations.push(Recommendation {
            id: Uuid::new_v4().to_string(),
            title: "Invest in Team Training and Skill Development".to_string(),
            description: "Provide comprehensive training on cloud technologies, microservices patterns, and modern development practices to ensure team readiness.".to_string(),
            category: RecommendationCategory::Organization,
            priority: Priority::High,
            effort_level: EffortLevel::Medium,
            impact: Impact::High,
            prerequisites: vec![
                "Skill gap analysis".to_string(),
                "Training program design".to_string(),
            ],
        });

        // DevOps and automation
        recommendations.push(Recommendation {
            id: Uuid::new_v4().to_string(),
            title: "Establish DevOps Practices and Automation".to_string(),
            description: "Implement CI/CD pipelines, infrastructure as code, and automated testing to support efficient and reliable migration and ongoing operations.".to_string(),
            category: RecommendationCategory::Process,
            priority: Priority::Medium,
            effort_level: EffortLevel::High,
            impact: Impact::High,
            prerequisites: vec![
                "DevOps tool selection".to_string(),
                "Pipeline design".to_string(),
            ],
        });

        Ok(recommendations)
    }

    fn generate_technology_recommendations(
        &self,
        system_overview: &SystemOverview,
        microservices_analysis: &MicroservicesAnalysis,
    ) -> Result<Vec<Recommendation>> {
        let mut recommendations = Vec::new();

        // Container adoption
        recommendations.push(Recommendation {
            id: Uuid::new_v4().to_string(),
            title: "Adopt Containerization Strategy".to_string(),
            description: "Package applications in containers to improve portability, scalability, and deployment consistency across environments.".to_string(),
            category: RecommendationCategory::Technology,
            priority: Priority::High,
            effort_level: EffortLevel::Medium,
            impact: Impact::High,
            prerequisites: vec![
                "Container platform selection".to_string(),
                "Container image design".to_string(),
            ],
        });

        // API management
        if microservices_analysis.recommended_services.len() > 1 {
            recommendations.push(Recommendation {
                id: Uuid::new_v4().to_string(),
                title: "Implement API Management Platform".to_string(),
                description: "Deploy API management tools to handle API lifecycle, security, monitoring, and developer portal capabilities.".to_string(),
                category: RecommendationCategory::Technology,
                priority: Priority::Medium,
                effort_level: EffortLevel::Medium,
                impact: Impact::Medium,
                prerequisites: vec![
                    "API management tool evaluation".to_string(),
                    "API standards definition".to_string(),
                ],
            });
        }

        // Data modernization technology
        if system_overview
            .external_interfaces
            .iter()
            .any(|i| matches!(i.interface_type, InterfaceType::FileSystem))
        {
            recommendations.push(Recommendation {
                id: Uuid::new_v4().to_string(),
                title: "Select Modern Data Technologies".to_string(),
                description: "Choose appropriate cloud-native databases and data processing technologies that align with microservices architecture and scalability requirements.".to_string(),
                category: RecommendationCategory::Technology,
                priority: Priority::High,
                effort_level: EffortLevel::High,
                impact: Impact::VeryHigh,
                prerequisites: vec![
                    "Data requirements analysis".to_string(),
                    "Technology evaluation".to_string(),
                ],
            });
        }

        Ok(recommendations)
    }

    fn generate_security_recommendations(
        &self,
        cloud_readiness: &CloudReadinessScore,
    ) -> Result<Vec<Recommendation>> {
        let mut recommendations = Vec::new();

        if cloud_readiness.categories.security < 80.0 {
            recommendations.push(Recommendation {
                id: Uuid::new_v4().to_string(),
                title: "Implement Zero Trust Security Model".to_string(),
                description: "Adopt zero trust principles with identity-based access control, network segmentation, and continuous security monitoring.".to_string(),
                category: RecommendationCategory::Security,
                priority: Priority::High,
                effort_level: EffortLevel::High,
                impact: Impact::VeryHigh,
                prerequisites: vec![
                    "Security architecture design".to_string(),
                    "Identity provider selection".to_string(),
                ],
            });

            recommendations.push(Recommendation {
                id: Uuid::new_v4().to_string(),
                title: "Establish Security Scanning and Compliance".to_string(),
                description: "Implement automated security scanning for vulnerabilities, compliance checking, and security testing in CI/CD pipelines.".to_string(),
                category: RecommendationCategory::Security,
                priority: Priority::Medium,
                effort_level: EffortLevel::Medium,
                impact: Impact::High,
                prerequisites: vec![
                    "Security tool selection".to_string(),
                    "Compliance requirements definition".to_string(),
                ],
            });
        }

        Ok(recommendations)
    }

    fn generate_performance_recommendations(
        &self,
        _system_overview: &SystemOverview,
        microservices_analysis: &MicroservicesAnalysis,
    ) -> Result<Vec<Recommendation>> {
        let mut recommendations = Vec::new();

        // Performance monitoring
        recommendations.push(Recommendation {
            id: Uuid::new_v4().to_string(),
            title: "Implement Performance Monitoring and APM".to_string(),
            description: "Deploy application performance monitoring tools to track system performance, identify bottlenecks, and ensure SLA compliance.".to_string(),
            category: RecommendationCategory::Performance,
            priority: Priority::Medium,
            effort_level: EffortLevel::Medium,
            impact: Impact::High,
            prerequisites: vec![
                "APM tool selection".to_string(),
                "Performance baseline establishment".to_string(),
            ],
        });

        // Caching strategy
        if microservices_analysis.recommended_services.len() > 2 {
            recommendations.push(Recommendation {
                id: Uuid::new_v4().to_string(),
                title: "Design Distributed Caching Strategy".to_string(),
                description: "Implement caching layers to improve performance and reduce load on backend services, especially for frequently accessed data.".to_string(),
                category: RecommendationCategory::Performance,
                priority: Priority::Low,
                effort_level: EffortLevel::Medium,
                impact: Impact::Medium,
                prerequisites: vec![
                    "Caching requirements analysis".to_string(),
                    "Cache technology selection".to_string(),
                ],
            });
        }

        Ok(recommendations)
    }

    // Helper methods
    fn calculate_recommendation_score(&self, recommendation: &Recommendation) -> f64 {
        let priority_weight = match recommendation.priority {
            Priority::Critical => 4.0,
            Priority::High => 3.0,
            Priority::Medium => 2.0,
            Priority::Low => 1.0,
        };

        let impact_weight = match recommendation.impact {
            Impact::VeryHigh => 4.0,
            Impact::High => 3.0,
            Impact::Medium => 2.0,
            Impact::Low => 1.0,
        };

        let effort_penalty = match recommendation.effort_level {
            EffortLevel::Low => 0.0,
            EffortLevel::Medium => 0.5,
            EffortLevel::High => 1.0,
            EffortLevel::VeryHigh => 1.5,
        };

        (priority_weight * 2.0 + impact_weight) - effort_penalty
    }

    fn extract_blocker_title(&self, description: &str) -> String {
        description
            .split('.')
            .next()
            .unwrap_or(description)
            .to_string()
    }

    fn extract_risk_title(&self, description: &str) -> String {
        description
            .split('.')
            .next()
            .unwrap_or(description)
            .to_string()
    }

    fn map_risk_to_effort(&self, impact: &Impact) -> EffortLevel {
        match impact {
            Impact::Low => EffortLevel::Low,
            Impact::Medium => EffortLevel::Medium,
            Impact::High => EffortLevel::High,
            Impact::VeryHigh => EffortLevel::VeryHigh,
        }
    }
}
