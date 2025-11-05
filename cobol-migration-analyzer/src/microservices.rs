use crate::analysis::AnalysisConfig;
use crate::models::*;
use anyhow::Result;
use cobol_ast::Program;
use std::collections::{HashMap, HashSet};

/// Analyzer for microservices architecture opportunities.
pub struct MicroservicesAnalyzer {
    config: AnalysisConfig,
}

impl MicroservicesAnalyzer {
    pub fn new(config: &AnalysisConfig) -> Self {
        Self {
            config: config.clone(),
        }
    }

    /// Analyze microservices opportunities for a set of COBOL programs.
    pub fn analyze(&self, programs: &[&Program]) -> Result<MicroservicesAnalysis> {
        let recommended_services = self.identify_microservice_candidates(programs)?;
        let service_dependencies = self.analyze_service_dependencies(&recommended_services)?;
        let data_consistency_challenges = self.identify_data_consistency_challenges(&recommended_services)?;
        let api_design_recommendations = self.generate_api_recommendations(&recommended_services)?;

        Ok(MicroservicesAnalysis {
            recommended_services,
            service_dependencies,
            data_consistency_challenges,
            api_design_recommendations,
        })
    }

    fn identify_microservice_candidates(&self, programs: &[&Program]) -> Result<Vec<MicroserviceCandidate>> {
        let mut candidates = Vec::new();

        // Group programs by business domain
        let domain_groups = self.group_programs_by_domain(programs);

        for (domain, domain_programs) in domain_groups {
            let cohesion_score = self.calculate_cohesion_score(&domain_programs);
            let coupling_score = self.calculate_coupling_score(&domain_programs, programs);

            // A good microservice candidate has high cohesion and low coupling
            if cohesion_score > 0.7 && coupling_score < 0.4 {
                let candidate = MicroserviceCandidate {
                    name: self.generate_service_name(&domain),
                    programs: domain_programs.iter().map(|p| 
                        p.identification.node.program_id.clone().unwrap_or_else(|| "UNKNOWN".to_string())
                    ).collect(),
                    business_capability: domain.clone(),
                    data_entities: self.extract_data_entities(&domain_programs),
                    estimated_size: self.estimate_service_size(&domain_programs),
                    migration_complexity: self.assess_migration_complexity(&domain_programs),
                    dependencies: self.identify_service_dependencies(&domain_programs, programs),
                };
                candidates.push(candidate);
            }
        }

        // If no natural groupings found, create service candidates based on size
        if candidates.is_empty() {
            candidates = self.create_size_based_services(programs)?;
        }

        Ok(candidates)
    }

    fn group_programs_by_domain<'a>(&self, programs: &[&'a Program]) -> HashMap<String, Vec<&'a Program>> {
        let mut domain_groups: HashMap<String, Vec<&Program>> = HashMap::new();

        for program in programs {
            let domain = self.infer_business_domain(program);
            domain_groups.entry(domain).or_default().push(program);
        }

        domain_groups
    }

    fn infer_business_domain(&self, program: &Program) -> String {
        let unknown = "UNKNOWN".to_string();
        let program_id = program.identification.node.program_id.as_ref().unwrap_or(&unknown);
        
        // Simple heuristics based on program naming conventions
        if program_id.contains("CUST") || program_id.contains("CLIENT") {
            "Customer Management".to_string()
        } else if program_id.contains("ORDER") || program_id.contains("SALE") {
            "Order Management".to_string()
        } else if program_id.contains("INV") || program_id.contains("STOCK") {
            "Inventory Management".to_string()
        } else if program_id.contains("PAY") || program_id.contains("BILL") {
            "Payment Processing".to_string()
        } else if program_id.contains("RPT") || program_id.contains("REPORT") {
            "Reporting".to_string()
        } else if program_id.contains("ACCT") || program_id.contains("LEDGER") {
            "Accounting".to_string()
        } else {
            "Core Business Logic".to_string()
        }
    }

    fn calculate_cohesion_score(&self, programs: &[&Program]) -> f64 {
        if programs.is_empty() {
            return 0.0;
        }

        // Calculate cohesion based on shared data structures and common functionality
        let shared_data_score = self.calculate_shared_data_score(programs);
        let functional_cohesion_score = self.calculate_functional_cohesion_score(programs);
        
        (shared_data_score + functional_cohesion_score) / 2.0
    }

    fn calculate_coupling_score(&self, domain_programs: &[&Program], all_programs: &[&Program]) -> f64 {
        let external_dependencies = self.count_external_dependencies(domain_programs, all_programs);
        let total_possible_dependencies = all_programs.len() - domain_programs.len();
        
        if total_possible_dependencies == 0 {
            return 0.0;
        }
        
        external_dependencies as f64 / total_possible_dependencies as f64
    }

    fn generate_service_name(&self, domain: &str) -> String {
        match domain {
            "Customer Management" => "customer-service".to_string(),
            "Order Management" => "order-service".to_string(),
            "Inventory Management" => "inventory-service".to_string(),
            "Payment Processing" => "payment-service".to_string(),
            "Reporting" => "reporting-service".to_string(),
            "Accounting" => "accounting-service".to_string(),
            _ => "business-service".to_string(),
        }
    }

    fn extract_data_entities(&self, programs: &[&Program]) -> Vec<String> {
        let mut entities = HashSet::new();
        
        for program in programs {
            // Extract data entities from program structure
            // This is a simplified implementation
            if let Some(ref data_division) = program.data {
                // Would analyze working storage and file sections to identify entities
                entities.insert("Customer".to_string());
                entities.insert("Transaction".to_string());
            }
        }
        
        entities.into_iter().collect()
    }

    fn estimate_service_size(&self, programs: &[&Program]) -> ServiceSize {
        let total_complexity = programs.iter()
            .map(|p| self.estimate_program_complexity(p))
            .sum::<usize>();
            
        match total_complexity {
            0..=100 => ServiceSize::Small,
            101..=500 => ServiceSize::Medium,
            _ => ServiceSize::Large,
        }
    }

    fn assess_migration_complexity(&self, programs: &[&Program]) -> DifficultyLevel {
        let complexity_factors = programs.iter()
            .map(|p| self.calculate_migration_complexity_factors(p))
            .sum::<f64>();
            
        let avg_complexity = complexity_factors / programs.len() as f64;
        
        match avg_complexity {
            0.0..=2.0 => DifficultyLevel::Low,
            2.1..=4.0 => DifficultyLevel::Medium,
            4.1..=6.0 => DifficultyLevel::High,
            _ => DifficultyLevel::VeryHigh,
        }
    }

    fn identify_service_dependencies(&self, domain_programs: &[&Program], all_programs: &[&Program]) -> Vec<String> {
        let mut dependencies = Vec::new();
        
        // Identify calls to programs outside this domain
        for program in domain_programs {
            let external_calls = self.find_external_program_calls(program, domain_programs, all_programs);
            for call in external_calls {
                let target_domain = self.infer_business_domain(&call);
                let service_name = self.generate_service_name(&target_domain);
                if !dependencies.contains(&service_name) {
                    dependencies.push(service_name);
                }
            }
        }
        
        dependencies
    }

    fn create_size_based_services(&self, programs: &[&Program]) -> Result<Vec<MicroserviceCandidate>> {
        let mut candidates = Vec::new();
        let mut service_counter = 1;
        
        // Group programs by estimated size to create balanced services
        for chunk in programs.chunks(3) { // Max 3 programs per service
            let candidate = MicroserviceCandidate {
                name: format!("legacy-service-{}", service_counter),
                programs: chunk.iter().map(|p| 
                    p.identification.node.program_id.clone().unwrap_or_else(|| format!("PROG{}", service_counter))
                ).collect(),
                business_capability: "Legacy Business Logic".to_string(),
                data_entities: vec!["LegacyData".to_string()],
                estimated_size: ServiceSize::Medium,
                migration_complexity: DifficultyLevel::Medium,
                dependencies: Vec::new(),
            };
            candidates.push(candidate);
            service_counter += 1;
        }
        
        Ok(candidates)
    }

    fn analyze_service_dependencies(&self, services: &[MicroserviceCandidate]) -> Result<Vec<ServiceDependency>> {
        let mut dependencies = Vec::new();
        
        for service in services {
            for dependency_name in &service.dependencies {
                if let Some(target_service) = services.iter().find(|s| &s.name == dependency_name) {
                    dependencies.push(ServiceDependency {
                        from_service: service.name.clone(),
                        to_service: target_service.name.clone(),
                        dependency_type: ServiceDependencyType::Synchronous,
                        communication_pattern: CommunicationPattern::RequestResponse,
                        data_consistency_requirement: ConsistencyRequirement::Eventual,
                    });
                }
            }
        }
        
        Ok(dependencies)
    }

    fn identify_data_consistency_challenges(&self, services: &[MicroserviceCandidate]) -> Result<Vec<DataConsistencyChallenge>> {
        let mut challenges = Vec::new();
        
        // Identify shared data entities across services
        let mut entity_services: HashMap<String, Vec<String>> = HashMap::new();
        
        for service in services {
            for entity in &service.data_entities {
                entity_services.entry(entity.clone()).or_default().push(service.name.clone());
            }
        }
        
        // Find entities shared by multiple services
        for (entity, sharing_services) in entity_services {
            if sharing_services.len() > 1 {
                challenges.push(DataConsistencyChallenge {
                    description: format!("Entity '{}' is shared across multiple services", entity),
                    affected_services: sharing_services,
                    consistency_pattern: ConsistencyPattern::EventSourcing,
                    implementation_complexity: DifficultyLevel::High,
                });
            }
        }
        
        Ok(challenges)
    }

    fn generate_api_recommendations(&self, services: &[MicroserviceCandidate]) -> Result<Vec<ApiRecommendation>> {
        let mut recommendations = Vec::new();
        
        for service in services {
            let api_style = self.recommend_api_style(service);
            let endpoints = self.generate_api_endpoints(service);
            let data_formats = self.recommend_data_formats(service);
            
            recommendations.push(ApiRecommendation {
                service: service.name.clone(),
                api_style,
                endpoints,
                data_formats,
            });
        }
        
        Ok(recommendations)
    }

    fn recommend_api_style(&self, service: &MicroserviceCandidate) -> ApiStyle {
        match service.business_capability.as_str() {
            "Reporting" => ApiStyle::GraphQL, // Good for flexible data queries
            "Payment Processing" => ApiStyle::Grpc, // High performance requirements
            _ => ApiStyle::Rest, // Default for most business services
        }
    }

    fn generate_api_endpoints(&self, service: &MicroserviceCandidate) -> Vec<ApiEndpoint> {
        let mut endpoints = Vec::new();
        
        // Generate CRUD endpoints for each data entity
        for entity in &service.data_entities {
            let entity_lower = entity.to_lowercase();
            endpoints.extend(vec![
                ApiEndpoint {
                    path: format!("/api/{}", entity_lower),
                    method: "GET".to_string(),
                    description: format!("List all {}", entity_lower),
                },
                ApiEndpoint {
                    path: format!("/api/{}/{{id}}", entity_lower),
                    method: "GET".to_string(),
                    description: format!("Get {} by ID", entity_lower),
                },
                ApiEndpoint {
                    path: format!("/api/{}", entity_lower),
                    method: "POST".to_string(),
                    description: format!("Create new {}", entity_lower),
                },
                ApiEndpoint {
                    path: format!("/api/{}/{{id}}", entity_lower),
                    method: "PUT".to_string(),
                    description: format!("Update {} by ID", entity_lower),
                },
                ApiEndpoint {
                    path: format!("/api/{}/{{id}}", entity_lower),
                    method: "DELETE".to_string(),
                    description: format!("Delete {} by ID", entity_lower),
                },
            ]);
        }
        
        endpoints
    }

    fn recommend_data_formats(&self, service: &MicroserviceCandidate) -> Vec<DataFormat> {
        vec![DataFormat::Json] // JSON is the most common choice for REST APIs
    }

    // Helper methods (simplified implementations)
    fn calculate_shared_data_score(&self, _programs: &[&Program]) -> f64 { 0.8 }
    fn calculate_functional_cohesion_score(&self, _programs: &[&Program]) -> f64 { 0.7 }
    fn count_external_dependencies(&self, _domain_programs: &[&Program], _all_programs: &[&Program]) -> usize { 2 }
    fn estimate_program_complexity(&self, _program: &Program) -> usize { 50 }
    fn calculate_migration_complexity_factors(&self, _program: &Program) -> f64 { 3.0 }
    fn find_external_program_calls(&self, _program: &Program, _domain_programs: &[&Program], _all_programs: &[&Program]) -> Vec<&Program> { 
        Vec::new() 
    }
}