use crate::models::*;
use serde::{Deserialize, Serialize};

/// Migration assessment results and utilities.
pub use crate::models::MigrationAssessment;

impl MigrationAssessment {
    /// Get a summary score for the migration readiness (0-100).
    pub fn overall_readiness_score(&self) -> f64 {
        let cloud_weight = 0.3;
        let microservices_weight = 0.2;
        let effort_weight = 0.2;
        let risk_weight = 0.3;

        let cloud_score = self.cloud_readiness.overall_score;
        let microservices_score = self.microservices_readiness_score();
        let effort_score = self.effort_favorability_score();
        let risk_score = self.risk_favorability_score();

        cloud_score * cloud_weight
            + microservices_score * microservices_weight
            + effort_score * effort_weight
            + risk_score * risk_weight
    }

    /// Calculate microservices readiness score based on analysis.
    pub fn microservices_readiness_score(&self) -> f64 {
        let service_count = self.microservices_analysis.recommended_services.len();
        let dependency_complexity = self.microservices_analysis.service_dependencies.len() as f64;
        let consistency_challenges = self
            .microservices_analysis
            .data_consistency_challenges
            .len() as f64;

        // Higher service count is good, but too many dependencies and consistency issues are bad
        let base_score = (service_count as f64 * 10.0).min(100.0);
        let penalty = (dependency_complexity + consistency_challenges * 2.0) * 5.0;

        (base_score - penalty).max(0.0)
    }

    /// Calculate effort favorability score (lower effort = higher score).
    pub fn effort_favorability_score(&self) -> f64 {
        let effort_months = self.effort_estimation.total_effort_months;

        // Convert effort to favorability score (inverse relationship)
        match effort_months {
            0.0..=6.0 => 100.0,
            6.1..=12.0 => 80.0,
            12.1..=24.0 => 60.0,
            24.1..=36.0 => 40.0,
            36.1..=48.0 => 20.0,
            _ => 10.0,
        }
    }

    /// Calculate risk favorability score (lower risk = higher score).
    pub fn risk_favorability_score(&self) -> f64 {
        match self.risk_assessment.overall_risk {
            RiskLevel::Low => 90.0,
            RiskLevel::Medium => 70.0,
            RiskLevel::High => 40.0,
            RiskLevel::Critical => 10.0,
        }
    }

    /// Get the most critical recommendations.
    pub fn critical_recommendations(&self) -> Vec<&Recommendation> {
        self.recommendations
            .iter()
            .filter(|r| matches!(r.priority, Priority::Critical))
            .collect()
    }

    /// Get high-impact, low-effort recommendations (quick wins).
    pub fn quick_wins(&self) -> Vec<&Recommendation> {
        self.recommendations
            .iter()
            .filter(|r| {
                matches!(r.impact, Impact::High | Impact::VeryHigh)
                    && matches!(r.effort_level, EffortLevel::Low | EffortLevel::Medium)
            })
            .collect()
    }

    /// Get the migration phases sorted by start month.
    pub fn ordered_migration_phases(&self) -> Vec<&RoadmapPhase> {
        let mut phases: Vec<&RoadmapPhase> = self.migration_roadmap.phases.iter().collect();
        phases.sort_by_key(|p| p.start_month);
        phases
    }

    /// Calculate total estimated cost.
    pub fn total_estimated_cost(&self) -> f64 {
        self.effort_estimation.cost_estimation.total_cost
    }

    /// Get technical debt hotspots sorted by severity.
    pub fn worst_technical_debt_hotspots(&self) -> Vec<&TechnicalDebtHotspot> {
        let mut hotspots: Vec<&TechnicalDebtHotspot> =
            self.technical_debt.hotspots.iter().collect();
        hotspots.sort_by(|a, b| b.debt_score.partial_cmp(&a.debt_score).unwrap());
        hotspots
    }

    /// Check if the system is ready for cloud migration.
    pub fn is_cloud_ready(&self) -> bool {
        self.cloud_readiness.overall_score >= 70.0
            && self
                .cloud_readiness
                .blockers
                .iter()
                .all(|b| !matches!(b.severity, Severity::Critical))
    }

    /// Check if the system is suitable for microservices architecture.
    pub fn is_microservices_suitable(&self) -> bool {
        !self.microservices_analysis.recommended_services.is_empty()
            && self.microservices_analysis.recommended_services.len() >= 2
            && self
                .microservices_analysis
                .data_consistency_challenges
                .len()
                <= 3
    }

    /// Get a migration readiness report as text.
    pub fn generate_executive_summary(&self) -> String {
        let readiness_score = self.overall_readiness_score();
        let recommendation_level = match readiness_score {
            80.0..=100.0 => "Highly Recommended",
            60.0..=79.9 => "Recommended with Preparation",
            40.0..=59.9 => "Proceed with Caution",
            20.0..=39.9 => "Significant Preparation Required",
            _ => "Not Recommended",
        };

        format!(
            "Migration Readiness Assessment\n\
            ================================\n\
            Overall Readiness Score: {:.1}/100\n\
            Recommendation: {}\n\
            \n\
            Key Metrics:\n\
            - Cloud Readiness: {:.1}/100\n\
            - Estimated Effort: {:.1} months\n\
            - Risk Level: {:?}\n\
            - Recommended Microservices: {}\n\
            - Technical Debt Score: {:.1}/100\n\
            \n\
            Critical Actions Required: {}\n\
            Quick Wins Available: {}\n\
            \n\
            Total Estimated Cost: ${:.0}\n\
            Expected ROI Timeline: {} months",
            readiness_score,
            recommendation_level,
            self.cloud_readiness.overall_score,
            self.effort_estimation.total_effort_months,
            self.risk_assessment.overall_risk,
            self.microservices_analysis.recommended_services.len(),
            self.technical_debt.debt_score,
            self.critical_recommendations().len(),
            self.quick_wins().len(),
            self.total_estimated_cost(),
            self.effort_estimation.cost_estimation.roi_timeline_months
        )
    }

    /// Export assessment as JSON for integration with other tools.
    pub fn to_json(&self) -> Result<String, serde_json::Error> {
        serde_json::to_string_pretty(self)
    }

    /// Create a migration decision matrix.
    pub fn create_decision_matrix(&self) -> MigrationDecisionMatrix {
        MigrationDecisionMatrix {
            readiness_score: self.overall_readiness_score(),
            effort_months: self.effort_estimation.total_effort_months,
            risk_level: self.risk_assessment.overall_risk.clone(),
            cost: self.total_estimated_cost(),
            roi_months: self.effort_estimation.cost_estimation.roi_timeline_months,
            cloud_compatibility: self.is_cloud_ready(),
            microservices_suitability: self.is_microservices_suitable(),
            technical_debt_severity: self.categorize_technical_debt_severity(),
            recommendation: self.get_migration_recommendation(),
        }
    }

    fn categorize_technical_debt_severity(&self) -> TechnicalDebtSeverity {
        match self.technical_debt.debt_score {
            0.0..=25.0 => TechnicalDebtSeverity::Low,
            25.1..=50.0 => TechnicalDebtSeverity::Moderate,
            50.1..=75.0 => TechnicalDebtSeverity::High,
            _ => TechnicalDebtSeverity::Critical,
        }
    }

    fn get_migration_recommendation(&self) -> MigrationRecommendation {
        let readiness = self.overall_readiness_score();
        let risk = &self.risk_assessment.overall_risk;
        let effort = self.effort_estimation.total_effort_months;

        match (readiness, risk, effort) {
            (80.0..=100.0, RiskLevel::Low, 0.0..=12.0) => MigrationRecommendation::Proceed,
            (60.0..=79.9, RiskLevel::Low | RiskLevel::Medium, 0.0..=24.0) => {
                MigrationRecommendation::ProceedWithPrep
            }
            (40.0..=59.9, _, _) => MigrationRecommendation::ExtensivePrep,
            (_, RiskLevel::Critical, _) => MigrationRecommendation::NotRecommended,
            (_, _, 48.0..) => MigrationRecommendation::ReconsiderScope,
            _ => MigrationRecommendation::NeedsAssessment,
        }
    }
}

/// Decision matrix for migration go/no-go decisions.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct MigrationDecisionMatrix {
    pub readiness_score: f64,
    pub effort_months: f64,
    pub risk_level: RiskLevel,
    pub cost: f64,
    pub roi_months: usize,
    pub cloud_compatibility: bool,
    pub microservices_suitability: bool,
    pub technical_debt_severity: TechnicalDebtSeverity,
    pub recommendation: MigrationRecommendation,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum TechnicalDebtSeverity {
    Low,
    Moderate,
    High,
    Critical,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum MigrationRecommendation {
    Proceed,
    ProceedWithPrep,
    ExtensivePrep,
    ReconsiderScope,
    NotRecommended,
    NeedsAssessment,
}
