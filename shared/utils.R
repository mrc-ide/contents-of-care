my_facets <- function(p) {
  p +
    facet_grid(
      trimester ~ anc,
      scales = "free",
      labeller = labeller(
        trimester = to_title_case, anc = to_title_case
      )
    )
}

recode_oui_non <- function(x) {
  x <- tolower(x)
  x <- ifelse(x %in% "oui", 1, ifelse(x %in% "non", 0, NA))
  x
}

theme_manuscript <- function() {
  theme_classic() +
    theme(
      ## text = element_text(family = "Arial", ),
      plot.title = element_text(size = 14, hjust = 0.5),
      axis.title = element_text(size = 12),
      axis.text = element_text(size = 10),
      legend.text = element_text(size = 11),
      legend.position = "bottom",
      legend.title = element_blank(),
      strip.text = element_text(size = 14),
      panel.grid.major.y = element_line(color = "#00000010"),
      panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5)
    )
}


ggsave_manuscript <- function(
    filename,
    plot,
    width = 6,
    height = 4,
    units = "in",
    dpi = 300) {
  ggsave(
    filename = paste0(filename, ".png"), plot = plot, width = width,
    height = height, units = units, dpi = dpi
  )

  ggsave(
    filename = paste0(filename, ".pdf"), plot = plot, width = width,
    height = height, units = units, dpi = dpi
  )
}

my.render.cont <- function(x) {
  with(stats.apply.rounding(stats.default(x), digits = 2), c("",
    "Mean (SD)" = sprintf("%s (%s)", MEAN, SD)
  ))
}

## See https://blog.djnavarro.net/posts/2024-06-21_table1/#adding-extra-columns
render_p_value_cat <- function(x, ...) {
  dat <- bind_rows(map(x, ~ data.frame(value = .)), .id = "group")
  mod <- aov(value ~ group, dat)
  p <- summary(mod)[[1]][1, 5]
  label_pvalue()(p)
}

covariates_nice_names <- c(
  "consultation_languageFrançais" = "Language: French",
  "consultation_languageLingala" = "Language: Lingala",
  "consultation_languageKIKONGO" = "Language: Kikongo",
  "consultation_languageOther" = "Language: Other",
  "consultation_languageSwahili" = "Language: Swahili",
  "hcw_qualificationOther" = "HCW Qualification: Other",
  "hcw_qualificationMidwife/Obstetrician" =
    "HCW Qualification: Midwife/Obstetrician",
  "hcw_qualificationNurse" = "HCW Qualification: Nurse",
  "hcw_sexMale" = "HCW Sex: Male",
  "provincekl Kwilu Province" = "Province: Kwilu",
  "provincemd Maindombe Province" = "Province: Maindombe",
  "first_pregnancyOui" = "First Pregnancy: Yes",
  "first_pregnancyNon" = "First Pregnancy: No",
  "first_pregnancyyes" = "First Pregnancy: Yes",
  "first_pregnancyno" = "First Pregnancy: No",
  "pregnancy_in_weeks" = "Length of pregnancy (weeks)",
  "time_elapsed_since_start_of_day" = "Time elapsed since 6AM (minutes)",
  "facility_statusPublic" = "Facility Status: Public",
  "facility_statusPrivate" = "Facility Status: Private",
  "provinceSouth Kivu" = "Province: South Kivu",
  "provinceNorth Kivu" = "Province: North Kivu",
  "provinceManiema" = "Province: Maniema",
  "provinceKatanga" = "Province: Katanga",
  "provinceEcuador" = "Province: Ecuador",
  "milieu_of_residenceUrban" = "Residence: Urban",
  "milieu_of_residenceRural" = "Residence: Rural"  
)

start_of_day <- lubridate::hm("06:00")


compute_icc <- function(fit, group = NULL) {
  # Extract posterior draws
  draws <- posterior::as_draws_df(fit)

  # Find the random intercept SD for the group
  sd_col <- if (is.null(group)) {
    grep("^sd_.*__Intercept$", names(draws), value = TRUE)
  } else {
    paste0("sd_", group, "__Intercept")
  }

  if (length(sd_col) != 1 || !sd_col %in% names(draws)) {
    stop("Could not find the group-level intercept SD in the posterior draws. Check the group name.")
  }

  # Extract variance components
  var_group <- draws[[sd_col]]^2
  var_resid <- draws[["sigma"]]^2

  # Compute ICC for each posterior draw
  icc_draws <- var_group / (var_group + var_resid)

  # Return posterior summary
  data.frame(
    group = if (is.null(group)) sd_col else group,
    icc_mean = mean(icc_draws),
    icc_median = median(icc_draws),
    icc_lower = quantile(icc_draws, 0.025),
    icc_upper = quantile(icc_draws, 0.975)
  )
}


prior_spec <- c(
  brms::prior(normal(0, 1), class = b), # fixed effects
  brms::prior(normal(0, 0.5), class = sd) # random effects
)



probability_of_direction <- function(fit) {
  fixed_names <- rownames(brms::fixef(fit))

  # Create hypothesis strings like "var = 0"
  hypotheses <- paste0(fixed_names, " > 0")

  # Run brms hypothesis test
  out <- brms::hypothesis(fit, hypotheses)
  out[[1]]$rowname <- fixed_names
  out
}

intervention_types <- list(
  `Patient-HCW interaction` = c(
    # From earlier version
    "hcw_intro_name",
    "hcw_intro_degree",
    "procedure_explained",
    "questions_encouraged",
    "third_person_assistance_asked",
    "confidentiality_ensured",
    "exam_results_explained",
    "questions_invited_at_the_end",
    "visual_aids_used",

    # New f3_02_* equivalents
    "hcw_introduced_name",
    "hcw_introduced_professional_grade",
    "hcw_explained_procedure",
    "hcw_encouraged_questions",
    "hcw_offered_third_party_support",
    "hcw_asked_patient_name",
    "hcw_asked_if_patient_had_questions",
    "hcw_used_visual_aids",
    "hcw_ensured_exam_confidentiality"
  ),

  `Maternal and fetal assessment` = c(
    # From earlier version
    "weight", "patient_weight",
    "temperature", "patient_temperature",
    "blood_pressure", "patient_pulse", "patient_respiratory_rate",
    "inspection_of_mucous_membranes",
    "speculum_exam",
    "patient_chest_auscultation",
    "lower_limb_edema", "breast_exam",
    "patient_blood_taken",
    "hemoglobin_test_requested",
    "urine_test_albumin_glucose",
    "albuminuria", "glucoserie",
    "us_requested",
    "uterus_measured2", "uterus_measured3", "uterus_size_exam",
    "fetal_presentation_exam",
    "fetal_heartbeat_checked2", "fetal_heartbeat_checked3",
    "fetal_heartbeat_listened",
    "abdominal_pain", "breathing_difficulty", "severe_vomiting",
    "convulsions", "health_problems_before_or_during_pregnancy",
    "stress", "domestic_violence", "drug_treatment_course",
    "vaginal_bleeding", "bleeding", "fever",
    "headache", "headache_or_blurred_vision",
    "swelling", "tiredness",
    "fetus_movement", "other_symptoms", "pregnancy_related_symptoms",

    # New f3_02_* variables
    "hcw_measured_patient_weight",
    "hcw_measured_patient_pulse",
    "hcw_measured_respiratory_rate",
    "hcw_measured_temperature",
    "hcw_measured_blood_pressure",
    "hcw_performed_chest_auscultation",
    "hcw_examined_conjunctiva_or_palms_for_anemia",
    "hcw_examined_legs_or_feet_for_edema",
    "hcw_palpated_abdomen_for_fetal_position_or_used_ultrasound",
    "hcw_asked_about_severe_abdominal_pain",
    "hcw_asked_about_severe_breathing_difficulty",
    "hcw_asked_about_severe_vomiting",
    "hcw_asked_about_seizures_or_epilepsy",
    "hcw_asked_about_health_problems_during_pregnancy",
    "hcw_asked_about_stress_or_depression",
    "hcw_asked_about_domestic_or_partner_violence",
    "hcw_asked_about_current_medications",
    "hcw_asked_about_hiv_status",
    "hcw_asked_about_tetanus_vaccination_status",
    "hcw_asked_about_bleeding",
    "hcw_asked_about_fever",
    "hcw_asked_about_headache_or_blurred_vision",
    "hcw_asked_about_swelling_face_hands_feet",
    "hcw_asked_about_fatigue_or_shortness_of_breath",
    "hcw_asked_if_fetus_moved",
    "hcw_asked_about_other_symptoms_or_problems",
    "hcw_asked_about_symptoms_related_to_pregnancy"
  ),

  `Informational Interventions` = c(
    # From earlier version
    "info_complications", "info_placenta_previa", "info_premature_rupture",
    "info_postpartum_hemorrhage", "info_retained_placenta",
    "info_uterus_inverted", "info_ectopic_pregnancy",
    "info_hemorrhage_antepartum", "info_prolonged_labor_latent",
    "info_prolonged_labor_second", "info_cpd", "info_uterine_rupture",
    "info_abnormal_presentation", "info_infection", "info_uterine_perforation",
    "info_hypertensive_crises", "info_ecclampsia", "info_pre_eclampsia",
    "info_severe_anemia", "info_multiple_pregnancy", "info_embolism",
    "fever_as_risk_factor_explained",
    "tiredness_as_risk_factor_explained",
    "swelling_as_risk_factor_explained",
    "headache_or_blurred_vision_as_risk_factor_explained",
    "diet_suggested", "vaccination_suggested",
    "diet_during_pregnancy_explained",
    "ifa_role_explained", "ifa_dosage_explained", "ifa_side_effects_explained",
    "pregnancy_progression_explained",
    "ipt_role_explained", "ipt_dosage_explained", "antimalarial_side_effects_explained",
    "ipt_second_dose_importance_explained", "itn_use_importance_explained",
    "tetanus_injection_role_explained",
    "projecting_delivery", "delivery_place_discussed",
    "birth_preparedness_advised", "skilled_birth_attendant_advised",
    "home_birth_items_discussed",
    "newborn_vaccination_importance_explained",
    "exclusive_breastfeeding_explained",
    "contraception_barrier_methods_discussed",
    "contraception_hormonal_methods_discussed",
    "contraception_surgical_methods_discussed",
    "contraception_traditional_methods_discussed",
    "contraception_methods_compared",

    # New f3_02_* variables
    "hcw_advised_on_nutrition_during_pregnancy",
    "patient_informed_about_pregnancy_progress",
    "hcw_explained_ifa_role",
    "hcw_explained_ifa_dosage",
    "hcw_explained_ifa_side_effects",
    "hcw_explained_ipt_role",
    "hcw_explained_ipt_dosage",
    "hcw_explained_ipt_side_effects",
    "hcw_explained_importance_second_ipt_dose",
    "hcw_explained_tetanus_vaccine_role",
    "hcw_asked_about_birth_plan",
    "hcw_asked_delivery_location",
    "hcw_advised_on_birth_preparedness",
    "hcw_advised_use_of_skilled_birth_attendant",
    "hcw_discussed_items_needed_for_home_birth",
    "hcw_discussed_importance_of_newborn_vaccination",
    "hcw_advised_exclusive_breastfeeding_until_6_months",
    "hcw_asked_about_awareness_of_contraceptive_methods",
    "hcw_asked_about_past_use_of_contraceptive_methods",
    "hcw_asked_about_intent_to_use_contraceptives_postpartum",
    "hcw_asked_if_patient_knows_where_to_access_fp_services",
    "hcw_discussed_barrier_methods",
    "hcw_discussed_hormonal_methods",
    "hcw_discussed_surgical_methods",
    "hcw_discussed_traditional_methods",
    "hcw_compared_contraceptive_methods_effectiveness_and_cost"
  ),

  `Preventive measures` = c(
    # From earlier version
    "fansidar_given2", "fansidar_prescribed",
    "sp_given3", "sp_prescribed3", "sp_ensured3",
    "antimalarial_prescribed_or_given",
    "ipt_first_dose_given",
    "mosq_kit_prescribed", "mosq_kit_requested",
    "itn_given",
    "ifa_prescribed_or_given",
    "tetanus_injection_prescribed_or_given",
    "syphilis_test_requested",
    "hiv_test_voluntary_offered",
    "hiv_counselling_referred",
    "additional_bw_tests_requested",
    "toxoplasmosis_test_requested",
    "hygiene_measures_before_touching_patient",

    # New f3_02_* variables
    "hcw_provided_ifa_from_six_months",
    "hcw_provided_tetanus_toxoid_injection",
    "hcw_provided_ipt_treatment",
    "hcw_provided_quinine_treatment",
    "hcw_provided_act_treatment",
    "patient_received_first_dose_ipt_at_facility",
    "patient_received_insecticide_treated_net"
  ),

  `Continuity of care` = c(
    # From earlier version
    "patients_age_reported", "patients_height_reported",
    "patient_residence_reported", "patient_taken_drugs",
    "prev_interruption_enquired", "previous_pregnancy_enquired",
    "number_of_previous_pregnancies_enquired",
    "prev_delivery_preterm", "children_death_during_first_week",
    "heavy_bleeding_during_or_after_delivery", "prev_assisted_delivery",
    "prev_voluntary_interruption",
    "num_prev_anc_before_this_pregnancy", "date_last_period",
    "blood_type_enquired", "blood_type_proof_enquired",
    "blood_type_test_requested", "blood_group_rhesus_requested",
    "vaccination_record_checked", "health_card_checked",
    "hiv_status", "vaccination_against_tetanus",

    # New f3_02_* variables
    "hcw_asked_patient_age",
    "hcw_asked_patient_residence",
    "hcw_asked_patient_medications",
    "hcw_asked_number_previous_anc_visits",
    "hcw_asked_last_menstrual_period_date",
    "hcw_asked_number_previous_pregnancies",
    "hcw_asked_about_therapeutic_abortions",
    "hcw_asked_about_preterm_deliveries",
    "hcw_asked_about_early_neonatal_deaths",
    "hcw_asked_about_excessive_bleeding",
    "hcw_asked_about_assisted_deliveries",
    "hcw_asked_about_induced_abortions",
    "hcw_asked_about_normal_deliveries",
    "hcw_recorded_in_health_booklet"
  )
)


ks_distance_matrix <- function(data_split, response_var) {
  n <- length(data_split)
  group_names <- names(data_split)
  D <- data.frame(
    statistic = numeric(0), p.value = numeric(0), method = character(0),
    stratum1 = character(0), stratum2 = character(0)
  )
  # Compute pairwise KS distances
  for (i in 1:(n - 1)) {
    for (j in (i + 1):n) {
      print(glue("Comparing {group_names[i]} and {group_names[j]}"))
      x1 <- data_split[[i]][[response_var]]
      x2 <- data_split[[j]][[response_var]]
      tmp <- tidy(ks.test(x1, x2))
      tmp$stratum1 <- group_names[i]
      tmp$stratum2 <- group_names[j]
      D <- bind_rows(D, tmp)

      ## Distance is symmetric, so we can add the reverse comparison
      tmp$stratum1 <- group_names[j]
      tmp$stratum2 <- group_names[i]
      D <- bind_rows(D, tmp)
      
    }
  }
  D
}
