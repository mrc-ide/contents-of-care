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
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      axis.text = element_text(size = 10),
      legend.text = element_text(size = 11),
      legend.position = "bottom",
      legend.title = element_blank(),
      strip.text = element_text(size = 14),
      panel.grid.major.y = element_line(color = "#00000010")
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

## Categories are:
## Nutritional Interventions,
## Maternal and fetal assessment; 
intervention_types <- list(
  `Maternal and fetal assessment` =
    c("weight", "temperature", "inspection_of_mucous_membranes",
      "speculum_exam",
      "blood_pressure", "albuminuria", "glucoserie",
      "us_requested",
      "uterus_measured2",
      "fetal_heartbeat_checked2",
      "uterus_measured3",
      "fetal_heartbeat_checked3",
      "lower_limb_edema"),
  `Informational Interventions` = c(
      "info_complications",
      "info_placenta_previa",
      "info_premature_rupture",
      "info_postpartum_hemorrhage",
      "info_retained_placenta",
      "info_uterus_inverted",
      "info_ectopic_pregnancy",
      "info_hemorrhage_antepartum",
      "info_prolonged_labor_latent",
      "info_prolonged_labor_second",
      "info_cpd",
      "info_uterine_rupture",
      "info_abnormal_presentation",
      "info_infection",
      "info_uterine_perforation",
      "info_hypertensive_crises",
      "info_ecclampsia",
      "info_pre_eclampsia",
      "info_severe_anemia",
      "info_multiple_pregnancy",
      "info_embolism",
      "diet_suggested",
      "vaccination_suggested"
  ),
  `Preventive measures` =
    c("additional_bw_tests_requested",
      "toxoplasmosis_test_requested",
      "mosq_kit_prescribed", "mosq_kit_requested",
      "fansidar_given2",
      "fansidar_prescribed",
      "sp_given3", "sp_prescribed3", "sp_ensured3"),
  ## Alternatively - completeness of records
  `Continuity of care` =
    c("patients_age_reported", "patients_height_reported",
      "previous_pregnancy_enquired",
      "number_of_previous_pregnancies_enquired", "blood_type_enquired",
      "blood_type_proof_enquired", "blood_type_test_requested",
      "vaccination_record_checked")
)
