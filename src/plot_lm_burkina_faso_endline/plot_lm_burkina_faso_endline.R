library(brms)
library(cli)
library(dplyr)
library(ggplot2)
library(glue)
library(orderly2)
library(performance)
library(posterior)
library(purrr)
library(snakecase)
library(tibble)
library(tidyr)

orderly_shared_resource(utils.R = "utils.R")
source("utils.R")

orderly_dependency(
  "lm_burkina_faso_endline", "latest",
  files = c("bfa_endline_dco_fits.rds")
)

fits <- readRDS("bfa_endline_dco_fits.rds")
## map(fits, function(fit) nrow(fit$data))

fixed_effects <- map_dfr(fits, function(fit) {
  x <- as.data.frame(fixef(fit, probs = c(0.025, 0.5, 0.975)))
  rownames_to_column(x)
}, .id = "datacut")

fixed_effects <- separate(
  fixed_effects, datacut,
  into = c("anc", "trimester"), sep = "_"
)

saveRDS(fixed_effects, file = "bfa_endline_dco_bayes_fixed_effects.rds")
orderly_artefact(
  files = "bfa_endline_dco_bayes_fixed_effects.rds",
  description = "Fixed effects for DRC 2015 DCO model fits"
)

attribute_of <- list(
  `Health facility` = c(
    "num_csps_in_district",
    "milieu_of_residenceUrban",
    "milieu_of_residenceUnknown",
    "num_csps_in_district_scaled",
    "facility_level_mappingPrimary",
    "doctor_or_nursing_and_midwifery_per_10000_scaled",
    "total_attendance_scaled",
    "attendance_pregnant_women_scaled",
    "num_personnel_scaled",
    "num_maternal_deaths"
  ),
  `Patient` = c(
    "patage_i",
    "patlit_iLiterate",
    "patmar_iMarried",
    "patdist_i_scaled",
    "wealth_decile_i",
    "pregnancy_week",
    "first_pregnancyYes",
    "first_pregnancyUnknown"
  ),
  `HCW` = c(
    "hcw_age",
    "hcw_time_in_service",
    "hcw_sexMale",
    "hcw_sexUnknown",
    "hcw_qualificationDoctor",
    "hcw_qualificationMidwife",
    "hcw_qualificationNurse",
    "hcw_qualificationOther"
  ),

  `Appointment` = c(
    "consultation_languageFrench",
    "consultation_languageMoore",
    "consultation_languageOther",
    "consultation_languageUnknown",
    "time_elapsed_since_start_of_day"
  )
)

x <- filter(fixed_effects, !rowname %in% "Intercept")
x$attribute_of <- NA_character_
x$attribute_of <- map_chr(
  x$rowname, function(y) {
    names(attribute_of)[sapply(attribute_of, function(x) y %in% x)]
    }
)


breaks <- c(
  "region_nameCentreMest",
  "region_nameCentreMnord",
  "region_nameCentreMouest",
  "region_nameNord",
  "region_nameSudMouest",
  "num_csps_in_district",
  "milieu_of_residenceUrban",  
  "milieu_of_residenceUnknown",
  "num_csps_in_district_scaled",  
  "facility_level_mappingPrimary",
  "doctor_or_nursing_and_midwifery_per_10000_scaled",
  "total_attendance_scaled",
  "attendance_pregnant_women_scaled",
  "num_personnel_scaled",
  "num_maternal_deaths",

  "patage_i",
  "patlit_iLiterate",
  "patmar_iMarried",
  "patdist_i_scaled",
  "wealth_decile_i",
  "pregnancy_week",
  "first_pregnancyYes",
  "first_pregnancyUnknown",  

  "hcw_age",
  "hcw_time_in_service",
  "hcw_sexMale",
  "hcw_sexUnknown",
  "hcw_qualificationDoctor",
  "hcw_qualificationMidwife",
  "hcw_qualificationNurse",
  "hcw_qualificationOther",

  "consultation_languageFrench",
  "consultation_languageMoore",
  "consultation_languageOther",
  "consultation_languageUnknown",
  "time_elapsed_since_start_of_day"
)

x$rowname <- factor(x$rowname, levels = breaks, ordered = TRUE)

labels <- c(
  "Centre-est",
  "Centre-nord",
  "Centre-ouest",
  "Nord",
  "Sud-ouest",
  "Number of CSPS in district",
  "Urban",  
  "Unknown",
  "CSPS in district",  
  "Primary healthcare",
  "Doctor/N&M per 10,000",
  "Total attendance last month",
  "Attendance pregnant women last month",
  "Personnel in HF",
  "Maternal deaths in HF",

  "Patient age",
  "Patient literacy:Illiterate",
  "Patient marital status:Married",
  "Patient distance",
  "Wealth decile",
  "Pregnancy week",
  "First pregnancy:Yes",
  "First pregnancy:Unknown",  

  "HCW age",
  "HCW time in service (years)",
  "HCW:Male",
  "HCW sex:Unknown",
  "Doctor",
  "Midwife",
  "Nurse",
  "Qualification:Other",

  "French",
  "Moore",
  "Language:Other",
  "Language:Unknown",
  "Hours since 6AM"
)

p <- p <- ggplot() +
  geom_vline(xintercept = 0, linetype = "dashed") +
  theme_manuscript()

xsplit <- split(x, x$attribute_of)

iwalk(xsplit, function(df, att) {
  plocal <- p + 
    geom_point(data = df, aes(y = rowname, x = Q50)) +
    geom_errorbarh(
      data = df,
      aes(y = rowname, xmin = `Q2.5`, xmax = `Q97.5`),
      height = 0
    ) + scale_y_discrete(breaks = breaks, labels = labels)

  plocal <- my_facets(plocal)

  cli_inform(glue("Saving plot for {att}"))
  outfile <- glue("bfa_endline_dco_bayes_fixed_effects_{to_snake_case(att)}")
  ggsave_manuscript(
    plocal,
    file = outfile,
    width = 12, height = 8
  )
  orderly_artefact(
    files = glue("{outfile}.png"),
    description = "Fixed effects for DRC 2015 DCO model fits"
  )
  
})



## Random effects
ran_effects <- map_dfr(fits, function(fit) {
  x <- ranef(fit, probs = c(0.025, 0.5, 0.975))
  x <- as.data.frame(x$region_name[, , "Intercept"])
  rownames_to_column(x)
}, .id = "datacut")

ran_effects <- separate(
  ran_effects, datacut,
  into = c("anc", "trimester"), sep = "_"
)


saveRDS(ran_effects, file = "bfa_endline_dco_bayes_random_effects.rds")
orderly_artefact(
  files = "bfa_endline_dco_bayes_random_effects.rds",
  description = "Random effects for DRC 2015 DCO model fits"
)

p <- ggplot() +
  geom_vline(xintercept = 0, linetype = "dashed") +
  geom_point(data = ran_effects, aes(y = rowname, x = Q50)) +
  geom_errorbarh(
    data = ran_effects,
    aes(y = rowname, xmin = `Q2.5`, xmax = `Q97.5`),
    height = 0
  ) +
  theme_manuscript()

p <- my_facets(p)

ggsave_manuscript(
  p,
  file = "bfa_endline_dco_bayes_random_effects",
  width = 12, height = 8
)

icc <- map(fits, compute_icc)

saveRDS(icc, file = "bfa_endline_dco_bayes_icc.rds")

orderly_artefact(
  files = "bfa_endline_dco_bayes_icc.rds",
  description = "ICC for DRC 2015 DCO model fits"
)


bayes_r2 <- map(fits, bayes_R2, probs = c(0.025, 0.5, 0.975))

saveRDS(bayes_r2, file = "bfa_endline_dco_bayes_r2.rds")
orderly_artefact(
  files = "bfa_endline_dco_bayes_r2.rds",
  description = "Bayes R2 for DRC 2015 DCO model fits"
)


coeffs_gt_0 <- map_dfr(
  fits, function(fit) probability_of_direction(fit)[[1]],
  .id = "datacut"
)
coeffs_gt_0 <- separate(
  coeffs_gt_0, datacut,
  into = c("anc", "trimester"), sep = "_"
)

saveRDS(coeffs_gt_0, file = "bfa_endline_dco_bayes_coeffs_gt_0.rds")
orderly_artefact(
  files = "bfa_endline_dco_bayes_coeffs_gt_0.rds",
  description = "Coefficients greater than 0 for DRC 2015 DCO model fits"
)

coeffs_gt_0 <- filter(coeffs_gt_0, rowname != "Intercept")


coeffs_gt_0$attribute_of <- NA_character_
coeffs_gt_0$attribute_of <- map_chr(
  coeffs_gt_0$rowname, function(y) {
    names(attribute_of)[sapply(attribute_of, function(x) y %in% x)]
  }
)

coeffs_gt_0$rowname <- factor(
  coeffs_gt_0$rowname,
  levels = breaks, ordered = TRUE
)

p <- ggplot() +
  geom_vline(xintercept = 0.5, linetype = "dashed", alpha = 0.5) +
  xlim(0, 1) +
  xlab("Posterior probability of coefficient > 0") +
  theme_manuscript() +
  theme(axis.title.x = element_text(size = 12))


xsplit <- split(coeffs_gt_0, coeffs_gt_0$attribute_of)

iwalk(xsplit, function(df, att) {
  
  plocal <- p +
  geom_tile(
    data = df, aes(x = 0.5, y = rowname, width = 1, height = 0.25),
    fill = "gray"
  ) +
    geom_tile(
      data = df, aes(x = `Post.Prob` / 2, y = rowname, width = `Post.Prob`, height = 0.25),
      fill = "red"
    ) +
    scale_y_discrete(breaks = breaks, labels = labels)

  plocal <- my_facets(plocal)

  cli_inform(glue("Saving plot for {att}"))
  outfile <- glue("bfa_endline_dco_bayes_coeffs_gt_0_{to_snake_case(att)}")
  ggsave_manuscript(
    plocal,
    file = outfile,
    width = 12, height = 8
  )
  orderly_artefact(
    files =glue("{outfile}.png"),
    description = "Coefficients greater than 0 for Benin model fits"
  )
})


