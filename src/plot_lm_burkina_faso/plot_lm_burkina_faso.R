library(brms)
library(dplyr)
library(ggplot2)
library(orderly2)
library(performance)
library(posterior)
library(purrr)
library(tibble)
library(tidyr)

orderly_shared_resource(utils.R = "utils.R")
source("utils.R")

orderly_dependency(
  "lm_burkina_faso_baseline", "latest",
  files = c("bfa_baseline_dco_fits.rds")
)
fits <- readRDS("bfa_baseline_dco_fits.rds")

fixed_effects <- map_dfr(fits, function(fit) {
  x <- as.data.frame(fixef(fit, probs = c(0.025, 0.5, 0.975)))
  rownames_to_column(x)
}, .id = "datacut")

fixed_effects <- separate(
  fixed_effects, datacut,
  into = c("first_anc", "trimester"), sep = "_"
)

saveRDS(fixed_effects, file = "bfa_baseline_dco_bayes_fixed_effects.rds")
orderly_artefact(
  files = "bfa_baseline_dco_bayes_fixed_effects.rds",
  description = "Fixed effects for DRC 2015 DCO model fits"
)

x <- filter(fixed_effects, !rowname %in% "Intercept")

breaks <- c(
  "region_nameCentreMest",
  "region_nameCentreMnord",
  "region_nameCentreMouest",
  "region_nameNord",
  "region_nameSudMouest",
  "milieu_of_residenceUrban",  
  "milieu_of_residenceUnknown",
  "num_csps_in_district_scaled",  
  "facility_level_mappingPrimaryhealthcare",
  "doctor_or_nursing_and_midwifery_per_10000_scaled",
  "total_attendance_scaled",
  "attendance_pregnant_women_scaled",
  "num_personnel_scaled",

  "pregnancy_week",
  "first_pregnancyYes",
  "first_pregnancyUnknown",  

  "hcw_sexMale",
  "hcw_sexUnknown",
  "hcw_qualificationMidwife",
  "hcw_qualificationNurse",
  "hcw_qualificationOther",

  "consult_languageFrench",
  "consult_languageMoore",
  "consult_languageOtherDUnknown",
  "time_elapsed_since_start_of_day"
)

x$rowname <- factor(x$rowname, levels = breaks, ordered = TRUE)

labels <- c(
  "Centre-est",
  "Centre-nord",
  "Centre-ouest",
  "Nord",
  "Sud-ouest",
  "Urban",  
  "Unknown",
  "CSPS in district",  
  "Primary healthcare",
  "Doctor/N&M per 10,000",
  "Total attendance last month",
  "Attendance pregnant women last month",
  "Personnel in HF",

  "Pregnancy week",
  "First pregnancy:Yes",
  "First pregnancy:Unknown",  

  "HCW:Male",
  "HCW sex:Unknown",
  "Midwife",
  "Nurse",
  "Qualification:Other",

  "French",
  "Moore",
  "Other/Unknown",
  "Hours since 6AM"
)


p <- ggplot() +
  geom_vline(xintercept = 0, linetype = "dashed") +
  geom_point(data = x, aes(y = rowname, x = Q50)) +
  geom_errorbarh(
    data = x,
    aes(y = rowname, xmin = `Q2.5`, xmax = `Q97.5`),
    height = 0
  ) +
  facet_grid(
    trimester ~ first_anc,
    scales = "free",
    ## labeller = labeller(.rows = label_both, .cols = label_value)
  ) +
  theme_manuscript() +
  theme(axis.title.y = element_blank())

p <- p + scale_y_discrete(breaks = breaks, labels = labels)


ggsave_manuscript(
  p,
  file = "bfa_baseline_dco_bayes_fixed_effects",
  width = 8, height = 10
)

## Random effects
ran_effects <- map_dfr(fits, function(fit) {
  x <- ranef(fit, probs = c(0.025, 0.5, 0.975))
  x <- as.data.frame(x$health_zone[, , "Intercept"])
  tibble::rownames_to_column(x)
}, .id = "datacut")

ran_effects <- separate(
  ran_effects, datacut,
  into = c("first_anc", "trimester"), sep = "_"
)

ran_effects$first_anc <- factor(
  ran_effects$first_anc,
  levels = c("oui", "non"),
  labels = c("First ANC", "Follow-up ANC"),
  ordered = TRUE
)

saveRDS(ran_effects, file = "bfa_baseline_dco_bayes_random_effects.rds")
orderly_artefact(
  files = "bfa_baseline_dco_bayes_random_effects.rds",
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
  facet_grid(
    trimester ~ first_anc,
    scales = "free",
    ## labeller = labeller(.rows = label_both, .cols = label_value)
  ) +
  theme_manuscript() +
  theme(axis.title.y = element_blank())

ggsave_manuscript(
  p,
  file = "bfa_baseline_dco_bayes_random_effects",
  width = 8, height = 10
)

icc <- map(fits, performance::icc)

saveRDS(icc, file = "bfa_baseline_dco_bayes_icc.rds")

orderly_artefact(
  files = "bfa_baseline_dco_bayes_icc.rds",
  description = "ICC for DRC 2015 DCO model fits"
)


bayes_r2 <- map(fits, bayes_R2, probs = c(0.025, 0.5, 0.975))

saveRDS(bayes_r2, file = "bfa_baseline_dco_bayes_r2.rds")

orderly_artefact(
  files = "bfa_baseline_dco_bayes_r2.rds",
  description = "Bayes R2 for DRC 2015 DCO model fits"
)
