library(brms)
library(dplyr)
library(ggplot2)
library(orderly2)
library(performance)
library(posterior)
library(purrr)
library(tidyr)

orderly_shared_resource(utils.R = "utils.R")
source("utils.R")

orderly_dependency("lm_drc", "latest", files = c("drc_fits.rds"))
fits <- readRDS("drc_fits.rds")

fixed_effects <- map_dfr(fits, function(fit) {
  x <- as.data.frame(fixef(fit, probs = c(0.025, 0.5, 0.975)))
  tibble::rownames_to_column(x)
}, .id = "datacut")

fixed_effects <- separate(
  fixed_effects, datacut,
  into = c("first_anc", "trimester"), sep = "_"
)

saveRDS(fixed_effects, file = "drc_dco_bayes_fixed_effects.rds")
orderly_artefact(
  files = "drc_dco_bayes_fixed_effects.rds",
  description = "Fixed effects for DRC 2015 DCO model fits"
)

x <- filter(fixed_effects, !rowname %in% "Intercept")


breaks <- c(
  "provinceEcuador",
  "provinceKatanga",
  "provinceManiema",
  "provinceNorthKivu",
  "provinceSouthKivu",   

  "milieu_of_residenceUrban",

  "facility_status_mappingPublic",
  "facility_typeHospital",

  "doctor_or_nursing_and_midwifery_per_10000",  
  "total_attendance_last_month",
  "pregnant_women_last_month",
  "maternal_deaths_last_month",

  "patients_pay_for_consumablesYes",
  "patients_pay_for_consumablesUnknown",

  "hf_has_fetoscopeYes",
  "hf_has_fetoscopeUnknown",

  "pregnancy_in_weeks",
  "first_pregnancyNo",

  "hcw_sexMale",
  "hcw_sexUnknown",   

  "hcw_qualificationMidwifeDObstetrician",  
  "hcw_qualificationNurse",
  "hcw_qualificationOther",
  "hcw_qualificationUnknown",

  "consultation_languageOther",  
  "consultation_languageUnknown",
  "consultation_languageSwahili",  
  "consultation_languageLingala",

  "time_elapsed_since_start_of_day"
)

x$rowname <- factor(x$rowname, levels = breaks, ordered = TRUE)

labels <- c(
  "Ecuador",
  "Katanga",
  "Maniema",
  "NorthKivu",
  "SouthKivu",
  
  "Urban",

  "Public",
  "Hospital",
  "Doctor/N&M per 10000",
  "Total attendance last_month",
  "Pregnant womrn last month",
  "Maternal deaths last month",
  "Patients pay for consumables:Yes",
  "Patients pay for consumables:Unknown",
  "Fetoscope:Yes",
  "Fetoscope:Unknown",
  "Pregnancy in weeks",
  "First pregnancy:No",
  
  "HCW sex:Male",
  "HCW sex:Unknown",
  
  "Midwife/Obstetrician",
  "Nurse",
  "Other",
  "Qualification:Unknown",

  "Consultation language:Swahili",
  "Consultation language:Lingala",  
  "Consultation language:Other",
  "Consultation language:Unknown",

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
  file = "drc_dco_bayes_fixed_effects",
  width = 8, height = 10
)

## Random effects
ran_effects <- map_dfr(fits, function(fit) {
  x <- ranef(fit, probs = c(0.025, 0.5, 0.975))
  x <- as.data.frame(x$province[, , "Intercept"])
  tibble::rownames_to_column(x)
}, .id = "datacut")

ran_effects <- separate(
  ran_effects, datacut,
  into = c("first_anc", "trimester"), sep = "_"
)


saveRDS(ran_effects, file = "drc_dco_bayes_random_effects.rds")
orderly_artefact(
  files = "drc_dco_bayes_random_effects.rds",
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
  file = "drc_dco_bayes_random_effects",
  width = 8, height = 10
)

icc <- map(fits, compute_icc)

saveRDS(icc, file = "drc_dco_bayes_icc.rds")

orderly_artefact(
  files = "drc_dco_bayes_icc.rds",
  description = "ICC for DRC 2015 DCO model fits"
)


bayes_r2 <- map(fits, bayes_R2, probs = c(0.025, 0.5, 0.975))

saveRDS(bayes_r2, file = "drc_dco_bayes_r2.rds")

orderly_artefact(
  files = "drc_dco_bayes_r2.rds",
  description = "Bayes R2 for DRC 2015 DCO model fits"
)
