library(broom)
library(dplyr)
library(ggforce)
library(ggplot2)
library(glmnet)
library(glue)
library(janitor)
library(lubridate)
library(orderly2)
library(purrr)
library(rsample)
library(tibble)
library(tidylog)
library(tidyr)

orderly_shared_resource("utils.R")
source("utils.R")



orderly_dependency("process_drc", "latest", files = c("drc_dco_2015_augmented.rds"))
drc_dco_2015 <- readRDS("drc_dco_2015_augmented.rds")




drc_baseline_small <- select(
  drc_dco_2015, consult_length_calc,
  province = province.x, 
  ## Facility characteristics
  doctor_or_nursing_and_midwifery_per_10000,
  facility_status = facility_status.x,
  facility_type = facility_type.y, ## .y is coded properly
  ## milieu_of_residence.y and milieu_of_residence.x differ in 34 rows;
  ## we use the one from the facility survey, which is .y
  ## 34 rows correspond to 17 facilities
  milieu_of_residence = milieu_of_residence.y,
  total_attendance_last_month,
  patients_pay_for_consumables,
  hf_has_fetoscope,
  ## Patient characteristics
  pregnancy_in_weeks, first_pregnancy, first_anc,
  trimester,
  ## HCW characteristics
  hcw_sex, hcw_qualification,
  ## Appointment characteristics
  consultation_language,
  time_elapsed_since_start_of_day
)

drc_baseline_small$first_anc <- factor(
  drc_baseline_small$first_anc,
  levels = c("yes", "no"),
  labels = c("First ANC: Yes", "First ANC: No"),
  ordered = TRUE
)


drc_baseline_small <- mutate_if(
  drc_baseline_small, is.character, ~ ifelse(is.na(.), "Unknown", .)
)


drc_baseline_small$log_consult_length <- log(drc_baseline_small$consult_length_calc)

drc_baseline_small$province <- case_when(
  drc_baseline_small$province %in% "Katanga (comparison)" ~ "Katanga",
  TRUE ~ drc_baseline_small$province
)

drc_baseline_small$hcw_qualification <- case_when(
  drc_baseline_small$hcw_qualification %in% "Lab technician" ~ "Other",
  TRUE ~ drc_baseline_small$hcw_qualification
)


drc_baseline_small$facility_status <- case_when(
  drc_baseline_small$facility_status %in% "Public" ~ "Public",
  drc_baseline_small$facility_status %in% c(
    "Private not for profit", "Public-private partnership",
    "Private for profit" 
  ) ~ "Not public"
)


set.seed(42)
## province is soaking up a lot of variation;
## But we can't stratify by it because within each province, we have very limited
## to no variation in other covariates.



drc_baseline_split <- split(
  drc_baseline_small,
  list(drc_dco_2015$first_anc, drc_dco_2015$trimester),
  sep = "_"
)

## Remove strata with less than 30 observations
map(drc_baseline_split, nrow)
map(drc_baseline_split, function(x) {
  map(x, ~ sum(is.na(.))) |> keep(~ . > 0)
})

##drc_baseline_split <- keep(drc_baseline_split, function(x) nrow(x) >= 30)

drc_baseline_split <- map(drc_baseline_split, na.omit)

## Exclude the very small dataset as we get -Inf AIC
drc_baseline_split <- drc_baseline_split[names(drc_baseline_split) != "yes_First Trimester"]
models <- map(drc_baseline_split, function(x) {
  x <- select(x, -first_anc, -trimester, -consult_length_calc)
  insuff_levels <- map(x, ~ length(unique(.))) |> keep(~ . < 2)
  x <- select(x, -names(insuff_levels))
  full_model <- lm(log_consult_length ~ (.), data = x)
  scope_terms <- terms(log_consult_length ~ (.), data = x)

  final <- step(
    full_model,
    direction = "backward", scope = scope_terms, trace = 1
  )
  list(initial = full_model, final = final)
})

coeffs <- map_dfr(
  models, function(x) tidy(x$final, conf.int = TRUE),
  .id = "datacut"
)
coeffs <- separate(
  coeffs, datacut,
  into = c("first_anc", "trimester"), sep = "_"
)
coeffs$significant <- coeffs$p.value < 0.05

## Extract R2 and nobs
r2_nobs <- map_dfr(models, function(x) {
  data.frame(
    r2 = summary(x$final)$r.squared,
    nobs = nobs(x$final)
  )
}, .id = "datacut") |>
  separate(datacut, into = c("first_anc", "trimester"), sep = "_")

r2_nobs$nobs_label <- glue::glue("n = {r2_nobs$nobs}")
r2_nobs$r2_label <- glue::glue("R² = {percent(r2_nobs$r2)}")
r2_nobs$label <- glue("{r2_nobs$nobs_label}; {r2_nobs$r2_label}")

delta_aic <- map_dfr(models, function(x) {
  aic_initial <- AIC(x$initial)
  aic_final <- AIC(x$final)
  data.frame(delta_aic = aic_initial - aic_final)
}, .id = "datacut") |>
  separate(datacut, into = c("first_anc", "trimester"), sep = "_")

delta_aic$delta_aic <- glue("Diff AIC = {round(delta_aic$delta_aic, 2)}")

referece_categories <- map_dfr(
  models, function(x) {
    xlevels <- x$final$xlevels
    data.frame(
      term = names(xlevels),
      reference = map_chr(xlevels, ~ .[1]),
      estimate = 0
    )
  }, .id = "datacut"
) |> 
  separate(datacut, into = c("first_anc", "trimester"), sep = "_")

 
first_anc_labels <- c(
  "yes" = "First ANC: Yes",
  "no" = "First ANC: No"
)

coeffs$term <- factor(
  coeffs$term,
  levels = c(
    "(Intercept)",
    "milieu_of_residenceUrban",
    "provinceEcuador",
    "provinceKatanga",
    "provinceManiema",
    "provinceNorth Kivu",
    "provinceSouth Kivu",
    "doctor_or_nursing_and_midwifery_per_10000",
    "facility_statusPublic",
    "facility_typeHospital",
    "total_attendance_last_month",
    "patients_pay_for_consumablesYes",
    "patients_pay_for_consumablesUnknown",
    "hf_has_fetoscopeYes",
    "hf_has_fetoscopeUnknown",
    "pregnancy_in_weeks",
    "first_pregnancyno",
    "hcw_sexMale",
    "hcw_sexUnknown",
    "hcw_qualificationMidwife/Obstetrician",
    "hcw_qualificationNurse",
    "hcw_qualificationOther",
    "time_elapsed_since_start_of_day",
    "consultation_languageLingala",
    "consultation_languageOther",
    "consultation_languageSwahili",
    "consultation_languageUnknown"
  ),
      labels = c(
        "Urban",
        "Ecuador",
        "Katanga",
        "Maniema",
        "North Kivu",
        "South Kivu",
        "Doctor/N&M per 10000",
        "Public",
        "Hospital",
        "Total attendance last month",
        "Patients pay for consumables: Yes",
        "Patients pay for consumables: Unknown",
        "Fetoscope: Yes",
        "Fetoscope: Unknown",
        "Pregnancy in weeks",
        "First pregnancy: No",
        "HCW Sex: Male",
        "HCW Sex: Unknown",
        "Midwife/Obstetrician",
        "Nurse",
        "Other",
        "Hours elapsed_since 6AM",
        "Lingala",
        "Other",
        "Swahili",
        "Unknown"
      ),
  ordered = TRUE
)
      
p <- ggplot(coeffs[coeffs$term != "(Intercept)", ]) +
  geom_point(aes(estimate, term, col = significant)) +
  geom_errorbarh(
    aes(y = term, xmin = conf.low, xmax = conf.high, col = significant)
  ) +
  geom_vline(xintercept = 0, linetype = "dashed", col = "grey") +
  facet_grid(
    first_anc ~ trimester,
    scales = "free",
    labeller = labeller(.rows = c('yes' = "First ANC: Yes", 'no' = "First ANC: No"))
  ) +
  scale_y_reverse() +
  theme_manuscript() +
  theme(axis.text.y = element_text(size = 10), legend.title = element_text(size = 12)) +
  labs(x = "Estimate", color = "Significant") 


p1 <- p +
  geom_text_npc(data = r2_nobs, aes(npcx = 0.1, npcy = 0.1, label = label)) +
  geom_text_npc(
    data = delta_aic, aes(npcx = 0.1, npcy = 0.15, label = delta_aic)
  ) 

ggsave_manuscript("drc_2015_stepwise", p1, width = 12, height = 8)


orderly_resource("lm_lasso.R")
source("lm_lasso.R")

orderly_resource("lm_drc_multilevel.R")
source("lm_drc_multilevel.R")
