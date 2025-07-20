library(broom)
library(dplyr)
library(ggforce)
library(ggplot2)
library(glue)
library(ggpmisc)
library(glmnet)
library(glue)
library(janitor)
library(lubridate)
library(orderly2)
library(purrr)
library(rsample)
library(scales)
library(tibble)
library(tidylog)
library(tidyr)

orderly_shared_resource("utils.R")
source("utils.R")



orderly_dependency("process_drc", "latest", files = c("drc_dco_2015_augmented.rds"))
drc_dco_2015 <- readRDS("drc_dco_2015_augmented.rds")

drc_baseline_small <- select(
  drc_dco_2015,
  consult_length_calc,
  province,
  ## Facility characteristics
  doctor_or_nursing_and_midwifery_per_10000,
  facility_status_mapping,
  facility_type,
  milieu_of_residence,
  total_attendance_last_month,
  pregnant_women_last_month,
  maternal_deaths_last_month,
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

## drc_baseline_small$first_anc <- factor(
##   drc_baseline_small$first_anc,
##   levels = c("yes", "no"),
##   labels = c("First ANC: Yes", "First ANC: No"),
##   ordered = TRUE
## )


drc_baseline_small <- mutate_if(
  drc_baseline_small, is.character, ~ ifelse(is.na(.), "Unknown", .)
)
drc_baseline_small$log_consult_length <-
  log(drc_baseline_small$consult_length_calc)

drc_baseline_small <- select(drc_baseline_small, -consult_length_calc)

drc_baseline_small$province <- case_when(
  drc_baseline_small$province %in% "Katanga (comparison)" ~ "Katanga",
  TRUE ~ drc_baseline_small$province
)

drc_baseline_small$hcw_qualification <- case_when(
  drc_baseline_small$hcw_qualification %in% "Lab technician" ~ "Other",
  TRUE ~ drc_baseline_small$hcw_qualification
)

## Scale continuous variables before splitting
drc_baseline_small <- mutate(
  drc_baseline_small,
  across(
    c(
      doctor_or_nursing_and_midwifery_per_10000,
      total_attendance_last_month
    ),
    scale
  )
)



set.seed(42)

drc_baseline_split <- split(
  drc_baseline_small,
  list(drc_dco_2015$first_anc, drc_dco_2015$trimester),
  sep = "_"
)


map(drc_baseline_split, nrow)
map(drc_baseline_split, function(x) {
  map(x, ~ sum(is.na(.))) |> keep(~ . > 0)
})

##drc_baseline_split <- keep(drc_baseline_split, function(x) nrow(x) >= 30)

drc_baseline_split <- map(drc_baseline_split, na.omit)

factor_vars <- c(
  "province",
  "facility_status",
  "facility_type",
  "milieu_of_residence",
  "patients_pay_for_consumables",
  "hf_has_fetoscope",
  "first_pregnancy",
  "first_anc",
  "trimester",
  "hcw_sex",
  "hcw_qualification",
  "consultation_language"
)

drc_baseline_split <- map(drc_baseline_split, function(x) {
  insuff_levels <- map_int(factor_vars, function(var) length(unique(x[[var]])))
  insuff_levels <- factor_vars[which(insuff_levels == 1)]
  ## Drop invariant variables
  cli::cli_alert(
    "Dropping {length(insuff_levels)} invariant variables: {insuff_levels}"
  )
  x <- x[, !names(x) %in% insuff_levels]
  x
})

saveRDS(drc_baseline_split, "drc_baseline_split.rds")
orderly_artefact(
  files = "drc_baseline_split.rds",
  description = "DRC data used for model fitting"
)

orderly_resource("lm_drc_multilevel.R")
source("lm_drc_multilevel.R")


## ## Experiment: remove all covariates related to the facility
## ## and regress on facility_id only
## x <- drc_baseline_split$`yes_Third Trimester`
## model <- lm(
##   log_consult_length ~ as.factor(facility_id),
##   data = x
## )
## coeffs <- tidy(model, conf.int = TRUE)
## coeffs$significant <- coeffs$p.value < 0.05
## coeffs$facility_id <- as.numeric(gsub("as.factor\\(facility_id\\)", "", coeffs$term))
## ## Bring back the covariates_nice_names
## coeffs <- left_join(coeffs, x, by = "facility_id")
## coeffs <- arrange(coeffs, desc(estimate))
## coeffs$facility_id <- factor(
##   coeffs$facility_id,
##   levels = unique(coeffs$facility_id),
##   ordered = TRUE
## )
## coeffs <- filter(coeffs, !term %in% "(Intercept)")

## ggplot(coeffs, aes(as.factor(facility_id), estimate, col = doctor_or_nursing_and_midwifery_per_10000)) +
##   geom_point() +
##   geom_errorbar(aes(ymin = conf.low, ymax = conf.high)) +
##   geom_hline(yintercept = 0, linetype = "dashed", col = "grey") +
##   labs(x = "Facility ID", y = "Estimate", color = "Significant") +
##   theme_manuscript() 

## Exclude the very small dataset as we get -Inf AIC
drc_baseline_split <-
  drc_baseline_split[names(drc_baseline_split) != "First ANC_First Trimester"]

models <- map(drc_baseline_split, function(x) {
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

 

p <- ggplot(coeffs[coeffs$term != "(Intercept)", ]) +
  geom_point(aes(estimate, term, col = significant)) +
  geom_errorbarh(
    aes(y = term, xmin = conf.low, xmax = conf.high, col = significant)
  ) +
  geom_vline(xintercept = 0, linetype = "dashed", col = "grey") +
  facet_grid(
    first_anc ~ trimester,
    scales = "free"
  ) +
  theme_manuscript() +
  theme(axis.text.y = element_text(size = 10),
        legend.title = element_text(size = 12)) +
  labs(x = "Estimate", color = "Significant") 


p1 <- p +
  geom_text_npc(data = r2_nobs, aes(npcx = 0.1, npcy = 0.1, label = label)) +
  geom_text_npc(
    data = delta_aic, aes(npcx = 0.1, npcy = 0.15, label = delta_aic)
  ) 

ggsave_manuscript("drc_2015_stepwise", p1, width = 12, height = 8)

