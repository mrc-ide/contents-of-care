library(broom)
library(cli)
library(dplyr)
library(ggplot2)
library(ggpmisc)
library(glue)
library(glmnet)
library(orderly2)
library(purrr)
library(rsample)
library(scales)
library(tibble)
library(tidylog)
library(tidyr)

orderly_shared_resource("utils.R")
source("utils.R")

orderly_dependency(
  "process_burkina_faso", "latest",
  files = c("bfa_endline_dco.rds")
)
bfa_dco <- readRDS("bfa_endline_dco.rds")

bfa_small <- select(
  bfa_dco,
  consult_length = consult_length_calc,
  ## HF attributes
  region_name,
  num_csps_in_district = EFF,
  num_personnel = PERSO,
  milieu_of_residence = milieu_of_residence.x,
  doctor_or_nursing_and_midwifery_per_10000,
  facility_level_mapping = facility_level_mapping.x,
  total_attendance,
  attendance_pregnant_women,
  num_maternal_deaths,
  ## patient attributes
  patage_i,
  patlit_i,
  patmar_i,
  first_anc,
  trimester,
  pregnancy_week,
  first_pregnancy = patpar_f3_i,
  ## HCW attributes
  hcw_time_in_service = hwsen_i,
  hcw_age = hwage_i,
  hcw_sex = hwsex_i,
  hcw_qualification,
  ## Appointment attributes
  consult_language,
  time_elapsed_since_start_of_day
)

bfa_small$patlit_i <- factor(
  bfa_small$patlit_i,
  levels = c(0, 1),
  labels = c("Illiterate", "Literate"),
  ordered = FALSE
)

bfa_small$first_pregnancy <- factor(
  bfa_small$first_pregnancy,
  levels = c(0, 1),
  labels = c("No", "Yes"),
  ordered = FALSE
)


bfa_small$patmar_i <- factor(
  bfa_small$patmar_i,
  levels = c(0, 1),
  labels = c("Not married", "Married"),
  ordered = FALSE
)

bfa_small$hcw_sex <- factor(
  bfa_small$hcw_sex,
  levels = c(0, 1), labels = c("Female", "Male"),
  ordered = FALSE
)



bfa_small <- filter(bfa_small, consult_length != 0)
bfa_small$log_consult_length <- log(bfa_small$consult_length)

bfa_small <- mutate_if(
  bfa_small, is.character, ~ ifelse(is.na(.), "Unknown", .)
)
## Remove missing continuous variables
bfa_small <- bfa_small[complete.cases(bfa_small), ]
## Scale continuous variables
cols_to_scale <- c(
  "num_csps_in_district",
  "doctor_or_nursing_and_midwifery_per_10000",
  "total_attendance",
  "attendance_pregnant_women",
  "num_personnel"
)
bfa_small <- mutate(
  bfa_small,
  across(
   all_of(cols_to_scale),
    ~ scale(.)[, 1],
    .names = "{.col}_scaled"
  )
)

## Drop unscaled vars
bfa_small <- select(bfa_small, -all_of(cols_to_scale))
set.seed(42)

bfa_split <- split(
  bfa_small,
  list(bfa_small$first_anc, bfa_small$trimester),
  sep = "_"
)

## Remove strata with less than 30 observations
map(bfa_split, nrow)
## Sanity check; this should be empty
map(bfa_split, function(x) {
  map(x, ~ sum(is.na(.))) |> keep(~ . > 0)
})


models <- map(bfa_split, function(x) {
  x <- select(x, -first_anc, -trimester, -consult_length)
  insuff_levels <- map(x, ~ length(unique(.))) |> keep(~ . < 2)
  cli_alert_info(
    "Removing variables with insufficient levels: {names(insuff_levels)}"
  )
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
r2_nobs$label <- glue::glue("{r2_nobs$nobs_label}; {r2_nobs$r2_label}")

delta_aic <- map_dfr(models, function(x) {
  aic_initial <- AIC(x$initial)
  aic_final <- AIC(x$final)
  data.frame(delta_aic = aic_initial - aic_final)
}, .id = "datacut") |>
  separate(datacut, into = c("first_anc", "trimester"), sep = "_")

delta_aic$delta_aic <- glue("Diff AIC = {round(delta_aic$delta_aic, 2)}")


      
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
  ##scale_y_reverse() +
  theme_manuscript() +
  theme(
    axis.text.y = element_text(size = 10),
    legend.title = element_text(size = 12)
  ) +
  labs(x = "Estimate", color = "Significant") 


p1 <- p +
  geom_text_npc(data = r2_nobs, aes(npcx = 0.01, npcy = 0.1, label = label)) +
  geom_text_npc(
    data = delta_aic, aes(npcx = 0.01, npcy = 0.15, label = delta_aic)
  ) 

ggsave_manuscript("bfa_endline_stepwise", p1, width = 12, height = 8)




orderly_resource("lm_burkina_faso_endline_multilevel.R")
source("lm_burkina_faso_endline_multilevel.R")
