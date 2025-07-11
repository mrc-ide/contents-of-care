library(broom)
library(dplyr)
library(ggplot2)
library(glmnet)
library(orderly2)
library(purrr)
library(rsample)
library(tibble)
library(tidylog)
library(tidyr)

orderly_shared_resource("utils.R")
source("utils.R")

orderly_dependency("process_burkina_faso", "latest", files = c("bfa_dco.rds"))
bfa_dco <- readRDS("bfa_dco.rds")


bfa_small <- select(
  bfa_dco, consult_length = consult_length_calc,
  facility_type, hcw_sex, hcw_qualification,
  doctor_or_nursing_and_midwifery_per_10000,
  first_anc, trimester, time_elapsed_since_start_of_day,
  first_pregnancy, consult_language,
  region = `REGION.x`
)

bfa_small$facility_type <- case_when(
  bfa_small$facility_type %in% "government facility" ~ "Public",
  ! bfa_small$facility_type %in% "government facility" ~ "Not Public",
  TRUE ~ NA_character_
)

bfa_small$hcw_sex <- case_when(
  bfa_small$hcw_sex %in% 1 ~ "Male",
  bfa_small$hcw_sex %in% 2 ~ "Female",
  TRUE ~ NA_character_
)


bfa_small$region <- case_when(
  bfa_small$region %in% 1 ~ "Boucle du Mouhoun",
  bfa_small$region %in% 2 ~ "Centre-Est",
  bfa_small$region %in% 3 ~ "Centre-Nord",
  bfa_small$region %in% 4 ~ "Centre-Ouest",
  bfa_small$region %in% 5 ~ "Nord",
  bfa_small$region %in% 6 ~ "Sud-Quest",
  TRUE ~ NA_character_
)

bfa_small$consult_language <- case_when(
  bfa_small$consult_language %in% 1 ~ "French",
  bfa_small$consult_language %in% 2 ~ "Moore",
  bfa_small$consult_language %in% 3 ~ "Dioula",
  bfa_small$consult_language %in% c(4:10, 97) ~ "Other",
  TRUE ~ NA_character_
)

bfa_small$first_pregnancy <- case_when(
  bfa_small$first_pregnancy %in% 1 ~ "Yes",
  bfa_small$first_pregnancy %in% 2 ~ "No",
  TRUE ~ NA_character_
)

bfa_small <- filter(bfa_small, consult_length != 0)
bfa_small$log_consult_length <- log(bfa_small$consult_length)

bfa_small <- mutate_if(
  bfa_small, is.character, ~ ifelse(is.na(.), "Unknown", .)
)

set.seed(42)

## Stratify by facility type and first ANC; 97% of the observations are from government facilities
bfa_split <- split(
  bfa_small,
  list(bfa_small$first_anc, bfa_small$trimester),
  sep = "_"
)

## Remove strata with less than 30 observations
map(bfa_split, nrow)
map(bfa_split, function(x) {
  map(x, ~ sum(is.na(.))) |> keep(~ . > 0)
})
##bfa_split <- keep(bfa_split, function(x) nrow(x) >= 30)
bfa_split <- map(bfa_split, na.omit)
## Remove strata with very few observations
bfa_split <- keep(bfa_split, function(x) nrow(x) >= 30)
## This retains 48 observations with unknown trimster
models <- map(bfa_split, function(x) {
  x <- select(x, -first_anc, -trimester, -consult_length)
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
r2_nobs$label <- glue::glue("{r2_nobs$nobs_label}; {r2_nobs$r2_label}")

delta_aic <- map_dfr(models, function(x) {
  aic_initial <- AIC(x$initial)
  aic_final <- AIC(x$final)
  data.frame(delta_aic = aic_initial - aic_final)
}, .id = "datacut") |>
  separate(datacut, into = c("first_anc", "trimester"), sep = "_")

delta_aic$delta_aic <- glue("Diff AIC = {round(delta_aic$delta_aic, 2)}")


 
first_anc_labels <- c(
  "yes" = "First ANC: Yes",
  "no" = "First ANC: No"
)

coeffs$term <- factor(
  coeffs$term,
  levels = c(
    "(Intercept)",
    "regionCentre-Est",
    "regionCentre-Nord",
    "regionCentre-Ouest",
    "regionNord",
    "regionSud-Quest",    
    "facility_typePublic",
    "doctor_or_nursing_and_midwifery_per_10000",
    "hcw_sexMale",
    "hcw_qualificationMidwife",
    "hcw_qualificationNurse",
    "hcw_qualificationOther",
    "first_pregnancyYes",
    "first_pregnancyUnknown",
    "consult_languageFrench",
    "consult_languageMoore",
    "consult_languageOther",
    "consult_languageUnknown",
    "time_elapsed_since_start_of_day"),
      labels = c(
    "(Intercept)",
    "Centre-Est",
    "Centre-Nord",
    "Centre-Ouest",
    "Nord",
    "Sud-Quest",    
    "Public",
    "Doctor/N&M per 10000",
    "HCW: Male",
    "HCW Qualification:Midwife",
    "HCW Qualification:Nurse",
    "HCW Qualification:Other",
    "First pregnancy:Yes",
    "First pregnancy:Unknown",
    "Consult language:French",
    "Consult language:Moore",
    "Consult language:Other",
    "Consult language:Unknown",
    "Hour elapsed since 6AM"),
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
    labeller = labeller(.rows = c('Yes' = "First ANC: Yes", 'No' = "First ANC: No"))
  ) +
  ##scale_y_reverse() +
  theme_manuscript() +
  theme(axis.text.y = element_text(size = 10), legend.title = element_text(size = 12)) +
  labs(x = "Estimate", color = "Significant") 


p1 <- p +
  geom_text_npc(data = r2_nobs, aes(npcx = 0.01, npcy = 0.1, label = label)) +
  geom_text_npc(
    data = delta_aic, aes(npcx = 0.01, npcy = 0.15, label = delta_aic)
  ) 

ggsave_manuscript("bfa_stepwise", p1, width = 12, height = 8)




orderly_resource("lm_burkina_faso_multilevel.R")
source("lm_burkina_faso_multilevel.R")

orderly_resource("lm_burkina_faso_lasso.R")
source("lm_burkina_faso_lasso.R")
