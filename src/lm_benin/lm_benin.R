library(broom)
library(brms)
library(dplyr)
library(ggplot2)
library(ggpmisc)
library(glmnet)
library(glue)
library(orderly2)
library(purrr)
library(rsample)
library(tibble)
library(tidylog)
library(tidyr)




orderly_shared_resource(utils.R = "utils.R")
source("utils.R")




orderly_dependency("process_benin", "latest", files = c("benin_dco.rds"))
benin_dco <- readRDS("benin_dco.rds")
benin_dco$time_elapsed_since_start_of_day <- round(benin_dco$time_elapsed_since_start_of_day / 60)
## Some questions use 1 for yes and 2 for no; recode as 0 for no and 1 for yes
benin_dco$fetoscope <- case_when(
  benin_dco$fetoscope %in% 2L ~ "non",
  benin_dco$fetoscope %in% 1L ~ "oui",
  TRUE ~ NA_character_
)

benin_dco$women_in_labour_pay <- case_when(
  benin_dco$women_in_labour_pay %in% 2L ~ "non",
  benin_dco$women_in_labour_pay %in% 1L ~ "oui",
  TRUE ~ NA_character_
)

benin_dco$pregnant_women_private_space <- case_when(
  benin_dco$pregnant_women_private_space %in% 2L ~ "non",
  benin_dco$pregnant_women_private_space %in% 1L ~ "oui",
  TRUE ~ NA_character_
)


benin_small <- select(
  benin_dco, consult_length, m0_milieu, health_zone, facility_type, facility_status,
  pregnant_women_private_space, doctor_or_nursing_and_midwifery_per_10000,
  women_in_labour_pay, hcw_qualification, first_anc, trimester, time_elapsed_since_start_of_day,
  fetoscope, number_of_births_2009
)

benin_small$hcw_qualification <- case_when(
  !benin_small$hcw_qualification %in% c("Doctor", "Midwife", "Nurese") ~ "Other",
  TRUE ~ benin_small$hcw_qualification
)

benin_small$log_consult_length <- log(benin_small$consult_length)

set.seed(42)

## Stratify by facility type and first ANC
benin_split <- split(
  benin_small, list(benin_dco$first_anc, benin_dco$trimester),
  sep = "_"
)

## Remove strata with less than 30 observations
map(benin_split, nrow)
## Find out the number if NAs in each column in each stratum
map(benin_split, function(x) {
  map(x, ~ sum(is.na(.))) |> keep(~ . > 0) 
})
## Make NAs into "Unknown"
benin_split <- map(benin_split, 
  ~ mutate_all(.x, ~ ifelse(is.na(.), "Unknown", .)) 
)


benin_split <- map(benin_split, na.omit)
benin_split <- keep(benin_split, function(x) nrow(x) >= 30)




## Stepwise regression
models <- map(benin_split, function(x) {
  x <- select(x, -first_anc, -trimester, -consult_length)
  insuff_levels <- map(x, ~ length(unique(.))) |> keep(~ . < 2)  
  x <- select(x, -names(insuff_levels))
  ref <- lm(log_consult_length ~ ., data = x)
  final <- step(
    ref, direction = "backward", scope = formula(lm(log_consult_length ~ ., data = x)),, trace = 10)
  ref <- lm(log_consult_length ~ 1, data = x)
  final_fw <- step(
    ref, direction = "forward", scope = formula(lm(log_consult_length ~ ., data = x)),
    trace = 10)

  list(
    backward = final,
    forward = final_fw
  )
})

## Visualise the models
coeffs <- map_dfr(models, function(x) tidy(x$backward), .id = "datacut")
coeffs <- separate(coeffs, datacut, into = c("first_anc", "trimester"), sep = "_")
coeffs$significant <- coeffs$p.value < 0.05

ggplot(coeffs[coeffs$term != "(Intercept)", ]) +
  geom_point(aes(estimate, term, col = significant)) +
  geom_errorbarh(aes(y = term, xmin = estimate - std.error, xmax = estimate + std.error, col = significant)) +
  geom_vline(xintercept = 0, linetype = "dashed", col = "grey") + 
  facet_grid(first_anc ~ trimester, scales = "free_x") +
  scale_y_discrete(labels = snakecase::to_title_case) +
  labs(x = "Estimate") +
  theme_minimal() +
  theme(axis.text.y = element_text(size = 8))


## Compare systematcally
map(models, function(x) AIC(x$backward, x$forward))
map(models, function(x) anova(x$backward, x$forward, "LRT")) 



orderly_resource("lm_benin_multilevel.R")
source("lm_benin_multilevel.R")






orderly_resource("lm_benin_lasso.R")
source("lm_benin_lasso.R")


