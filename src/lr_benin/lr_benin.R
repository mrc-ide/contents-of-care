library(broom)
library(brms)
library(cli)
library(dplyr)
library(ggplot2)
library(ggpmisc)
library(glmnet)
library(glue)
library(orderly2)
library(purrr)
library(rsample)
library(scales)
library(snakecase)
library(tibble)
library(tidylog)
library(tidyr)




orderly_shared_resource(utils.R = "utils.R")
source("utils.R")

orderly_dependency("process_benin", "latest", files = c("benin_dco.rds"))
benin_dco <- readRDS("benin_dco.rds")
benin_small <- select(
  benin_dco, consult_length,
  milieu_of_residence, health_zone,
  facility_level_mapping,
  facility_status_mapping,
  pregnant_women_private_space, doctor_or_nursing_and_midwifery_per_10000,
  fetoscope, number_of_births_2009, women_in_labour_pay,
  hcw_qualification, first_anc, trimester, time_elapsed_since_start_of_day,
  m0_11
)

benin_small$first_anc <- case_when(
  benin_small$first_anc %in% "oui" ~ "First ANC",
  benin_small$first_anc %in% "non" ~ "Follow-up ANC",
  TRUE ~ benin_small$first_anc
)


benin_small$hcw_qualification <- case_when(
  !benin_small$hcw_qualification %in%
    c("Doctor", "Midwife", "Nurse") ~ "Other",
  TRUE ~ benin_small$hcw_qualification
)
benin_dco$hcw_qualification <- factor(benin_dco$hcw_qualification)
benin_dco$hcw_qualification <- relevel(
  benin_dco$hcw_qualification, ref = "Doctor"
)

factor_vars <- c(
  "milieu_of_residence", "health_zone", "facility_level_mapping",
  "facility_status_mapping", "pregnant_women_private_space",
  "fetoscope", "women_in_labour_pay",
  "hcw_qualification", "first_anc", "trimester"
)


## Make NAs into "Unknown"
benin_small <- mutate(
  benin_small, across(all_of(factor_vars), function(x) {
    x <- as.character(x)
    x[is.na(x)] <- "Unknown"
    x
  }
))


set.seed(42)


## Scale continuous variables before splitting
cols_to_scale <- "doctor_or_nursing_and_midwifery_per_10000"
 
benin_small <- mutate(
  benin_small,
  across(
    all_of(cols_to_scale),
    ~ scale(.)[, 1],
    .names = "{.col}_scaled"
  )
)

## Drop unscaled vars
benin_small <- select(benin_small, -all_of(cols_to_scale))

benin_split <- split(
  benin_small, list(benin_dco$first_anc, benin_dco$trimester),
  sep = "_"
)

benin_split <- map(benin_split, function(x) {
  insuff_levels <- map_int(factor_vars, function(var) length(unique(x[[var]])))
  insuff_levels <- factor_vars[which(insuff_levels == 1)]
  ## Drop invariant variables
  cli::cli_alert(
    "Dropping {length(insuff_levels)} invariant variables: {insuff_levels}"
  )
  x <- x[, !names(x) %in% insuff_levels]
  x
})


## benin_split <- split(
##   benin_dco, list(benin_dco$first_anc, benin_dco$trimester), sep = "_"
## )

## out <- map(benin_split, function(this_split) {
##   this_consult_length <- unique(this_split$consult_length)
##   map_dfr(this_consult_length, function(consult_len) {
##     x <- filter(
##       this_split, consult_length %in% consult_len
##     )
##     steps_x <- select(x, m0_03:m3_6)
##     tmp <- map(steps_x, tabyl, show_na = TRUE) |>
##       map(function(x){
##         x$`.x[[i]]` <- as.character(x$`.x[[i]]`)
##         x
##       }) |> bind_rows(.id = "step")

##     tmp$consult_length <- consult_len
##     tmp
##   })
## })

## out2 <- bind_rows(out, .id = "first_anc_trimester")


## m0_11 is testing for Albuminurie;
x <- benin_split[[1]]

fits <- map(benin_split, function(x) {
  x$m0_11 <- ifelse(x$m0_11 %in% "oui", 1, 0)
  brm(
    formula = bf(m0_11 ~ . - health_zone + (1 | health_zone)),
    data = x,
    family = bernoulli(link = "logit"),
    drop_unused_levels = TRUE,
    chains = 4,
    cores = 4,
    iter = 4000,
    prior = prior_spec,
    control = list(adapt_delta = 0.99)
)})
