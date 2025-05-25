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



orderly_dependency("process_drc", "latest", files = c("drc_dco_2015.rds"))
drc_dco_2015 <- readRDS("drc_dco_2015.rds")
start_of_day <- hm("06:00")
drc_dco_2015$time_elapsed_since_start_of_day <- as.numeric(
  drc_dco_2015$consult_start_formatted - start_of_day,
  units = "mins"
)

drc_baseline_small <- select(
  drc_dco_2015, consult_length_calc,
  province, 
  facility_status,
  milieu_of_residence,
  pregnancy_in_weeks, first_pregnancy, first_anc,
  trimester,
  hcw_sex, hcw_qualification,
  consultation_language,
  time_elapsed_since_start_of_day
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


## For covariates included in the model, tabulate the number of observations
nobs <- select(
  drc_baseline_small, province,
  facility_status,
  milieu_of_residence,
  first_pregnancy, first_anc,
  trimester,
  hcw_sex, hcw_qualification,
  consultation_language,
  ) |> map_dfr(function(x) {
    out <- tabyl(x)
    colnames(out)[1] <- "Variable"
    out
  })


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
drc_baseline_split <- keep(drc_baseline_split, function(x) nrow(x) >= 30)
drc_baseline_split <- map(drc_baseline_split, na.omit)

orderly_resource("lm_lasso.R")
source("lm_lasso.R")

orderly_resource("lm_drc_multilevel.R")
source("lm_drc_multilevel.R")
