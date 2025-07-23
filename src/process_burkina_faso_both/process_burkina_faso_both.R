library(cli)
library(dplyr)
library(janitor)
library(lubridate)
library(orderly2)
library(purrr)
library(readr)
library(skimr)
library(table1)
library(tidylog)



orderly_shared_resource("utils.R")
source("utils.R")

orderly_shared_resource("bfa_utils.R")
source("bfa_utils.R")

orderly_dependency(
  "process_burkina_faso", "latest", "bfa_baseline_dco.rds"
)

orderly_dependency(
  "process_burkina_faso_endline", "latest", "bfa_endline_dco.rds"
)

bfa_baseline_dco <- readRDS("bfa_baseline_dco.rds")
bfa_endline_dco <- readRDS("bfa_endline_dco.rds")

common_cols <- intersect(names(bfa_baseline_dco), names(bfa_endline_dco))

bfa_dco <- bind_rows(
  list(
    baseline = bfa_baseline_dco[, common_cols],
    endline = bfa_endline_dco[, common_cols]
  ),
  .id = "survey"
)



saveRDS(bfa_dco, "bfa_dco.rds")
orderly_artefact(
  files = "bfa_dco.rds",
  description = "DCO for Burkina Faso baseline and endline surveys"
)

## Both
bfa_small <- select(
  bfa_dco,
  consult_length = consult_length_calc,
  ## HF attributes
  region_name,
  num_csps_in_district,
  num_personnel,
  milieu_of_residence = milieu_of_residence.x,
  doctor_or_nursing_and_midwifery_per_10000,
  facility_level_mapping = facility_level_mapping.x,
  total_attendance,
  attendance_pregnant_women,
  num_maternal_deaths,
  ## patient attributes
  first_anc,
  trimester,
  pregnancy_week,
  first_pregnancy,
  ## HCW attributes
  hcw_sex,
  hcw_qualification,
  ## Appointment attributes
  consult_language,
  time_elapsed_since_start_of_day
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


saveRDS(bfa_split, "bfa_baseline_split.rds")
orderly_artefact(
  files = "bfa_baseline_split.rds",
  description = "Data used for model fitting"
)
