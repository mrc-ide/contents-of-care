library(cli)
library(dplyr)
library(lubridate)
library(orderly2)
library(purrr)
library(tidylog)
library(tidyr)

orderly_shared_resource("utils.R")
source("utils.R")

orderly_dependency(
  "process_burkina_faso", "latest", "bfa_baseline_dco.rds"
)
bfa_baseline_dco <- readRDS("bfa_baseline_dco.rds")

## Unlike Benin DCO, Questions here have not been broken down by trimester
bfa_split <- split(
  bfa_baseline_dco, list(bfa_baseline_dco$trimester, bfa_baseline_dco$first_anc)
)

## First ensure that there are recorded steps for each intervention type
## If not, drop that intervention type.
intervention_types <-
  keep(intervention_types, function(intv_type) {
    length(intersect(intv_type, colnames(bfa_baseline_dco)))> 0 
})

## For each type of intervention, create new variable that indicates
## the number of steps taken
with_completeness_idx <- imap(
  intervention_types, function(intv_type, intv_name) {
    map(bfa_split, function(df) {
        x <- select(df, any_of(intv_type))
        cli_inform("I found {ncol(x)} columns for {intv_name}")
        steps_taken <- rowwise(x) |>
          mutate(
            steps_taken = sum(c_across(any_of(intv_type)) %in% 1),
            steps_missed = sum(c_across(any_of(intv_type)) %in% 2),
            steps_na = sum(is.na(c_across(any_of(intv_type))))
          ) |>
          select(steps_taken, steps_missed, steps_na) 

        x <- cbind(df, steps_taken)
        x$steps_total <-
          (steps_taken$steps_taken + steps_taken$steps_missed +
           steps_taken$steps_na)
        x
    }) 
  })


## Select covariates

with_completeness_idx <- map_depth(
  with_completeness_idx, 2, function(df) {
    scaled_vars <- grep("scaled", colnames(df), value = TRUE)
    cli_inform("Selecting covariates for {scaled_vars}")
    select(
      df,
      consult_length = consult_length_calc,
      ## HF attributes
      region_name,
      milieu_of_residence = milieu_of_residence.x,
      facility_level_mapping = facility_level_mapping.x,
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
      time_elapsed_since_start_of_day,
      steps_taken, steps_total,
      all_of(scaled_vars)
    )
 })

saveRDS(
  with_completeness_idx,
  file = "bfa_baseline_dco_with_completeness_idx.rds",
  compress = "xz"
)

orderly_artefact(
  files = "bfa_baseline_dco_with_completeness_idx.rds",
  description = "Bfa_Baseline DCO with completeness index"
)



