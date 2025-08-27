library(cli)
library(dplyr)
library(lubridate)
library(orderly2)
library(purrr)
library(tidylog)
library(tidyr)

## Data dictionary available at
## https://microdata.worldbank.org/index.php/catalog/2176/data-dictionary

## Lemière, C., & de Walque, D. (2014). Health Results-Based Financing Impact
## Evaluation Survey 2010-2011, Baseline [Data set]. World Bank, Development Data
## Group. https://doi.org/10.48529/QKMG-R734

orderly_shared_resource("utils.R")
source("utils.R")

orderly_dependency("process_benin", "latest", "benin_dco.rds")
benin_dco <- readRDS("benin_dco.rds")

## Trimester relevant questions; 
first_trimester <- select(benin_dco, !(uterus_measured2:sp_ensured3)) |>
  filter(trimester %in% "First Trimester")

second_trimester <- select(
  benin_dco, !(patients_age_reported:mosq_kit_prescribed),
  !(uterus_measured3:sp_ensured3)
) |> filter(trimester %in% "Second Trimester")

third_trimester <- select(
  benin_dco, !(patients_age_reported:fansidar_prescribed)
) |> filter(trimester %in% "Third Trimester")


## For each type of intervention, create new variable that indicates
## the number of steps taken
with_completeness_idx <- imap(
  intervention_types, function(intv_type, intv_name) {
    map(list(first_trimester, second_trimester, third_trimester), 
      function(df) {
        x <- select(df, any_of(intv_type))
        steps_taken <- rowwise(x) |>
          mutate(
            steps_taken = sum(c_across(any_of(intv_type)) %in% "Yes"),
            steps_missed = sum(c_across(any_of(intv_type)) %in% "No"),
            steps_na = sum(is.na(c_across(any_of(intv_type))))
          ) |>
          select(steps_taken, steps_missed, steps_na) 

        x <- cbind(df, steps_taken)
        x$steps_total <-
          (steps_taken$steps_taken + steps_taken$steps_missed +
           steps_taken$steps_na)

        ## Split by first anc because that's how we will model it
        split(x, x$first_anc)
      }) 
  })


## Select covariates
scaled_vars <- grep("scaled", colnames(x), value = TRUE)
with_completeness_idx <- map_depth(
  with_completeness_idx, 3, function(df) {
    select(
      df, consult_length,
      milieu_of_residence, health_zone,
      facility_level_mapping,
      facility_status_mapping,
      pregnant_women_private_space,
      hf_has_fetoscope, women_in_labour_pay,
      hcw_qualification, first_anc, trimester, time_elapsed_since_start_of_day,
      steps_taken, steps_total,
      all_of(scaled_vars)
    )
 })

saveRDS(
  with_completeness_idx,
  file = "benin_dco_with_completeness_idx.rds",
  compress = "xz"
)

orderly_artefact(
  files = "benin_dco_with_completeness_idx.rds",
  description = "Benin DCO with completeness index"
)



