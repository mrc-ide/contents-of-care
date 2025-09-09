library(cli)
library(dplyr)
library(lubridate)
library(orderly2)
library(purrr)
##library(tidylog)
library(tidyr)

orderly_shared_resource("utils.R")
source("utils.R")

orderly_dependency(
  "process_drc", "latest", "drc_dco_2015_augmented.rds"
)

orderly_dependency(
  "process_drc_endline", "latest", "drc_dco_2021.rds"
)

drc_baseline_dco <- readRDS("drc_dco_2015_augmented.rds")
drc_endline_dco <- readRDS("drc_dco_2021.rds")

pars <- orderly_parameters(survey = "baseline")
drc_dco <- if (pars$survey == "baseline") drc_baseline_dco else drc_endline_dco


## scale consult length here
x <- scale(drc_dco$consult_length)
drc_dco$consult_length_scaled <- x[, 1]

scaled_attrs <- data.frame(
  variable = "consult_length",
  mean = attr(x, "scaled:center"),
  sd = attr(x, "scaled:scale")
)

outfile <- "drc_consult_len_scaled_attrs.rds"
saveRDS(
  scaled_attrs,
  file = outfile,
  compress = "xz"
)

orderly_artefact(
  files = outfile,
  description = "Attributes of scaled variables in Drc DCO"
)


## Unlike Benin DCO, Questions here have not been broken down by trimester
drc_split <- split(
  drc_dco, list(drc_dco$trimester, drc_dco$first_anc), sep = "_"
)

## First ensure that there are recorded steps for each intervention type
## If not, drop that intervention type.
intervention_types <-
  keep(intervention_types, function(intv_type) {
    length(intersect(intv_type, colnames(drc_dco)))> 0 
})

## For each type of intervention, create new variable that indicates
## the number of steps taken
with_completeness_idx <- imap(
  intervention_types, function(intv_type, intv_name) {
    map(drc_split, function(df) {
        x <- select(df, any_of(intv_type))
        cli_inform("I found {ncol(x)} columns for {intv_name}")
        steps_taken <- rowwise(x) |>
          mutate(
            steps_taken = sum(c_across(any_of(intv_type)) %in% "yes"),
            steps_missed = sum(c_across(any_of(intv_type)) %in% "no"),
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
      province,
      facility_status_mapping,
      facility_type = facility_status_mapping,
      milieu_of_residence,
      patients_pay_for_consumables,
      hf_has_fetoscope,
      ## Patient characteristics
      pregnancy_in_weeks, first_pregnancy, first_anc,
      trimester,
      ## HCW characteristics
      hcw_sex, hcw_qualification,
      ## Appointment characteristics
      consultation_language,
      time_elapsed_since_start_of_day,
     steps_taken, steps_total,
      all_of(scaled_vars),
      any_of(c("maternal_deaths_last_month", "languages_aligned"))
    )
 })

saveRDS(
  with_completeness_idx,
  file = "drc_dco_with_completeness_idx.rds",
  compress = "xz"
)

orderly_artefact(
  files = "drc_dco_with_completeness_idx.rds",
  description = "DRC DCO with completeness index"
)



