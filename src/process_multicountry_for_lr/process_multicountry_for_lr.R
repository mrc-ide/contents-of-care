library(brms)
library(cli)
library(dplyr)
library(glue)
library(orderly2)
library(performance)
library(purrr)
library(snakecase)
library(tidyr)
library(tidylog)

orderly_shared_resource("utils.R")
source("utils.R")


orderly_dependency(
  "process_benin_for_lr", "latest",
  files = "benin_dco_with_completeness_idx.rds"
)

orderly_dependency(
  "process_drc_for_lr", "latest(parameter:survey == 'baseline')",
  files = c(drc_2015.rds = "drc_dco_with_completeness_idx.rds")
)

orderly_dependency(
  "process_drc_for_lr", "latest(parameter:survey == 'endline')",
  files = c(drc_2021.rds = "drc_dco_with_completeness_idx.rds")
)


orderly_dependency(
  "process_bfa_for_lr", "latest(parameter:survey == 'baseline')",
  files = c(bfa_2015.rds = "bfa_dco_with_completeness_idx.rds")
)

orderly_dependency(
  "process_bfa_for_lr", "latest(parameter:survey == 'endline')",
  files = c(bfa_2021.rds = "bfa_dco_with_completeness_idx.rds")
)

benin_split <- readRDS("benin_dco_with_completeness_idx.rds")

drc_baseline_split <- readRDS("drc_2015.rds")
drc_endline_split <- readRDS("drc_2021.rds")

bfa_baseline_split <- readRDS("bfa_2015.rds")
bfa_endline_split <- readRDS("bfa_2021.rds")

## Benin has a slightly different structure to DRC and Burkina Faso
## So first we will ensure they have the same structure.
a <- benin_split[["Maternal and fetal assessment"]]
b <- benin_split[["Informational Interventions"]]
c <- benin_split[["Preventive measures"]]
d <- benin_split[["Continuity of care"]]

benin <- list(
  "Maternal and fetal assessment" =
    list(
      "First Trimester_First ANC" = a[["First Trimester"]][["First ANC"]],
      "Second Trimester_First ANC" = a[["Second Trimester"]][["First ANC"]],
      "Third Trimester_First ANC" = a[["Third Trimester"]][["First ANC"]],
      "First Trimester_Follow-up ANC" = a[["First Trimester"]][["Follow-up ANC"]],
      "Second Trimester_Follow-up ANC" = a[["Second Trimester"]][["Follow-up ANC"]],
      "Third Trimester_Follow-up ANC" = a[["Third Trimester"]][["Follow-up ANC"]]
    ),
  "Informational Interventions" =     list(
      "First Trimester_First ANC" = b[["First Trimester"]][["First ANC"]],
      "Second Trimester_First ANC" = b[["Second Trimester"]][["First ANC"]],
      "Third Trimester_First ANC" = b[["Third Trimester"]][["First ANC"]],
      "First Trimester_Follow-up ANC" = b[["First Trimester"]][["Follow-up ANC"]],
      "Second Trimester_Follow-up ANC" = b[["Second Trimester"]][["Follow-up ANC"]],
      "Third Trimester_Follow-up ANC" = b[["Third Trimester"]][["Follow-up ANC"]]
    ),
  "Preventive measures" = list(
      "First Trimester_First ANC" = c[["First Trimester"]][["First ANC"]],
      "Second Trimester_First ANC" = c[["Second Trimester"]][["First ANC"]],
      "Third Trimester_First ANC" = c[["Third Trimester"]][["First ANC"]],
      "First Trimester_Follow-up ANC" = c[["First Trimester"]][["Follow-up ANC"]],
      "Second Trimester_Follow-up ANC" = c[["Second Trimester"]][["Follow-up ANC"]],
      "Third Trimester_Follow-up ANC" = c[["Third Trimester"]][["Follow-up ANC"]]
  ),
  "Continuity of care" = list(
      "First Trimester_First ANC" = d[["First Trimester"]][["First ANC"]],
      "Second Trimester_First ANC" = d[["Second Trimester"]][["First ANC"]],
      "Third Trimester_First ANC" = d[["Third Trimester"]][["First ANC"]],
      "First Trimester_Follow-up ANC" = d[["First Trimester"]][["Follow-up ANC"]],
      "Second Trimester_Follow-up ANC" = d[["Second Trimester"]][["Follow-up ANC"]],
      "Third Trimester_Follow-up ANC" = d[["Third Trimester"]][["Follow-up ANC"]]
  )
)
  

drc_baseline_split <- map_depth(drc_baseline_split, 2, function(x) {
  x$country <- "DRC (2015)"
  x
})


drc_endline_split <- map_depth(drc_endline_split, 2, function(x) {
  x$country <- "DRC (2021)"
  x
})


benin_split <- map_depth(benin, 2, function(x) {
  x$country <- "Benin (2010)"

  x <- rename(x, patients_pay_for_consumables = women_in_labour_pay)
  x
})

bfa_baseline_split <- map_depth(bfa_baseline_split, 2, function(x) {
  x$country <- "Burkina Faso (2013)"

  x$hcw_qualification <- case_when(
    x$hcw_qualification %in% "CHCW" ~ "Other",
    TRUE ~ x$hcw_qualification
  )
  x
})


bfa_endline_split <- map_depth(bfa_endline_split, 2, function(x) {
  x$country <- "Burkina Faso (2017)"

  x$hcw_qualification <- case_when(
    x$hcw_qualification %in% "CHCW" ~ "Other",
    TRUE ~ x$hcw_qualification
  )
  x
})


drc_b <- drc_baseline_split[["Maternal and fetal assessment"]][[1]]
drc_e <- drc_endline_split[["Maternal and fetal assessment"]][[1]]
benin <- benin_split[["Maternal and fetal assessment"]][[1]]
bfa_b <- bfa_baseline_split[["Maternal and fetal assessment"]][[1]]
bfa_e <- bfa_endline_split[["Maternal and fetal assessment"]][[1]]

common_cols <- Reduce(
  intersect,
  list(
    colnames(benin),
    colnames(drc_b), colnames(bfa_b),
    colnames(drc_e), colnames(bfa_e)
  )
)

strata <- names(drc_baseline_split[[1]])
names(strata) <- strata

multicountry_split <- map(
  names(intervention_types), function(intv_type) {
    
    map(strata, function(stratum) {

     cli_alert_info(glue("Processing {intv_type} - {stratum}"))
      
     benin <- pluck(benin_split, intv_type, stratum, .default = NULL)
     drc_b <- pluck(drc_baseline_split, intv_type, stratum, .default = NULL)
     drc_e <- pluck(drc_endline_split, intv_type, stratum, .default = NULL)
     bfa_b <- pluck(bfa_baseline_split, intv_type, stratum, .default = NULL)
     bfa_e <- pluck(bfa_endline_split, intv_type, stratum, .default = NULL)
      
     datasets <- compact(list(benin, drc_b, drc_e, bfa_b, bfa_e))

     if (length(datasets) == 0) {
       return(NULL) # or return empty tibble: return(tibble())
     }

     out <- bind_rows(
       map(datasets, ~ select(.x, all_of(common_cols)))
     )

      out$hcw_qualification <- factor(out$hcw_qualification)
      out$hcw_qualification <- relevel(
        out$hcw_qualification, ref = "Midwife"
      )
      out <- na.omit(out)
      
      out
    })

 })

  
names(multicountry_split) <- names(intervention_types)

saveRDS(multicountry_split, "multicountry_split.rds")

orderly_artefact(
  files = "multicountry_split.rds",
  description = "Combined cleaned datasets from all countries"
)
