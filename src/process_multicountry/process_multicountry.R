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

orderly_dependency("process_benin", "latest", files = c("benin_split.rds"))

orderly_dependency("process_drc", "latest", files = c("drc_baseline_split.rds"))
orderly_dependency(
  "process_drc_endline", "latest",
  files = c("drc_endline_split.rds")
)

orderly_dependency(
  "process_burkina_faso", "latest", files = c("bfa_baseline_split.rds")
)
orderly_dependency(
  "process_burkina_faso_endline", "latest",
  files = c("bfa_endline_split.rds")
)


benin_split <- readRDS("benin_split.rds")

drc_baseline_split <- readRDS("drc_baseline_split.rds")
drc_endline_split <- readRDS("drc_endline_split.rds")

bfa_baseline_split <- readRDS("bfa_baseline_split.rds")
bfa_endline_split <- readRDS("bfa_endline_split.rds")

drc_baseline_split <- map(drc_baseline_split, function(x) {
  x$country <- "DRC (2015)"
  x <- rename(x, "consult_length" = "consult_length_calc")
  x
})


drc_endline_split <- map(drc_endline_split, function(x) {
  x$country <- "DRC (2021)"
  x <- rename(x, "consult_length" = "consult_length_calc")
  x
})


benin_split <- map(benin_split, function(x) {
  x$country <- "Benin (2010)"
  ##x <- rename(x, geography = health_zone)
  x <- rename(x, patients_pay_for_consumables = women_in_labour_pay)

  x
})

bfa_baseline_split <- map(bfa_baseline_split, function(x) {
  x$country <- "Burkina Faso (2013)"

  x$hcw_qualification <- case_when(
    x$hcw_qualification %in% "CHCW" ~ "Other",
    TRUE ~ x$hcw_qualification
  )
  x
})


bfa_endline_split <- map(bfa_endline_split, function(x) {
  x$country <- "Burkina Faso (2017)"

  x$hcw_qualification <- case_when(
    x$hcw_qualification %in% "CHCW" ~ "Other",
    TRUE ~ x$hcw_qualification
  )
  x
})


multicountry_split <- map(
  names(bfa_baseline_split), function(stratum) {

    drc_b <- drc_baseline_split[[stratum]]
    drc_e <- drc_endline_split[[stratum]]
    benin <- benin_split[[stratum]]
    bfa_b <- bfa_baseline_split[[stratum]]
    bfa_e <- bfa_endline_split[[stratum]]

    common_cols <- Reduce(
      intersect,
      list(
        colnames(benin),
        colnames(drc_b), colnames(bfa_b),
        colnames(drc_e), colnames(bfa_e)
      )
    )
    cli_inform("Common columns for {stratum} are: {common_cols}")
    out <- bind_rows(
      select(benin, all_of(common_cols)),
      select(drc_b, all_of(common_cols)),
      select(drc_e, all_of(common_cols)),
      select(bfa_b, all_of(common_cols)),
      select(bfa_e, all_of(common_cols))
    )
    out$hcw_qualification <- factor(out$hcw_qualification)
    out$hcw_qualification <- relevel(
      out$hcw_qualification,
      ref = "Midwife"
    )

    out$facility_level_mapping <- factor(out$facility_level_mapping)
    out$facility_level_mapping <- relevel(
      out$facility_level_mapping,
      ref = "Primary"
    )

    out <- na.omit(out)
    
    out
  }
)

names(multicountry_split) <- names(bfa_baseline_split)
saveRDS(multicountry_split, "multicountry_split.rds")

orderly_artefact(
  files = "multicountry_split.rds",
  description = "Combined cleaned datasets from all countries"
)



multicountry_split <- map(
  names(bfa_baseline_split), function(stratum) {

    drc_b <- drc_baseline_split[[stratum]]
    bfa_b <- bfa_baseline_split[[stratum]]
    bfa_e <- bfa_endline_split[[stratum]]

    common_cols <- Reduce(
      intersect,
      list(colnames(drc_b), colnames(bfa_b), colnames(bfa_e))
    )
    cli_inform("Common columns for {stratum} are: {common_cols}")
    out <- bind_rows(
      select(drc_b, all_of(common_cols)),
      select(bfa_b, all_of(common_cols)),
      select(bfa_e, all_of(common_cols))
    )
    out$hcw_qualification <- factor(out$hcw_qualification)
    out$hcw_qualification <- relevel(
      out$hcw_qualification,
      ref = "Midwife"
    )

    out$facility_level_mapping <- factor(out$facility_level_mapping)
    out$facility_level_mapping <- relevel(
      out$facility_level_mapping,
      ref = "Primary"
    )
    out <- na.omit(out)
    
    out
  }
)

names(multicountry_split) <- names(bfa_baseline_split)
saveRDS(multicountry_split, "drc_and_bfa_split.rds")

orderly_artefact(
  files = "drc_and_bfa_split.rds",
  description = "Combined cleaned datasets from DRC and Burkina Faso only"
)
