library(brms)
library(dplyr)
library(orderly2)
library(performance)
library(purrr)
library(tidyr)
library(tidylog)


orderly_shared_resource("utils.R")
source("utils.R")


orderly_dependency("process_benin", "latest", files = c("benin_split.rds"))
orderly_dependency("process_drc", "latest", files = c("drc_baseline_split.rds"))
orderly_dependency(
  "process_burkina_faso_both", "latest", files = c("bfa_both_split.rds")
)

## Lots of renaming of variabeles to make them consistent
## across countries
benin_split <- readRDS("benin_split.rds")
drc_split <- readRDS("drc_baseline_split.rds")
bfa_split <- readRDS("bfa_both_split.rds")

drc_split <- map(drc_split, function(x) {
  x$country <- "drc"
  ##x <- rename(x, geography = province)

  x$facility_type <- ifelse(
    x$facility_type %in% "Hospital", "Secondary", x$facility_type
  )
  x$facility_type <- ifelse(
    x$facility_type %in% "Health center", "Primary", x$facility_type
  )
  x$hcw_qualification <- ifelse(
    x$hcw_qualification %in% "Midwife/Obstetrician", "Midwife",
    x$hcw_qualification
  )
  x
})


benin_split <- map(benin_split, function(x) {
  x$country <- "benin"
  ##x <- rename(x, geography = health_zone)
  x <- rename(x, hf_has_fetoscope = fetoscope)
  x <- rename(x, patients_pay_for_consumables = women_in_labour_pay)
  x$patients_pay_for_consumables <- ifelse(
    x$patients_pay_for_consumables %in% "oui", "Yes",
    ifelse(x$patients_pay_for_consumables %in% "non", "No",
           x$patients_pay_for_consumables)
  )
  x <- rename(x, facility_type = facility_level_mapping)
  if (! ("facility_status" %in% colnames(x))) {
    x$facility_status <- "Public"
  }
  x$facility_status <- ifelse(
    x$facility_status %in% "Public", "Public", "Not public"
  )
  x$hf_has_fetoscope <- case_when(
    x$hf_has_fetoscope %in% "oui" ~ "Yes",
    x$hf_has_fetoscope %in% "non" ~ "No",
    TRUE ~ x$hf_has_fetoscope
  )
  x
})

bfa_split <- map(bfa_split, function(x) {
  x$country <- "burkina_faso"
  x <- rename(x, facility_type = facility_level_mapping)
  x
})


multicountry_split <- map(
  names(drc_split), function(stratum) {
    drc <- drc_split[[stratum]]
    benin <- benin_split[[stratum]]
    bfa <- bfa_split[[stratum]]
    common_cols <- Reduce(
      intersect,
      list(colnames(benin), colnames(drc), colnames(bfa))
    )
    cli::cli_inform("Common columns for {stratum} are: {common_cols}")
    out <- bind_rows(
      select(benin, all_of(common_cols)),
      select(drc, all_of(common_cols)),
      select(bfa, all_of(common_cols))
    )
    out
  })


fits <- map(multicountry_split, function(x) {
  brm(
    formula = bf(log_consult_length ~ . - country + (1 | country)),
    data = x,
    family = gaussian(),
    drop_unused_levels = TRUE,
    chains = 4,
    cores = 1,
    iter = 4000,
    prior = prior_spec,
    control = list(adapt_delta = 0.99)
  )
})

saveRDS(fits, file = "multicountry_fits.rds")

orderly_artefact(files = "multicountry_fits.rds", description = "multicountry_fits")
