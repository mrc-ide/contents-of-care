library(brms)
library(dplyr)
library(orderly2)
library(performance)
library(purrr)
library(tidyr)
library(tidylog)

orderly_dependency("lm_benin", "latest", files = c("benin_split.rds"))
orderly_dependency("lm_drc", "latest", files = c("drc_baseline_split.rds"))

## Lots of renaming of variabeles to make them conssistent
## across countries
benin_split <- readRDS("benin_split.rds")
drc_split <- readRDS("drc_baseline_split.rds")

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
  x <- rename(x, milieu_of_residence = m0_milieu)
  x$facility_type <- case_when(
    x$facility_type %in% "former_communal_health_center" ~ "Primary",
    x$facility_type %in% "former_district_health_center" ~ "Secondary",
    x$facility_type %in% "zone_hospital" ~ "Secondary",
    x$facility_type %in% "faith_based_private_facility" ~ "Unclear",
    TRUE ~ x$facility_type
  )
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

## Names are not aligned; make sure we are stacking the right
## strata
common_cols <- intersect(
  colnames(benin_split[["non_First Trimester"]]),
  colnames(drc_split[["no_First Trimester"]])
)

multicountry_split <- list(
  no_first_trimester = bind_rows(
    select(benin_split[["non_First Trimester"]], all_of(common_cols)),
    select(drc_split[["no_First Trimester"]], all_of(common_cols))
  )
)



fits <- map(multicountry_split, function(x) {
  brm(
    formula = bf(log_consult_length ~ . + (1 | country)),
    data = x,
    family = gaussian(),
    drop_unused_levels = TRUE,
    chains = 4,
    cores = 4,
    iter = 4000,
    priors <- prior(normal(0, 1), class = "b"),
    control = list(adapt_delta = 0.99)
  )
})
