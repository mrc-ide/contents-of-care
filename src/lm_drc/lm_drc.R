library(broom)
library(cli)
library(dplyr)
library(glue)
library(glue)
library(janitor)
library(lubridate)
library(orderly2)
library(purrr)
library(scales)
library(tibble)
library(tidylog)
library(tidyr)

orderly_shared_resource("utils.R")
source("utils.R")

pars <- orderly_parameters(debug = TRUE)

if (pars[["debug"]]) iter <- 10 else iter <- 8000


orderly_dependency("process_drc", "latest", "drc_baseline_split.rds")
drc_baseline_split <- readRDS("drc_baseline_split.rds")

cols_to_scale <- grep("scaled", names(drc_baseline_split[[1]]), value = TRUE)
x <- bind_rows(drc_baseline_split)

centers <- sapply(cols_to_scale, \(col) mean(x[[col]], na.rm = TRUE))
scales <- sapply(cols_to_scale, \(col) sd(x[[col]], na.rm = TRUE))

x <- mutate(x, across(all_of(cols_to_scale), \(v) as.numeric(scale(v))))

scaled_attrs <- data.frame(
  variable = names(centers),
  center   = unname(centers),
  scale    = unname(scales)
)

saveRDS(scaled_attrs, file = "scaled_attributes.rds")
orderly_artefact(
  files = "scaled_attributes.rds",
  description = "scaled_attributes"
)

drc_baseline_split <- split(x, list(x$first_anc, x$trimester), sep = "_")

factor_vars <- c(
  "province",
  "facility_status",
  "facility_level_mapping",
  "milieu_of_residence",
  "patients_pay_for_consumables",
  "hf_has_fetoscope",
  "first_pregnancy",
  "first_anc",
  "trimester",
  "hcw_sex",
  "hcw_qualification",
  "consultation_language",
  "day_of_visit"
)

drc_baseline_split <- map(drc_baseline_split, function(x) {
  insuff_levels <- map_int(factor_vars, function(var) length(unique(x[[var]])))
  insuff_levels <- factor_vars[which(insuff_levels == 1)]
  ## Drop invariant variables

  cli_alert(
    "Dropping {length(insuff_levels)} invariant variables: {insuff_levels}"
  )
  x <- x[, !names(x) %in% insuff_levels]

  ## Also omit NAs if any
  x <- na.omit(x)
  cli_alert("Retaining {nrow(x)} rows")
  x
})

## Make sure to only retain non-empty splits
drc_baseline_split <- drc_baseline_split[map_lgl(drc_baseline_split, ~ nrow(.) > 0)]

set.seed(42)
orderly_resource("lm_drc_multilevel.R")
source("lm_drc_multilevel.R")


