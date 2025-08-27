library(broom)
library(brms)
library(cli)
library(dplyr)
library(ggplot2)
library(ggpmisc)
library(glmnet)
library(glue)
library(orderly2)
library(purrr)
library(rsample)
library(scales)
library(snakecase)
library(tibble)
library(tidylog)
library(tidyr)




orderly_shared_resource(utils.R = "utils.R")
source("utils.R")

orderly_dependency("process_benin", "latest", files = c("benin_split.rds"))
benin_split <- readRDS("benin_split.rds")

factor_vars <- c(
  "milieu_of_residence", "health_zone", "facility_level_mapping",
  "facility_status_mapping", "pregnant_women_private_space",
  "hf_has_fetoscope", "women_in_labour_pay",
  "hcw_qualification", "first_anc", "trimester"
)


benin_split <- map(benin_split, function(x) {
  insuff_levels <- map_int(factor_vars, function(var) length(unique(x[[var]])))
  insuff_levels <- factor_vars[which(insuff_levels == 1)]
  ## Drop invariant variables
  cli_alert(
    "Dropping {length(insuff_levels)} invariant variables: {insuff_levels}"
  )
  x <- x[, !names(x) %in% insuff_levels]
  x
})

benin_split <- map(benin_split, function(x) na.omit(x))



orderly_resource("lm_benin_multilevel.R")
source("lm_benin_multilevel.R")

## orderly_resource("lm_benin_lasso.R")
## source("lm_benin_lasso.R")


