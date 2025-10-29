library(bayestestR)
library(brms)
library(cli)
library(dplyr)
library(ggplot2)
library(ggpmisc)
library(glue)
library(orderly2)
library(performance)
library(posterior)
library(purrr)
library(snakecase)
library(stringr)
library(tibble)
library(tidyr)


pars <- orderly_parameters(survey = "baseline")

orderly_shared_resource(utils.R = "utils.R")
source("utils.R")

orderly_dependency(
  "lr_drc",
  "latest(parameter:survey == this:survey)",
  "fits/"
)

orderly_shared_resource(utils.R = "utils.R")
source("utils.R")



infiles <- list.files("fits", pattern = "*rds")
fits <- map(infiles, function(infile) readRDS(file.path("fits", infile)))
infiles <- gsub("_rds", "", infiles) 
infiles <- gsub("drc_lr_fit_", "", infiles)
names(fits) <- infiles

all_hdi <- map_dfr(fits, hdi, .id = "datacut")
all_map <- map_dfr(fits, point_estimate, .id = "datacut")
all_pd <- map_dfr(fits, probability_of_direction, .id = "datacut")


## intervention_names
intv_names <- to_snake_case(names(intervention_types))
trimester_names <- paste(c("first", "second", "third"), "trimester", sep = "_")
anc_names <- c("first_anc", "follow_up_anc")



pattern <- sprintf(
  "(%s)_(%s)_(%s)",
  paste(intv_names, collapse = "|"),
  paste(trimester_names, collapse = "|"),
  paste(anc_names, collapse = "|")
)

## throwaway function to split datacut into components
f <- function(x) {
  extract(
    x, datacut,
    into = c("intervention", "trimester", "first_anc"),
    regex = pattern)
}
all_hdi <- f(all_hdi)
all_map <- f(all_map)
all_pd <- f(all_pd)

saveRDS(all_hdi, file = "drc_dco_bayes_hdi.rds")
saveRDS(all_map, file = "drc_dco_bayes_map.rds")
saveRDS(all_pd, file = "drc_dco_bayes_pd.rds")

orderly_artefact(
  files = c(
    "drc_dco_bayes_hdi.rds",
    "drc_dco_bayes_map.rds",
    "drc_dco_bayes_pd.rds"
  ),
  description = "HDI, MAP, and PD for DRC  model fits"
)
