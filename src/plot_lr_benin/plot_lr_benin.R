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


orderly_dependency("lr_benin", "latest", "./")

source("utils.R")

probability_of_direction <- function(fit) {
  fixed_names <- rownames(brms::fixef(fit))

  # Create hypothesis strings like "var = 0"
  hypotheses <- paste0(fixed_names, " > 0")

  # Run brms hypothesis test
  out <- brms::hypothesis(fit, hypotheses)
  out$hypothesis$rowname <- fixed_names
  out$hypothesis
}


infiles <- list.files(".", pattern = "*anc.rds")
benin_lr_fits <- map(infiles, readRDS)
names(benin_lr_fits) <- gsub("\\.rds$", "", infiles)

all_hdi <- map_dfr(benin_lr_fits, hdi, .id = "datacut")
all_map <- map_dfr(benin_lr_fits, point_estimate, .id = "datacut")
all_pd <- map_dfr(benin_lr_fits, probability_of_direction, .id = "datacut")

fit_files <- list.files(path = ".", pattern = "*.rds")
fits <- map(fit_files, readRDS)
names(fits) <- gsub("\\.rds$", "", fit_files)

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

saveRDS(all_hdi, file = "benin_dco_bayes_hdi.rds")
saveRDS(all_map, file = "benin_dco_bayes_map.rds")
saveRDS(all_pd, file = "benin_dco_bayes_pd.rds")

orderly_artefact(
  files = c(
    "benin_dco_bayes_hdi.rds",
    "benin_dco_bayes_map.rds",
    "benin_dco_bayes_pd.rds"
  ),
  description = "HDI, MAP, and PD for Benin model fits"
)
