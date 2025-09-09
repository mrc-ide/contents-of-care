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

##pars <- orderly_parameters(debug = TRUE, all_countries = TRUE)

if (pars[["debug"]]) iter <- 10 else iter <- 8000


orderly_shared_resource(utils.R = "utils.R")
source("utils.R")

orderly_dependency(
  "process_multicountry", "latest",
  files = c("multicountry_split.rds", "drc_and_bfa_split.rds")
)

multicountry_split <- readRDS("multicountry_split.rds")
drc_and_bfa_split <- readRDS("drc_and_bfa_split.rds")

if (pars[["all_countries"]]) {
  split_to_use <- multicountry_split
} else {
  split_to_use <- drc_and_bfa_split
}

dir.create("fits", showWarnings = FALSE)
outfiles <- glue("fits/{names(multicountry_split)}_multicountry_fit.rds")

fits <- walk2(split_to_use, outfiles, function(x, outfile) {
  x <- select(x, -first_anc, -trimester)
  fit <- brm(
    formula = bf(consult_length ~ . - country + (1 | country)),
    data = x,
    family = lognormal(),
    drop_unused_levels = TRUE,
    chains = 4,
    cores = 4,
    iter = iter,
    prior = prior_spec,
    control = list(adapt_delta = 0.99)
  )
 
  saveRDS(fit, file = outfile)
  rm(fit)
  
})


orderly_artefact(files = outfiles, description = "multicountry_fits")
