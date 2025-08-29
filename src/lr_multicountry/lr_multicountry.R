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

pars <- orderly_parameters(debug = TRUE)

if (pars[["debug"]]) iter <- 1 else iter <- 4000


orderly_shared_resource(utils.R = "utils.R")
source("utils.R")

orderly_dependency(
  "process_multicountry_for_lr", "latest",
  files = c("multicountry_split.rds")
)

multicountry_split <- readRDS("multicountry_split.rds")

dir.create("fits", showWarnings = FALSE)
outfiles <- glue("fits/{names(multicountry_split)}_multicountry_fit.rds")

set.seed(42)
dir.create("fits", showWarnings = FALSE)

fits <- imap(multicountry_split, function(intv_type, intv_name) {
  imap(intv_type, function(x, stratum) {
    
    insuff_levels <- map(x, ~ length(unique(.)))
    insuff_levels <- insuff_levels[
      !names(insuff_levels) %in% c("steps_taken", "steps_total")
    ]

    insuff_levels <- keep(insuff_levels, ~ . < 2)
    cli_inform(
      "Removing variables with insufficient levels: {names(insuff_levels)}"
    )
    x <- select(x, -names(insuff_levels))
    fit <- brm(
      formula = bf(
        steps_taken | trials(steps_total) ~ . - country + (1 | country)
      ),
      data = x,
      family = binomial(link = "logit"),
      drop_unused_levels = TRUE,
      chains = 4,
      cores = 4,
      iter = iter,
      prior = prior_spec,
      control = list(adapt_delta = 0.99)
    )
    outfile <- paste0(
      "fits/",
      to_snake_case(glue("multicountry_lr_fit_{intv_name}_{stratum}.rds"))
    )
    saveRDS(fit, outfile)
    rm(fit)
    gc()
    NULL
  })
})

outfiles <- list.files(
  path = "fits/",
  pattern = "multicountry_lr_fit_.*rds", full.names = TRUE
)

orderly_artefact(
  files = outfiles,
  description = "multicountry_lr_fits.rds"
)
