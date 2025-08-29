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



pars <- orderly_parameters(survey = "baseline", debug = TRUE)

if (pars[["debug"]]) iter <- 1 else iter <- 4000



orderly_shared_resource(utils.R = "utils.R")
source("utils.R")

orderly_dependency(
  "process_drc_for_lr",
  "latest(parameter:survey == this:survey)",
  "drc_dco_with_completeness_idx.rds"
)

drc_split <- readRDS("drc_dco_with_completeness_idx.rds")


set.seed(42)
dir.create("fits", showWarnings = FALSE)

fits <- imap(drc_split, function(intv_type, intv_name) {
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
        steps_taken | trials(steps_total) ~ . - province + (1 | province)
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
      to_snake_case(glue("drc_lr_fit_{intv_name}_{stratum}.rds"))
    )
    saveRDS(fit, outfile)
    rm(fit)
    gc()
    NULL
  })
})

outfiles <- list.files(
  path = "fits/",
  pattern = "drc_lr_fit_.*rds", full.names = TRUE
)

orderly_artefact(
  files = outfiles,
  description = "drc_lr_fits.rds"
)
