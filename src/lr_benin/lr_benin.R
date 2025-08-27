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

pars <- orderly_parameters(debug = TRUE)

if (pars[["debug"]]) iter <- 100 else iter <- 4000



orderly_shared_resource(utils.R = "utils.R")
source("utils.R")

orderly_dependency(
  "process_benin_for_lr", "latest",
  "benin_dco_with_completeness_idx.rds"
)

benin_split <- readRDS("benin_dco_with_completeness_idx.rds")
names(benin_split[[1]]) <- c("First Trimester",
                             "Second Trimester",
                             "Third Trimester")
v1 <- to_snake_case(names(benin_split))
v2 <- to_snake_case(names(benin_split[[1]]))
v3 <- to_snake_case(names(benin_split[[1]][[1]]))
fit_names <- do.call(
  paste, c(expand.grid(v1, v2, v3, stringsAsFactors = FALSE), sep = "_")
)
counter <- 1
set.seed(42)

fits <- map_depth(benin_split, 3, function(x) {
  insuff_levels <- map(x, ~ length(unique(.)))
  insuff_levels <- insuff_levels[
    ! names(insuff_levels) %in% c("steps_taken", "steps_total")
  ]

  insuff_levels <- keep(insuff_levels,  ~ . < 2)
  cli_inform(
    "Removing variables with insufficient levels: {names(insuff_levels)}"
  )
  x <- select(x, -names(insuff_levels))
  fit <- brm(
    formula = bf(
      steps_taken | trials(steps_total) ~ . - health_zone + (1 | health_zone)
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
  saveRDS(fit, glue("{fit_names[counter]}.rds"))
  counter <<- counter + 1
  rm(fit)
  gc()
  NULL
})


saveRDS(fits, "benin_lr_fits.rds")

orderly_artefact(
  files = glue("{fit_names}.rds"),
  description = "benin_lr_fits.rds"
)
