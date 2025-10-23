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

cols_to_scale <-
  grep("scaled", names(multicountry_split[[1]][[1]]), value = TRUE)

x <- map_dfr(multicountry_split, bind_rows, .id = "intervention")

centers <- sapply(cols_to_scale, \(col) mean(x[[col]], na.rm = TRUE))
scales <- sapply(cols_to_scale, \(col) sd(x[[col]], na.rm = TRUE))

x <- mutate(x, across(all_of(cols_to_scale), \(v) as.numeric(scale(v))))

scaled_attrs <- data.frame(
  variable = names(centers), center = unname(centers), scale = unname(scales)
)

saveRDS(scaled_attrs, file = "scaled_attributes.rds")
orderly_artefact(
  files = "scaled_attributes.rds",
  description = "scaled_attributes"
)

multicountry_split <- split(x, x$intervention) |>
  map(function(y) split(y, list(y$trimester, y$first_anc), sep = "_"))



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
