library(broom)
library(brms)
library(cli)
library(dplyr)
library(glue)
library(orderly2)
library(purrr)
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

cols_to_scale <- grep(
  "scaled", names(benin_split[[1]][[1]][[1]]),
  value = TRUE
)
x <- map_dfr(
  benin_split, function(y) map_dfr(y, bind_rows),
  .id = "intervention"
)

centers <- sapply(cols_to_scale, \(col) mean(x[[col]], na.rm = TRUE))
scales <- sapply(cols_to_scale, \(col) sd(x[[col]], na.rm = TRUE))

x <- mutate(x, across(all_of(cols_to_scale), \(v) as.numeric(scale(v))))

scaled_attrs <- data.frame(
  variable = names(centers),
  center   = unname(centers),
  scale    = unname(scales)
)

tmp <- split(x, x$intervention) |>
  map(function(y) split(y, y$trimester)) 

benin_split <-
  map_depth(tmp, 2, function(z) split(z, z$first_anc))


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


orderly_artefact(
  files = glue("{fit_names}.rds"),
  description = "benin_lr_fits.rds"
)
