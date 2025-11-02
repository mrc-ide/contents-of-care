library(brms)
library(cli)
library(dplyr)
library(glue)
library(orderly2)
library(performance)
library(purrr)
library(snakecase)
library(tidyr)


pars <- orderly_parameters(debug = TRUE, all_countries = TRUE)

if (pars[["debug"]]) iter <- 10 else iter <- 8000


orderly_shared_resource(utils.R = "utils.R")
source("utils.R")

orderly_dependency(
  "process_multicountry", "latest",
  files = c("multicountry_split.rds", "minus_drc_endline_split.rds")
)

if (pars[["all_countries"]]) {
  split_to_use <- readRDS("multicountry_split.rds")
} else {
  split_to_use <- readRDS("minus_drc_endline_split.rds")
}

dir.create("fits", showWarnings = FALSE)
outfiles <- glue("fits/{names(multicountry_split)}_multicountry_fit.rds")

## Scale here; we did a fake scaling in processing individual datasets
## We want to be able to scale across all countries as that makes more sense
## So we will do the real scaling here

x <- bind_rows(split_to_use)
## Remove rows with 0 patients seen
filter(x, patients_per_staff_per_year_scaled > 1)
## First set all values greater than threshold to threshold
threshold <- quantile(x$consult_length, 0.99, na.rm = TRUE)
x$consult_length <- pmin(x$consult_length, threshold)
## For the full dataset
## This involves changing 82 values. Total number of rows is 8128
## 3 in First, 39 in Second, 40 in Third trimester
## 45 first ANC, 37 second ANC
## Benin:4, BFA 2013 17, BFA 2017:15, DRC 2015:37, DRC 2021:9

cols_to_scale <- grep("scaled", names(split_to_use[[1]]), value = TRUE)
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

split_to_use <- split(x, list(x$first_anc, x$trimester), sep = "_")

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
