library(broom)
library(cli)
library(dplyr)
library(ggplot2)
library(ggpmisc)
library(glue)
library(glmnet)
library(orderly2)
library(purrr)
library(rsample)
library(scales)
library(tibble)
library(tidylog)
library(tidyr)

orderly_shared_resource("utils.R")
source("utils.R")

pars <- orderly_parameters(debug = TRUE)
if (pars[["debug"]]) iter <- 10 else iter <- 8000


orderly_dependency(
  "process_burkina_faso", "latest", files = c("bfa_baseline_split.rds")
)

bfa_split <- readRDS("bfa_baseline_split.rds")
cols_to_scale <- grep("scaled", names(bfa_split[[1]]), value = TRUE)
x <- bind_rows(bfa_split)

centers <- sapply(cols_to_scale, \(col) mean(x[[col]], na.rm = TRUE))
scales <- sapply(cols_to_scale, \(col) sd(x[[col]], na.rm = TRUE))

x <- mutate(x, across(all_of(cols_to_scale), \(v) as.numeric(scale(v))))

scaled_attrs <- data.frame(
  variable = names(centers), center   = unname(centers), scale = unname(scales)
)

saveRDS(scaled_attrs, file = "scaled_attributes.rds")
orderly_artefact(
  files = "scaled_attributes.rds",
  description = "scaled_attributes"
)

bfa_split <- split(x, list(x$first_anc, x$trimester), sep = "_")


orderly_resource("lm_burkina_faso_baseline_multilevel.R")
source("lm_burkina_faso_baseline_multilevel.R")

