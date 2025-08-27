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

orderly_dependency(
  "process_burkina_faso", "latest",
  files = c("bfa_baseline_split.rds")
)

bfa_split <- readRDS("bfa_baseline_split.rds")

orderly_resource("lm_burkina_faso_baseline_multilevel.R")
source("lm_burkina_faso_baseline_multilevel.R")

