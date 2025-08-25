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

set.seed(42)

orderly_shared_resource("utils.R")
source("utils.R")

orderly_dependency(
  "process_burkina_faso_both", "latest",
  files = c("bfa_both_split.rds")
)

bfa_split <- readRDS("bfa_both_split.rds")

orderly_resource("lm_burkina_faso_multilevel.R")
source("lm_burkina_faso_multilevel.R")

