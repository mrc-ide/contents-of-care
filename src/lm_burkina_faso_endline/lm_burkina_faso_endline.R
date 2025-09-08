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
  "process_burkina_faso_endline", "latest",
  files = c("bfa_endline_split.rds")
)

bfa_split <- readRDS("bfa_endline_split.rds")

orderly_resource("lm_burkina_faso_endline_multilevel.R")
source("lm_burkina_faso_endline_multilevel.R")
