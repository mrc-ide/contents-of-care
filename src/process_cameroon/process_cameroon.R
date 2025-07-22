library(orderly2)
library(dplyr)
library(foreign)
library(janitor)
library(readr)
library(skimr)

## https://microdata.worldbank.org/index.php/catalog/2047
indir <- "resources/cameroon/CMR_2012_RBFIE-FBL_v01_M_Stata8"
infile <- "f1_personnel_NoID.dta"
orderly_shared_resource(cmr_hcw.dta = paste(indir, infile, sep = "/"))
cmr_hcw1 <- read.dta("cmr_hcw.dta", convert.factors = FALSE)


infile <- "f1_principal_NoID.dta"
orderly_shared_resource(cmr_hcw2.dta = paste(indir, infile, sep = "/"))
cmr_hcw2 <- read.dta("cmr_hcw2.dta", convert.factors = FALSE)


infile <- "f3_obs_directe_consultation_NoID.dta"
orderly_shared_resource(cmr_dco.dta = paste(indir, infile, sep = "/"))
cmr_dco <- read.dta("cmr_dco.dta", convert.factors = FALSE)
