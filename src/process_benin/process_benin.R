library(dplyr)
library(foreign)
library(lubridate)
library(orderly2)
## Data dictionary available at
## https://microdata.worldbank.org/index.php/catalog/2176/data-dictionary
## Lemière, C., & de Walque, D. (2014). Health Results-Based Financing Impact Evaluation Survey 2010-2011, Baseline [Data set]. World Bank, Development Data Group. https://doi.org/10.48529/QKMG-R734
indir <- "resources/benin/BEN_2010_HRBF_v01_M_v01_A_PUF_Stata8/"
infile <- "benhrbf2_m.dta" 
benin <- orderly_shared_resource(benin.dta = paste0(indir, infile))
benin <- read.dta("benin.dta")
## colnames
benin <- rename(
  benin,
  hour_start = m0_h1d,
  min_start = m0_m1d,
  hour_end = m0_h1f,
  min_end = m0_m1f,
  trimester = m0_01,
  first_anc = m0_02,
  pregnancy_stage = m3_4
)

start <- hm(paste(benin$hour_start, benin$min_start, sep = ":")) 
end <- hm(paste(benin$hour_end, benin$min_end, sep = ":"))

benin$consult_length <- time_length(end - start, unit = "minute")

## m0_03 to m3_6 describe the individual steps in the examnination
## get number of ouis and nons for each row
## First recode oui as 1 and non as 0
f <- function(x) {
  x <- tolower(x)
  case_when(
    x == "oui" ~ 1,
    x == "non" ~ 0,
    TRUE ~ NA
  )
}
## Answers to pregnancy_stage are at most 36 weeks, and more than 36 weeks
benin <- relocate(benin, pregnancy_stage, .after = m3_6)
benin <- mutate(benin, across(m0_03:m3_6, f))
benin <- rowwise(benin) |> mutate(nsteps = sum(c_across(m0_03:m3_6), na.rm = TRUE))

saveRDS(benin, "benin.rds")
