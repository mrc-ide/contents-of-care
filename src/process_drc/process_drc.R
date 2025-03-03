library(orderly2)
library(readr)


### Baseline survey data
### Midline survey data
indir <- "resources/drc/midline-survey/COD_2018_HRBFIE-FML_v01_M_CSV" 
infile <- "f3_do_anc.csv"

orderly_shared_resource(drc_midline.csv = paste(indir, infile, sep = "/"))
drc_midline <- read_csv("drc_midline.csv")

### Endline survey data
