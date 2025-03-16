library(orderly2)
library(quarto)

orderly_dependency(
  "eda_benin", "latest", "benin_consult_length_by_facility.png"
)
quarto_render("summary.qmd")
