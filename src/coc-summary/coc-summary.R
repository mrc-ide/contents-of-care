library(orderly2)
library(quarto)

orderly_dependency(
  "eda_benin", "latest",
  c(
    "benin_consult_length_by_overall.png",
    "benin_consult_length_by_facility.png",
    "benin_consult_length_by_healthzone.png",
    "benin_consult_length_by_first_anc.png",
    "benin_consult_length_by_trimester.png"
  )
)

orderly_dependency("lm_benin", "latest", c("benin_dco_lasso_coefficients.png"))

orderly_dependency("lm_burkina_faso", "latest", c("bfa_lasso_bootstrapped.png"))


orderly_dependency(
  "eda_burkina_faso", "latest",
  c(
    "bfa_consult_length_by_overall.png",
    "bfa_consult_length_by_region.png",
    "bfa_consult_length_by_first_anc.png",
    "bfa_consult_length_by_trimester.png"
  )
)


orderly_dependency(
  "eda_drc", "latest", c("drc_2015_consult_length_by_overall.png")
)

orderly_dependency(
  "lm_drc", "latest",c("drc_2015_dco_lasso_coefficients.png")
)

##quarto_render("summary-presentation.qmd")
system("pdflatex main.tex")
