library(orderly2)
library(quarto)

################################################################################
## Benin
################################################################################
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

orderly_dependency(
  "lm_benin", "latest",
  c("benin_dco_lasso_coefficients.png", "benin_dco_bayes_coefs.png")
)
################################################################################
## Burkina Faso
################################################################################
orderly_dependency(
  "eda_burkina_faso", "latest",
  c("bfa_consult_length_by_overall.png",
    "bfa_consult_length_by_region.png",
    "bfa_consult_length_by_first_anc.png",
    "bfa_consult_length_by_trimester.png"
  )
)
orderly_dependency(
  "lm_burkina_faso", "latest",
  c("bfa_dco_lasso_coefficients.png", "bfa_dco_bayes_coefs.png")
)

################################################################################
## Cameroon
################################################################################

################################################################################
## Central African Republic
################################################################################

################################################################################
## DRC
################################################################################
orderly_dependency(
  "eda_drc", "latest", c("drc_2015_consult_length_by_overall.png")
)

orderly_dependency(
  "lm_drc", "latest",
  c("drc_2015_dco_lasso_coefficients.png", "drc_2015_dco_bayes_coefs.png")
)


orderly_dependency(
  "eda_drc_midline", "latest", c("drc_midline_consult_length_by_overall.png")
)

orderly_dependency(
  "lm_drc_midline", "latest", c("drc_midline_dco_lasso_coefficients.png")
)

## Lesotho






## quarto_render("summary-presentation.qmd")
system("pdflatex main.tex")
##system("biber main")
system("pdflatex main.tex")
