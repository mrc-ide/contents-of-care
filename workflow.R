library(orderly2)

orderly_run("process_benin")
orderly_run("lm_benin")


orderly_run("process_drc")
orderly_run("lm_drc", parameters = list(debug = TRUE))

orderly_run("process_drc_endline")
orderly_run("lm_drc_endline")


orderly_run("process_burkina_faso")
orderly_run("lm_burkina_faso_baseline", parameters = list(debug = FALSE))

orderly_run("process_burkina_faso_endline")
orderly_run("lm_burkina_faso_endline", parameters = list(debug = FALSE))

orderly_run("process_benin_for_lr")

orderly_run("process_bfa_for_lr", parameters = list(survey = "baseline"))
orderly_run("process_bfa_for_lr", parameters = list(survey = "endline"))

orderly_run("process_drc_for_lr", parameters = list(survey = "baseline"))
orderly_run("process_drc_for_lr", parameters = list(survey = "endline"))

orderly_run("process_multicountry_for_lr")

