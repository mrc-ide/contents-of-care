library(orderly2)

orderly_run("process_benin")
orderly_run("lm_benin")


orderly_run("process_drc")
orderly_run("lm_drc")

orderly_run("process_drc_endline")
orderly_run("lm_drc_endline")


orderly_run("process_burkina_faso")
orderly_run("lm_burkina_faso_baseline")

orderly_run("process_burkina_faso_endline")
orderly_run("lm_burkina_faso_endline")

orderly_run("process_benin_for_lr")

orderly_run("process_bfa_for_lr", parameters = list(survey = "baseline"))
orderly_run("process_bfa_for_lr", parameters = list(survey = "endline"))

orderly_run("process_drc_for_lr", parameters = list(survey = "baseline"))
orderly_run("process_drc_for_lr", parameters = list(survey = "endline"))

orderly_run("process_multicountry_for_lr")

## Lab notebook
## 20.08.2025
## orderly_run("process_benin") 'ae2b3a1c4ee62d45b5da8509a6d746dc'
## orderly_run("lm_benin") 6b72b2d6c802924331a59dd196ff85b7'

## id <- task_create_expr(orderly2::orderly_run("process_drc")) ## 1c5827f81b93d054f728a9e2fd1cc476
## id <- task_create_expr(orderly2::orderly_run("lm_drc")) ## '59755115f18647ea73a0ce3ac1667535'

## 25.08.2025
## task_create_expr(orderly2::orderly_run("lm_burkina_faso_baseline")) ## 99107b8930b3f4704be481a13cc4b9ea
## task_create_expr(orderly2::orderly_run("lm_drc")) ## fd9d22a6e6f5291a33e9a22e1a796caa

## 27.08.2025
## task_create_expr(orderly2::orderly_run("lm_drc_endline")) ## "337afedf3a5c64c8e96a4fcd16b71363"
## task_create_expr(orderly2::orderly_run("lm_multicountry", parameters = list(debug = FALSE))) ## "fe9fcdc08cd83000173a247a22265bd5"
## task_create_expr(orderly2::orderly_run("lr_benin", parameters = list(debug = FALSE))) ## "7a2e39f42036a6ee8d3d7fca9328b409"

## 29.08.2025
## task_create_expr(orderly2::orderly_run("lr_multicountry", parameters = list(debug = FALSE))) ## "5bb42c3085058882a53d271d28431046"
