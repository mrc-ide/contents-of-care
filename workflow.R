library(orderly2)

orderly_run("process_benin")
orderly_run("lm_benin")


orderly_run("process_drc")
orderly_run("lm_drc")


orderly_run("process_burkina_faso")
orderly_run("lm_burkina_faso_baseline")

orderly_run("process_burkina_faso_endline")
orderly_run("lm_burkina_faso_endline")




## Lab notebook
## 20.08.2025
## orderly_run("process_benin") 'ae2b3a1c4ee62d45b5da8509a6d746dc'
## orderly_run("lm_benin") 6b72b2d6c802924331a59dd196ff85b7'

## id <- task_create_expr(orderly2::orderly_run("process_drc")) ## 1c5827f81b93d054f728a9e2fd1cc476
## id <- task_create_expr(orderly2::orderly_run("lm_drc")) ## '59755115f18647ea73a0ce3ac1667535'
