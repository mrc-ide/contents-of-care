library(broom)
library(dplyr)
library(ggplot2)
library(ggpmisc)
library(ggpubr)
library(glue)
library(knitr)
library(orderly2)
library(purrr)
library(scales)
library(snakecase)
library(table1)

orderly_shared_resource("utils.R")
source("utils.R")
## Notes:
## 33 rows with consult_length > 75 minutes i.e. 97.5th percentile
## I have checked the times, and there are no obvious data entry
## errors; so likely these are real outliers.
orderly_dependency(
  "process_drc", "latest", files = c("drc_dco_2015.rds")
)

drc_2015_dco <- readRDS("drc_dco_2015.rds")
outfile_prefix <- "drc_2015_consult_length_by"

out <- summary(drc_2015_dco$consult_length_calc) |>
  round(2) |>
  tidy()

p1 <- gghistogram(
  drc_2015_dco,
  x = "consult_length_calc", add = "mean", rug = TRUE, add_density = TRUE,
  binwidth = 5
) + geom_table_npc(data = out, label = list(out), npcx = 1, npcy = 0.7) +
  labs(title = "Consultation length (minutes) in DRC") +
  theme_pubr() +
  xlab("Consultation length (minutes)")

outfile <- glue("{outfile_prefix}_overall")
ggsave_manuscript(outfile, p1)
orderly_artefact(
  description = "Consultation length in DRC", files = glue("{outfile}.png")
)



cat_covariates <- c(
  "province", "district", ##"health_zone" (exclude because too many levels),
  "facility_status", "milieu_of_residence",
  "num_prev_anc_visits", "trimester",
  "first_pregnancy", "first_anc", 
  "hcw_sex", "hcw_qualification", "consultation_language"
)


walk(cat_covariates, function(x) {
  f <- as.formula((paste("~ consult_length_calc |", x)))
  small_df <- drc_2015_dco[!is.na(drc_2015_dco[[x]]), ]
  small_df[[x]] <- factor(small_df[[x]])
  label(small_df$consult_length_calc) <- to_title_case(x)
  tab <- table1(
    f, data = small_df, extra.col = list("p-value" = render_p_value_cat),
    overall = FALSE, transpose = FALSE, render.cont = my.render.cont,
    caption = to_title_case(x),
  )
  kable(as.data.frame(tab), format = "latex", booktabs = TRUE) |>
    writeLines(glue("drc_2015_dco_{x}.tex"))
 })

outfiles <- glue("drc_2015_dco_{cat_covariates}.tex")

orderly_artefact(
  description = "Consultation length by categorical covariates",
  files = outfiles
)

