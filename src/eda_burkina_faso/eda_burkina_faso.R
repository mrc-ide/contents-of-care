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

orderly_dependency(
  "process_burkina_faso", "latest",
  files = c("bfa_baseline_dco.rds")
)
bfa_baseline_dco <- readRDS("bfa_baseline_dco.rds")
outfile_prefix <- "bfa_consult_length_by"

out <- summary(bfa_baseline_dco$consult_length) |>
  round(2) |>
  tidy()

p1 <- gghistogram(
  bfa_baseline_dco,
  x = "consult_length", add = "mean", rug = TRUE, add_density = TRUE,
  binwidth = 5
) + geom_table_npc(data = out, label = list(out), npcx = 1, npcy = 0.7) +
  labs(title = "Consultation length (minutes) in Burkina Faso") +
  theme_pubr() +
  xlab("Consultation length (minutes)")

outfile <- glue("{outfile_prefix}_overall.png")
ggexport(plotlist = list(p1), filename = outfile)
orderly_artefact(description = "Consultation length in Benin", files = outfile)

p1 <- ggsummarystats(
  data = bfa_baseline_dco, x = "REGION.x", y = "consult_length",
  ggfunc = ggboxplot, add = "jitter"
)

p1$main.plot <- p1$main.plot +
  stat_compare_means(
    label = "p.signif", ref.group = ".all.", label.y.npc = 0.75
  ) +
  ylab("Consultation length (minutes)") +
  theme_pubr() +
  xlab("Region")

outfile <- glue("{outfile_prefix}_region.png")
ggexport(plotlist = list(p1), filename = outfile)
orderly_artefact(
  description = "Consultation length by region", files = outfile
)



p1 <- ggsummarystats(
  data = bfa_baseline_dco, x = "num_prev_anc_visits", y = "consult_length",
  ggfunc = ggboxplot, add = "jitter"
)

p1$main.plot <- p1$main.plot +
  stat_compare_means(
    label = "p.signif", ref.group = ".all.", label.y.npc = 0.75
  ) +
  ylab("Consultation length (minutes)") +
  theme_pubr() +
  xlab("Number of previous ANC visits")

outfile <- glue("{outfile_prefix}_prev_anc_visits.png")
ggexport(plotlist = list(p1), filename = outfile)
orderly_artefact(
  description = "Consultation length by region", files = outfile
)


p1 <- ggsummarystats(
  data = bfa_baseline_dco, x = "first_pregnancy", y = "consult_length",
  ggfunc = ggboxplot, add = "jitter"
)

p1$main.plot <- p1$main.plot +
  stat_compare_means(
    label = "p.signif", ref.group = ".all.", label.y.npc = 0.75
  ) +
  ylab("Consultation length (minutes)") +
  theme_pubr() +
  xlab("First pregnancy")

outfile <- glue("{outfile_prefix}_first_pregnancy.png")
ggexport(plotlist = list(p1), filename = outfile)
orderly_artefact(
  description = "Consultation length by region", files = outfile
)

p1 <- ggsummarystats(
  data = bfa_baseline_dco, x = "first_anc", y = "consult_length",
  ggfunc = ggboxplot, add = "jitter"
)

p1$main.plot <- p1$main.plot +
  stat_compare_means(
    label = "p.signif", label.y.npc = 0.75,
    comparisons = list(c("0", "1"))
  ) +
  ylab("Consultation length (minutes)") +
  theme_pubr() +
  xlab("First ANC")

outfile <- glue("{outfile_prefix}_first_anc.png")
ggexport(plotlist = list(p1), filename = outfile)
orderly_artefact(
  description = "Consultation length by region", files = outfile
)




p1 <- ggsummarystats(
  data = bfa_baseline_dco, x = "facility_type", y = "consult_length",
  ggfunc = ggboxplot, add = "jitter"
)

p1$main.plot <- p1$main.plot +
  stat_compare_means(
    label = "p.signif", label.y.npc = 0.75,
    ref.group = ".all."
  ) +
  ylab("Consultation length (minutes)") +
  theme_pubr() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.title.x = element_blank()
  ) 

outfile <- glue("{outfile_prefix}_facility_type.png")
ggexport(plotlist = list(p1), filename = outfile)
orderly_artefact(
  description = "Consultation length by region", files = outfile
)





p1 <- ggsummarystats(
  data = bfa_baseline_dco, x = "hcw_sex", y = "consult_length",
  ggfunc = ggboxplot, add = "jitter", na.rm = TRUE
)

p1$main.plot <- p1$main.plot +
  stat_compare_means(
    label = "p.signif", ref.group = ".all.", label.y.npc = 0.75
  ) +
  ylab("Consultation length (minutes)") +
  theme_pubr() +
  xlab("HCW Sex")

outfile <- glue("{outfile_prefix}_hcw_sex.png")
ggexport(plotlist = list(p1), filename = outfile)
orderly_artefact(
  description = "Consultation length by hcw sex", files = outfile
)


p1 <- ggsummarystats(
  data = bfa_baseline_dco[!is.na(bfa_baseline_dco$hcw_qualification), ],
  x = "hcw_qualification", y = "consult_length",
  ggfunc = ggboxplot, add = "jitter", na.rm = TRUE
)

p1$main.plot <- p1$main.plot +
  stat_compare_means(
    label = "p.signif", ref.group = ".all.", label.y.npc = 0.75
  ) +
  ylab("Consultation length (minutes)") +
  theme_pubr() +
  xlab("HCW Qualification")

outfile <- glue("{outfile_prefix}_hcw_sex.png")
ggexport(plotlist = list(p1), filename = outfile)
orderly_artefact(
  description = "Consultation length by hcw sex", files = outfile
)

bfa_baseline_dco$trimester <- factor(
  bfa_baseline_dco$trimester,
  levels = c("First Trimester", "Second Trimester", "Third Trimester"),
  ordered = TRUE
)

p1 <- ggsummarystats(
  data = bfa_baseline_dco,
  x = "trimester", y = "consult_length",
  ggfunc = ggboxplot, add = "jitter", na.rm = TRUE
)

p1$main.plot <- p1$main.plot +
  stat_compare_means(
    label = "p.signif", label.y.npc = 0.75,
    comparisons = list(c("First Trimester", "Second Trimester"),
                       c("First Trimester", "Third Trimester"),
                        c("Second Trimester", "Third Trimester"))
  ) +
  ylab("Consultation length (minutes)") +
  theme_pubr() +
  xlab("")

outfile <- glue("{outfile_prefix}_trimester.png")
ggexport(plotlist = list(p1), filename = outfile)
orderly_artefact(
  description = "Consultation length by trimester", files = outfile
)



## EDA exit survey
orderly_dependency(
  "process_burkina_faso", "latest",files = c("bfa_baseline_exit.rds")
)
bfa_baseline_exit <- readRDS("bfa_baseline_exit.rds")

## Select variables to explore
## response var is consult_length
## categorical covariates run aov
## continuous covariates run cor.test

my.render.cont <- function(x) {
  with(stats.apply.rounding(stats.default(x), digits = 2), c("",
    "Mean (SD)" = sprintf("%s (%s)", MEAN, SD)
  ))
}

## See https://blog.djnavarro.net/posts/2024-06-21_table1/#adding-extra-columns
render_p_value_cat <- function(x, ...) {
  dat <- bind_rows(map(x, ~ data.frame(value = .)), .id = "group")
  mod <- aov(value ~ group, dat)
  p <- summary(mod)[[1]][1, 5]
  label_pvalue()(p)
}

## Render function should accept data from all groups as a list of vectors
## and return a string to be printed in the table
render_p_value_cont <- function(x, ...) {
  
}

## Categorical covariates

cat_covariates <- c(
  "facility_level", "patient_can_read_write",
  "patient_highest_education", "patient_marital_status",
  "patient_partner_highest_education",
  "first_pregnancy", "first_anc_at_this_hf", "num_prev_anc_visits_other_hf",
  "patient_paid_consult_fee", "patient_paid_extra_fee",
  "patient_seen_chw", "patient_mode_of_transport"
)

## Exclude , "patient_seen_tba" because it only has 1 i.e. seen TBA for all obs
walk(cat_covariates, function(x) {
  f <- as.formula((paste("~ consult_length |", x)))
  small_df <- bfa_baseline_exit[!is.na(bfa_baseline_exit[[x]]), ]
  small_df[[x]] <- factor(small_df[[x]])
  label(small_df$consult_length) <- to_title_case(x)
  tab <- table1(
    f, data = small_df, extra.col = list("p-value" = render_p_value_cat),
    overall = FALSE, transpose = FALSE, render.cont = my.render.cont,
    caption = to_title_case(x),
  )
  kable(as.data.frame(tab), format = "latex", booktabs = TRUE) |>
    writeLines(glue("bfa_exit_{x}.tex"))
 })

  ## exclude num_prev_anc_visits because it has 535 NAs
cont_covariates <- c(
  "num_of_weeks_pregnant_an_book", "num_of_weeks_pregnant_self_report",
  "patient_age", "num_of_hcws_seen",
  "patient_residence_distance",
  "patient_residence_travel_time", "patient_travel_cost",
  "patient_wait_time",
  "patient_consult_fee", "total_hf_fees"
)  

bfa_baseline_exit_cont <- map_dfr(cont_covariates, function(x) {
  mod <- cor.test(
    bfa_baseline_exit$consult_length, bfa_baseline_exit[[x]],
    method = "pearson"
  )
  out <- tidy(mod)
  out <- mutate_if(out, is.numeric, ~ round(., 2))
  ##out <- mutate_if(out, is.numeric, ~ format(., nsmall = 2))
  out$pval <- label_pvalue()(out$`p.value`)
  data.frame(
    variable = to_title_case(x),
    estimate = glue("{out$estimate} ({out$conf.low}, {out$conf.high})"),
    pval = out$pval
  )
})

kable(bfa_baseline_exit_cont, format = "latex", booktabs = TRUE) |>
  writeLines("bfa_exit_cont_covariates.tex")

orderly_artefact(
  description = "Consultation length by categorical covariates",
  files = c(
    "bfa_exit_facility_level.tex",
    "bfa_exit_patient_can_read_write.tex",
    "bfa_exit_patient_highest_education.tex",
    "bfa_exit_patient_marital_status.tex",
    "bfa_exit_patient_partner_highest_education.tex",
    "bfa_exit_first_pregnancy.tex",
    "bfa_exit_first_anc_at_this_hf.tex",
    "bfa_exit_num_prev_anc_visits_other_hf.tex",
    "bfa_exit_patient_paid_consult_fee.tex",
    "bfa_exit_patient_paid_extra_fee.tex",
    "bfa_exit_patient_seen_chw.tex",
    "bfa_exit_patient_mode_of_transport.tex"
  )
)

orderly_artefact(
  description = "Consultation length by continuous covariates",
  files = c("bfa_exit_cont_covariates.tex")
)
