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
  "process_burkina_faso", "latest", files = c("bfa_dco.rds")
)
bfa_dco <- readRDS("bfa_dco.rds")
outfile_prefix <- "bfa_consult_length_by"

out <- summary(bfa_dco$consult_length_calc) |>
  round(2) |>
  tidy()

p1 <- gghistogram(
  bfa_dco,
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
  data = bfa_dco, x = "REGION.x", y = "consult_length",
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
  data = bfa_dco, x = "num_prev_anc_visits", y = "consult_length",
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
  data = bfa_dco, x = "first_pregnancy", y = "consult_length",
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
  data = bfa_dco, x = "first_anc", y = "consult_length",
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
  data = bfa_dco, x = "facility_type", y = "consult_length",
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
  data = bfa_dco, x = "hcw_sex", y = "consult_length",
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
  data = bfa_dco[!is.na(bfa_dco$hcw_qualification), ],
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

bfa_dco$trimester <- factor(
  bfa_dco$trimester,
  levels = c("First Trimester", "Second Trimester", "Third Trimester"),
  ordered = TRUE
)

p1 <- ggsummarystats(
  data = bfa_dco,
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



