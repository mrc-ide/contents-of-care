library(broom)
library(dplyr)
library(gghalves)
library(ggpmisc)
library(glue)
library(orderly2)
library(purrr)
library(tidyr)
library(zip)

dir.create("figures")

orderly_shared_resource(utils.R = "utils.R")
source("utils.R")

orderly_dependency(
  "process_benin", "latest", "benin_dco.rds"
)

orderly_dependency(
  "process_drc", "latest", "drc_dco_2015.rds"
)

orderly_dependency(
  "process_drc_endline", "latest",
  c("drc_dco_2021.rds" = "drc_dco_2015.rds")
)


orderly_dependency(
  "process_burkina_faso", "latest", "bfa_baseline_dco.rds"
)

orderly_dependency(
  "process_burkina_faso_endline", "latest", "bfa_endline_dco.rds"
)

benin <- readRDS("benin_dco.rds")
drc <- readRDS("drc_dco_2015.rds")
drc_2021 <- readRDS("drc_dco_2021.rds")
bfa_baseline <- readRDS("bfa_baseline_dco.rds")
bfa_endline <- readRDS("bfa_endline_dco.rds")

consult_len <- bind_rows(
  `Benin (2010)` = select(benin, consult_length, anc = first_anc, trimester),
  `DRC (2015)` =
    select(drc, consult_length = consult_length_calc, anc = first_anc, trimester),
  `DRC (2021)` =
    select(drc_2021, consult_length = consult_length_calc, anc = first_anc, trimester),  
  `BFA (2013)` =
    select(bfa_baseline, consult_length = consult_length_calc, anc = first_anc, trimester),
  `BFA (2017)` =
    select(bfa_endline, consult_length = consult_length_calc, anc = first_anc, trimester),
  .id = "country"
)

## Summary tables
consult_len_summary <- group_by(consult_len, country) |>
  summarise(
    n = n(),
    median = median(consult_length, na.rm = TRUE),
    low = quantile(consult_length, 0.25, na.rm = TRUE),
    high = quantile(consult_length, 0.75, na.rm = TRUE),
  ) |>
  ungroup()

consult_len_summary$label <- glue(
  "{consult_len_summary$median} ({consult_len_summary$low};{consult_len_summary$high})"
)

p <- ggplot(consult_len) +
  geom_half_violin(
    aes(x = country, y = consult_length, fill = country),
    side = "r",
    draw_quantiles = c(0.25, 0.5, 0.75), alpha = 0.5
  ) +
  geom_half_point(
    aes(x = country, y = consult_length, col = country),
    side = "l", alpha = 0.5
  ) +
  scale_y_continuous(
    name = "Consultation length (minutes)",
    breaks = seq(0, 300, 60)
  ) +
  theme_manuscript() +
  theme(axis.title.x = element_blank()) +
  ylab("Consultation length (minutes)") 

out <- select(consult_len_summary, Country = country, `Median (IQR)` = label)

p1 <- p +
  geom_table_npc(
    data = out, label = list(out), npcx = 0.11, npcy = 0.9,
    table.theme =
      ttheme_gtminimal(base_size = 12, padding = grid::unit(c(1, 1), "char"))
  ) 

ggsave_manuscript(
  "figures/consultation_length_by_country", p1, width = 12, height = 8
)


consult_len <- na.omit(consult_len)

p <- ggplot(consult_len) +
  geom_half_violin(
    aes(x = country, y = consult_length, fill = trimester),
    side = "r",
    draw_quantiles = c(0.25, 0.5, 0.75), alpha = 0.5
  ) +
  geom_half_point(
    aes(x = country, y = consult_length, col = trimester),
    side = "l", alpha = 0.5
  ) +
  scale_y_continuous(
    name = "Consultation length (minutes)",
    breaks = seq(0, 300, 60)
  ) +
  theme_manuscript() +
  theme(axis.title.x = element_blank()) +
  ylab("Consultation length (minutes)")

ggsave_manuscript(
  "figures/consultation_length_by_trimester", p,
  width = 12, height = 8
)


p <- ggplot(consult_len) +
  geom_half_violin(
    aes(x = country, y = consult_length, fill = anc),
    side = "r",
    draw_quantiles = c(0.25, 0.5, 0.75), alpha = 0.5
  ) +
  geom_half_point(
    aes(x = country, y = consult_length, col = anc),
    side = "l", alpha = 0.5
  ) +
  scale_y_continuous(
    name = "Consultation length (minutes)",
    breaks = seq(0, 70, 10),
    limits = c(0, 70)
  ) +
  theme_manuscript() +
  theme(axis.title.x = element_blank()) +
  ylab("Consultation length (minutes)")

ggsave_manuscript(
  "figures/consultation_length_by_anc", p,
  width = 12, height = 8
)


signf_level <- 0.01

## How dissimilar are the distributions of consultation lengths
## across countries?
pbasic <- ggplot() +
  scale_x_discrete(limits = unique(consult_len$country)) +
  scale_y_discrete(limits = unique(consult_len$country)) +
  scale_fill_viridis_c() +
  scale_alpha_manual(
    values = c("FALSE" = 0.01, "TRUE" = 1), guide = "none"
  ) +  
  theme_manuscript() +
  theme(legend.key.width = unit(1, "null"))

  

data_split <- split(consult_len, list(consult_len$country), sep = "_")
out <- ks_distance_matrix(data_split, "consult_length")
out$significant <- out$p.value < signf_level

p <- pbasic + geom_tile(
  data= out, aes(stratum1, stratum2, fill = statistic, alpha = significant)
) 


ggsave_manuscript(
  "figures/consultation_length_ks_by_country", p,
  width = 8, height = 8
)


## How dissimilar are the distributions of consultation lengths
## across countries when stratified by ANC and Trimester?
data_split <-
  split(consult_len, list(consult_len$anc, consult_len$trimester), sep = "_")

out <- map_dfr(data_split, function(this_split) {
  by_country <- split(this_split, this_split$country)
  ks_distance_matrix(by_country, "consult_length")
}, .id = "datacut")

out <- separate(
  out, "datacut", into = c("anc", "trimester"), sep = "_"
)

out$significant <- out$p.value < signf_level

  
p <- pbasic +
  geom_tile(
    data = out, aes(stratum1, stratum2, fill = statistic, alpha = significant)
  ) + facet_grid(anc ~ trimester) 
  

ggsave_manuscript(
  plot = p, filename = "figures/consultation_length_ks_by_country_anc_trimester",
  width = 10, height = 10
)

## Within a country, how similar are the distributions of consultation
## lengths across trimesters?

data_split <- split(consult_len, list(consult_len$country), sep = "_")
out <- map_dfr(data_split, function(this_split) {
  by_trimester <- split(this_split, this_split$trimester)
  ks_distance_matrix(by_trimester, "consult_length")
}, .id = "country")

out$significant <- out$p.value < signf_level


p <- pbasic +
  geom_tile(
    data = out, aes(stratum1, stratum2, fill = statistic, alpha = significant)
  ) + 
  scale_x_discrete(
    limits = c(
      "First Trimester", "Second Trimester", "Third Trimester"
    )
  ) +
  scale_y_discrete(
    limits = c(
      "First Trimester", "Second Trimester", "Third Trimester"
    )
  ) 

p <- p + facet_wrap( ~ country)

ggsave_manuscript(
  plot = p, filename = "figures/consultation_length_ks_by_country_trimester",
  width = 10, height = 10
)
## Across ANCs
out <- map_dfr(data_split, function(this_split) {
  by_anc <- split(this_split, this_split$anc)
  ks_distance_matrix(by_anc, "consult_length")
}, .id = "country")

out$significant <- out$p.value < signf_level

p <- pbasic +
  geom_tile(
    data = out, aes(stratum1, stratum2, fill = statistic, alpha = significant)
  ) + 
  scale_x_discrete(
    limits = c("First ANC", "Follow-up ANC")
  ) +
  scale_y_discrete(
    limits = c("First ANC", "Follow-up ANC")
  ) + facet_wrap( ~ country)

ggsave_manuscript(
  plot = p, filename = "figures/consultation_length_ks_by_country_anc",
  width = 10, height = 10)

zip::zip("figures.zip", "figures", recurse = TRUE)
orderly_artefact(
  files = "figures.zip",
  description = "Figures for manuscript on consultation length"
)
