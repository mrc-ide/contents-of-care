library(bayestestR)
library(brms)
library(dplyr)
library(ggplot2)
library(glue)
library(orderly2)
library(performance)
library(posterior)
library(purrr)
library(snakecase)
library(stringr)
library(tidyr)

dir.create("figures", showWarnings = FALSE)


orderly_shared_resource(utils.R = "utils.R")
source("utils.R")

benin <- orderly_dependency(
  "lm_benin", "latest", "benin_dco_fits.rds"
)
benin <- readRDS("benin_dco_fits.rds")

drc <- orderly_dependency(
  "lm_drc", "latest", "drc_fits.rds"
)
drc <- readRDS("drc_fits.rds")

drc <- orderly_dependency(
  "lm_drc_endline", "latest", "drc_endline_fits.rds"
)
drc_endline <- readRDS("drc_endline_fits.rds")

bfa_baseline <- orderly_dependency(
  "lm_burkina_faso_baseline", "latest",
  "bfa_baseline_dco_fits.rds"
)
bfa_baseline <- readRDS("bfa_baseline_dco_fits.rds")

bfa_endline <- orderly_dependency(
  "lm_burkina_faso_endline", "latest",
  "bfa_endline_dco_fits.rds"
)
bfa_endline <- readRDS("bfa_endline_dco_fits.rds")

orderly_dependency(
  "lm_multicountry", "latest", "fits/"
)

infiles <- list.files("fits", full.names = TRUE)

multicountry_fits <- map(infiles, readRDS)
names(multicountry_fits) <- str_remove(infiles, "fits/") |>
  str_remove("_multicountry_fit.rds")
  


deps <- list(
  `Benin (2010)` = benin,
  `DRC (2015)` = drc,
  `DRC (2021)` = drc_endline,
  `BFA (2013)` = bfa_baseline,
  `BFA (2017)` = bfa_endline,
  Multicountry = multicountry_fits
)

all_hdi <-
  map_dfr(deps,
    function(dep) map_dfr(dep, hdi, .id = "datacut"),
    .id = "country"
  )

all_map <-
  map_dfr(deps,
    function(dep) map_dfr(dep, point_estimate, .id = "datacut"),
    .id = "country"
  )

all_pd <-
  map_dfr(
    deps,
    function(dep) map_dfr(dep, probability_of_direction, .id = "datacut"),
    .id = "country"
  )

var <- c(
  "b_time_elapsed_since_start_of_day"
)

xhdi <- filter(all_hdi, Parameter %in% var) |>
  separate(
  datacut,
  into = c("first_anc", "trimester"),
  sep = "[_.]"
  ) 

ymap <- filter(all_map, Parameter %in% var) |>
  separate(
    datacut,
    into = c("first_anc", "trimester"),
    sep = "[_.]"
  )

zpd <- filter(all_pd, Parameter %in% var) |>
  separate(
    datacut,
    into = c("first_anc", "trimester"),
    sep = "[_.]"
  )

p <- ggplot() +
  geom_vline(xintercept = 0, linetype = "dashed") +
  geom_point(
    data = ymap, aes(y = country, x = `MAP`),
    size = 4
  ) +  
  geom_linerange(
    data = xhdi,
    aes(y = country, xmin = `CI_low`, xmax = `CI_high`),
    linewidth = 0.8
  ) +
  facet_grid(
    first_anc ~ trimester,
    scales = "free_x"
  ) +
  theme_manuscript() 


p <- p +
  xlab("MAP estimate and 95% Highest Density Interval") +
  ylab("") +
  ggtitle(
    "Effect of time elapsed since 6AM on ANC contact length"
  )



ggsave_manuscript(
  "figures/effect_time_elapsed_since_start_of_day.png",
  p,
  width = 12,
  height = 8
)


p <- ggplot(zpd) +
  geom_tile(
    aes(x = 0.5, y = country, width = 1, height = 0.25),
    fill = "gray"
  ) +
  geom_tile(
    aes(x = pd / 2, y = country, width = pd, height = 0.25),
    fill = "red"
  ) +
  geom_vline(xintercept = 0.5, linetype = "dashed", alpha = 0.5) +
  facet_grid(
    first_anc ~ trimester,
    scales = "free_x"
  ) +
  xlim(0, 1) +
  xlab("Posterior probability of coefficient > 0") +
  ylab("") +
  ggtitle(
    "Posterior probability that time elapsed since 6AM has a positive effect"
  ) +
  theme_manuscript() +
  theme(axis.title.x = element_text(size = 12))

ggsave_manuscript(
  "figures/pd_time_elapsed_since_start_of_day.png",
  p,
  width = 12,
  height = 8
)

#######################################################################
## Doctors and N&M per 10k
#######################################################################

var <- "b_doctor_or_nursing_and_midwifery_scaled"

xhdi <- filter(all_hdi, Parameter %in% var) |>
  separate(
  datacut,
  into = c("first_anc", "trimester"),
  sep = "[_.]"
  ) 

ymap <- filter(all_map, Parameter %in% var) |>
  separate(
    datacut,
    into = c("first_anc", "trimester"),
    sep = "[_.]"
  )

zpd <- filter(all_pd, Parameter %in% var) |>
  separate(
    datacut,
    into = c("first_anc", "trimester"),
    sep = "[_.]"
  )

p <- ggplot() +
  geom_vline(xintercept = 0, linetype = "dashed") +
  geom_point(
    data = ymap, aes(y = country, x = `MAP`),
    size = 4
  ) +  
  geom_linerange(
    data = xhdi,
    aes(y = country, xmin = `CI_low`, xmax = `CI_high`),
    linewidth = 0.8
  ) +
  facet_grid(
    first_anc ~ trimester,
    scales = "free_x"
  ) +
  theme_manuscript() 


p <- p +
  xlab("MAP estimate and 95% Highest Density Interval") +
  ylab("") +
  ggtitle(
    "Effect of Doctors/N&M per 10k on ANC contact length"
  )



ggsave_manuscript(
  "figures/effect_dnm.png",
  p,
  width = 12,
  height = 8
)


p <- ggplot(zpd) +
  geom_tile(
    aes(x = 0.5, y = country, width = 1, height = 0.25),
    fill = "gray"
  ) +
  geom_tile(
    aes(x = pd / 2, y = country, width = pd, height = 0.25),
    fill = "red"
  ) +
  geom_vline(xintercept = 0.5, linetype = "dashed", alpha = 0.5) +
  facet_grid(
    first_anc ~ trimester,
    scales = "free_x"
  ) +
  xlim(0, 1) +
  xlab("Posterior probability of coefficient > 0") +
  ylab("") +
  theme_manuscript() +
  theme(axis.title.x = element_text(size = 12))

ggsave_manuscript(
  "figures/pd_dnm.png",
  p,
  width = 12,
  height = 8
)


#######################################################################
# Other variables tracking busyness
#######################################################################
vars <- c(
  "b_total_attendance_last_year_scaled",
  "b_anc_visits_last_year_scaled",
  "b_total_births_last_year_scaled",
  "b_anc_contacts_last_week_scaled",
  "b_anc_contacts_last_month_scaled",
  "b_day_of_visitWeekend",
  "b_pregnant_women_last_year_scaled",
  "b_num_personnel_scaled",
  "b_num_csps_in_district_scaled"
)

xhdi <- filter(all_hdi, Parameter %in% vars)
ymap <- filter(all_map, Parameter %in% vars)


xhdi <- xhdi |>
  separate(
    datacut,
    into = c("first_anc", "trimester"),
    sep = "[_.]"
  )

ymap <- ymap |>
  separate(
    datacut,
    into = c("first_anc", "trimester"),
    sep = "[_.]"
  )


xhdi$newx <- paste(xhdi$Parameter, xhdi$country)



ymap$Parameter <- str_replace_all(
  ymap$Parameter, "^b_", ""
)

ymap$first_anc <- case_when(
  ymap$first_anc == "First ANC" ~ "First",
  ymap$first_anc == "Follow-up ANC" ~ "Follow-up"
)


xhdi$Parameter <- str_replace_all(
  xhdi$Parameter, "^b_", ""
)

xhdi$first_anc <- case_when(
  xhdi$first_anc == "First ANC" ~ "First",
  xhdi$first_anc == "Follow-up ANC" ~ "Follow-up"
)


p <- ggplot() +
  geom_vline(xintercept = 0, linetype = "dashed") +
  geom_point(
    data = ymap, aes(y = Parameter, x = `MAP`),
    size = 2, position = position_dodge(width = 0.7)
  ) +
  geom_linerange(
    data = xhdi,
    aes(y = Parameter, xmin = `CI_low`, xmax = `CI_high`),
    linewidth = 0.8, position = position_dodge(width = 0.7)
  ) +
  facet_nested(
    country + first_anc ~ trimester,
    scales = "free_x"
  ) +
  theme_manuscript()

p <- p +
  xlab("MAP estimate and 95% Highest Density Interval") +
  ylab("") +
  ggtitle(
    "Effects of facility busyness variables on ANC contact length"
  )

p <- p + scale_y_discrete(
  labels = c(
    "anc_visits_last_year_scaled" = "ANC visits last year",
    "day_of_visitWeekend" = "Day of visit: Weekend",
    "num_csps_in_district_scaled" = "CSPS in district",
    "num_personnel_scaled" = "Number of personnel",
    "pregnant_women_last_year_scaled" = "Pregnant women last year",
    "total_attendance_last_year_scaled" = "Total attendance last year",
    "total_births_last_year_scaled" = "Total births last year"
  )
  )

ggsave_manuscript(
  "figures/map_and_hdi_other_busyness_vars",
  p,
  width = 12,
  height = 12
)

zpd <-
  filter(all_pd, rowname %in% str_replace_all(vars, "^b_", ""))

zpd$datacut <- str_replace_all(zpd$datacut, "[_.]", "/")

p <- ggplot(zpd, aes(datacut, rowname, fill = `Post.Prob`)) +
  geom_tile() +
  scale_fill_gradient2(
    low = "firebrick", mid = "white", high = "steelblue",
    midpoint = 0.5, limits = c(0, 1), name = "Pr(β > 0)"
  )  +
  facet_wrap(~country, nrow = 1) +
  coord_fixed(ratio = 1) +
  ggtitle("Posterior probabilities that coefficients > 0") +
  theme_manuscript() +
  theme(axis.title = element_blank()) 


## Fix the x-axis labels
p <- p +
  scale_x_discrete(
    limits = c(
      "First ANC/First Trimester",
      "Follow-up ANC/First Trimester",
      "First ANC/Second Trimester",
      "Follow-up ANC/Second Trimester",
      "First ANC/Third Trimester",
      "Follow-up ANC/Third Trimester"
    )
  ) +
  theme(
    axis.text.x = element_text(
      angle = 90, hjust = 1, vjust = 0.5
    )
  ) 

## Fix y-axis
p <- p +
  scale_y_discrete(
    labels = c(
      "anc_visits_last_year_scaled" = "ANC visits last year",
      "day_of_visitWeekend" = "Day of visit: Weekend",
      "num_csps_in_district_scaled" = "CSPS in district",
      "num_personnel_scaled" = "Number of personnel",
      "pregnant_women_last_year_scaled" = "Pregnant women last year",
      "total_attendance_last_year_scaled" = "Total attendance last year",
      "total_births_last_year_scaled" = "Total births last year"
    )
  ) 

## Fix legend
p <- p +
  theme(
    legend.position = "bottom",
    legend.key.width = unit(2, "cm")
  )

ggsave_manuscript(
  "figures/p_beta_positive_other_busyness_vars.png",
  p,
  width = 12,
  height = 6
)
