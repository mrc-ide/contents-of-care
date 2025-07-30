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

orderly_shared_resource(utils.R = "utils.R")
source("utils.R")

benin <- orderly_dependency(
  "plot_lm_benin", "latest", files = c("./")
)

drc <- orderly_dependency(
  "plot_lm_drc", "latest", files = "./"
)

bfa_baseline <- orderly_dependency(
  "plot_lm_burkina_faso", "latest", files = "./"
)

bfa_endline <- orderly_dependency(
  "plot_lm_burkina_faso_endline", "latest", files = "./"
)

bfa <- orderly_dependency(
  "plot_lm_burkina_faso_both", "latest", files = "./"
)

multicountry <- orderly_dependency(
  "plot_lm_multicountry", "latest", files = "./"
)

deps <- bind_rows(
  Benin = benin, DRC = drc, `BFA (baseline)` = bfa_baseline,
  `BFA (endline)` = bfa_endline, BFA = bfa, Multicountry = multicountry,
  .id = "country"
)

fit_files <- grep("fits.rds", deps$files$here, value = TRUE)
fits <- map(fit_files, readRDS)
x <- str_replace_all(names(fits[[1]]), "oui", "First ANC")
x <- str_replace_all(x, "non", "Follow-up ANC")
names(fits[[1]]) <- x

names(fits) <- deps$country[grepl("fits.rds", deps$files$here)]

raw_data <- map_dfr(fits, function(fit) {
  out <- map_dfr(fit, function(x) x$data, .id = "datacut")
  out <- separate(
    out, "datacut",
    into = c("anc", "trimester"), sep = "_"
  )
  out
}, .id = "country")

raw_data$anc <- str_replace_all(raw_data$anc, "oui", "First ANC")
raw_data$anc <- str_replace_all(raw_data$anc, "non", "Follow-up ANC")

p <- ggplot(raw_data) +
  stat_ecdf(aes(x = exp(log_consult_length), color = country)) +
  theme_manuscript() 


fef_files <- grep("fixed_effects.rds", deps$files$here, value = TRUE)
names(fef_files) <- deps$country[grepl("fixed_effects.rds", deps$files$here)]
fixed_effects_coeffs <- map(fef_files, readRDS) |>
  map(function(x) {
    x$rowname <- str_replace_all(x$rowname, "_scaled", "")
    x
  })


common_cols <- Reduce(
  intersect, map(fixed_effects_coeffs, function(df) df$rowname)
)

coeffs <- map_dfr(
  fixed_effects_coeffs, function(df) filter(df, rowname %in% common_cols), 
  .id = "country"
)


walk(common_cols, function(col) {

  xintercept <- filter(coeffs, rowname %in% col)

  p <- ggplot(xintercept) +
    geom_point(aes(x = Q50, y = country)) +
    geom_errorbarh(
      aes(y = country, xmin = `Q2.5`, xmax = `Q97.5`),
      height = 0
    ) +
    geom_vline(xintercept = 0, linetype = "dashed") +
    theme_manuscript() +
    theme(
      axis.title.y = element_blank(), axis.title.x = element_text(size = 12)
    ) +
    labs(x = to_title_case(xintercept$rowname[1]))

  p <- my_facets(p)
  
  ggsave_manuscript(
    plot = p, filename = col, width = 12, height = 8
  )
})


idx <- grepl("gt_0.rds", deps$files$here)
pd_files <- deps$files$here[idx]
names(pd_files) <- deps$country[idx]

pd <- map_dfr(pd_files, readRDS, .id = "country")
pd$rowname <- str_replace_all(pd$rowname, "_scaled", "")

walk(common_cols, function(col) {

  xintercept <- filter(pd, rowname %in% col)

  p <- ggplot(xintercept) +
    geom_tile(
      aes(x = 0.5, y = country, width = 1, height = 0.25), fill = "gray"
    ) +
    geom_tile(
      aes(x = `Post.Prob` / 2, y = country, width = `Post.Prob`, height = 0.25),
      fill = "red"
    ) +
    facet_grid(trimester ~ anc, scales = "free") +
    xlim(0, 1) + 
    theme_manuscript() +
    theme(
      axis.title.y = element_blank(), axis.title.x = element_text(size = 12)
    ) + labs(x = to_title_case(xintercept$rowname[1]))

  ggsave_manuscript(
    plot = p, filename = glue("{col}_pd"), width = 12, height = 8
  )
})
