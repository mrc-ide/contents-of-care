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

deps <- bind_rows(
  Benin = benin, DRC = drc, `BFA (baseline)` = bfa_baseline,
  `BFA (endline)` = bfa_endline, BFA = bfa, .id = "country"
)


fef_files <- grep("fixed_effects.rds", deps$files$here, value = TRUE)

fef_vars <- map_chr(fef_files, function(file) {
  var_name <- gsub(".rds", "", file)
  assign(var_name, readRDS(file), envir = .GlobalEnv)
  var_name
})


x <- mget(fef_vars, envir = .GlobalEnv)

fixed_effects_coeffs <- map(x, function(df) {
  df$rowname <- str_replace_all(df$rowname, "_scaled", "")
  df$first_anc <- case_when(
    df$first_anc %in% "oui" ~ "First ANC",
    df$first_anc %in% "non" ~ "Follow-up ANC",
    TRUE ~ df$first_anc
  )
  df
}
)


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
    facet_grid(
      trimester ~ first_anc,
      scales = "free"
    ) +
    theme_manuscript() +
    theme(
      axis.title.y = element_blank(), axis.title.x = element_text(size = 12)
    ) +
    labs(x = to_title_case(xintercept$rowname[1]))

  ggsave_manuscript(
    plot = p, filename = col, width = 10, height = 8
  )
})


idx <- grepl("gt_0", deps$files$here)
pd_files <- deps$files$here[idx]
names(pd_files) <- deps$country[idx]

pd <- map_dfr(pd_files, readRDS, .id = "country")
pd$rowname <- str_replace_all(pd$rowname, "_scaled", "")
pd$first_anc <- case_when(
  pd$first_anc %in% "oui" ~ "First ANC",
  pd$first_anc %in% "non" ~ "Follow-up ANC",
  TRUE ~ pd$first_anc
)

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
    facet_grid(trimester ~ first_anc, scales = "free") +
    xlim(0, 1) + 
    theme_manuscript() +
    theme(
      axis.title.y = element_blank(), axis.title.x = element_text(size = 12)
    ) + labs(x = to_title_case(xintercept$rowname[1]))

  ggsave_manuscript(
    plot = p, filename = glue("{col}_pd"), width = 10, height = 8
  )
})
