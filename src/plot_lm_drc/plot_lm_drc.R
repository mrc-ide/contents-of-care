library(brms)
library(dplyr)
library(emmeans)
library(glue)
library(ggplot2)
library(orderly2)
library(performance)
library(posterior)
library(purrr)
library(snakecase)
library(tibble)
library(tidyr)

orderly_shared_resource(utils.R = "utils.R")
source("utils.R")

pars <- orderly_parameters(survey = "baseline")


orderly_dependency("lm_drc", "latest", files = c("drc_fits.rds"))
orderly_dependency(
  "lm_drc_endline", "latest",
  files = c("drc_endline_fits.rds")
)
baseline_fits <- readRDS("drc_fits.rds")
endline_fits <- readRDS("drc_endline_fits.rds")

if (pars$survey == "baseline") {
  fits <- baseline_fits
} else {
  fits <- endline_fits
}



fixed_effects <- map_dfr(fits, function(fit) {
  x <- as.data.frame(fixef(fit, probs = c(0.025, 0.5, 0.975)))
  rownames_to_column(x)
}, .id = "datacut")

fixed_effects <- separate(
  fixed_effects, datacut,
  into = c("anc", "trimester"), sep = "_"
)

saveRDS(fixed_effects, file = "drc_dco_bayes_fixed_effects.rds")
orderly_artefact(
  files = "drc_dco_bayes_fixed_effects.rds",
  description = "Fixed effects for DRC 2015 DCO model fits"
)


## Random effects
ran_effects <- map_dfr(fits, function(fit) {
  x <- ranef(fit, probs = c(0.025, 0.5, 0.975))
  x <- as.data.frame(x$province[, , "Intercept"])
  rownames_to_column(x)
}, .id = "datacut")

ran_effects <- separate(
  ran_effects, datacut,
  into = c("anc", "trimester"), sep = "_"
)


saveRDS(ran_effects, file = "drc_dco_bayes_random_effects.rds")
orderly_artefact(
  files = "drc_dco_bayes_random_effects.rds",
  description = "Random effects for DRC 2015 DCO model fits"
)

p <- ggplot() +
  geom_vline(xintercept = 0, linetype = "dashed") +
  geom_point(data = ran_effects, aes(y = rowname, x = Q50)) +
  geom_errorbarh(
    data = ran_effects,
    aes(y = rowname, xmin = `Q2.5`, xmax = `Q97.5`),
    height = 0
  ) +
  theme_manuscript()

p <- my_facets(p)

ggsave_manuscript(
  p,
  file = "drc_dco_bayes_random_effects",
  width = 12, height = 8
)

icc <- map(fits, compute_icc)

saveRDS(icc, file = "drc_dco_bayes_icc.rds")

orderly_artefact(
  files = "drc_dco_bayes_icc.rds",
  description = "ICC for DRC 2015 DCO model fits"
)


bayes_r2 <- map(fits, bayes_R2, probs = c(0.025, 0.5, 0.975))

saveRDS(bayes_r2, file = "drc_dco_bayes_r2.rds")
orderly_artefact(
  files = "drc_dco_bayes_r2.rds",
  description = "Bayes R2 for DRC 2015 DCO model fits"
)

coeffs_gt_0 <- map_dfr(
  fits, function(fit) probability_of_direction(fit)[[1]],
  .id = "datacut"
)
coeffs_gt_0 <- separate(
  coeffs_gt_0, datacut,
  into = c("anc", "trimester"), sep = "_"
)

saveRDS(coeffs_gt_0, file = "drc_dco_bayes_coeffs_gt_0.rds")
orderly_artefact(
  files = "drc_dco_bayes_coeffs_gt_0.rds",
  description = "Coefficients greater than 0 for DRC 2015 DCO model fits"
)


coeffs_gt_0 <- filter(coeffs_gt_0, !rowname %in% "Intercept")

p <- ggplot(coeffs_gt_0) +
  geom_tile(
    aes(x = 0.5, y = rowname, width = 1, height = 0.25),
    fill = "gray"
  ) +
  geom_tile(
    aes(x = `Post.Prob` / 2, y = rowname, width = `Post.Prob`, height = 0.25),
    fill = "red"
  ) +
  geom_vline(xintercept = 0.5, linetype = "dashed", alpha = 0.5) +
##  scale_y_discrete(breaks = breaks, labels = labels) +
  xlim(0, 1) +
  xlab("Posterior probability of coefficient > 0") +
  theme_manuscript() +
  theme(axis.title.x = element_text(size = 12))

p <- my_facets(p)

ggsave_manuscript(
  "drc_dco_bayes_coeffs_gt_0",
  plot = p, width = 12, height = 8
)
