library(brms)

fits <- map(drc_baseline_split, function(x) {
  brm(
    formula = bf(consult_length_calc ~ . -province + (1 | province)),
    data = x,
    family = lognormal(),
    drop_unused_levels = TRUE,
    chains = 4,
    cores = 4,
    iter = iter,
    prior = prior_spec,
    control = list(adapt_delta = 0.99)
)})

saveRDS(fits, file = "drc_fits.rds")
orderly_artefact(
  files = "drc_fits.rds",
  description = "brms models for DRC baseline data"
)



