library(brms)

prior_spec <- c(
  prior(normal(0, 1), class = b), # fixed effects
  prior(normal(0, 0.5), class = sd) # random effects
)

fits <- map(bfa_split, function(x) {
  x <- select(x, -first_anc, -trimester, -consult_length)
  insuff_levels <- map(x, ~ length(unique(.))) |> keep(~ . < 2)
  cli_alert_info(
    "Removing variables with insufficient levels: {names(insuff_levels)}"
  )
  x <- select(x, -names(insuff_levels))
  brm(
    formula = bf(log_consult_length ~ . - region_name + (1 | region_name)),
    data = x,
    family = gaussian(),
    drop_unused_levels = TRUE,
    chains = 4,
    cores = 1,
    iter = 4000,
    prior = prior_spec,
    control = list(adapt_delta = 0.99)
  )
  
})

saveRDS(fits, file = "bfa_both_dco_fits.rds")
orderly_artefact(
  files = "bfa_both_dco_fits.rds",
  description = "BFA baseline and endline DCO model fits"
)
