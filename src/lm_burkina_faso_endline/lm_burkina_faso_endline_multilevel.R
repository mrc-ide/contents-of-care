library(brms)


fits <- map(bfa_split, function(x) {
  x <- select(x, -first_anc, -trimester)
  insuff_levels <- map(x, ~ length(unique(.))) |> keep(~ . < 2)
  cli_alert_info(
    "Removing variables with insufficient levels: {names(insuff_levels)}"
  )
  x <- select(x, -names(insuff_levels))
  x <- na.omit(x)
  brm(
    formula = bf(consult_length ~ . -region_name + (1 | region_name)),
    data = x,
    family = lognormal(),
    drop_unused_levels = TRUE,
    chains = 4,
    cores = 4,
    iter = iter,
    prior = prior_spec,
    silent = 0,
    control = list(adapt_delta = 0.99)
  )
})

saveRDS(fits, file = "bfa_endline_dco_fits.rds")
orderly_artefact(
  files = "bfa_endline_dco_fits.rds",
  description = "BFA endline DCO model fits"
)
