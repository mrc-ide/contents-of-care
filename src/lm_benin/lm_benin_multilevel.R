library(brms)


fits <- map(benin_split, function(x) {

  insuff_levels <- map(x, ~ length(unique(.))) |> keep(~ . < 2)
  cli_alert_info(
    "Removing variables with insufficient levels: {names(insuff_levels)}"
  )
  x <- select(x, -names(insuff_levels))
  brm(
    formula = bf(log_consult_length ~ . - health_zone + (1| health_zone)),
    data = x,
    family = gaussian(),
    drop_unused_levels = TRUE,
    chains = 4,
    cores = 4,
    iter = 4000,
    prior = prior_spec,
    control = list(adapt_delta = 0.99)
  )
})

saveRDS(fits, file = "benin_dco_fits.rds")
orderly_artefact(
  files = "benin_dco_fits.rds",
  description = "DRC 2015 DCO model fits"
)












