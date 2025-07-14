library(brms)


fits <- map(benin_split, function(x) {
  
  brm(
    formula = bf(log_consult_length ~ . + (1| health_zone)),
    data = x,
    family = gaussian(),
    drop_unused_levels = TRUE,
    chains = 4,
    cores = 4,
    iter = 4000,
    priors <- prior(normal(0, 1), class = "b"),
    control = list(adapt_delta = 0.99)
  )
})

saveRDS(fits, file = "benin_dco_fits.rds")
orderly_artefact(
  files = "benin_dco_fits.rds",
  description = "DRC 2015 DCO model fits"
)












