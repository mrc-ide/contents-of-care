
library(brms)

drc_endline_split <-
  drc_endline_split[!names(drc_endline_split) %in% "Follow-up ANC_First Trimester"]

fits <- map(drc_endline_split, function(x) {
  brm(
    formula = bf(log_consult_length ~ . -province + (1 | province)),
    data = x,
    family = gaussian(),
    drop_unused_levels = TRUE,
    chains = 4,
    cores = 4,
    iter = 4000,
    prior = prior_spec,
    control = list(adapt_delta = 0.99)
)})

saveRDS(fits, file = "drc_endline_fits.rds")
orderly_artefact(
  files = "drc_endline_fits.rds",
  description = "brms models for DRC endline data"
)



