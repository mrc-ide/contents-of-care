library(brms)
library(dplyr)
library(ggplot2)
library(orderly2)
library(purrr)
library(snakecase)
library(stringr)

orderly_dependency("lr_multicountry", "latest", "fits/")
infiles <- list.files("fits/")
fits <- map(infiles, ~ readRDS(file.path("fits", .x)))
names(fits) <- infiles

trimesters <- paste0(c("first", "second", "third"), "_trimester")
anc_types <- c("first_anc", "follow_up_anc")

preds <- imap(fits, function(fit, infile) {
  cli::cli_inform("Processing {infile}")
  newdat <- model.frame(fit)
  ep <- posterior_epred(fit, newdata = newdat)
  p_draws <- sweep(ep, 2, newdat$steps_total, "/")
  qmat <- apply(p_draws, 2, quantile, probs = c(0.025, 0.5, 0.975), na.rm = TRUE)
  preds <- as.data.frame(t(qmat)) |>
    setNames(c("lo", "med", "hi")) |>
    bind_cols(newdat)

  preds$trimester <- str_extract(infile, trimesters, group = NULL) |>
    keep(~ !is.na(.))

  preds$anc_type <- str_extract(infile, anc_types, group = NULL) |>
    keep(~ !is.na(.))

  preds$intervention <-
    gsub(x = infile, pattern = "multicountry_lr_fit_", replacement = "") |>
    gsub(pattern = preds$trimester[1], replacement = "") |>
    gsub(pattern = preds$anc_type[1], replacement = "") |>
    gsub(pattern = "_rds", replacement = "") |>
    to_sentence_case()

  preds
  
})

preds <- bind_rows(preds)

x <- split(preds, preds$intervention)

iwalk(x, function(tmp, name) {
  
  p <- ggplot(tmp, aes(x = consult_length, y = med)) +
   geom_errorbar(aes(ymin = lo, ymax = hi), alpha = 0.2) +
    geom_point() +
    ylim(0, 1) +
    facet_grid(trimester ~ anc_type) +
    labs(
      x = "Consultation length (minutes)",
      y = "Predicted number of steps"
    ) +
    theme_minimal()

  outfile <- to_snake_case(name)
  ggsave(filename = paste0("figures/", outfile, ".png"), plot = p)
})
