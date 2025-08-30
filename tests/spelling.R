if (requireNamespace('spelling', quietly = TRUE))
  spelling::spell_check_test(vignettes = TRUE, error = FALSE,
    kip_on_cran = TRUE)
