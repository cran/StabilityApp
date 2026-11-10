test_that("the request is specific to the method and carries the computed findings", {
  d <- stability::ge_data
  fits <- list(
      stab_reg   = stability::stab_reg(.data = d, .y = Yield, .rep = Rep, .gen = Gen, .env = Env)
    , ammi       = stability::ammi(.data = d, .y = Yield, .rep = Rep, .gen = Gen, .env = Env)
    , gge_biplot = stability::gge_biplot(.data = d, .y = Yield, .gen = Gen, .env = Env, .which = "which-won-where")
    , stab_fox   = stability::stab_fox(.data = d, .y = Yield, .gen = Gen, .env = Env)
    )
  focus <- vapply(StabilityApp:::.sa_registry()[names(fits)], `[[`, "", "focus")
  texts <- character(0)
  for (n in names(fits)) {
    p <- sa_prompt(x = fits[[n]], higher_better = TRUE, trait = "grain yield", rows = 10)
    expect_named(p, c("system", "user", "interpretation"))
    expect_match(p$user, focus[[n]], fixed = TRUE)
    expect_match(p$user, p$interpretation$headline, fixed = TRUE)
    expect_match(p$user, p$interpretation$guide, fixed = TRUE)
    expect_match(p$user, "60 genotypes in 11 environments")
    expect_match(p$user, "## 4. Next steps", fixed = TRUE)
    texts <- c(texts, p$user)
  }
  expect_equal(length(unique(texts)), length(fits))
  lo <- sa_prompt(x = fits$stab_fox, higher_better = FALSE)
  expect_match(lo$user, "lower values are better")
})
