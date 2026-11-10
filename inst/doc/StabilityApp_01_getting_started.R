## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(collapse = FALSE, comment = "", fig.width = 7, fig.height = 4.6,
                      dev = if (knitr::is_latex_output()) "cairo_pdf" else
                        if (requireNamespace("svglite", quietly = TRUE)) "svglite" else "png")
if (requireNamespace("StabilityApp", quietly = TRUE)) library(StabilityApp) else
  pkgload::load_all(export_all = FALSE, helpers = FALSE, quiet = TRUE)


## ----start, eval = FALSE------------------------------------------------------
# StabilityApp(
#     launch.browser = TRUE
#   )


## ----code---------------------------------------------------------------------
fit <-
  stability::stab_reg(
      .data = stability::ge_data
    , .y    = Yield
    , .rep  = Rep
    , .gen  = Gen
    , .env  = Env
    )
stability::stab_interpret(
    x             = fit
  , higher_better = TRUE
  , top           = 5
  )

