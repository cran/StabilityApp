## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(collapse = FALSE, comment = "", fig.width = 7, fig.height = 4.6,
                      dev = if (knitr::is_latex_output()) "cairo_pdf" else
                        if (requireNamespace("svglite", quietly = TRUE)) "svglite" else "png")
if (requireNamespace("StabilityApp", quietly = TRUE)) library(StabilityApp) else
  pkgload::load_all(export_all = FALSE, helpers = FALSE, quiet = TRUE)


## ----computed-----------------------------------------------------------------
fit <-
  stability::ammi(
      .data = stability::ge_data
    , .y    = Yield
    , .rep  = Rep
    , .gen  = Gen
    , .env  = Env
    )
it <-
  stability::stab_interpret(
      x             = fit
    , higher_better = TRUE
    , top           = 5
    )
it


## ----prompt-------------------------------------------------------------------
p <-
  sa_prompt(
      x             = fit
    , higher_better = TRUE
    , trait         = "grain yield (kg/ha)"
    , rows          = 8
    )
cat(p$system)
cat(substr(x = p$user, start = 1, stop = 1600))


## ----services-----------------------------------------------------------------
names(sa_services())
nrow(sa_services())


## ----ask, eval = FALSE--------------------------------------------------------
# txt <-
#   sa_ask(
#       service    = sa_services()$id[1]
#     , prompt     = p
#     , model      = sa_services()$model[1]
#     , key        = ""
#     , max_tokens = 1500
#     , timeout    = 90
#     )
# cat(txt)

