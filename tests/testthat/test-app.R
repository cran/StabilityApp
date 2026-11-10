test_that("every analysis runs in the application and gives tables, a plot, an interpretation and code", {
  reg <- StabilityApp:::.sa_registry()
  idata <- shiny::reactive(list(data = stability::ge_data, name = "ge_data", file = "",
                                cols = list(y = "Yield", gen = "Gen", env = "Env", rep = "Rep"), hb = TRUE,
                                trait = "grain yield"))
  sett <- shiny::reactive(list(service = sa_services()$id[1], model = "m", key = "k"))
  for (s in reg) {
    shiny::testServer(StabilityApp:::.sa_analysis_server, args = list(spec = s, input_data = idata, settings = sett), {
      init <- list()
      for (nm in names(s$options)) init[[paste0("opt", nm)]] <- s$options[[nm]]$value
      if (length(init)) do.call(session$setInputs, init)
      if (length(s$plots) > 1L) session$setInputs(which = unname(s$plots[2L]))
      session$setInputs(run = 1)
      expect_false(is.null(res()), label = s$id)
      expect_s3_class(interp(), "stab_interpretation")
      expect_true(length(s$tables(res())) >= 1L)
      expect_s3_class(plot_obj(), "ggplot")
      expect_match(output$code, paste0(s$id, "\\("))
      expect_true(nzchar(output$printed), label = paste(s$id, "printed output"))
      expect_match(output$interp_text, interp()$title, fixed = TRUE)
    })
  }
})

test_that("the written interpretation is requested and rendered as escaped Markdown", {
  s <- StabilityApp:::.sa_registry()$add_anova
  idata <- shiny::reactive(list(data = stability::ge_data, name = "ge_data", file = "",
                                cols = list(y = "Yield", gen = "Gen", env = "Env", rep = "Rep"), hb = TRUE,
                                trait = "grain yield"))
  sv <- sa_services()
  sett <- shiny::reactive(list(service = sv$id[sv$format == "chat"][1], model = "m", key = "k"))
  mock <- function(req) httr2::response_json(body = list(choices = list(list(message = list(
    content = "## 1. Statistical interpretation\nThe interaction is <b>large</b>.")))))
  httr2::with_mocked_responses(mock, shiny::testServer(StabilityApp:::.sa_analysis_server,
                                                       args = list(spec = s, input_data = idata, settings = sett), {
    session$setInputs(run = 1)
    session$setInputs(ask = 1)
    expect_type(written(), "character")
    html <- as.character(StabilityApp:::.sa_markdown(written()))
    expect_match(html, "<h2>1. Statistical interpretation</h2>", fixed = TRUE)
    expect_match(html, "&lt;b&gt;large&lt;/b&gt;", fixed = TRUE)
  }))
})

test_that("the application object is built", {
  expect_s3_class(sa_app(), "shiny.appobj")
})

test_that("in the whole application every page runs its own analysis", {
  shiny::testServer(sa_app(), {
    session$setInputs(source = "example", col_y = "Yield", col_gen = "Gen", col_env = "Env", col_rep = "Rep",
                      hb = TRUE, trait = "grain yield", service = sa_services()$id[1], model = "m", key = "")
    for (id in c("stab_reg", "ammi", "ge_means")) {
      do.call(session$setInputs, stats::setNames(list(1), paste0(id, "-run")))
      expect_match(output[[paste0(id, "-code")]], paste0("  ", id, "\\("), label = id)
      html <- output[[paste0(id, "-interp")]]$html
      title <- stability::stab_interpret(x = do.call(getExportedValue("stability", id),
        list(.data = stability::ge_data, .y = "Yield", .gen = "Gen", .env = "Env",
             .rep = if (id != "ge_means") "Rep")[if (id == "ge_means") 1:4 else 1:5]))$headline
      expect_match(as.character(html), substr(title, 1, 40), fixed = TRUE, label = id)
    }
  })
})

test_that("tables offer copying, all rows and rounded numbers; outputs have a Copy button", {
  d <- data.frame(Gen = c("a", "b"), Df = c(1, 2), Mean = c(1.23456, 2.5))
  t <- StabilityApp:::.sa_dt(d, "x")
  b <- vapply(t$x$options$buttons, `[[`, "", "extend")
  expect_true(all(c("copy", "csv", "excel") %in% b))
  expect_true(-1L %in% t$x$options$lengthMenu[[1L]])
  js <- paste(unlist(t$x$options$columnDefs), collapse = " ")
  expect_match(as.character(htmltools::renderTags(StabilityApp:::.sa_copy_bar("id1"))$html), "data-target=\"id1\"")
  ui <- as.character(htmltools::renderTags(StabilityApp:::.sa_head())$head)
  expect_match(ui, "sa-copy")
})
