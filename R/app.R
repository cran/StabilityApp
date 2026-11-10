#' Stability analysis of multi-environment trials in a web browser
#'
#' @description
#' \code{StabilityApp()} starts the application: load a trial (the example \code{ge_data} of \pkg{stability} or a CSV
#' file with one row per plot), choose the columns, and run any of the twenty analyses of \pkg{stability}. Each
#' analysis shows an interpretation written for its method and computed from its results
#' (\code{stability::stab_interpret()}), its tables (with CSV and Excel export), its plots (PNG and PDF), and the R
#' code that repeats it. A longer written interpretation can be requested from an external text service with your
#' own key (\code{\link{sa_services}()}; optional).
#'
#' @details
#' The analyses: means and rankings, interaction effects; analysis of variance in each environment, combined, and
#' Eberhart and Russell's; Finlay and Wilkinson's regression, variance measures, Shukla's stability variance with
#' Kang's yield-stability statistic, Kang's rank-sum, Fox's top third; AMMI analysis, AMMI biplots, AMMI stability
#' value, modified AMMI stability value, AMMI stability distance; GGE biplot (which won where, mean against stability,
#' discrimination).
#'
#' The key of a text service is typed on the Settings page (it stays in the session and is not saved) or read from
#' the environment variable named in \code{sa_services()}.
#'
#' @param launch.browser passed to \code{shiny::runApp()}: \code{TRUE} (default in an interactive session) opens the
#'   application in the web browser.
#' @param ... further arguments to \code{shiny::runApp()}, such as \code{port}.
#' @return Called for its side effect; returns the value of \code{shiny::runApp()} when the application is closed.
#'   \code{sa_app()} returns the application object (for \code{shiny::runApp()}, \code{shiny::testServer()} or
#'   deployment).
#' @author Muhammad Yaseen (\email{myaseen208@@gmail.com}), Kent M. Eskridge (\email{keskridge1@@unl.edu})
#' @seealso \code{\link{sa_prompt}}, \code{\link{sa_services}}
#' @examples
#' if (interactive()) {
#'   StabilityApp(
#'       launch.browser = TRUE
#'     )
#' }
#' @export
StabilityApp <- function(launch.browser = interactive(), ...) {
  shiny::runApp(sa_app(), launch.browser = launch.browser, ...)
}

#' @rdname StabilityApp
#' @export
sa_app <- function() shiny::shinyApp(ui = .sa_ui(), server = .sa_server)

.sa_theme <- function() {
  bslib::bs_theme(version = 5, bootswatch = "flatly", primary = "#2c5f8a",
                  base_font = .sa_font("IBM Plex Sans", "sans"), heading_font = .sa_font("IBM Plex Sans", "sans"),
                  code_font = .sa_font("IBM Plex Mono", "mono"))
}

# a Google font with system fallbacks (used when the browser cannot reach Google Fonts, for instance offline)
.sa_font <- function(family, kind = c("sans", "mono")) {
  fb <- switch(match.arg(kind),
               sans = c("system-ui", "-apple-system", "Segoe UI", "Helvetica Neue", "Arial", "sans-serif"),
               mono = c("SFMono-Regular", "Menlo", "Consolas", "Liberation Mono", "DejaVu Sans Mono", "monospace"))
  do.call(bslib::font_collection, c(list(bslib::font_google(family, local = FALSE)), as.list(fb)))
}

# styles of the outputs and the script of the Copy buttons (plain text of the element named in data-target, with a
# fallback for pages served without https)
.sa_head <- function() {
  shiny::tags$head(
      shiny::tags$style(shiny::HTML(paste(
          ".sa-written h2{font-size:1.15rem;margin-top:1rem} .sa-written p{margin-bottom:.6rem}"
        , "pre.shiny-text-output{font-family:var(--bs-font-monospace);font-size:.85em;line-height:1.5;white-space:pre;overflow-x:auto;background:#f6f8fa;"
        , "  border:1px solid #dde3ea;border-radius:6px;padding:10px 12px;}"
        , "table.dataTable thead th{background:#2c5f8a;color:#fff;font-weight:600;}"
        , ".sa-bar{display:flex;gap:.4rem;justify-content:flex-end;margin-bottom:.4rem}"
        , ".dataTables_wrapper .dt-buttons{float:left;margin:0 1rem .5rem 0}"
        , ".dataTables_wrapper .dataTables_length{float:left;margin-bottom:.5rem}"
        , ".dataTables_wrapper .dataTables_filter{float:right;margin-bottom:.5rem}"
        , ".dataTables_wrapper .dataTables_length label,.dataTables_wrapper .dataTables_filter label{display:inline-flex;"
        , "  align-items:center;gap:.4rem;font-weight:400;margin:0}"
        , ".dataTables_wrapper select,.dataTables_wrapper input[type=search]{display:inline-block;width:auto}"
        , ".dataTables_wrapper .dataTables_info{float:left;padding-top:.6rem;font-size:.9em}"
        , ".dataTables_wrapper .dataTables_paginate{float:right;padding-top:.4rem}"
        , ".dataTables_wrapper table.dataTable{clear:both}"
        , ".dataTables_wrapper::after{content:'';display:table;clear:both}")))
    , shiny::tags$script(shiny::HTML(paste(
          "$(document).on('click', '.sa-copy', function() {"
        , "  var el = document.getElementById($(this).data('target')); if (!el) return;"
        , "  var txt = el.innerText || el.textContent, b = $(this);"
        , "  var done = function() { b.find('.sa-copy-label').text('Copied'); setTimeout(function() { b.find('.sa-copy-label').text('Copy'); }, 1500); };"
        , "  if (navigator.clipboard && window.isSecureContext) { navigator.clipboard.writeText(txt).then(done); return; }"
        , "  var ta = document.createElement('textarea'); ta.value = txt; document.body.appendChild(ta); ta.select();"
        , "  document.execCommand('copy'); document.body.removeChild(ta); done();"
        , "});", sep = "\n")))
    )
}

.sa_ui <- function() {
  reg <- .sa_registry()
  groups <- unique(vapply(reg, `[[`, "", "group"))
  menus <- lapply(unname(groups), function(g) {
    items <- Filter(function(s) s$group == g, reg)
    do.call(bslib::nav_menu, c(list(title = g), unname(lapply(items, function(s) {
      bslib::nav_panel(s$label, value = s$id, .sa_analysis_ui(s$id, s))
    }))))
  })
  do.call(bslib::page_navbar, c(
      list(title = "StabilityApp", id = "nav", theme = .sa_theme(), fillable = FALSE, header = .sa_head())
    , list(bslib::nav_panel("Home", value = "home", .sa_home()),
           bslib::nav_panel("Data", value = "data", .sa_data_ui()))
    , menus
    , list(bslib::nav_panel("Settings", value = "settings", .sa_settings_ui()))
    ))
}

.sa_home <- function() {
  card <- function(title, text) bslib::card(bslib::card_header(title), shiny::p(text))
  shiny::tagList(
      shiny::h2("Stability analysis of multi-environment trials")
    , shiny::p(class = "lead", "Which genotypes yield well, and which yield well everywhere?")
    , bslib::layout_column_wrap(
          width = 1 / 3
        , card("1. Data", "Use the example trial (60 wheat genotypes, 11 environments) or upload a CSV file with one row per plot, then choose the columns.")
        , card("2. Analysis", "Start with the combined analysis of variance; if the interaction is significant, look at regression, AMMI and the GGE biplot.")
        , card("3. Interpretation", "Every result comes with an interpretation for its method, computed from its numbers; a longer written text can be requested from a text service.")
        )
    , shiny::p(class = "text-muted mt-3",
               sprintf("StabilityApp %s with stability %s.", utils::packageVersion("StabilityApp"),
                       utils::packageVersion("stability")))
    )
}

.sa_data_ui <- function() {
  bslib::layout_sidebar(
      sidebar = bslib::sidebar(
          width = 320
        , shiny::radioButtons("source", "Data", c(`Example: ge_data` = "example", `Upload a CSV file` = "upload"))
        , shiny::conditionalPanel("input.source == 'upload'", shiny::fileInput("file", NULL, accept = c(".csv", ".txt")))
        , shiny::uiOutput("columns")
        , shiny::textInput("trait", "Trait (for the text)", "grain yield")
        , shiny::checkboxInput("hb", "Higher values are better", TRUE)
        )
    , bslib::card(bslib::card_header("Summary"), shiny::verbatimTextOutput("data_summary"))
    , bslib::card(bslib::card_header("First rows"), DT::DTOutput("data_head"))
    )
}

.sa_settings_ui <- function() {
  sv <- sa_services()
  bslib::layout_columns(
      col_widths = c(6, 6)
    , bslib::card(
          bslib::card_header("Text service for the longer written interpretation (optional)")
        , shiny::selectInput("service", "Service", stats::setNames(sv$id, sv$label), selectize = FALSE)
        , shiny::textInput("model", "Model", sv$model[1L])
        , shiny::passwordInput("key", "Key", "")
        , shiny::helpText("The key stays in this session only. Leave it empty to use the environment variable of the service.")
        , shiny::actionButton("test", "Test the connection")
        , shiny::uiOutput("test_result")
        )
    , bslib::card(
          bslib::card_header("What is sent")
        , shiny::p("Only the text of one analysis: its name, the trait, the size of the trial, the interpretation computed by the stability package with its facts, and the first rows of its main table. No data file is sent. The computed interpretation is always available without any service.")
        )
    )
}

.sa_server <- function(input, output, session) {
  raw <- shiny::reactive({
    if (identical(input$source, "upload")) {
      shiny::req(input$file)
      d <- as.data.frame(data.table::fread(input$file$datapath, showProgress = FALSE))
      list(data = d, name = "trial", file = input$file$name)
    } else {
      list(data = stability::ge_data, name = "ge_data", file = "")
    }
  })
  output$columns <- shiny::renderUI({
    nm <- names(raw()$data)
    pick <- function(pat, default = nm[1L]) { h <- grep(pat, nm, ignore.case = TRUE, value = TRUE); if (length(h)) h[1L] else default }
    shiny::tagList(
        shiny::selectInput("col_y", "Response", nm, pick("^(yield|y|response|gy)$"), selectize = FALSE)
      , shiny::selectInput("col_gen", "Genotype", nm, pick("^(gen|genotype|variety|entry|cultivar)$"), selectize = FALSE)
      , shiny::selectInput("col_env", "Environment", nm, pick("^(env|environment|location|site|loc)$"), selectize = FALSE)
      , shiny::selectInput("col_rep", "Replicate (block)", c(`(none)` = "", nm), pick("^(rep|replicate|block)$", ""),
                           selectize = FALSE)
      )
  })
  input_data <- shiny::reactive({
    r <- raw()
    shiny::req(input$col_y, input$col_gen, input$col_env)
    c(r, list(cols = list(y = input$col_y, gen = input$col_gen, env = input$col_env, rep = input$col_rep %||% ""),
              hb = isTRUE(input$hb), trait = input$trait))
  })
  output$data_head <- DT::renderDT(DT::datatable(utils::head(raw()$data, 500L), rownames = FALSE,
                                                 options = list(pageLength = 10L, scrollX = TRUE)))
  output$data_summary <- shiny::renderPrint({
    d <- input_data()
    x <- d$data
    y <- x[[d$cols$y]]
    cat(sprintf("Rows: %d\nGenotypes: %d\nEnvironments: %d\nReplicates: %s\n%s: mean %s, range %s to %s\n",
                nrow(x), length(unique(x[[d$cols$gen]])), length(unique(x[[d$cols$env]])),
                if (nzchar(d$cols$rep)) length(unique(x[[d$cols$rep]])) else "(none)", d$cols$y,
                format(signif(mean(y, na.rm = TRUE), 5)), format(min(y, na.rm = TRUE)), format(max(y, na.rm = TRUE))))
  })
  shiny::observeEvent(input$service, {
    sv <- sa_services()
    shiny::updateTextInput(session, "model", value = sv$model[sv$id == input$service])
  })
  settings <- shiny::reactive(list(service = input$service, model = input$model, key = input$key %||% ""))
  shiny::observeEvent(input$test, {
    s <- settings()
    r <- tryCatch(sa_ask(service = s$service, prompt = "Answer with the single word OK.", model = s$model, key = s$key,
                         max_tokens = 20, timeout = 30), error = function(e) e)
    output$test_result <- shiny::renderUI(
      if (inherits(r, "error")) shiny::div(class = "alert alert-danger mt-2", conditionMessage(r))
      else shiny::div(class = "alert alert-success mt-2", "Connected: ", r)
    )
  })
  lapply(.sa_registry(), function(s) .sa_analysis_server(s$id, s, input_data, settings))
  invisible(NULL)
}

`%||%` <- function(a, b) if (is.null(a)) b else a
