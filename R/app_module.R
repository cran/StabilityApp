#-------------------------------------------------------------------------------------------------------------------
# One module for every analysis: options, run, tables, plot, interpretation, written interpretation, R code
#-------------------------------------------------------------------------------------------------------------------

.sa_analysis_ui <- function(id, spec) {
  ns <- shiny::NS(id)
  opts <- lapply(names(spec$options), function(nm) {
    o <- spec$options[[nm]]
    if (o$type == "numeric") shiny::numericInput(ns(paste0("opt", nm)), o$label, o$value, min = o$min, max = o$max,
                                                 step = o$step)
    else shiny::selectInput(ns(paste0("opt", nm)), o$label, o$choices, o$value, selectize = FALSE)
  })
  bslib::layout_sidebar(
      sidebar = bslib::sidebar(
          width = 280
        , shiny::h5(spec$label)
        , opts
        , shiny::actionButton(ns("run"), "Run the analysis", class = "btn-primary w-100", icon = shiny::icon("play"))
        , shiny::helpText("Columns and trait are set on the Data page.")
        )
    , bslib::navset_card_tab(
          id = ns("tabs")
        , bslib::nav_panel("Interpretation", .sa_copy_bar(ns("interp_text")),
                           shiny::uiOutput(ns("interp")),
                           shiny::tags$pre(id = ns("interp_text"), class = "shiny-text-output", style = "display:none"),
                           shiny::hr(),
                           shiny::h5("Longer written interpretation (optional)"),
                           shiny::p(class = "text-muted small",
                                    "Sent to the external text service chosen on the Settings page, with your key: the",
                                    "interpretation above, its facts and the first rows of the main table. No data file is sent."),
                           shiny::actionButton(ns("ask"), "Request a written interpretation", icon = shiny::icon("pen-to-square"),
                                               class = "w-auto"),
                           shiny::uiOutput(ns("written")))
        , bslib::nav_panel("Output", .sa_copy_bar(ns("printed")), shiny::verbatimTextOutput(ns("printed")))
        , bslib::nav_panel("Tables", shiny::uiOutput(ns("tables")))
        , bslib::nav_panel("Plot", if (length(spec$plots) > 1L)
                             shiny::radioButtons(ns("which"), NULL, spec$plots, inline = TRUE),
                           shiny::plotOutput(ns("plot"), height = "620px"),
                           shiny::div(shiny::downloadButton(ns("plot_png"), "PNG", class = "w-auto"),
                                      shiny::downloadButton(ns("plot_pdf"), "PDF", class = "w-auto")))
        , bslib::nav_panel("R code", .sa_copy_bar(ns("code")), shiny::verbatimTextOutput(ns("code")),
                           shiny::downloadButton(ns("report"), "Interpretation (Markdown)", class = "w-auto"))
        )
    )
}

.sa_analysis_server <- function(id, spec, input_data, settings) {
  # evaluate the arguments now: the modules are created in a loop, and a promise evaluated later would see the
  # last analysis of the loop
  force(spec)
  force(input_data)
  force(settings)
  shiny::moduleServer(id, function(input, output, session) {
    ns  <- session$ns
    res <- shiny::reactiveVal(NULL)
    written <- shiny::reactiveVal(NULL)

    args <- shiny::reactive({
      d <- input_data()
      a <- list(.data = d$data, .y = d$cols$y, .gen = d$cols$gen, .env = d$cols$env)
      if (spec$rep != "no" && nzchar(d$cols$rep)) a$.rep <- d$cols$rep
      for (nm in names(spec$options)) a[[nm]] <- input[[paste0("opt", nm)]]
      if (isTRUE(spec$hb)) a$higher_better <- isTRUE(d$hb)
      a
    })

    shiny::observeEvent(input$run, {
      d <- input_data()
      if (spec$rep == "required" && !nzchar(d$cols$rep)) {
        shiny::showNotification("This analysis needs the replicate column (Data page).", type = "error")
        return()
      }
      r <- tryCatch(
          shiny::withProgress(message = paste("Running:", spec$label), value = 0.5,
                              do.call(getExportedValue("stability", spec$id), args()))
        , error = function(e) e
        )
      if (inherits(r, "error")) {
        shiny::showNotification(conditionMessage(r), type = "error", duration = 10)
        return()
      }
      res(r)
      written(NULL)
    })

    interp <- shiny::reactive({
      shiny::req(res())
      stability::stab_interpret(x = res(), higher_better = input_data()$hb, top = 5L)
    })

    output$interp_text <- shiny::renderText(paste(format(interp(), style = "text", width = 100L), collapse = "\n"))

    # the result as R prints it: formatted like the console and easy to copy
    output$printed <- shiny::renderPrint({
      shiny::req(res())
      op <- options(width = 120L)
      on.exit(options(op))
      r <- res()
      # results whose print() draws a plot show their data instead
      if (inherits(r, "ggplot")) print(attr(r, "gge")[c("winners", "vertex", "atc", "envs", "percent")])
      else if (inherits(r, "stab_ammi_biplot")) print(r$scores) else print(r)
    })

    output$interp <- shiny::renderUI({
      if (is.null(res())) return(shiny::p(class = "text-muted", "Run the analysis to see its interpretation."))
      .sa_interp_html(interp())
    })

    output$tables <- shiny::renderUI({
      shiny::req(res())
      tabs <- spec$tables(res())
      lapply(seq_along(tabs), function(k) {
        oid <- paste0("tab", k)
        output[[oid]] <- DT::renderDT(.sa_dt(tabs[[k]], names(tabs)[k]))
        shiny::tagList(shiny::h5(names(tabs)[k]), DT::DTOutput(ns(oid)), shiny::br())
      })
    })

    plot_obj <- shiny::reactive({
      shiny::req(res())
      r <- res()
      if (inherits(r, "ggplot")) return(r)
      w <- if (length(spec$plots) > 1L) input$which else unname(spec$plots[1L])
      plot(r, which = w)
    })
    output$plot <- shiny::renderPlot(plot_obj(), res = 110)
    output$plot_png <- shiny::downloadHandler(
        filename = function() paste0(spec$id, ".png")
      , content  = function(file) ggplot2::ggsave(file, plot_obj(), width = 10, height = 7, dpi = 200)
      )
    output$plot_pdf <- shiny::downloadHandler(
        filename = function() paste0(spec$id, ".pdf")
      , content  = function(file) ggplot2::ggsave(file, plot_obj(), width = 10, height = 7)
      )

    output$code <- shiny::renderText({
      d <- input_data()
      o <- vapply(names(spec$options), function(nm) deparse(input[[paste0("opt", nm)]]), "")
      if (isTRUE(spec$hb)) o <- c(o, higher_better = as.character(isTRUE(d$hb)))
      .sa_code(spec, d, o)
    })

    output$report <- shiny::downloadHandler(
        filename = function() paste0(spec$id, "-interpretation.md")
      , content  = function(file) {
          txt <- format(interp(), style = "markdown")
          if (!is.null(written()) && !inherits(written(), "error")) txt <- c(txt, "", "## Written interpretation", "",
                                                                            written())
          writeLines(txt, file)
        }
      )

    shiny::observeEvent(input$ask, {
      if (is.null(res())) {
        shiny::showNotification("Run the analysis first.", type = "warning")
        return()
      }
      s <- settings()
      p <- sa_prompt(x = res(), higher_better = input_data()$hb, trait = input_data()$trait)
      out <- tryCatch(
          shiny::withProgress(message = "Waiting for the text service", value = 0.5,
                              sa_ask(service = s$service, prompt = p, model = s$model, key = s$key))
        , error = function(e) e
        )
      written(out)
    })

    output$written <- shiny::renderUI({
      w <- written()
      if (is.null(w)) return(NULL)
      if (inherits(w, "error")) return(shiny::div(class = "alert alert-danger mt-3", conditionMessage(w)))
      shiny::div(class = "card card-body mt-3 sa-written", .sa_markdown(w))
    })

    invisible(res)
  })
}

# the interpretation of stab_interpret() as HTML
.sa_interp_html <- function(it) {
  li <- function(s) if (length(s)) shiny::tags$ul(lapply(s, shiny::tags$li)) else NULL
  shiny::tagList(
      shiny::div(class = "alert alert-primary", shiny::strong(it$headline))
    , if (length(it$findings)) shiny::tagList(shiny::h6("Findings"), li(it$findings))
    , if (length(it$recommendations)) shiny::tagList(shiny::h6("Recommendations"), li(it$recommendations))
    , if (length(it$cautions)) shiny::tagList(shiny::h6("Cautions"), li(it$cautions))
    , shiny::tags$details(shiny::tags$summary("How this method is read"), shiny::p(it$guide))
    )
}

# text of a service rendered as Markdown, with any HTML of the text escaped
.sa_markdown <- function(txt) {
  shiny::HTML(commonmark::markdown_html(htmltools::htmlEscape(txt), smart = TRUE))
}

.sa_dt <- function(d, title) {
  num <- which(vapply(d, is.numeric, NA))
  t <- DT::datatable(d, rownames = FALSE, extensions = "Buttons", style = "auto",
                     class = "compact stripe hover cell-border",
                     options = list(pageLength = 15L, scrollX = TRUE, dom = "Blfrtip",
                                    lengthMenu = list(c(15L, 50L, -1L), c("15", "50", "All")),
                                    buttons = list(list(extend = "copy", title = NULL, className = .sa_btn),
                                                   list(extend = "csv", title = title, className = .sa_btn),
                                                   list(extend = "excel", title = title, className = .sa_btn))))
  # p-values with 3 significant digits; whole numbers (degrees of freedom, ranks, counts) without decimals; the
  # others rounded to 3 decimals
  pcol  <- num[grepl("^Pr\\(|^p\\.|^p$|p\\.value|^Prob", names(d)[num])]
  whole <- setdiff(num[vapply(d[num], function(v) all(is.na(v) | v == round(v)), NA)], pcol)
  dec   <- setdiff(num, c(whole, pcol))
  if (length(pcol)) t <- DT::formatSignif(t, pcol, digits = 3L)
  if (length(dec)) t <- DT::formatRound(t, dec, digits = 3L)
  if (length(whole)) t <- DT::formatRound(t, whole, digits = 0L)
  t
}

.sa_btn <- "btn btn-sm btn-outline-secondary me-1"

# a bar with a Copy button for the text of an output
.sa_copy_bar <- function(target) {
  shiny::div(class = "sa-bar",
             shiny::tags$button(type = "button", class = "btn btn-outline-secondary btn-sm sa-copy", `data-target` = target,
                                shiny::icon("copy"), shiny::span(class = "sa-copy-label", "Copy")))
}

# the R code that repeats an analysis, every argument on its own line
.sa_code <- function(spec, d, opts) {
  a <- c(.data = d$name, .y = d$cols$y, .rep = if (spec$rep != "no" && nzchar(d$cols$rep)) d$cols$rep,
         .gen = d$cols$gen, .env = d$cols$env, opts)
  w <- max(nchar(names(a)))
  body <- paste0("    ", c(" ", rep(",", length(a) - 1L)), " ", formatC(names(a), width = -w), " = ", a, collapse = "\n")
  paste0("library(stability)\n", if (d$name == "ge_data") "data(ge_data)\n" else
           sprintf("%s <- data.table::fread(\"%s\")\n", d$name, d$file),
         "fit <-\n  ", spec$id, "(\n", body, "\n    )\nfit\nstab_interpret(\n    x             = fit\n  , higher_better = ",
         d$hb, "\n  , top           = 5\n  )\n")
}
