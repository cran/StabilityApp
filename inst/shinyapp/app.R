source("ui.R")

# Define the server logic
server <- function(input = input, output = output, session = NULL) {
  # Reactive expression to use user-uploaded data or default ge_data if not uploaded
  data_input <- reactive({
    inFile <- input$file
    if (is.null(inFile)) {
      return(ge_data) # Use built-in ge_data if no file is uploaded
    } else {
      # Read the uploaded CSV file
      data <- read.csv(inFile$datapath)

      # Convert necessary columns to factors
      if ("Rep" %in% names(data)) {
        data$Rep <- as.factor(data$Rep)
      }
      if ("Gen" %in% names(data)) {
        data$Gen <- as.factor(data$Gen)
      }
      if ("Env" %in% names(data)) {
        data$Env <- as.factor(data$Env)
      }

      return(data)
    }
  })

  # Show the head of the dataset in the Data tab
  output$data_head <- DT::renderDT({
    data <- data_input()
    DT::datatable(data, filter = "top", options = list(pageLength = 5, autoWidth = TRUE))
  })

  # Perform the Individual ANOVA when the action button is clicked
  observeEvent(input$run_indiv_anova, {
    data <- data_input()

    # Check if necessary columns are present in the dataset
    if (!all(c("Yield", "Rep", "Gen", "Env") %in% names(data))) {
      output$indiv_anova_result <- renderPrint({
        "The dataset must contain 'Yield', 'Rep', 'Gen', and 'Env' columns."
      })
    } else {
      # Perform the Individual ANOVA
      indiv_anova_result <-
        indiv_anova(
          .data = data,
          .y = Yield,
          .rep = Rep,
          .gen = Gen,
          .env = Env
        )

      # Display the Individual ANOVA result
      output$indiv_anova_result <- renderPrint({
        indiv_anova_result
      })
    }
  })

  # Perform the Additive ANOVA when the action button is clicked
  observeEvent(input$run_add_anova, {
    data <- data_input()

    # Check if necessary columns are present in the dataset
    if (!all(c("Yield", "Rep", "Gen", "Env") %in% names(data))) {
      output$add_anova_result <- renderPrint({
        "The dataset must contain 'Yield', 'Rep', 'Gen', and 'Env' columns."
      })
    } else {
      # Perform the Additive ANOVA
      add_anova_result <-
        add_anova(
          .data = data,
          .y = Yield,
          .rep = Rep,
          .gen = Gen,
          .env = Env
        )

      # Display the ANOVA result
      output$add_anova_result <- renderPrint({
        add_anova_result$anova
      })
    }
  })

  # Perform the er_anova when the action button is clicked
  observeEvent(input$run_er_anova, {
    data <- data_input()

    if (!all(c("Yield", "Rep", "Gen", "Env") %in% names(data))) {
      output$er_anova_result <- renderPrint({
        "The dataset must contain 'Yield', 'Rep', 'Gen', and 'Env' columns."
      })
    } else {
      er_anova_result <-
        er_anova(
          .data = data,
          .y = Yield,
          .rep = Rep,
          .gen = Gen,
          .env = Env
        )

      output$er_anova_result1 <- renderPrint({
        er_anova_result[[1]]
      })

      output$er_anova_result2 <- renderPrint({
        er_anova_result[[2]] %>%
          mutate(across(where(is.numeric), ~ round(., 2)))
      })
    }
  })

  # Perform the ammi when the action button is clicked
  observeEvent(input$run_ammi, {
    data <- data_input()

    if (!all(c("Yield", "Rep", "Gen", "Env") %in% names(data))) {
      output$ammi_result <- renderPrint({
        "The dataset must contain 'Yield', 'Rep', 'Gen', and 'Env' columns."
      })
    } else {
      ammi_result <-
        ammi(
          .data = data,
          .y = Yield,
          .rep = Rep,
          .gen = Gen,
          .env = Env
        )


      output$ammi_result1 <- renderPrint({
        ammi_result[[1]]
      })

      output$ammi_result2 <- renderPrint({
        ammi_result[[2]] %>%
          mutate(across(where(is.numeric), ~ round(., 2)))
      })
    }
  })


  # Perform the ammi_biplot when the action button is clicked
  observeEvent(input$run_ammi_biplot, {
    data <- data_input()

    if (!all(c("Yield", "Rep", "Gen", "Env") %in% names(data))) {
      output$ammi_biplot_result <- renderPlot({
        "The dataset must contain 'Yield', 'Rep', 'Gen', and 'Env' columns."
      })
    } else {
      ammi_biplot_result <-
        ammi_biplot(
          .data = data,
          .y = Yield,
          .rep = Rep,
          .gen = Gen,
          .env = Env
        )


      output$ammi_biplot_result <- renderPlot({
        ammi_biplot_result[[1]] +
          ammi_biplot_result[[2]] + plot_layout(byrow = FALSE)
      })
    }
  })

  # Perform the gge_biplot when the action button is clicked
  observeEvent(input$run_gge_biplot, {
    data <- data_input()

    if (!all(c("Yield", "Rep", "Gen", "Env") %in% names(data))) {
      output$gge_biplot_result <- renderPlot({
        "The dataset must contain 'Yield', 'Rep', 'Gen', and 'Env' columns."
      })
    } else {
      gge_biplot_result <-
        gge_biplot(
          .data = data,
          .y = Yield,
          .rep = Rep,
          .gen = Gen,
          .env = Env
        )


      output$gge_biplot_result <- renderPlot({
        gge_biplot_result
      })
    }
  })


  # Perform the ammi_biplot when the action button is clicked
  observeEvent(input$run_ammi_biplot2, {
    data <- data_input()

    if (!all(c("Yield", "Rep", "Gen", "Env") %in% names(data))) {
      output$ammi_biplot2_result <- renderPlot({
        "The dataset must contain 'Yield', 'Rep', 'Gen', and 'Env' columns."
      })
    } else {
      ammi_biplot2_result <-
        ammi_biplot(
          .data = data,
          .y = Yield,
          .rep = Rep,
          .gen = Gen,
          .env = Env
        )


      output$ammi_biplot2_result <- renderPlot({
        ammi_biplot2_result[[1]] +
          ammi_biplot2_result[[2]] + plot_layout(byrow = FALSE)
      })
    }
  })

  # Perform the gge_biplot when the action button is clicked
  observeEvent(input$run_gge_biplot2, {
    data <- data_input()

    if (!all(c("Yield", "Rep", "Gen", "Env") %in% names(data))) {
      output$gge_biplot2_result <- renderPlot({
        "The dataset must contain 'Yield', 'Rep', 'Gen', and 'Env' columns."
      })
    } else {
      gge_biplot2_result <-
        gge_biplot(
          .data = data,
          .y = Yield,
          .rep = Rep,
          .gen = Gen,
          .env = Env
        )


      output$gge_biplot2_result <- renderPlot({
        gge_biplot2_result
      })
    }
  })


  # Perform the stab_reg when the action button is clicked
  observeEvent(input$run_stab_reg2, {
    data <- data_input()

    if (!all(c("Yield", "Rep", "Gen", "Env") %in% names(data))) {
      output$stab_reg2_result <- renderText({
        "The dataset must contain 'Yield', 'Rep', 'Gen', and 'Env' columns."
      })
    } else {
      stab_reg2_result <-
        stab_reg(
          .data = data,
          .y = Yield,
          .rep = Rep,
          .gen = Gen,
          .env = Env
        )


      output$stab_reg2_result <- DT::renderDT({
        DT::datatable(stab_reg2_result[[1]], filter = "top", options = list(pageLength = 25, autoWidth = TRUE)) %>%
          DT::formatRound(which(sapply(stab_reg2_result[[1]], is.numeric)), digits = 3)
      })

      output$stab_reg2_plot <- renderPlot({
        stab_reg2_result[[2]]
      })
    }
  })


  # Perform the stab_asv when the action button is clicked
  observeEvent(input$run_stab_asv, {
    data <- data_input() # Assuming data_input is a reactive function returning the dataset

    if (!all(c("Yield", "Rep", "Gen", "Env") %in% names(data))) {
      output$stab_asv_result <- renderText({
        "The dataset must contain 'Yield', 'Rep', 'Gen', and 'Env' columns."
      })
    } else {
      stab_asv_result <- stab_asv(
        .data = data,
        .y    = Yield,
        .rep  = Rep,
        .gen  = Gen,
        .env  = Env
      )

      # Display the result in a DT table
      output$stab_asv_result <- DT::renderDT({
        DT::datatable(stab_asv_result[[1]], filter = "top", options = list(pageLength = 25, autoWidth = TRUE)) %>%
          DT::formatRound(which(sapply(stab_asv_result[[1]], is.numeric)), digits = 3)
      })

      # CSV download handler
      output$download_asv_csv <- downloadHandler(
        filename = function() {
          paste("stab_asv_result", Sys.Date(), ".csv", sep = "")
        },
        content = function(file) {
          write.csv(stab_asv_result[[1]], file, row.names = FALSE)
        }
      )

      # PDF download handler
      output$download_asv_pdf <- downloadHandler(
        filename = function() {
          paste("stab_asv_result", Sys.Date(), ".pdf", sep = "")
        },
        content = function(file) {
          # Use gridExtra to render the table in a PDF
          pdf(file)
          gridExtra::grid.table(stab_asv_result[[1]])
          dev.off()
        }
      )
    }
  })

  # Perform the stab_dist when the action button is clicked
  observeEvent(input$run_stab_dist, {
    data <- data_input()

    if (!all(c("Yield", "Rep", "Gen", "Env") %in% names(data))) {
      output$stab_dist_result <- renderText({
        "The dataset must contain 'Yield', 'Rep', 'Gen', and 'Env' columns."
      })
    } else {
      stab_dist_result <-
        stab_dist(
          .data = data,
          .y = Yield,
          .rep = Rep,
          .gen = Gen,
          .env = Env
        )


      output$stab_dist_result <- DT::renderDT({
        DT::datatable(stab_dist_result[[1]], filter = "top", options = list(pageLength = 25, autoWidth = TRUE)) %>%
          DT::formatRound(which(sapply(stab_dist_result[[1]], is.numeric)), digits = 3)
      })
    }
  })


  # Perform the stab_fox when the action button is clicked
  observeEvent(input$run_stab_fox, {
    data <- data_input()

    if (!all(c("Yield", "Rep", "Gen", "Env") %in% names(data))) {
      output$stab_fox_result <- renderText({
        "The dataset must contain 'Yield', 'Rep', 'Gen', and 'Env' columns."
      })
    } else {
      stab_fox_result <-
        stab_fox(
          .data = data,
          .y = Yield,
          .rep = Rep,
          .gen = Gen,
          .env = Env
        )


      output$stab_fox_result <- DT::renderDT({
        DT::datatable(stab_fox_result[[1]], filter = "top", options = list(pageLength = 25, autoWidth = TRUE)) %>%
          DT::formatRound(which(sapply(stab_fox_result[[1]], is.numeric)), digits = 3)
      })
    }
  })


  # Perform the stab_kang when the action button is clicked
  observeEvent(input$run_stab_kang, {
    data <- data_input()

    if (!all(c("Yield", "Rep", "Gen", "Env") %in% names(data))) {
      output$stab_kang_result <- renderText({
        "The dataset must contain 'Yield', 'Rep', 'Gen', and 'Env' columns."
      })
    } else {
      stab_kang_result <-
        stab_kang(
          .data = data,
          .y = Yield,
          .rep = Rep,
          .gen = Gen,
          .env = Env
        )


      output$stab_kang_result <- DT::renderDT({
        DT::datatable(stab_kang_result[[1]], filter = "top", options = list(pageLength = 25, autoWidth = TRUE)) %>%
          DT::formatRound(which(sapply(stab_kang_result[[1]], is.numeric)), digits = 3)
      })
    }
  })


  # Perform the stab_masv when the action button is clicked
  observeEvent(input$run_stab_masv, {
    data <- data_input()

    if (!all(c("Yield", "Rep", "Gen", "Env") %in% names(data))) {
      output$stab_masv_result <- renderText({
        "The dataset must contain 'Yield', 'Rep', 'Gen', and 'Env' columns."
      })
    } else {
      stab_masv_result <-
        stab_masv(
          .data = data,
          .y = Yield,
          .rep = Rep,
          .gen = Gen,
          .env = Env
        )


      output$stab_masv_result <- DT::renderDT({
        DT::datatable(stab_masv_result[[1]], filter = "top", options = list(pageLength = 25, autoWidth = TRUE)) %>%
          DT::formatRound(which(sapply(stab_masv_result[[1]], is.numeric)), digits = 3)
      })
    }
  })


  # Perform the stab_measures when the action button is clicked
  observeEvent(input$run_stab_measures, {
    data <- data_input()

    if (!all(c("Yield", "Rep", "Gen", "Env") %in% names(data))) {
      output$stab_measures_result <- renderText({
        "The dataset must contain 'Yield', 'Rep', 'Gen', and 'Env' columns."
      })
    } else {
      stab_measures_result <-
        stab_measures(
          .data = data,
          .y = Yield,
          .gen = Gen,
          .env = Env
        )


      output$stab_measures_result <- DT::renderDT({
        DT::datatable(stab_measures_result[[1]], filter = "top", options = list(pageLength = 25, autoWidth = TRUE)) %>%
          DT::formatRound(which(sapply(stab_measures_result[[1]], is.numeric)), digits = 3)
      })

      output$stab_measures_plot <-
        renderPlot({
          stab_measures_result[[2]] +
            stab_measures_result[[3]] +
            stab_measures_result[[4]] +
            plot_layout(byrow = FALSE)
        })
    }
  })


  # Perform the stab_par when the action button is clicked
  observeEvent(input$run_stab_par, {
    data <- data_input()

    if (!all(c("Yield", "Rep", "Gen", "Env") %in% names(data))) {
      output$stab_par_result <- renderText({
        "The dataset must contain 'Yield', 'Rep', 'Gen', and 'Env' columns."
      })
    } else {
      stab_par_result <-
        stab_par(
          .data = data,
          .y = Yield,
          .rep = Rep,
          .gen = Gen,
          .env = Env
        )


      output$stab_par_result1 <- renderPrint({
        stab_par_result[[1]]
      })

      output$stab_par_result2 <- DT::renderDT({
        DT::datatable(stab_par_result[[2]], filter = "top", options = list(pageLength = 25, autoWidth = TRUE)) %>%
          DT::formatRound(which(sapply(stab_par_result[[2]], is.numeric)), digits = 3)
      })

      output$stab_par_result3 <- DT::renderDT({
        DT::datatable(stab_par_result[[3]], filter = "top", options = list(pageLength = 25, autoWidth = TRUE)) %>%
          DT::formatRound(which(sapply(stab_par_result[[3]], is.numeric)), digits = 3)
      })
    }
  })


  # Perform the stab_reg when the action button is clicked
  observeEvent(input$run_stab_reg, {
    data <- data_input()

    if (!all(c("Yield", "Rep", "Gen", "Env") %in% names(data))) {
      output$stab_reg_result <- renderText({
        "The dataset must contain 'Yield', 'Rep', 'Gen', and 'Env' columns."
      })
    } else {
      stab_reg_result <-
        stab_reg(
          .data = data,
          .y = Yield,
          .rep = Rep,
          .gen = Gen,
          .env = Env
        )


      output$stab_reg_result <- DT::renderDT({
        DT::datatable(stab_reg_result[[1]], filter = "top", options = list(pageLength = 25, autoWidth = TRUE)) %>%
          DT::formatRound(which(sapply(stab_reg_result[[1]], is.numeric)), digits = 3)
      })

      output$stab_reg_plot <- renderPlot({
        stab_reg_result[[2]]
      })
    }
  })

  # Perform the ge_means when the action button is clicked
  observeEvent(input$run_ge_means, {
    data <- data_input()

    if (!all(c("Yield", "Rep", "Gen", "Env") %in% names(data))) {
      output$ge_means_result <- renderText({
        "The dataset must contain 'Yield', 'Rep', 'Gen', and 'Env' columns."
      })
    } else {
      ge_means_result <-
        ge_means(
          .data = data,
          .y = Yield,
          .gen = Gen,
          .env = Env
        )


      output$ge_means_result1 <- DT::renderDT({
        DT::datatable(ge_means_result[[1]], filter = "top", options = list(pageLength = 5)) %>%
          DT::formatRound(which(sapply(ge_means_result[[1]], is.numeric)), digits = 3)
      })

      output$ge_means_result3 <- DT::renderDT({
        DT::datatable(ge_means_result[[3]], filter = "top", options = list(pageLength = 5)) %>%
          DT::formatRound(which(sapply(ge_means_result[[3]], is.numeric)), digits = 3)
      })

      output$ge_means_result4 <- DT::renderDT({
        DT::datatable(ge_means_result[[4]], filter = "top", options = list(pageLength = 5)) %>%
          DT::formatRound(which(sapply(ge_means_result[[4]], is.numeric)), digits = 3)
      })

      output$ge_means_result5 <- DT::renderDT({
        ge_means_result[[5]]
      })
    }
  })


  # Perform the ge_effects when the action button is clicked
  observeEvent(input$run_ge_effects, {
    data <- data_input()

    if (!all(c("Yield", "Rep", "Gen", "Env") %in% names(data))) {
      output$ge_effects_result <- renderText({
        "The dataset must contain 'Yield', 'Rep', 'Gen', and 'Env' columns."
      })
    } else {
      ge_effects_result <-
        ge_effects(
          .data = data,
          .y = Yield,
          .gen = Gen,
          .env = Env
        )


      output$ge_effects_result1 <- DT::renderDT({
        DT::datatable(ge_effects_result[[1]], filter = "top", options = list(pageLength = 10)) %>%
          DT::formatRound(which(sapply(ge_effects_result[[1]], is.numeric)), digits = 3)
      })

      output$ge_effects_result2 <- DT::renderDT({
        DT::datatable(as.data.frame(ge_effects_result[[2]]), filter = "top", options = list(pageLength = 10)) %>%
          DT::formatRound(which(sapply(as.data.frame(ge_effects_result[[2]]), is.numeric)), digits = 3)
      })

      output$ge_effects_result3 <- DT::renderDT({
        DT::datatable(as.data.frame(ge_effects_result[[3]]), filter = "top", options = list(pageLength = 10)) %>%
          DT::formatRound(which(sapply(as.data.frame(ge_effects_result[[3]]), is.numeric)), digits = 3)
      })
    }
  })
}

shinyApp(ui = ui, server = server)
