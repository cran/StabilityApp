# Load required libraries
library(dplyr)
library(DT)
library(gridExtra)
library(patchwork)
library(stability)
library(shiny)
library(shinydashboard)
library(shinydashboardPlus)


# Define the UI
ui <- dashboardPage(
  dashboardHeader(
    title = tagList(span(class = "logo-lg", "Stability Analysis App", style = "color: maroon; font-size: inherit;"), icon("leaf")),
    userOutput("user")
  ),

  # Sidebar for dataset selection and navigation between multiple pages
  dashboardSidebar(
    sidebarMenu(
      menuItem("Home", tabName = "Home", icon = icon("home")),
      menuItem("Introduction", tabName = "Intro", icon = icon("computer")),
      menuItem("Data", tabName = "tab-data", icon = icon("folder-open")),
      menuItem("ANOVA", icon = icon("cogs"), startExpanded = TRUE,
               menuSubItem("Individual ANOVAs", tabName = "tab-indiv_anova", icon = icon("chart-bar")),
               menuSubItem("Additive ANOVA", tabName = "tab-add_anova", icon = icon("cogs")),
               menuSubItem("Eberhart & Russel ANOVA", tabName = "tab-er_anova", icon = icon("cogs"))
      ),
      menuItem("AMMI", icon = icon("cogs"), startExpanded = TRUE,
               menuSubItem("AMMI", tabName = "tab-ammi", icon = icon("cogs")),
               menuSubItem("AMMI Biplot", tabName = "tab-ammi_biplot", icon = icon("cogs")),
               menuSubItem("GGE Biplot", tabName = "tab-gge_biplot", icon = icon("chart-bar"))
      ),
      menuItem("Biplots", icon = icon("cogs"), startExpanded = TRUE,
               menuSubItem("AMMI Biplot", tabName = "tab-ammi_biplot2", icon = icon("cogs")),
               menuSubItem("GGE Biplot", tabName = "tab-gge_biplot2", icon = icon("chart-bar"))
      ),
      menuItem("Regression", tabName = "tab-stab_reg2", icon = icon("cogs")),
      menuItem("Stability Measures", icon = icon("cogs"), startExpanded = TRUE,
               menuSubItem("ASV", tabName = "tab-stab_asv", icon = icon("cogs")),
               menuSubItem("Dist", tabName = "tab-stab_dist", icon = icon("chart-bar")),
               menuSubItem("Fox", tabName = "tab-stab_fox", icon = icon("chart-bar")),
               menuSubItem("Kang", tabName = "tab-stab_kang", icon = icon("chart-bar")),
               menuSubItem("MASV", tabName = "tab-stab_masv", icon = icon("chart-bar")),
               menuSubItem("Measures", tabName = "tab-stab_measures", icon = icon("chart-bar")),
               menuSubItem("Par", tabName = "tab-stab_par", icon = icon("chart-bar")),
               menuSubItem("Regression", tabName = "tab-stab_reg", icon = icon("chart-bar"))
      ),
      menuItem("Means & Effects", icon = icon("cogs"), startExpanded = TRUE,
               menuSubItem("Means", tabName = "tab-ge_means", icon = icon("cogs")),
               menuSubItem("Effects", tabName = "tab-ge_effects", icon = icon("chart-bar"))
               )
    )
    , disable   = c(TRUE, FALSE)[2]
    , width     = NULL
    , collapsed = c(TRUE, FALSE)[2]
  ),

  # Main dashboard body
  dashboardBody(
    tags$head(
      tags$style(
        "
        body, div, p, h1, h2, h3, h4, h5, h6 {
          font-family: 'Latin Modern Roman', serif;
          font-style: italic;
          font-weight: bold;
        }
        /* Enable both horizontal and vertical scrolling */
        .custom-table {
          height: 600px;  /* Adjust height */
          width: 100%;    /* Full width */
          overflow-x: auto;  /* Horizontal scroll */
          overflow-y: auto;  /* Vertical scroll */
          display: block;    /* Block display to manage scroll */
        }
        /* Adjust table to fit within its container */
        .dataTables_wrapper {
          width: 100%;  /* Ensure table takes up full width */
          overflow-x: auto;  /* Horizontal scrolling for wide tables */
          overflow-y: auto;  /* Vertical scrolling for long tables */
        }

        pre {
        font-size: 16px; /* Change to desired font size */
        line-height: 1.5; /* Optional: adjust line height */
        }

        "
      )
    ),

    tabItems(

      # Home Page
      tabItem(
        tabName = "Home"
      , h1(tags$span("Stability Analysis App", style = "color: green; font-size: inherit;"), align = "center")
      , br()
      , h4(
        "The", tags$span("Stability Analysis App", style = "color: green; font-size: inherit;"), "provides a comprehensive
         suite of tools for conducting", tags$span("Genotype by Environment Interaction (GEI)", style = "color: green; font-size: inherit;"), "analysis,
         helping researchers evaluate how different genotypes perform across various environments.
        This app includes a wide range of statistical models and visualization
        techniques that enable users to explore the interaction effects,
        stability, and adaptability of genotypes in multi-environment trials.
        It helps identify which genotype performs best in different environments,
        making it a valuable tool for determining winning genotypes in various conditions."
        , tags$span("Which Win Where!", style = "color: green; font-size: inherit;")
      ,  align = "justify"
      )
    , br()
    , br()
    , h4(
          tags$strong("Author/Maintainer:"),
          tags$a(href = "https://myaseen208.com/", "Muhammad Yaseen", target = "_blank"),
          align = "center"
        )
     , h4(
          tags$strong("Contributor(s):"),
          tags$a(href = "https://statistics.unl.edu/kent-m-eskridge", "Kent M. Eskridge", target = "_blank"),
          align = "center"
        )
     , h4(
          tags$strong("Email:"),
          tags$a(href = "mailto:myaseen208@gmail.com", "myaseen208@gmail.com", target = "_blank"),
          align = "center"
        )
    , h4(
          tags$strong("Website:"),
          tags$a(href = "https://myaseen208.com/stability", "https://myaseen208.com/stability", target = "_blank"),
          align = "center"
        )
    , br()
    , br()
    , h4(tags$span("The key functions available in the Stability App include:", style = "color: green; font-size: inherit;"))
    , tags$ul(
            style = "text-align: justify;"
            , tags$li(tags$a(href = "https://myaseen208.com/stability/reference/add_anova.html", target = "_blank", "add_anova()"),
                                    ": Performs the Additive ANOVA for analyzing the Genotype by Environment Interaction (GEI) model, breaking down the interaction into additive components for each genotype and environment.")
            , tags$li(tags$a(href = "https://myaseen208.com/stability/reference/ammi.html", target = "_blank", "ammi()"),
                                    ": Implements the Additive Main Effects and Multiplicative Interaction (AMMI) model, combining ANOVA for main effects with Principal Component Analysis (PCA) for interaction effects.")
            , tags$li(tags$a(href = "https://myaseen208.com/stability/reference/ammi_biplot.html", target = "_blank", "ammi_biplot()"),
                                    ": Generates an AMMI Biplot, a powerful graphical tool that visualizes the interaction between genotypes and environments, helping identify stable genotypes.")
            , tags$li(tags$a(href = "https://myaseen208.com/stability/reference/er_anova.html", target = "_blank", "er_anova()"),
                                    ": Conducts ANOVA based on Eberhart & Russel Model, a widely-used method for assessing genotype stability and adaptability across different environments.")
            , tags$li(tags$a(href = "https://myaseen208.com/stability/reference/ge_effects.html", target = "_blank", "ge_effects()"),
                                    ": Calculates Genotype by Environment Interaction (GEI) effects, offering insights into the nature and magnitude of interactions between genotypes and environments.")
            , tags$li(tags$a(href = "https://myaseen208.com/stability/reference/ge_means.html", target = "_blank", "ge_means()"),
                                    ": Computes Genotype by Environment Interaction Means and Ranks, providing a summary of genotype performance across environments, including rankings based on stability.")
            , tags$li(tags$a(href = "https://myaseen208.com/stability/reference/gge_biplot.html", target = "_blank", "gge_biplot()"),
                                    ": Creates a Genotype plus Genotype-by-Environment (GGE) Biplot, which focuses on both the genotype performance and interaction, helping to identify winning genotypes in specific environments.")
            , tags$li(tags$a(href = "https://myaseen208.com/stability/reference/indiv_anova.html", target = "_blank", "indiv_anova()"),
                                    ": Performs Individual ANOVA for each environment separately, giving a detailed analysis of how each genotype performs in distinct environments.")
            , tags$li(tags$a(href = "https://myaseen208.com/stability/reference/stab_asv.html", target = "_blank", "stab_asv()"),
                                    ": Calculates the Additive Main Effects and Multiplicative Interaction Stability Value (ASV), a stability index derived from the AMMI model.")
            , tags$li(tags$a(href = "https://myaseen208.com/stability/reference/stab_dist.html", target = "_blank", "stab_dist()"),
                                    ": Measures the Stability Distance in the AMMI model, providing a distance-based metric for identifying stable genotypes.")
            , tags$li(tags$a(href = "https://myaseen208.com/stability/reference/stab_fox.html", target = "_blank", "stab_fox()"),
                                    ": Implements the Stability Fox Function, which provides another stability metric based on genotype performance.")
            , tags$li(tags$a(href = "https://myaseen208.com/stability/reference/stab_kang.html", target = "_blank", "stab_kang()"),
                                    ": Computes the Stability Kang Function, a rank-based stability measure that combines yield and stability into a single score.")
            , tags$li(tags$a(href = "https://myaseen208.com/stability/reference/stab_masv.html", target = "_blank", "stab_masv()"),
                                    ": Provides the Modified Additive Main Effects and Multiplicative Interaction Stability Value (MASV), an alternative version of ASV with a modified formula for greater flexibility.")
            , tags$li(tags$a(href = "https://myaseen208.com/stability/reference/stab_measures.html", target = "_blank", "stab_measures()"),
                                    ": Includes a variety of Stability Measures for evaluating genotypes in terms of their interaction with different environments.")
            , tags$li(tags$a(href = "https://myaseen208.com/stability/reference/stab_par.html", target = "_blank", "stab_par()"),
                                    ": Calculates Stability Parameters for each genotype, which help quantify the stability and adaptability of genotypes in multi-environment trials.")
            , tags$li(tags$a(href = "https://myaseen208.com/stability/reference/stab_reg.html", target = "_blank", "stab_reg()"),
                                    ": Performs Individual Regression for each genotype, giving insight into genotype performance trends across environments based on regression analysis.")
            , tags$li(tags$a(href = "https://myaseen208.com/stability/reference/stability.html", target = "_blank", "stability()"),
                                    ": A comprehensive function for conducting a Stability Analysis of Genotype by Environment Interaction (GEI), combining multiple stability metrics and models to provide a holistic evaluation.")
            )
    , br()
    , br()
    , h4(
          tags$span(
            "By using this app, researchers & plant breeders can assess the
            stability & adaptability of genotypes, select stable & high-yielding
            genotypes, and make informed decisions in breeding programs aimed at
            improving crop performance across diverse environmental conditions."
          , style = "color: green; font-size: inherit;"
          )
        , align = "justify"
        )
    , br()
    , br()
    , h5(
          tags$strong("If you find this app to be useful, please let us know at ")
        , tags$a(href = "mailto:myaseen208@gmail.com", tags$strong("myaseen208@gmail.com."))
        , tags$strong("We can use this information to obtain more resources to make the app better. Thank you for your interest in our app!")
        , align = "justify"
        )
    ),

     # Intro Page
     tabItem(
        tabName = "Intro"
      , h1("Introduction")

      , box(
           title       = h3("ANOVA")
         , status      = "success"
         , solidHeader = TRUE
         , collapsible = TRUE
         , collapsed   = TRUE
         , width       = 12

         , box(
               title       = h3("Individual ANOVAs")
             , status      = "success"
             , solidHeader = TRUE
             , collapsible = TRUE
             , collapsed   = TRUE
             , width       = 12
             , p("Individual ANOVAs")
             )
          , br()
          , br()

          , box(
               title       = h3("Additive (Combined) ANOVA")
             , status      = "success"
             , solidHeader = TRUE
             , collapsible = TRUE
             , collapsed   = TRUE
             , width       = 12
             , p("Additive (Combined) ANOVA")
             )
          , br()
          , br()

         , box(
               title       = h3("Eberhart & Russel Model ANOVA")
             , status      = "success"
             , solidHeader = TRUE
             , collapsible = TRUE
             , collapsed   = TRUE
             , width       = 12
             , p("Eberhart & Russel Model ANOVA")
             )
        )

      , box(
           title       = h3("Additive Main Effects & Multiplicative Interaction (AMMI)")
         , status      = "success"
         , solidHeader = TRUE
         , collapsible = TRUE
         , collapsed   = TRUE
         , width       = 12
         , box(
                 title       = h3("AMMI")
               , status      = "success"
               , solidHeader = TRUE
               , collapsible = TRUE
               , collapsed   = TRUE
               , width       = 12
               , p("Additive Main Effects and Multiplicative Interaction (AMMI)")
               )
          , br()
          , br()

          , box(
               title       = h3("AMMI Biplot")
             , status      = "success"
             , solidHeader = TRUE
             , collapsible = TRUE
             , collapsed   = TRUE
             , width       = 12
             , p("AMMI Biplot")
             )
          , br()
          , br()

          , box(
               title       = h3("GGE Biplot")
             , status      = "success"
             , solidHeader = TRUE
             , collapsible = TRUE
             , collapsed   = TRUE
             , width       = 12
             , p("GGE Biplot")
             )
       )
      , br()
      , br()

      , box(
           title       = h3("Regression")
         , status      = "success"
         , solidHeader = TRUE
         , collapsible = TRUE
         , collapsed   = TRUE
         , width       = 12
         , p("Regression")
         )
      , br()
      , br()
      , box(
           title       = h3("Stability Measures")
         , status      = "success"
         , solidHeader = TRUE
         , collapsible = TRUE
         , collapsed   = TRUE
         , width       = 12

         , box(
               title       = h3("ASV")
             , status      = "success"
             , solidHeader = TRUE
             , collapsible = TRUE
             , collapsed   = TRUE
             , width       = 12
             , p("ASV")
             )
          , br()
          , br()

          , box(
               title       = h3("Dist")
             , status      = "success"
             , solidHeader = TRUE
             , collapsible = TRUE
             , collapsed   = TRUE
             , width       = 12
             , p("Dist")
             )
         , br()
          , br()

          , box(
               title       = h3("Fox")
             , status      = "success"
             , solidHeader = TRUE
             , collapsible = TRUE
             , collapsed   = TRUE
             , width       = 12
             , p("Fox")
             )
         , br()
          , br()

          , box(
               title       = h3("Kang")
             , status      = "success"
             , solidHeader = TRUE
             , collapsible = TRUE
             , collapsed   = TRUE
             , width       = 12
             , p("Kang")
             )
         , br()
          , br()

          , box(
               title       = h3("MASV")
             , status      = "success"
             , solidHeader = TRUE
             , collapsible = TRUE
             , collapsed   = TRUE
             , width       = 12
             , p("MASV")
             )
         , br()
          , br()

          , box(
               title       = h3("Measures")
             , status      = "success"
             , solidHeader = TRUE
             , collapsible = TRUE
             , collapsed   = TRUE
             , width       = 12
             , p("Measures")
             )
         , br()
          , br()

          , box(
               title       = h3("Par")
             , status      = "success"
             , solidHeader = TRUE
             , collapsible = TRUE
             , collapsed   = TRUE
             , width       = 12
             , p("Par")
             )
         , br()
          , br()

          , box(
               title       = h3("Regression")
             , status      = "success"
             , solidHeader = TRUE
             , collapsible = TRUE
             , collapsed   = TRUE
             , width       = 12
             , p("Regression")
             )
         )
      )
     ,

    # Data Page with Full Width Box
    tabItem(
      tabName = "tab-data",
      box(
        title = tagList(
              h3("Upload or Use Built-in Data")
             , actionButton(
                inputId = "run_data",
                label = "Load Data",
                icon = icon("upload"),
                style = "color: white; background-color: #007bff; border-color: red; float: right;"
              )
            ),
        status = "success",  # Blue header bar
        solidHeader = TRUE,
        collapsible = TRUE,
        icon        = icon("folder-open"),
        width = 12,
        sidebarLayout(
          sidebarPanel(
            fileInput("file", "Upload CSV File",
                      accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv")),
            helpText("Upload a CSV file with columns: Yield, Rep, Gen, and Env. If no file is uploaded, built-in ge_data will be used.")
          ),
          mainPanel(
            h3("Data Table"),
            DT::DTOutput("data_head")
          )
        )
      )
    ),

    # Individual ANOVA Page with actionButton at the top
    tabItem(
      tabName = "tab-indiv_anova",
      box(
        title = tagList(
          h3("Individual ANOVAs"),
          actionButton(
            inputId = "run_indiv_anova",
            label = "Individual ANOVAs",
            icon = icon("play"),
            style = "color: white; background-color: #007bff; border-color: red; float: right;",
            title = "Click to run the analysis. If you have uploaded your own data, it will be used; otherwise, the built-in data will be used for the analysis."
          )
        ),
        status = "success",
        solidHeader = TRUE,
        collapsible = TRUE,
        width = 12, # Full width
        verbatimTextOutput("indiv_anova_result")
      )
    ),

    # Additive ANOVA Page
    tabItem(
      tabName = "tab-add_anova",
      box(
        title = tagList(
          h3("Additive (Combined) ANOVA"),
          actionButton(
            inputId = "run_add_anova",
            label = "Additive ANOVA",
            icon = icon("play"), # Add an icon (e.g., play icon)
            style = "color: white; background-color: #007bff; border-color: red; float: right;",
            title = "Click to run the analysis. If you have uploaded your own data, it will be used; otherwise, the built-in data will be used for the analysis."
          )
        ),
        status = "success",
        solidHeader = TRUE,
        collapsible = TRUE,
        closable = FALSE,
        width = 12,
        verbatimTextOutput("add_anova_result")
      )
    ),

    # Eberhart & Russel Model ANOVA Page
    tabItem(
      tabName = "tab-er_anova",
      box(
        title = tagList(
          h3("Eberhart & Russel Model ANOVA"),
          actionButton(
            inputId = "run_er_anova",
            label = "ER ANOVA",
            icon = icon("play"), # Add an icon (e.g., play icon)
            style = "color: white; background-color: #007bff; border-color: red; float: right;",
            title = "Click to run the analysis. If you have uploaded your own data, it will be used; otherwise, the built-in data will be used for the analysis."
          )
        ),
        status = "success",
        solidHeader = TRUE,
        collapsible = TRUE,
        closable = FALSE,
        width = 12,
        box(
          title = "ANOVA",
          status = "success",
          solidHeader = TRUE,
          width = 12,
          collapsible = TRUE,
          verbatimTextOutput("er_anova_result1")
        ),
        br(),
        box(
          title = "ANOVA",
          status = "success",
          solidHeader = TRUE,
          width = 12,
          collapsible = TRUE,
          verbatimTextOutput("er_anova_result2")
        )

      )
    ),

    # AMMI Page
    tabItem(
      tabName = "tab-ammi",
      box(
        title = tagList(
          h3("Additive Main Effects and Multiplicative Interaction (AMMI)"),
          actionButton(
            inputId = "run_ammi",
            label = "AMMI",
            icon = icon("play"), # Add an icon (e.g., play icon)
            style = "color: white; background-color: #007bff; border-color: red; float: right;",
            title = "Click to run the analysis. If you have uploaded your own data, it will be used; otherwise, the built-in data will be used for the analysis."
          )
        ),
        status = "success",
        solidHeader = TRUE,
        collapsible = TRUE,
        closable = FALSE,
        width = 12,
        box(
          title = "ANOVA",
          status = "success",
          solidHeader = TRUE,
          width = 12,
          collapsible = TRUE,
          verbatimTextOutput("ammi_result1")
        ),
        br(),
        box(
          title = "PC ANOVA",
          status = "success",
          solidHeader = TRUE,
          width = 12,
          collapsible = TRUE,
          verbatimTextOutput("ammi_result2")
        )
      )
    ),

    # AMMI Biplot Page
    tabItem(
      tabName = "tab-ammi_biplot",
      box(
        title = tagList(
          h3("AMMI Biplot"),
          actionButton(
            inputId = "run_ammi_biplot",
            label = "AMMI Biplot",
            icon = icon("play"), # Add an icon (e.g., play icon)
            style = "color: white; background-color: #007bff; border-color: red; float: right;",
            title = "Click to run the analysis. If you have uploaded your own data, it will be used; otherwise, the built-in data will be used for the analysis."
          )
        ),
        status = "success",
        solidHeader = TRUE,
        collapsible = TRUE,
        closable = FALSE,
        width = 12,
        plotOutput("ammi_biplot_result")
      )
    ),

    # GGE Biplot Page
    tabItem(
      tabName = "tab-gge_biplot",
      box(
        title = tagList(
          h3("GGE Biplot"),
          actionButton(
            inputId = "run_gge_biplot",
            label = "GGE Biplot",
            icon = icon("play"), # Add an icon (e.g., play icon)
            style = "color: white; background-color: #007bff; border-color: red; float: right;",
            title = "Click to run the analysis. If you have uploaded your own data, it will be used; otherwise, the built-in data will be used for the analysis."
          )
        ),
        status = "success",
        solidHeader = TRUE,
        collapsible = TRUE,
        closable = FALSE,
        width = 12,
        plotOutput("gge_biplot_result")
      )
    ),

    # AMMI Biplot Page 2
    tabItem(
      tabName = "tab-ammi_biplot2",
      box(
        title = tagList(
          h3("AMMI Biplot"),
          actionButton(
            inputId = "run_ammi_biplot2",
            label = "AMMI Biplot",
            icon = icon("play"), # Add an icon (e.g., play icon)
            style = "color: white; background-color: #007bff; border-color: red; float: right;",
            title = "Click to run the analysis. If you have uploaded your own data, it will be used; otherwise, the built-in data will be used for the analysis."
          )
        ),
        status = "success",
        solidHeader = TRUE,
        collapsible = TRUE,
        closable = FALSE,
        width = 12,
        plotOutput("ammi_biplot2_result")
      )
    ),

    # GGE Biplot Page 2
    tabItem(
      tabName = "tab-gge_biplot2",
      box(
        title = tagList(
          h3("GGE Biplot"),
          actionButton(
            inputId = "run_gge_biplot2",
            label = "GGE Biplot",
            icon = icon("play"), # Add an icon (e.g., play icon)
            style = "color: white; background-color: #007bff; border-color: red; float: right;",
            title = "Click to run the analysis. If you have uploaded your own data, it will be used; otherwise, the built-in data will be used for the analysis."
          )
        ),
        status = "success",
        solidHeader = TRUE,
        collapsible = TRUE,
        closable = FALSE,
        width = 12,
        plotOutput("gge_biplot2_result")
      )
    ),

    # Regression Page
    tabItem(
      tabName = "tab-stab_reg2",
      box(
        title = tagList(
          h3("Individual Regression for Each Genotype"),
          actionButton(
            inputId = "run_stab_reg2",
            label = "Regression",
            icon = icon("play"),
            style = "color: white; background-color: #007bff; border-color: red; float: right;",
            title = "Click to run the analysis. If you have uploaded your own data, it will be used; otherwise, the built-in data will be used for the analysis."
          )
        ),
        status = "success",
        solidHeader = TRUE,
        collapsible = TRUE,
        closable = FALSE,
        width = 12,
        DT::DTOutput("stab_reg2_result"),
        br(),
        plotOutput("stab_reg2_plot")
      )
    ),

    # ASV Page
    tabItem(
      tabName = "tab-stab_asv",
      box(
        title = tagList(
          h3("AMMI Stability Value (ASV)"),
          actionButton(
            inputId = "run_stab_asv",
            label = "ASV",
            icon = icon("play"),
            style = "color: white; background-color: #007bff; border-color: red; float: right;",
            title = "Click to run the analysis. If you have uploaded your own data, it will be used; otherwise, the built-in data will be used for the analysis."
          )
        ),
        status = "success",
        solidHeader = TRUE,
        collapsible = TRUE,
        closable = FALSE,
        width = 12,
        DT::DTOutput("stab_asv_result"),

        # Add download buttons for CSV and PDF
        downloadButton("download_asv_csv", "Download as CSV", style = "color: white; background-color: #28a745; border-color: green;"),
        downloadButton("download_asv_pdf", "Download as PDF", style = "color: white; background-color: #dc3545; border-color: red;")
      )
    ),

    # Dist Page
    tabItem(
      tabName = "tab-stab_dist",
      box(
        title = tagList(
          h3("Stability Distance in AMMI"),
          actionButton(
            inputId = "run_stab_dist",
            label = "Distance",
            icon = icon("play"),
            style = "color: white; background-color: #007bff; border-color: red; float: right;",
            title = "Click to run the analysis. If you have uploaded your own data, it will be used; otherwise, the built-in data will be used for the analysis."
          )
        ),
        status = "success",
        solidHeader = TRUE,
        collapsible = TRUE,
        closable = FALSE,
        width = 12,
        DT::DTOutput("stab_dist_result")
      )
    ),

    # Fox Page
    tabItem(
      tabName = "tab-stab_fox",
      box(
        title = tagList(
          h3("Stability Fox Function"),
          actionButton(
            inputId = "run_stab_fox",
            label = "Fox",
            icon = icon("play"),
            style = "color: white; background-color: #007bff; border-color: red; float: right;",
            title = "Click to run the analysis. If you have uploaded your own data, it will be used; otherwise, the built-in data will be used for the analysis."
          )
        ),
        status = "success",
        solidHeader = TRUE,
        collapsible = TRUE,
        closable = FALSE,
        width = 12,
        DT::DTOutput("stab_fox_result")
      )
    ),

    # Kang Page
    tabItem(
      tabName = "tab-stab_kang",
      box(
        title = tagList(
          h3("Stability Kang Function"),
          actionButton(
            inputId = "run_stab_kang",
            label = "Kang",
            icon = icon("play"),
            style = "color: white; background-color: #007bff; border-color: red; float: right;",
            title = "Click to run the analysis. If you have uploaded your own data, it will be used; otherwise, the built-in data will be used for the analysis."
          )
        ),
        status = "success",
        solidHeader = TRUE,
        collapsible = TRUE,
        closable = FALSE,
        width = 12,
        DT::DTOutput("stab_kang_result")
      )
    ),

    # MASV Page
    tabItem(
      tabName = "tab-stab_masv",
      box(
        title = tagList(
          h3("Modified AMMI Stability Value (MASV)"),
          actionButton(
            inputId = "run_stab_masv",
            label = "MASV",
            icon = icon("play"),
            style = "color: white; background-color: #007bff; border-color: red; float: right;",
            title = "Click to run the analysis. If you have uploaded your own data, it will be used; otherwise, the built-in data will be used for the analysis."
          )
        ),
        status = "success",
        solidHeader = TRUE,
        collapsible = TRUE,
        closable = FALSE,
        width = 12,
        DT::DTOutput("stab_masv_result")
      )
    ),

    # Measures Page
    tabItem(
      tabName = "tab-stab_measures",
      box(
        title = tagList(
          h3("Stability Measures for GEI"),
          actionButton(
            inputId = "run_stab_measures",
            label = "Measures",
            icon = icon("play"),
            style = "color: white; background-color: #007bff; border-color: red; float: right;",
            title = "Click to run the analysis. If you have uploaded your own data, it will be used; otherwise, the built-in data will be used for the analysis."
          )
        ),
        status = "success",
        solidHeader = TRUE,
        collapsible = TRUE,
        closable = FALSE,
        width = 12,
        DT::DTOutput("stab_measures_result"),
        br(),
        plotOutput("stab_measures_plot")
      )
    ),

    # Par Page
    tabItem(
      tabName = "tab-stab_par",
      box(
        title = tagList(
          h3("Parameters"),
          actionButton(
            inputId = "run_stab_par",
            label = "Parameters",
            icon = icon("play"),
            style = "color: white; background-color: #007bff; border-color: red; float: right;",
            title = "Click to run the analysis. If you have uploaded your own data, it will be used; otherwise, the built-in data will be used for the analysis."
          )
        ),
        status = "success",
        solidHeader = TRUE,
        collapsible = TRUE,
        closable = FALSE,
        width = 12,
        box(
          title = "ANOVA",
          status = "success",
          solidHeader = TRUE,
          width = 12,
          collapsible = TRUE,
          verbatimTextOutput("stab_par_result1")
        ),
        br(),
        box(
          title = "Stability Parameters",
          status = "success",
          solidHeader = TRUE,
          width = 12,
          collapsible = TRUE,
          DT::DTOutput("stab_par_result2")
        ),
        br(),
        box(
          title = "Simaltenous Selection",
          status = "success",
          solidHeader = TRUE,
          width = 12,
          collapsible = TRUE,
          DT::DTOutput("stab_par_result3")
        )
      )
    )


    # Regression Page
    , tabItem(
      tabName = "tab-stab_reg",
      box(
        title = tagList(
          h3("Individual Regression for Each Genotype"),
          actionButton(
            inputId = "run_stab_reg",
            label = "Regression",
            icon = icon("play"),
            style = "color: white; background-color: #007bff; border-color: red; float: right;",
            title = "Click to run the analysis. If you have uploaded your own data, it will be used; otherwise, the built-in data will be used for the analysis."
          )
        ),
        status = "success",
        solidHeader = TRUE,
        collapsible = TRUE,
        closable = FALSE,
        width = 12,
        DT::DTOutput("stab_reg_result"),
        br(),
        plotOutput("stab_reg_plot")
      )
    ),

    # Means Page
    tabItem(
      tabName = "tab-ge_means",
      box(
        title = tagList(
          h3("Genotype by Environment Interaction Means"),
          actionButton(
            inputId = "run_ge_means",
            label = "Means",
            icon = icon("play"),
            style = "color: white; background-color: #007bff; border-color: red; float: right;",
            title = "Click to run the analysis. If you have uploaded your own data, it will be used; otherwise, the built-in data will be used for the analysis."
          )
        ),
        status = "success",  # Green header bar
        solidHeader = TRUE,
        collapsible = TRUE,
        closable = FALSE,
        width = 12,
        br(),

        box(
          title = "Genotyp by Environment Interaction Means",
          status = "success",
          solidHeader = TRUE,
          width = 12,
          collapsible = TRUE,
          DT::DTOutput("ge_means_result1")
        ),
        br(),

        box(
          title = "Genotype Means",
          status = "success",
          solidHeader = TRUE,
          width = 12,
          collapsible = TRUE,
          DT::DTOutput("ge_means_result3")
        ),
        br(),

        box(
          title = "Environment Means",
          status = "success",
          solidHeader = TRUE,
          width = 12,
          collapsible = TRUE,
          DT::DTOutput("ge_means_result4")
        ) ,
        br(),

        box(
          title = "Genotype Ranks within Environments",
          status = "success",
          solidHeader = TRUE,
          width = 12,
          collapsible = TRUE,
          DT::DTOutput("ge_means_result5")
        )
      )
    )


      # Effects Page
    , tabItem(
      tabName = "tab-ge_effects",
      box(
        title = tagList(
          h3("Genotype by Environment Interaction Effects"),
          actionButton(
            inputId = "run_ge_effects",
            label = "Effects",
            icon = icon("play"),
            style = "color: white; background-color: #007bff; border-color: red; float: right;",
            title = "Click to run the analysis. If you have uploaded your own data, it will be used; otherwise, the built-in data will be used for the analysis."
          )
        ),
        status = "success",  # Green header bar
        solidHeader = TRUE,
        collapsible = TRUE,
        closable = FALSE,
        width = 12,
        br(),
        # Box for GE Interaction Effects
        box(
          title = "GE Means",
          status = "success",
          solidHeader = TRUE,
          width = 12,
          collapsible = TRUE,
          DT::DTOutput("ge_effects_result1")
        ),
        br(),
        # Box for GE Effects
        box(
          title = "GE Effects",
          status = "success",
          solidHeader = TRUE,
          width = 12,
          collapsible = TRUE,
          DT::DTOutput("ge_effects_result2")
        ),
        br(),
        # Box for GGE Effects
        box(
          title = "GGE Effects",
          status = "success",
          solidHeader = TRUE,
          width = 12,
          collapsible = TRUE,
          DT::DTOutput("ge_effects_result3")
        )
      )
    )




        )
      )
    )
