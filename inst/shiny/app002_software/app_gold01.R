library(shiny)
library(bslib)

ui <- page_sidebar(
  title = "📊 Statistical Analysis Software",
  theme = bs_theme(version = 5, bootswatch = "minty"),

  sidebar = sidebar(
    width = 300,

    # Selector de dataset
    card(
      card_header("Dataset"),
      selectInput("dataset", "Choose dataset:",
                  choices = c("mtcars", "iris", "ToothGrowth")),
      actionButton("load_data", "Load", icon = icon("folder-open"))
    ),

    # ACORDEÓN DE ANÁLISIS (como Jamovi)
    accordion(
      id = "analysis_menu",
      open = c("desc"),

      # DESCRIPTIVE STATISTICS
      accordion_panel(
        title = tagList(icon("chart-pie"), "Descriptive Statistics"),
        value = "desc",
        actionButton("btn_freq", "Frequencies",
                     class = "d-block w-100 mb-2", icon = icon("table")),
        actionButton("btn_descriptives", "Descriptives",
                     class = "d-block w-100 mb-2", icon = icon("calculator")),
        actionButton("btn_explore", "Explore",
                     class = "d-block w-100 mb-2", icon = icon("search")),
        actionButton("btn_crosstab", "Crosstabs",
                     class = "d-block w-100 mb-2", icon = icon("project-diagram"))
      ),

      # T-TESTS
      accordion_panel(
        title = tagList(icon("balance-scale"), "T-Tests"),
        value = "ttests",
        actionButton("btn_ttest_onesample", "One-Sample t-test",
                     class = "d-block w-100 mb-2"),
        actionButton("btn_ttest_independent", "Independent Samples t-test",
                     class = "d-block w-100 mb-2"),
        actionButton("btn_ttest_paired", "Paired Samples t-test",
                     class = "d-block w-100 mb-2")
      ),

      # ANOVA
      accordion_panel(
        title = tagList(icon("chart-bar"), "ANOVA"),
        value = "anova",
        actionButton("btn_anova_oneway", "One-Way ANOVA",
                     class = "d-block w-100 mb-2"),
        actionButton("btn_anova_twoway", "Two-Way ANOVA",
                     class = "d-block w-100 mb-2"),
        actionButton("btn_anova_rm", "Repeated Measures ANOVA",
                     class = "d-block w-100 mb-2"),
        actionButton("btn_ancova", "ANCOVA",
                     class = "d-block w-100 mb-2")
      ),

      # REGRESSION
      accordion_panel(
        title = tagList(icon("line-chart"), "Regression"),
        value = "regression",
        actionButton("btn_reg_simple", "Simple Linear Regression",
                     class = "d-block w-100 mb-2"),
        actionButton("btn_reg_multiple", "Multiple Regression",
                     class = "d-block w-100 mb-2"),
        actionButton("btn_reg_logistic", "Logistic Regression",
                     class = "d-block w-100 mb-2")
      ),

      # NONPARAMETRIC
      accordion_panel(
        title = tagList(icon("chart-scatter"), "Nonparametric"),
        value = "nonparametric",
        actionButton("btn_mannwhitney", "Mann-Whitney U",
                     class = "d-block w-100 mb-2"),
        actionButton("btn_wilcoxon", "Wilcoxon Signed-Rank",
                     class = "d-block w-100 mb-2"),
        actionButton("btn_kruskal", "Kruskal-Wallis",
                     class = "d-block w-100 mb-2"),
        actionButton("btn_friedman", "Friedman Test",
                     class = "d-block w-100 mb-2")
      )
    ),

    # Panel de variables (aparece cuando hay dataset)
    uiOutput("variables_panel")
  ),

  # ÁREA PRINCIPAL CON PESTAÑAS
  navset_card_tab(
    id = "main_tabs",
    full_screen = TRUE,

    # Pestaña de datos
    nav_panel(
      title = tagList(icon("database"), "Data"),
      card(
        card_header("Dataset Viewer"),
        DTOutput("data_table"),
        verbatimTextOutput("data_summary")
      )
    ),

    # Pestaña de análisis (dinámica)
    nav_panel(
      title = tagList(icon("calculator"), "Analysis"),
      uiOutput("analysis_output")
    ),

    # Pestaña de gráficos
    nav_panel(
      title = tagList(icon("chart-bar"), "Plots"),
      uiOutput("plots_output")
    ),

    # Pestaña de resultados
    nav_panel(
      title = tagList(icon("file-alt"), "Results"),
      uiOutput("results_output")
    )
  )
)

server <- function(input, output, session) {

  # Dataset reactivo
  data_r <- reactiveVal(mtcars)

  observeEvent(input$load_data, {
    dataset_name <- input$dataset
    new_data <- switch(dataset_name,
                       "mtcars" = mtcars,
                       "iris" = iris,
                       "ToothGrowth" = ToothGrowth)
    data_r(new_data)

    showNotification(paste("Loaded:", dataset_name),
                     type = "message", icon = icon("check"))
  })

  # Panel de variables dinámico
  output$variables_panel <- renderUI({
    data <- data_r()
    req(data)

    card(
      card_header("Variables", class = "bg-light"),
      h6("Numeric:", class = "text-muted mt-2"),
      checkboxGroupInput("num_vars", NULL,
                         choices = names(data)[sapply(data, is.numeric)],
                         inline = TRUE),

      h6("Categorical:", class = "text-muted mt-3"),
      checkboxGroupInput("cat_vars", NULL,
                         choices = names(data)[sapply(data, is.factor) |
                                                 sapply(data, is.character)],
                         inline = TRUE)
    )
  })

  # Tabla de datos
  output$data_table <- renderDT({
    datatable(data_r(),
              options = list(pageLength = 10, scrollX = TRUE),
              class = "display compact")
  })

  output$data_summary <- renderPrint({
    cat("Dataset Summary\n")
    cat("Observations:", nrow(data_r()), "\n")
    cat("Variables:", ncol(data_r()), "\n")
    cat("\nVariable types:\n")
    print(sapply(data_r(), class))
  })

  # Análisis seleccionado
  current_analysis <- reactiveVal(NULL)

  # Observadores para todos los botones de análisis
  observeEvent(input$btn_freq, {
    current_analysis("frequencies")
    updateNavbarTab("main_tabs", "Analysis")
  })

  observeEvent(input$btn_descriptives, {
    current_analysis("descriptives")
    updateNavbarTab("main_tabs", "Analysis")
  })

  observeEvent(input$btn_ttest_independent, {
    current_analysis("ttest_independent")
    updateNavbarTab("main_tabs", "Analysis")
  })

  observeEvent(input$btn_anova_oneway, {
    current_analysis("anova_oneway")
    updateNavbarTab("main_tabs", "Analysis")
  })

  observeEvent(input$btn_reg_simple, {
    current_analysis("reg_simple")
    updateNavbarTab("main_tabs", "Analysis")
  })

  # Output de análisis
  output$analysis_output <- renderUI({
    analysis <- current_analysis()
    data <- data_r()

    if (is.null(analysis)) {
      return(
        card(
          class = "text-center text-muted",
          div(style = "padding: 100px 0;",
              icon("mouse-pointer", class = "fa-3x mb-3"),
              h4("Select an analysis from the sidebar"),
              p("Choose from Descriptive Statistics, T-Tests, ANOVA, etc.")
          )
        )
      )
    }

    # UI específica para cada análisis
    analysis_ui <- switch(analysis,
                          "frequencies" = card(
                            card_header("Frequencies", class = "bg-primary text-white"),
                            selectInput("freq_var", "Select categorical variable:",
                                        choices = names(data)[sapply(data, is.factor) |
                                                                sapply(data, is.character)]),
                            actionButton("run_freq", "Run", class = "btn-success"),
                            verbatimTextOutput("freq_results")
                          ),

                          "descriptives" = card(
                            card_header("Descriptive Statistics", class = "bg-primary text-white"),
                            selectizeInput("desc_vars", "Select numeric variables:",
                                           choices = names(data)[sapply(data, is.numeric)],
                                           multiple = TRUE),
                            checkboxGroupInput("desc_stats", "Statistics:",
                                               choices = c("Mean", "SD", "Median", "Min", "Max",
                                                           "Skewness", "Kurtosis", "SE"),
                                               selected = c("Mean", "SD", "Min", "Max")),
                            actionButton("run_desc", "Run", class = "btn-success"),
                            verbatimTextOutput("desc_results")
                          ),

                          "ttest_independent" = card(
                            card_header("Independent Samples t-test", class = "bg-primary text-white"),
                            layout_column_wrap(
                              width = 1/2,
                              selectInput("ttest_var", "Test variable:",
                                          choices = names(data)[sapply(data, is.numeric)]),
                              selectInput("ttest_group", "Grouping variable:",
                                          choices = names(data)[sapply(data, is.factor) |
                                                                  sapply(data, is.character)])
                            ),
                            checkboxInput("ttest_var_eq", "Assume equal variances", TRUE),
                            checkboxInput("ttest_ci", "Confidence intervals", TRUE),
                            actionButton("run_ttest", "Run t-test", class = "btn-success"),
                            verbatimTextOutput("ttest_results"),
                            plotOutput("ttest_plot")
                          ),

                          "anova_oneway" = card(
                            card_header("One-Way ANOVA", class = "bg-primary text-white"),
                            layout_column_wrap(
                              width = 1/2,
                              selectInput("anova_var", "Dependent variable:",
                                          choices = names(data)[sapply(data, is.numeric)]),
                              selectInput("anova_factor", "Factor:",
                                          choices = names(data)[sapply(data, is.factor) |
                                                                  sapply(data, is.character)])
                            ),
                            checkboxGroupInput("anova_options", "Options:",
                                               choices = c("Descriptive statistics",
                                                           "Homogeneity test",
                                                           "Post-hoc tests",
                                                           "Effect size")),
                            actionButton("run_anova", "Run ANOVA", class = "btn-success"),
                            verbatimTextOutput("anova_results"),
                            plotOutput("anova_plot")
                          ),

                          "reg_simple" = card(
                            card_header("Simple Linear Regression", class = "bg-primary text-white"),
                            layout_column_wrap(
                              width = 1/2,
                              selectInput("reg_y", "Dependent variable (Y):",
                                          choices = names(data)[sapply(data, is.numeric)]),
                              selectInput("reg_x", "Independent variable (X):",
                                          choices = names(data)[sapply(data, is.numeric)])
                            ),
                            checkboxGroupInput("reg_options", "Output:",
                                               choices = c("Model summary", "ANOVA table",
                                                           "Coefficients", "Diagnostics")),
                            actionButton("run_reg", "Run Regression", class = "btn-success"),
                            verbatimTextOutput("reg_results"),
                            plotOutput("reg_plot")
                          )
    )

    return(analysis_ui)
  })

  # Resultados de análisis
  output$freq_results <- renderPrint({
    req(input$run_freq, input$freq_var)
    cat("Frequencies for:", input$freq_var, "\n\n")
    print(table(data_r()[[input$freq_var]]))
  })

  output$desc_results <- renderPrint({
    req(input$run_desc, input$desc_vars)
    cat("Descriptive Statistics\n\n")
    for (var in input$desc_vars) {
      cat("\n=== ", var, " ===\n")
      x <- data_r()[[var]]
      cat("N:", length(x), "\n")
      if ("Mean" %in% input$desc_stats) cat("Mean:", mean(x, na.rm = TRUE), "\n")
      if ("SD" %in% input$desc_stats) cat("SD:", sd(x, na.rm = TRUE), "\n")
      if ("Median" %in% input$desc_stats) cat("Median:", median(x, na.rm = TRUE), "\n")
      if ("Min" %in% input$desc_stats) cat("Min:", min(x, na.rm = TRUE), "\n")
      if ("Max" %in% input$desc_stats) cat("Max:", max(x, na.rm = TRUE), "\n")
    }
  })

  output$ttest_results <- renderPrint({
    req(input$run_ttest, input$ttest_var, input$ttest_group)

    formula <- as.formula(paste(input$ttest_var, "~", input$ttest_group))
    result <- t.test(formula, data = data_r(),
                     var.equal = input$ttest_var_eq,
                     conf.level = 0.95)

    cat("Independent Samples t-test\n")
    cat("Formula:", format(formula), "\n\n")
    print(result)
  })

  output$ttest_plot <- renderPlot({
    req(input$run_ttest, input$ttest_var, input$ttest_group)

    formula <- as.formula(paste(input$ttest_var, "~", input$ttest_group))
    boxplot(formula, data = data_r(),
            main = paste("Boxplot of", input$ttest_var, "by", input$ttest_group),
            xlab = input$ttest_group, ylab = input$ttest_var,
            col = c("#3498db", "#e74c3c"))
  })

  output$anova_results <- renderPrint({
    req(input$run_anova, input$anova_var, input$anova_factor)

    formula <- as.formula(paste(input$anova_var, "~", input$anova_factor))
    result <- aov(formula, data = data_r())

    cat("One-Way ANOVA\n")
    cat("Formula:", format(formula), "\n\n")
    print(summary(result))
  })

  output$anova_plot <- renderPlot({
    req(input$run_anova, input$anova_var, input$anova_factor)

    formula <- as.formula(paste(input$anova_var, "~", input$anova_factor))
    boxplot(formula, data = data_r(),
            main = paste("Boxplot of", input$anova_var, "by", input$anova_factor),
            xlab = input$anova_factor, ylab = input$anova_var,
            col = rainbow(length(unique(data_r()[[input$anova_factor]]))))
  })

  output$reg_results <- renderPrint({
    req(input$run_reg, input$reg_y, input$reg_x)

    formula <- as.formula(paste(input$reg_y, "~", input$reg_x))
    result <- lm(formula, data = data_r())

    cat("Simple Linear Regression\n")
    cat("Formula:", format(formula), "\n\n")
    print(summary(result))
  })

  output$reg_plot <- renderPlot({
    req(input$run_reg, input$reg_y, input$reg_x)

    formula <- as.formula(paste(input$reg_y, "~", input$reg_x))
    plot(formula, data = data_r(),
         main = paste("Scatterplot:", input$reg_y, "vs", input$reg_x),
         xlab = input$reg_x, ylab = input$reg_y,
         pch = 19, col = "#2c3e50")
    abline(lm(formula, data = data_r()), col = "#e74c3c", lwd = 2)
  })

  # Outputs de otras pestañas
  output$plots_output <- renderUI({
    card(
      card_header("Plots Gallery"),
      plotOutput("main_plot")
    )
  })

  output$results_output <- renderUI({
    card(
      card_header("Analysis Results"),
      verbatimTextOutput("all_results")
    )
  })
}

shinyApp(ui, server)
