library(shiny)
library(bslib)

ui <- page_sidebar(
  title = "Step-by-Step Tool Selector",
  theme = bs_theme(version = 5),

  sidebar = sidebar(
    width = 300,
    # Barra de progreso
    card(
      card_header("Progress"),
      div(
        class = "progress mb-2",
        style = "height: 20px;",
        div(
          id = "progress_bar",
          class = "progress-bar",
          style = "width: 33%;",
          role = "progressbar",
          "Step 1 of 3"
        )
      ),
      p(class = "text-center small", "Select analysis type")
    )
  ),

  # ÁREA PRINCIPAL CON FLUJO
  navset_hidden(
    id = "wizard_tabs",

    # PASO 1: Tipo de análisis
    nav_panel(
      "step1",
      card(
        height = "500px",
        class = "text-center",
        card_header("Step 1: What type of analysis do you need?",
                    class = "bg-primary text-white"),
        br(),

        layout_column_wrap(
          width = 1/2,
          # Columna 1
          div(
            class = "card h-100 hover-card",
            style = "cursor: pointer;",
            onclick = "Shiny.setInputValue('analysis_type', 'descriptive')",
            div(style = "padding: 40px;",
                icon("chart-pie", class = "fa-4x text-primary mb-3"),
                h4("Descriptive Statistics"),
                p("Summarize and describe data"),
                tags$small("Frequencies, descriptives, crosstabs")
            )
          ),

          # Columna 2
          div(
            class = "card h-100 hover-card",
            style = "cursor: pointer;",
            onclick = "Shiny.setInputValue('analysis_type', 'inferential')",
            div(style = "padding: 40px;",
                icon("calculator", class = "fa-4x text-success mb-3"),
                h4("Inferential Tests"),
                p("Hypothesis testing"),
                tags$small("t-tests, chi-square, normality tests")
            )
          )
        ),

        layout_column_wrap(
          width = 1/2,
          # Columna 3
          div(
            class = "card h-100 hover-card",
            style = "cursor: pointer;",
            onclick = "Shiny.setInputValue('analysis_type', 'modeling')",
            div(style = "padding: 40px;",
                icon("line-chart", class = "fa-4x text-warning mb-3"),
                h4("Modeling"),
                p("Build statistical models"),
                tags$small("ANOVA, regression, mixed models")
            )
          ),

          # Columna 4
          div(
            class = "card h-100 hover-card",
            style = "cursor: pointer;",
            onclick = "Shiny.setInputValue('analysis_type', 'specialized')",
            div(style = "padding: 40px;",
                icon("flask", class = "fa-4x text-danger mb-3"),
                h4("Specialized"),
                p("Advanced methods"),
                tags$small("Survival, correlation, nonparametric")
            )
          )
        )
      )
    ),

    # PASO 2: Sub-categoría
    nav_panel(
      "step2",
      card(
        height = "500px",
        card_header("Step 2: Specify analysis details",
                    class = "bg-primary text-white"),
        uiOutput("step2_content"),

        # Botón de siguiente (dentro del paso 2)
        div(
          class = "position-absolute bottom-0 end-0 m-3",
          actionButton("step2_next", "Next →",
                       class = "btn-primary",
                       style = "display: none;")  # Oculto inicialmente
        )
      )
    ),

    # PASO 3: Herramienta específica
    nav_panel(
      "step3",
      card(
        height = "500px",
        card_header("Step 3: Select specific tool",
                    class = "bg-primary text-white"),
        uiOutput("step3_content")
      )
    )
  ),

  # Botones de navegación (fuera del sidebar) - CORREGIDO
  div(
    class = "fixed-bottom p-3 bg-light border-top",
    div(
      class = "container",
      div(
        class = "d-flex justify-content-between",
        actionButton("prev_btn", "← Previous",
                     class = "btn-outline-secondary",
                     disabled = TRUE),
        span(id = "step_text", "Step 1 of 3", class = "align-self-center"),
        actionButton("next_btn", "Next →",
                     class = "btn-primary",
                     disabled = TRUE)
      )
    )
  )
)

server <- function(input, output, session) {

  # Estado
  current_step <- reactiveVal(1)
  selected_type <- reactiveVal(NULL)
  selected_subtype <- reactiveVal(NULL)

  # Observar selecciones del paso 1
  observeEvent(input$analysis_type, {
    selected_type(input$analysis_type)
    current_step(2)
    nav_select("wizard_tabs", "step2")

    # Actualizar UI
    updateActionButton(session, "prev_btn", disabled = FALSE)
    updateActionButton(session, "next_btn", disabled = TRUE)  # Deshabilitado hasta selección
    updateActionButton(session, "next_btn", label = "Next →")
    shinyjs::html("step_text", "Step 2 of 3")
    shinyjs::runjs('document.getElementById("progress_bar").style.width = "66%";')
    shinyjs::runjs('document.getElementById("progress_bar").textContent = "Step 2 of 3";')
  })

  # Contenido paso 2 - CORREGIDO con inputId correcto
  output$step2_content <- renderUI({
    req(selected_type())

    choices <- switch(selected_type(),
                      "descriptive" = list(
                        "One variable (univariate)" = c(
                          "1 Quantitative" = "desc_1q",
                          "1 Categorical" = "desc_1c"
                        ),
                        "Two variables (bivariate)" = c(
                          "2 Quantitative - Paired" = "desc_2q_paired",
                          "2 Quantitative - Independent" = "desc_2q_indep",
                          "2 Categorical - Paired" = "desc_2c_paired",
                          "2 Categorical - Independent" = "desc_2c_indep",
                          "Quantitative vs Categorical" = "desc_qc"
                        )
                      ),
                      "inferential" = list(
                        "Compare means" = c(
                          "One sample t-test" = "ttest_onesample",
                          "Independent t-test" = "ttest_indep",
                          "Paired t-test" = "ttest_paired"
                        ),
                        "Compare proportions" = c(
                          "Proportions test" = "prop_test"
                        ),
                        "Test distributions" = c(
                          "Normality test" = "normality_test",
                          "Homogeneity of variance" = "homogeneity_test"
                        )
                      ),
                      "modeling" = list(
                        "General Linear Models" = c(
                          "ANOVA - One Way" = "anova_1way",
                          "ANOVA with Block" = "anova_1way_block",
                          "ANOVA - Two Way" = "anova_2way",
                          "ANOVA - Random Effects" = "anova_random",
                          "ANOVA - Mixed Effects" = "anova_mixed",
                          "ANCOVA" = "ancova"
                        ),
                        "Regression" = c(
                          "Simple Linear" = "reg_simple",
                          "Multiple Linear" = "reg_multiple"
                        ),
                        "Generalized Linear Models" = c(
                          "GLM - ANOVA" = "glm_anova",
                          "GLM - Regression" = "glm_reg"
                        )
                      ),
                      "specialized" = list(
                        "Nonparametric" = c(
                          "Kruskal-Wallis" = "np_kruskal",
                          "Friedman" = "np_friedman"
        ),
        "Survival Analysis" = c(
          "Kaplan-Meier" = "survival_km",
          "Cox Regression" = "survival_cox"
        ),
        "Correlation" = c(
          "Pearson" = "corr_pearson",
          "Spearman" = "corr_spearman",
          "Chi-square" = "chi2_test"
        )
      )
    )

    # Crear grupos de opciones con inputId único
    tagList(
      tags$div(
        id = "step2_options",
        lapply(names(choices), function(group_name) {
          group_choices <- choices[[group_name]]

          tags$div(
            class = "mb-4",
            h5(group_name, class = "text-primary"),
            lapply(names(group_choices), function(option_name) {
              option_value <- group_choices[[option_name]]

              tags$div(
                class = "form-check",
                tags$input(
                  class = "form-check-input",
                  type = "radio",
                  name = "subtype_selection",  # MISMO nombre para todos
                  id = paste0("opt_", option_value),
                  value = option_value,
                  onchange = paste0("
                          Shiny.setInputValue('selected_subtype', '", option_value, "');
                          Shiny.setInputValue('subtype_changed', Math.random());
                          ")
                ),
                tags$label(
                  class = "form-check-label",
                  `for` = paste0("opt_", option_value),
                  option_name
                )
              )
            })
          )
        })
      )
    )
  })

  # Observar cuando se selecciona una opción en el paso 2
  observeEvent(input$selected_subtype, {
    selected_subtype(input$selected_subtype)

    # Habilitar botón Next cuando hay selección
    updateActionButton(session, "next_btn", disabled = FALSE)
  })

  # Observar cuando se presiona Next en el paso 2
  observeEvent(input$next_btn, {
    req(current_step() == 2, selected_subtype())

    current_step(3)
    nav_select("wizard_tabs", "step3")

    # Actualizar UI
    updateActionButton(session, "next_btn", label = "Finish")
    updateActionButton(session, "next_btn", disabled = FALSE)
    shinyjs::html("step_text", "Step 3 of 3")
    shinyjs::runjs('document.getElementById("progress_bar").style.width = "100%";')
    shinyjs::runjs('document.getElementById("progress_bar").textContent = "Step 3 of 3";')
  })

  # Contenido paso 3 - SIMPLIFICADO para asegurar funcionamiento
  output$step3_content <- renderUI({
    req(selected_subtype())

    # Mapeo simple de subtipos a nombres
    tool_names <- list(
      "desc_1q" = "Descriptive - 1 Quantitative Variable",
      "desc_1c" = "Descriptive - 1 Categorical Variable",
      "desc_2q_paired" = "Descriptive - 2 Quantitative (Paired)",
      "desc_2q_indep" = "Descriptive - 2 Quantitative (Independent)",
      "desc_2c_paired" = "Descriptive - 2 Categorical (Paired)",
      "desc_2c_indep" = "Descriptive - 2 Categorical (Independent)",
      "desc_qc" = "Descriptive - Quantitative vs Categorical",
      "ttest_onesample" = "One-Sample t-test",
      "ttest_indep" = "Independent t-test",
      "ttest_paired" = "Paired t-test",
      "prop_test" = "Proportions Test",
      "normality_test" = "Normality Test",
      "homogeneity_test" = "Homogeneity of Variance Test",
      "anova_1way" = "ANOVA - One Way",
      "anova_1way_block" = "ANOVA with Block",
      "anova_2way" = "ANOVA - Two Way",
      "anova_random" = "ANOVA - Random Effects",
      "anova_mixed" = "ANOVA - Mixed Effects",
      "ancova" = "ANCOVA",
      "reg_simple" = "Simple Linear Regression",
      "reg_multiple" = "Multiple Linear Regression",
      "glm_anova" = "GLM - ANOVA",
      "glm_reg" = "GLM - Regression",
      "np_kruskal" = "Kruskal-Wallis Test",
      "np_friedman" = "Friedman Test",
      "survival_km" = "Kaplan-Meier Survival Analysis",
      "survival_cox" = "Cox Regression",
      "corr_pearson" = "Pearson Correlation",
      "corr_spearman" = "Spearman Correlation",
      "chi2_test" = "Chi-Square Test"
    )

    tool_name <- tool_names[[selected_subtype()]] %||% selected_subtype()

    div(
      class = "text-center",
      style = "padding: 50px;",
      icon("check-circle", class = "fa-5x text-success mb-4"),
      h3("Ready to load:", class = "mb-3"),
      h2(tool_name, class = "text-primary mb-4"),
      p("You have selected:", selected_subtype(), class = "text-muted mb-4"),
      br(),
      actionButton("load_tool", "Load Tool Now",
                  class = "btn-success btn-lg",
                  icon = icon("play")),
      br(), br(),
      actionButton("start_over", "← Start Over",
                  class = "btn-outline-secondary")
    )
  })

  # Observar cuando se presiona Finish en el paso 3
  observeEvent(input$next_btn, {
    req(current_step() == 3)

    showNotification(
      paste("Loading tool:", selected_subtype()),
      type = "success",
      duration = 5
    )

    cat("\n=== TOOL SELECTION COMPLETE ===\n")
    cat("Analysis Type:", selected_type(), "\n")
    cat("Tool ID:", selected_subtype(), "\n")
    cat("==============================\n")
  })

  # Observar botón Load Tool
  observeEvent(input$load_tool, {
    showNotification(
      paste("Now loading:", selected_subtype()),
      type = "message",
      duration = 5
    )
  })

  # Observar botón Start Over
  observeEvent(input$start_over, {
    # Resetear todo
    current_step(1)
    selected_type(NULL)
    selected_subtype(NULL)

    nav_select("wizard_tabs", "step1")

    # Actualizar UI
    updateActionButton(session, "prev_btn", disabled = TRUE)
    updateActionButton(session, "next_btn", disabled = TRUE)
    updateActionButton(session, "next_btn", label = "Next →")
    shinyjs::html("step_text", "Step 1 of 3")
    shinyjs::runjs('document.getElementById("progress_bar").style.width = "33%";')
    shinyjs::runjs('document.getElementById("progress_bar").textContent = "Step 1 of 3";')
  })

  # Navegación con botón Previous
  observeEvent(input$prev_btn, {
    if (current_step() > 1) {
      current_step(current_step() - 1)
      nav_select("wizard_tabs", paste0("step", current_step()))

      # Actualizar UI
      if (current_step() == 1) {
        updateActionButton(session, "prev_btn", disabled = TRUE)
        updateActionButton(session, "next_btn", disabled = TRUE)
        shinyjs::html("step_text", "Step 1 of 3")
        shinyjs::runjs('document.getElementById("progress_bar").style.width = "33%";')
        shinyjs::runjs('document.getElementById("progress_bar").textContent = "Step 1 of 3";')
      } else if (current_step() == 2) {
        updateActionButton(session, "next_btn", disabled = is.null(selected_subtype()))
        updateActionButton(session, "next_btn", label = "Next →")
        shinyjs::html("step_text", "Step 2 of 3")
        shinyjs::runjs('document.getElementById("progress_bar").style.width = "66%";')
        shinyjs::runjs('document.getElementById("progress_bar").textContent = "Step 2 of 3";')
      }
    }
  })

  # Habilitar/deshabilitar botón Next basado en selección
  observe({
    if (current_step() == 2) {
      if (is.null(input$selected_subtype) || input$selected_subtype == "") {
        updateActionButton(session, "next_btn", disabled = TRUE)
      } else {
        updateActionButton(session, "next_btn", disabled = FALSE)
      }
    }
  })
}

shinyApp(ui, server)

