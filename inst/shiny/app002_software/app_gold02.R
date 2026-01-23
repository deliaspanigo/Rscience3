library(shiny)
library(bslib)

ui <- page_sidebar(
  title = "🔍 Search-First Tool Selector",
  theme = bs_theme(version = 5),

  sidebar = sidebar(
    width = 350,

    # BUSCADOR PRINCIPAL (CORREGIDO)
    card(
      card_header("Quick Search", class = "bg-primary text-white"),
      div(
        class = "input-group",
        tags$input(
          id = "tool_search",
          type = "text",
          class = "form-control",
          placeholder = "Type to search tools... (e.g., 'anova', 't-test', 'regression')",
          `aria-label` = "Search tools"
        ),
        span(
          class = "input-group-text",
          icon("search")
        )
      ),
      div(
        class = "text-muted small mt-2",
        icon("lightbulb"),
        " Try: '1way', 'categorical', 'mixed effects', 'nonparametric'"
      )
    ),

    # FILTROS RÁPIDOS
    card(
      card_header("Quick Filters"),
      layout_column_wrap(
        width = 1/2,
        actionButton("filter_descriptive", "📊 Descriptive",
                     class = "btn-outline-primary"),
        actionButton("filter_glm", "📈 GLM",
                     class = "btn-outline-success")
      ),
      layout_column_wrap(
        width = 1/2,
        actionButton("filter_glmm", "🎯 GLMM",
                     class = "btn-outline-warning"),
        actionButton("filter_nonpar", "📉 NonParam",
                     class = "btn-outline-danger")
      ),
      div(
        class = "mt-2",
        actionButton("clear_filters", "Clear All Filters",
                     class = "btn-sm btn-outline-secondary")
      )
    ),

    # HISTORIAL RECIENTE
    uiOutput("recent_tools")
  ),

  # ÁREA DE RESULTADOS
  navset_card_pill(
    id = "results_tabs",
    full_screen = TRUE,

    nav_panel(
      title = tagList(icon("list"), "Search Results"),
      uiOutput("search_results")
    ),

    nav_panel(
      title = tagList(icon("layer-group"), "By Category"),
      uiOutput("category_view")
    ),

    nav_panel(
      title = tagList(icon("star"), "Favorites"),
      uiOutput("favorites_view")
    )
  )
)

server <- function(input, output, session) {

  # BASE DE DATOS DE HERRAMIENTAS
  tools_db <- reactive({
    data.frame(
      id = c(
        # Descriptive
        "desc_1q", "desc_1c", "desc_2q_paired", "desc_2q_indep",
        "desc_2c_paired", "desc_2c_indep", "desc_qc",
        # GLM
        "anova_1way", "anova_1way_block", "anova_2way", "anova_1way_random",
        "anova_2way_random", "anova_2way_mixed", "ancova",
        "reg_simple", "reg_double", "reg_multiple",
        # GLMM
        "glmm_anova_1way", "glmm_reg_simple",
        # Nonparametric
        "np_kruskal", "np_friedman",
        # Classical
        "ttest_onesample", "ttest_indep", "ttest_paired",
        "prop_test", "homogeneity_test", "normality_test",
        # Survival
        "survival_km", "survival_cox",
        # Correlation
        "corr_pearson", "corr_spearman", "chi2_test"
      ),
      name = c(
        "Descriptive - 1 Quantitative",
        "Descriptive - 1 Categorical",
        "Descriptive - 2 Quantitative (Paired)",
        "Descriptive - 2 Quantitative (Independent)",
        "Descriptive - 2 Categorical (Paired)",
        "Descriptive - 2 Categorical (Independent)",
        "Descriptive - Quantitative vs Categorical",
        "ANOVA - One Way Fixed",
        "ANOVA - One Way with Block",
        "ANOVA - Two Way Fixed",
        "ANOVA - One Way Random",
        "ANOVA - Two Way Random",
        "ANOVA - Two Way Mixed",
        "ANCOVA",
        "Simple Linear Regression",
        "Double Linear Regression",
        "Multiple Linear Regression",
        "GLM - ANOVA One Way",
        "GLM - Simple Regression",
        "Kruskal-Wallis Test",
        "Friedman Test",
        "One-Sample t-test",
        "Independent t-test",
        "Paired t-test",
        "Proportions Test",
        "Homogeneity Test",
        "Normality Test",
        "Kaplan-Meier Survival",
        "Cox Regression",
        "Pearson Correlation",
        "Spearman Correlation",
        "Chi-Square Test"
      ),
      category = rep(c(
        "Descriptive", "Descriptive", "Descriptive", "Descriptive",
        "Descriptive", "Descriptive", "Descriptive",
        "GLM", "GLM", "GLM", "GLM", "GLM", "GLM", "GLM",
        "GLM", "GLM", "GLM",
        "GLMM", "GLMM",
        "Nonparametric", "Nonparametric",
        "Classical", "Classical", "Classical", "Classical",
        "Classical", "Classical",
        "Survival", "Survival",
        "Correlation", "Correlation", "Correlation"
      ), each = 1),
      subcategory = c(
        "Univariate", "Univariate", "Bivariate", "Bivariate",
        "Bivariate", "Bivariate", "Bivariate",
        "ANOVA", "ANOVA", "ANOVA", "ANOVA", "ANOVA", "ANOVA", "ANCOVA",
        "Regression", "Regression", "Regression",
        "ANOVA", "Regression",
        "Rank Tests", "Rank Tests",
        "t-tests", "t-tests", "t-tests", "Proportions",
        "Variance", "Distribution",
        "Nonparametric", "Parametric",
        "Parametric", "Nonparametric", "Categorical"
      ),
      keywords = c(
        # Palabras clave para búsqueda
        "quantitative univariate stats descriptivo",
        "categorical frequency table descriptivo",
        "paired quantitative bivariate descriptivo",
        "independent quantitative bivariate descriptivo",
        "paired categorical mcnemar descriptivo",
        "independent categorical chi2 descriptivo",
        "quantitative categorical descriptivo",
        "anova fixed effects one way glm",
        "anova randomized block design glm",
        "anova two way factorial glm",
        "anova random effects one way glm",
        "anova random effects two way glm",
        "anova mixed effects two way glm",
        "ancova covariance analysis glm",
        "simple linear regression glm",
        "double linear regression glm",
        "multiple linear regression glm",
        "generalized linear model anova glmm",
        "generalized linear model regression glmm",
        "kruskal wallis nonparametric rank test",
        "friedman nonparametric repeated measures",
        "one sample t test classical parametric",
        "independent samples t test classical",
        "paired samples t test classical",
        "proportions test z test classical",
        "homogeneity variance test classical",
        "normality test shapiro kolmogorov classical",
        "kaplan meier survival analysis",
        "cox proportional hazards survival",
        "pearson correlation parametric",
        "spearman correlation nonparametric rank",
        "chi square test categorical"
      ),
      stringsAsFactors = FALSE
    )
  })

  # Filtro activo
  active_filter <- reactiveVal(NULL)

  # Observar filtros
  observeEvent(input$filter_descriptive, {
    active_filter("Descriptive")
  })
  observeEvent(input$filter_glm, {
    active_filter("GLM")
  })
  observeEvent(input$filter_glmm, {
    active_filter("GLMM")
  })
  observeEvent(input$filter_nonpar, {
    active_filter("Nonparametric")
  })
  observeEvent(input$clear_filters, {
    active_filter(NULL)
  })

  # Búsqueda inteligente
  search_results <- reactive({
    query <- input$tool_search
    db <- tools_db()

    # Si hay filtro activo
    if (!is.null(active_filter())) {
      db <- db[db$category == active_filter(), ]
    }

    if (is.null(query) || query == "") return(db)

    # Búsqueda en múltiples campos
    query_lower <- tolower(query)
    matches <- grepl(query_lower, tolower(db$name)) |
      grepl(query_lower, tolower(db$category)) |
      grepl(query_lower, tolower(db$subcategory)) |
      grepl(query_lower, tolower(db$keywords))

    db[matches, ]
  })

  # Resultados de búsqueda
  output$search_results <- renderUI({
    results <- search_results()

    if (nrow(results) == 0) {
      return(
        card(
          class = "text-center text-muted",
          icon("search", class = "fa-3x mb-3"),
          h4("No tools found"),
          p("Try different keywords or clear filters")
        )
      )
    }

    # Crear tarjetas para cada resultado
    tool_cards <- lapply(1:nrow(results), function(i) {
      tool <- results[i, ]

      card(
        class = "mb-3",
        card_header(
          div(
            class = "d-flex justify-content-between",
            h6(tool$name, class = "mb-0"),
            actionButton(
              paste0("select_", tool$id),
              "Select",
              class = "btn-sm btn-primary"
            )
          )
        ),
        div(
          class = "d-flex justify-content-between text-muted small",
          span(paste("Category:", tool$category)),
          span(paste("Type:", tool$subcategory))
        ),
        if (!is.null(tool$keywords)) {
          tags$small(
            class = "text-muted",
            "Keywords: ",
            tags$i(tool$keywords)
          )
        }
      )
    })

    div(tool_cards)
  })

  # Vista por categoría
  output$category_view <- renderUI({
    db <- tools_db()
    categories <- unique(db$category)

    # Crear un accordion manualmente
    panels <- lapply(categories, function(cat) {
      tools_in_cat <- db[db$category == cat, ]

      # Contador
      count <- nrow(tools_in_cat)

      tags$div(
        class = "card mb-2",
        tags$div(
          class = "card-header",
          id = paste0("heading_", cat),
          tags$h6(
            class = "mb-0",
            tags$button(
              class = "btn btn-link w-100 text-start",
              `data-bs-toggle` = "collapse",
              `data-bs-target` = paste0("#collapse_", cat),
              `aria-expanded` = "false",
              `aria-controls` = paste0("collapse_", cat),
              cat,
              tags$span(class = "badge bg-secondary ms-2", count)
            )
          )
        ),
        tags$div(
          id = paste0("collapse_", cat),
          class = "collapse",
          `aria-labelledby` = paste0("heading_", cat),
          tags$div(
            class = "card-body",
            lapply(1:nrow(tools_in_cat), function(i) {
              tool <- tools_in_cat[i, ]
              tags$div(
                class = "d-flex justify-content-between align-items-center p-2 border-bottom",
                tags$span(tool$name),
                actionButton(
                  paste0("cat_select_", tool$id),
                  "Select",
                  class = "btn-xs btn-outline-primary"
                )
              )
            })
          )
        )
      )
    })

    tagList(panels)
  })

  # Vista de favoritos
  output$favorites_view <- renderUI({
    card(
      class = "text-center text-muted",
      icon("star", class = "fa-3x mb-3"),
      h4("No favorites yet"),
      p("Click the star icon on any tool to add it to favorites")
    )
  })

  # Historial reciente
  output$recent_tools <- renderUI({
    card(
      card_header("Recent Tools"),
      tags$ul(
        class = "list-unstyled",
        tags$li(
          class = "border-bottom py-2",
          actionLink("recent_anova", "ANOVA - One Way Fixed")
        ),
        tags$li(
          class = "border-bottom py-2",
          actionLink("recent_ttest", "Independent t-test")
        ),
        tags$li(
          class = "py-2",
          actionLink("recent_reg", "Simple Linear Regression")
        )
      )
    )
  })

  # Observadores para selección
  observe({
    db <- tools_db()
    lapply(db$id, function(tool_id) {
      observeEvent(input[[paste0("select_", tool_id)]], {
        tool_name <- db$name[db$id == tool_id]
        showNotification(
          paste("Selected:", tool_name),
          type = "message",
          duration = 3
        )
        cat("\nSELECTED TOOL:", tool_name, "\n")
        cat("ID:", tool_id, "\n")
        # Aquí cargarías la herramienta
      })
    })

    # Observadores para selección desde categoría
    lapply(db$id, function(tool_id) {
      observeEvent(input[[paste0("cat_select_", tool_id)]], {
        tool_name <- db$name[db$id == tool_id]
        showNotification(
          paste("Selected from category:", tool_name),
          type = "message",
          duration = 3
        )
      })
    })
  })

  # Observadores para historial
  observeEvent(input$recent_anova, {
    showNotification("Loading: ANOVA - One Way Fixed", type = "message")
  })

  observeEvent(input$recent_ttest, {
    showNotification("Loading: Independent t-test", type = "message")
  })

  observeEvent(input$recent_reg, {
    showNotification("Loading: Simple Linear Regression", type = "message")
  })
}

shinyApp(ui, server)
