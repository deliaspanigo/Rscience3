library(shiny)
library(bslib)
library(visNetwork)

ui <- page_sidebar(
  title = "🗺️ Concept Map Selector",
  theme = bs_theme(version = 5),

  sidebar = sidebar(
    width = 300,
    card(
      card_header("Navigation"),
      div(
        class = "btn-group w-100 mb-2",
        actionButton("zoom_in", "Zoom In", class = "btn-sm"),
        actionButton("zoom_out", "Zoom Out", class = "btn-sm"),
        actionButton("reset_view", "Reset", class = "btn-sm")
      ),
      hr(),
      h6("Selected Node:"),
      verbatimTextOutput("selected_node_info"),
      hr(),
      h6("Tool Details:"),
      uiOutput("tool_details"),
      hr(),
      actionButton("load_tool", "Load Selected Tool",
                   class = "btn-success w-100",
                   disabled = TRUE)
    )
  ),

  # Mapa visual
  navset_card_tab(
    full_screen = TRUE,
    nav_panel(
      title = tagList(icon("project-diagram"), "Concept Map"),
      visNetworkOutput("concept_map", height = "600px")
    ),
    nav_panel(
      title = tagList(icon("list"), "List View"),
      DTOutput("tools_list")
    ),
    nav_panel(
      title = tagList(icon("info-circle"), "Legend"),
      card(
        card_header("Map Legend"),
        tags$ul(
          class = "list-unstyled",
          tags$li(
            tags$span(style = "display: inline-block; width: 20px; height: 20px; background-color: #3498db; border-radius: 3px; margin-right: 10px;"),
            "Main Category"
          ),
          tags$li(
            tags$span(style = "display: inline-block; width: 20px; height: 20px; background-color: #e74c3c; border-radius: 50%; margin-right: 10px;"),
            "Sub-category"
          ),
          tags$li(
            tags$span(style = "display: inline-block; width: 20px; height: 20px; background-color: #2ecc71; border-radius: 50%; margin-right: 10px;"),
            "Available Tool"
          ),
          tags$li(
            tags$span(style = "display: inline-block; width: 20px; height: 20px; background-color: #95a5a6; border-radius: 3px; margin-right: 10px;"),
            "Analysis Type"
          )
        ),
        hr(),
        h6("How to use:"),
        tags$ol(
          tags$li("Click on any node to select it"),
          tags$li("Green nodes are available tools"),
          tags$li("Click 'Load Selected Tool' to proceed"),
          tags$li("Use mouse wheel to zoom in/out"),
          tags$li("Drag to move around the map")
        )
      )
    )
  )
)

server <- function(input, output, session) {

  # Datos para el mapa conceptual
  nodes <- reactive({
    data.frame(
      id = 1:33,
      label = c(
        "Descriptive Stats",
        "1 Quantitative", "1 Categorical",
        "2 Quantitative", "2 Categorical", "QC",
        "Paired", "Independent",
        "GLM",
        "ANOVA", "Regression", "ANCOVA",
        "1-Way", "2-Way", "Mixed", "Random",
        "With Block",
        "Simple", "Multiple", "Double",
        "GLMM",
        "Nonparametric",
        "Kruskal-Wallis", "Friedman",
        "Classical Tests",
        "t-tests", "Proportions", "Normality",
        "Survival",
        "Kaplan-Meier", "Cox",
        "Correlation",
        "Chi-square"
      ),
      group = c(
        "category",
        "tool", "tool",
        "subcategory", "subcategory", "tool",
        "type", "type",
        "category",
        "subcategory", "subcategory", "tool",
        "type", "type", "type", "type",
        "tool",
        "type", "type", "type",
        "category",
        "category",
        "tool", "tool",
        "category",
        "subcategory", "tool", "tool",
        "category",
        "tool", "tool",
        "category",
        "tool"
      ),
      title = c(
        "Descriptive Statistics - Summarize and describe data",
        "One quantitative variable - Mean, SD, distribution",
        "One categorical variable - Frequency table, proportions",
        "Two quantitative variables - Scatterplots, correlation",
        "Two categorical variables - Crosstabs, association",
        "Quantitative vs Categorical - Group comparisons",
        "Paired/Repeated measures - Same subjects measured twice",
        "Independent groups - Different groups compared",
        "General Linear Models - ANOVA, regression, ANCOVA",
        "Analysis of Variance - Compare means across groups",
        "Regression Analysis - Model relationships",
        "Analysis of Covariance - ANOVA with covariates",
        "One factor - Single independent variable",
        "Two factors - Two independent variables",
        "Mixed effects - Fixed and random effects combined",
        "Random effects - Groups as random sample",
        "ANOVA with blocking factor - Control for nuisance variables",
        "One predictor - Simple linear regression",
        "Multiple predictors - Multiple linear regression",
        "Two predictors - Double linear regression",
        "Generalized Linear Mixed Models - Extensions of GLM",
        "Distribution-free tests - No distribution assumptions",
        "Kruskal-Wallis test - Nonparametric one-way ANOVA",
        "Friedman test - Nonparametric repeated measures",
        "Classical parametric tests - Traditional statistical tests",
        "Student's t-tests - Compare means",
        "Proportions tests - Compare proportions",
        "Normality tests - Test for normal distribution",
        "Survival Analysis - Time-to-event data",
        "Kaplan-Meier estimator - Nonparametric survival curves",
        "Cox regression - Proportional hazards model",
        "Correlation Analysis - Measure associations",
        "Chi-square test - Test for independence"
      ),
      shape = c(
        "box", "dot", "dot", "triangle", "triangle", "dot",
        "square", "square", "box", "triangle", "triangle", "dot",
        "square", "square", "square", "square", "dot",
        "square", "square", "square", "box", "box",
        "dot", "dot", "box", "triangle", "dot", "dot",
        "box", "dot", "dot", "box", "dot"
      ),
      color = c(
        "#3498db", "#2ecc71", "#2ecc71", "#f39c12", "#f39c12", "#2ecc71",
        "#95a5a6", "#95a5a6", "#9b59b6", "#e74c3c", "#e74c3c", "#2ecc71",
        "#95a5a6", "#95a5a6", "#95a5a6", "#95a5a6", "#2ecc71",
        "#95a5a6", "#95a5a6", "#95a5a6", "#1abc9c", "#e67e22",
        "#2ecc71", "#2ecc71", "#34495e", "#7f8c8d", "#2ecc71", "#2ecc71",
        "#d35400", "#2ecc71", "#2ecc71", "#16a085", "#2ecc71"
      ),
      size = c(
        30, 20, 20, 25, 25, 20,
        15, 15, 30, 25, 25, 20,
        15, 15, 15, 15, 20,
        15, 15, 15, 30, 30,
        20, 20, 30, 25, 20, 20,
        30, 20, 20, 30, 20
      ),
      borderWidth = 2,
      font = list(
        size = c(18, 14, 14, 16, 16, 14, 12, 12, 18, 16, 16, 14,
                 12, 12, 12, 12, 14, 12, 12, 12, 18, 18, 14, 14,
                 18, 16, 14, 14, 18, 14, 14, 18, 14),
        multi = TRUE
      )
    )
  })

  edges <- reactive({
    data.frame(
      from = c(
        1, 1, 1,
        4, 4,
        5, 5,
        9, 9, 9,
        10, 10, 10, 10, 10,
        11, 11, 11,
        21,
        22, 22,
        25, 25, 25,
        28, 28,
        31, 31,
        32, 5
      ),
      to = c(
        2, 3, 6,
        7, 8,
        7, 8,
        10, 11, 12,
        13, 14, 15, 16, 17,
        18, 19, 20,
        13,
        23, 24,
        26, 27, 33,
        29, 30,
        2, 3,
        33, 33
      ),
      arrows = "to",
      color = "#7f8c8d",
      width = 2
    )
  })

  # Tool information database
  tool_database <- reactive({
    list(
      "desc_1q" = list(
        id = "desc_1q",
        name = "Descriptive - 1 Quantitative Variable",
        category = "Descriptive Statistics",
        description = "Summary statistics for one quantitative variable: mean, median, standard deviation, range, quartiles, etc.",
        icon = "chart-line",
        color = "#2ecc71",
        script = "desc_1q_s001",
        folder = "../tool_001_descriptive_1q"
      ),
      "desc_1c" = list(
        id = "desc_1c",
        name = "Descriptive - 1 Categorical Variable",
        category = "Descriptive Statistics",
        description = "Frequency table and proportions for one categorical variable.",
        icon = "chart-pie",
        color = "#2ecc71",
        script = "desc_1c_s001",
        folder = "../tool_002_descriptive_1c"
      ),
      "desc_qc" = list(
        id = "desc_qc",
        name = "Descriptive - Quantitative vs Categorical",
        category = "Descriptive Statistics",
        description = "Compare quantitative variables across categorical groups.",
        icon = "chart-bar",
        color = "#2ecc71",
        script = "desc_qc_s001",
        folder = "../tool_003_descriptive_qc"
      ),
      "ancova" = list(
        id = "ancova",
        name = "Analysis of Covariance (ANCOVA)",
        category = "General Linear Models",
        description = "ANOVA with continuous covariates to control for confounding variables.",
        icon = "sliders-h",
        color = "#2ecc71",
        script = "ancova_s001",
        folder = "../tool_004_ancova"
      ),
      "anova_block" = list(
        id = "anova_block",
        name = "ANOVA with Blocking",
        category = "General Linear Models",
        description = "Randomized block design ANOVA to control for nuisance factors.",
        icon = "th-large",
        color = "#2ecc71",
        script = "anova_block_s001",
        folder = "../tool_005_anova_block"
      ),
      "np_kruskal" = list(
        id = "np_kruskal",
        name = "Kruskal-Wallis Test",
        category = "Nonparametric",
        description = "Nonparametric alternative to one-way ANOVA for comparing 3+ groups.",
        icon = "sort-amount-up",
        color = "#2ecc71",
        script = "kruskal_s001",
        folder = "../tool_006_kruskal"
      ),
      "np_friedman" = list(
        id = "np_friedman",
        name = "Friedman Test",
        category = "Nonparametric",
        description = "Nonparametric alternative to repeated measures ANOVA.",
        icon = "retweet",
        color = "#2ecc71",
        script = "friedman_s001",
        folder = "../tool_007_friedman"
      ),
      "normality_test" = list(
        id = "normality_test",
        name = "Normality Test",
        category = "Classical Tests",
        description = "Test if data follows normal distribution (Shapiro-Wilk, Kolmogorov-Smirnov).",
        icon = "bell",
        color = "#2ecc71",
        script = "normality_s001",
        folder = "../tool_008_normality"
      ),
      "survival_km" = list(
        id = "survival_km",
        name = "Kaplan-Meier Survival",
        category = "Survival Analysis",
        description = "Nonparametric estimator of survival function for time-to-event data.",
        icon = "heartbeat",
        color = "#2ecc71",
        script = "km_s001",
        folder = "../tool_009_kaplan_meier"
      ),
      "survival_cox" = list(
        id = "survival_cox",
        name = "Cox Regression",
        category = "Survival Analysis",
        description = "Proportional hazards regression model for survival data.",
        icon = "heart",
        color = "#2ecc71",
        script = "cox_s001",
        folder = "../tool_010_cox"
      ),
      "chi2_test" = list(
        id = "chi2_test",
        name = "Chi-Square Test",
        category = "Correlation",
        description = "Test for independence between categorical variables.",
        icon = "percent",
        color = "#2ecc71",
        script = "chi2_s001",
        folder = "../tool_011_chi2"
      )
    )
  })

  # Mapeo de nodos a herramientas
  node_to_tool <- reactive({
    list(
      "2" = "desc_1q",      # 1 Quantitative
      "3" = "desc_1c",      # 1 Categorical
      "6" = "desc_qc",      # QC
      "12" = "ancova",      # ANCOVA
      "17" = "anova_block", # With Block
      "23" = "np_kruskal",  # Kruskal-Wallis
      "24" = "np_friedman", # Friedman
      "27" = "normality_test", # Normality
      "29" = "survival_km", # Kaplan-Meier
      "30" = "survival_cox", # Cox
      "33" = "chi2_test"    # Chi-square
    )
  })

  # Renderizar mapa
  output$concept_map <- renderVisNetwork({
    visNetwork(nodes(), edges()) %>%
      visGroups(groupname = "category",
                shape = "box",
                color = list(background = "#3498db", border = "#2980b9"),
                font = list(size = 16, color = "white")) %>%
      visGroups(groupname = "subcategory",
                shape = "triangle",
                color = list(background = "#e74c3c", border = "#c0392b"),
                font = list(size = 14, color = "white")) %>%
      visGroups(groupname = "tool",
                shape = "dot",
                color = list(background = "#2ecc71", border = "#27ae60"),
                font = list(size = 12)) %>%
      visGroups(groupname = "type",
                shape = "square",
                color = list(background = "#95a5a6", border = "#7f8c8d"),
                font = list(size = 10)) %>%
      visOptions(highlightNearest = list(enabled = TRUE, degree = 1, hover = TRUE),
                 nodesIdSelection = list(enabled = TRUE, style = 'width: 200px;')) %>%
      visPhysics(stabilization = TRUE, solver = "repulsion") %>%
      visLayout(randomSeed = 42) %>%
      visInteraction(navigationButtons = TRUE,
                     keyboard = TRUE,
                     dragNodes = TRUE,
                     dragView = TRUE,
                     zoomView = TRUE) %>%
      visEvents(select = "function(nodes) {
                Shiny.setInputValue('selected_node', nodes.nodes);
                }") %>%
      addFontAwesome() %>%
      visIgraphLayout(layout = "layout_with_fr")
  })

  # Información del nodo seleccionado
  selected_node_id <- reactiveVal(NULL)
  selected_tool_id <- reactiveVal(NULL)

  observeEvent(input$selected_node, {
    if (length(input$selected_node) > 0) {
      node_id <- as.character(input$selected_node[1])
      selected_node_id(node_id)

      # Verificar si es una herramienta disponible
      tool_mapping <- node_to_tool()
      if (node_id %in% names(tool_mapping)) {
        tool_id <- tool_mapping[[node_id]]
        selected_tool_id(tool_id)
        updateActionButton(session, "load_tool", disabled = FALSE)
      } else {
        selected_tool_id(NULL)
        updateActionButton(session, "load_tool", disabled = TRUE)
      }
    }
  })

  output$selected_node_info <- renderPrint({
    req(selected_node_id())

    node_id <- as.numeric(selected_node_id())
    node_data <- nodes()[nodes()$id == node_id, ]

    cat("Node: ", node_data$label, "\n")
    cat("Type: ", switch(node_data$group,
                         "category" = "Main Category",
                         "subcategory" = "Sub-category",
                         "tool" = "Available Tool",
                         "type" = "Analysis Type",
                         node_data$group), "\n")
    cat("\nDescription:\n", node_data$title)
  })

  # Detalles de la herramienta seleccionada
  output$tool_details <- renderUI({
    req(selected_tool_id())

    tool <- tool_database()[[selected_tool_id()]]

    if (is.null(tool)) {
      return(p("Select a green node (available tool) for details."))
    }

    tagList(
      h5(tool$name, style = paste0("color: ", tool$color, ";")),
      p(tool$description, style = "font-size: 0.9em;"),
      tags$ul(
        class = "list-unstyled small",
        tags$li(icon("folder"), " Script: ", tool$script),
        tags$li(icon("folder-open"), " Folder: ", basename(tool$folder))
      )
    )
  })

  # Tabla de lista de herramientas
  output$tools_list <- renderDT({
    tools <- tool_database()
    tools_df <- do.call(rbind, lapply(names(tools), function(tool_id) {
      tool <- tools[[tool_id]]
      data.frame(
        Name = tool$name,
        Category = tool$category,
        Description = tool$description,
        Actions = paste0(
          '<button class="btn btn-sm btn-primary select-tool-btn" data-id="',
          tool_id, '">Select</button>'
        ),
        stringsAsFactors = FALSE
      )
    }))

    datatable(
      tools_df,
      selection = 'none',
      rownames = FALSE,
      escape = FALSE,
      options = list(
        pageLength = 10,
        dom = 'tip',
        columnDefs = list(
          list(targets = 3, orderable = FALSE, width = '100px'),
          list(targets = 2, width = '300px')
        )
      )
    )
  })

  # Observar clics en botones de la tabla
  observeEvent(input$tools_list_cell_clicked, {
    info <- input$tools_list_cell_clicked

    if (!is.null(info$col) && info$col == 3) {  # Columna de Actions
      tool_id <- gsub('.*data-id="([^"]+)".*', '\\1', info$value)
      if (tool_id %in% names(tool_database())) {
        selected_tool_id(tool_id)
        updateActionButton(session, "load_tool", disabled = FALSE)

        showNotification(
          paste("Selected:", tool_database()[[tool_id]]$name),
          type = "info"
        )
      }
    }
  })

  # Botones de zoom
  observeEvent(input$zoom_in, {
    visNetworkProxy("concept_map") %>%
      visGetScale() %>%
      visSetOptions(list(physics = FALSE)) %>%
      visFit(nodes = NULL, animation = list(duration = 500))

    shinyjs::runjs('
      var network = $("#concept_map").data("visNetwork");
      if (network) {
        var scale = network.getScale();
        network.moveTo({scale: scale * 1.2});
      }
    ')
  })

  observeEvent(input$zoom_out, {
    shinyjs::runjs('
      var network = $("#concept_map").data("visNetwork");
      if (network) {
        var scale = network.getScale();
        network.moveTo({scale: scale * 0.8});
      }
    ')
  })

  observeEvent(input$reset_view, {
    visNetworkProxy("concept_map") %>%
      visFit(nodes = NULL, animation = list(duration = 1000))
  })

  # Cargar herramienta seleccionada
  observeEvent(input$load_tool, {
    req(selected_tool_id())

    tool <- tool_database()[[selected_tool_id()]]

    showNotification(
      paste("Loading:", tool$name),
      type = "success",
      duration = 5
    )

    cat("\n" , strrep("=", 60), "\n")
    cat("LOADING TOOL FROM CONCEPT MAP\n")
    cat("Tool ID:", tool$id, "\n")
    cat("Name:", tool$name, "\n")
    cat("Category:", tool$category, "\n")
    cat("Script:", tool$script, "\n")
    cat("Folder:", tool$folder, "\n")
    cat(strrep("=", 60), "\n\n")

    # Aquí iría la lógica para:
    # 1. Cargar el script desde tool$folder
    # 2. Inicializar el módulo
    # 3. Mostrar la interfaz de la herramienta
  })
}

shinyApp(ui, server)
