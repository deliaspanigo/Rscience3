library(shiny)
library(bslib)
library(yaml)
library(DT)

ui <- page_sidebar(
  title = "📊 Rscience - Statistical Tools Repository",
  theme = bs_theme(version = 5, primary = "#2c3e50"),

  sidebar = sidebar(
    width = 350,

    # Filtros principales
    card(
      card_header("🔍 Filtros"),

      # 1. Categoría nivel 1
      selectInput("category01", "Categoría Nivel 1:",
                  choices = c("Todas", "Descriptive Statistics",
                              "General Linear Models", "Generalized Linear Models")),

      # 2. Categoría nivel 2
      selectInput("category02", "Categoría Nivel 2:",
                  choices = c("Todas", "Univariate", "Bivariate", "Multivariate")),

      # 3. Categoría nivel 3
      selectInput("category03", "Categoría Nivel 3:",
                  choices = c("Todas", "test", "model", "exploratory")),

      # 4. Tags
      selectizeInput("tags", "Etiquetas:",
                     choices = NULL, multiple = TRUE,
                     options = list(placeholder = "Escribe para buscar...")),

      # 5. Tipo de script
      checkboxGroupInput("script_types", "Tipo de Script:",
                         choices = c("basic", "standard", "advanced", "custom"),
                         selected = c("basic", "standard", "advanced", "custom")),

      # 6. Buscador
      textInput("search", "Buscar:",
                placeholder = "Nombre, descripción..."),

      # Botón para reset
      actionButton("reset_filters", "Limpiar filtros",
                   class = "btn-outline-secondary btn-sm")
    ),

    # Estadísticas
    card(
      card_header("📈 Estadísticas"),
      uiOutput("stats_info")
    )
  ),

  mainPanel(
    # Header con resultados
    uiOutput("results_header"),

    # Tabla de HERRAMIENTAS (no scripts)
    DTOutput("tools_table"),

    # Detalles de herramienta seleccionada con sus scripts
    uiOutput("tool_detail_panel")
  )
)

server <- function(input, output, session) {

  # Cargar configuración
  config <- reactive({
    file_path <- "tools_config.yml"

    if (!file.exists(file_path)) {
      showNotification("❌ No se encuentra el archivo de configuración",
                       type = "error")
      return(NULL)
    }

    tryCatch({
      data <- yaml::read_yaml(file_path)

      # Validar estructura
      if (is.null(data$tools)) {
        showNotification("❌ Estructura YML inválida - falta 'tools'", type = "error")
        return(NULL)
      }

      showNotification(paste("✅ Cargadas", length(data$tools), "herramientas"),
                       type = "message")

      data
    }, error = function(e) {
      showNotification(paste("❌ Error en YML:", e$message), type = "error")
      NULL
    })
  })

  # Extraer todas las HERRAMIENTAS con información de scripts
  all_tools <- reactive({
    req(config())

    tools_list <- config()$tools

    # Convertir a data frame para filtrado eficiente
    tools_df <- do.call(rbind, lapply(names(tools_list), function(tool_id) {
      tool <- tools_list[[tool_id]]

      # Contar scripts
      script_count <- if (!is.null(tool$scripts)) length(tool$scripts) else 0

      # Extraer tipos de scripts disponibles
      script_types <- if (!is.null(tool$scripts)) {
        paste(sapply(tool$scripts, function(s) s$script_type), collapse = ", ")
      } else ""

      data.frame(
        id = tool_id,
        label = tool$label %||% "",
        description = tool$description %||% "",
        category01 = tool$category01 %||% "",
        category02 = tool$category02 %||% "",
        category03 = tool$category03 %||% "",
        icon = tool$icon %||% "",
        tags = paste(tool$tags %||% "", collapse = ", "),
        script_count = script_count,
        script_types = script_types,
        has_scripts = script_count > 0,
        stringsAsFactors = FALSE
      )
    }))

    tools_df
  })

  # Actualizar opciones de filtros dinámicamente
  observe({
    tools <- all_tools()
    req(tools)

    # Actualizar categorías nivel 2 según nivel 1
    if (input$category01 != "Todas") {
      cat02 <- unique(tools$category02[tools$category01 == input$category01])
    } else {
      cat02 <- unique(tools$category02)
    }
    updateSelectInput(session, "category02",
                      choices = c("Todas", sort(cat02[cat02 != ""])))

    # Actualizar categorías nivel 3 según nivel 1 y 2
    if (input$category01 != "Todas" && input$category02 != "Todas") {
      cat03 <- unique(tools$category03[tools$category01 == input$category01 &
                                         tools$category02 == input$category02])
    } else if (input$category01 != "Todas") {
      cat03 <- unique(tools$category03[tools$category01 == input$category01])
    } else if (input$category02 != "Todas") {
      cat03 <- unique(tools$category03[tools$category02 == input$category02])
    } else {
      cat03 <- unique(tools$category03)
    }
    updateSelectInput(session, "category03",
                      choices = c("Todas", sort(cat03[cat03 != ""])))

    # Extraer todas las tags únicas
    all_tags <- unique(unlist(strsplit(tools$tags, ", ")))
    updateSelectizeInput(session, "tags",
                         choices = sort(all_tags[all_tags != ""]),
                         server = TRUE)
  })

  # Filtrar HERRAMIENTAS
  filtered_tools <- reactive({
    tools <- all_tools()
    req(tools)

    filtered <- tools

    # Filtro por categoría nivel 1
    if (input$category01 != "Todas") {
      filtered <- filtered[filtered$category01 == input$category01, ]
    }

    # Filtro por categoría nivel 2
    if (input$category02 != "Todas") {
      filtered <- filtered[filtered$category02 == input$category02, ]
    }

    # Filtro por categoría nivel 3
    if (input$category03 != "Todas") {
      filtered <- filtered[filtered$category03 == input$category03, ]
    }

    # Filtro por tags
    if (length(input$tags) > 0) {
      has_tags <- sapply(strsplit(filtered$tags, ", "), function(x) {
        any(input$tags %in% x)
      })
      filtered <- filtered[has_tags, ]
    }

    # Filtro por tipo de script
    if (length(input$script_types) > 0) {
      has_script_type <- sapply(strsplit(filtered$script_types, ", "), function(x) {
        any(input$script_types %in% x)
      })
      filtered <- filtered[has_script_type, ]
    }

    # Filtro por búsqueda
    if (nzchar(input$search)) {
      search_lower <- tolower(input$search)
      filtered <- filtered[
        grepl(search_lower, tolower(filtered$label)) |
          grepl(search_lower, tolower(filtered$description)) |
          grepl(search_lower, tolower(filtered$tags)),
      ]
    }

    filtered
  })

  # Resetear filtros
  observeEvent(input$reset_filters, {
    updateSelectInput(session, "category01", selected = "Todas")
    updateSelectInput(session, "category02", selected = "Todas")
    updateSelectInput(session, "category03", selected = "Todas")
    updateSelectizeInput(session, "tags", selected = character(0))
    updateCheckboxGroupInput(session, "script_types",
                             selected = c("basic", "standard", "advanced", "custom"))
    updateTextInput(session, "search", value = "")
  })

  # Estadísticas
  output$stats_info <- renderUI({
    tools <- all_tools()
    filtered <- filtered_tools()

    # Contar scripts totales
    total_scripts <- sum(tools$script_count)
    filtered_scripts <- sum(filtered$script_count)

    tagList(
      div(icon("toolbox"), " Herramientas: ", nrow(tools)),
      div(icon("file-code"), " Scripts totales: ", total_scripts),
      hr(style = "margin: 5px 0;"),
      div(icon("filter"), " Herramientas filtradas: ", nrow(filtered)),
      div(icon("file-code"), " Scripts filtrados: ", filtered_scripts)
    )
  })

  # Header de resultados
  output$results_header <- renderUI({
    filtered <- filtered_tools()

    if (nrow(filtered) == 0) {
      return(
        div(class = "alert alert-warning",
            icon("exclamation-triangle"),
            " No se encontraron herramientas con los filtros actuales.")
      )
    }

    total_scripts <- sum(filtered$script_count)

    div(
      class = "d-flex justify-content-between align-items-center mb-3",
      div(
        h4(paste("📋", nrow(filtered), "herramientas")),
        p(class = "text-muted mb-0",
          paste("(", total_scripts, "scripts disponibles)"))
      ),
      downloadButton("download_list", "Exportar lista",
                     class = "btn-sm")
    )
  })

  # Tabla de HERRAMIENTAS (no scripts individuales)
  output$tools_table <- renderDT({
    filtered <- filtered_tools()
    req(nrow(filtered) > 0)

    # Preparar datos para mostrar
    display_data <- filtered[, c("label", "category01", "category02",
                                 "category03", "script_count", "script_types")]

    # Añadir acción
    display_data$action <- paste0(
      '<button class="btn btn-sm btn-primary select-tool" data-id="',
      filtered$id, '">Ver Scripts</button>'
    )

    datatable(
      display_data,
      selection = 'none',
      rownames = FALSE,
      colnames = c("Herramienta", "Cat. N1", "Cat. N2", "Cat. N3",
                   "Scripts", "Tipos", "Acción"),
      options = list(
        pageLength = 10,
        dom = 'tip',
        columnDefs = list(
          list(targets = 6, orderable = FALSE, width = '100px'),
          list(targets = 4, className = 'dt-center'),
          list(targets = 5, width = '150px')
        )
      ),
      escape = FALSE
    )
  })

  # Detalles de herramienta seleccionada
  selected_tool <- reactiveVal(NULL)

  # Observar clics en botones de selección
  observeEvent(input$tools_table_cell_clicked, {
    info <- input$tools_table_cell_clicked

    if (!is.null(info$col) && info$col == 6) {  # Columna de acción
      tool_id <- info$value
      if (grepl('data-id="', tool_id)) {
        # Extraer el ID del botón
        tool_id <- gsub('.*data-id="([^"]+)".*', '\\1', tool_id)

        # Buscar la herramienta en config
        tools <- config()$tools
        if (tool_id %in% names(tools)) {
          selected_tool(tools[[tool_id]])
          selected_tool()$id <- tool_id  # Añadir ID
        }
      }
    }
  })

  # Panel de detalles de herramienta CON SUS SCRIPTS
  output$tool_detail_panel <- renderUI({
    tool <- selected_tool()
    if (is.null(tool)) return(NULL)

    # Obtener scripts de esta herramienta
    scripts <- tool$scripts

    card(
      card_header(
        class = "bg-primary text-white",
        div(
          class = "d-flex justify-content-between align-items-center",
          h5(tool$label, class = "mb-0"),
          span(
            class = "badge bg-light text-dark",
            paste(length(scripts), "scripts")
          )
        )
      ),

      # Información general de la herramienta
      card(
        card_header("📋 Información General"),
        layout_column_wrap(
          width = 1/2,
          card(
            tags$dl(
              class = "row",
              tags$dt(class = "col-sm-4", "Descripción:"),
              tags$dd(class = "col-sm-8", tool$description),

              tags$dt(class = "col-sm-4", "Categorías:"),
              tags$dd(class = "col-sm-8",
                      paste(tool$category01, "→", tool$category02, "→", tool$category03)),

              tags$dt(class = "col-sm-4", "Icono:"),
              tags$dd(class = "col-sm-8", icon(tool$icon))
            )
          ),
          card(
            strong("Etiquetas:"),
            div(
              class = "mt-1",
              lapply(tool$tags, function(tag) {
                span(class = "badge bg-secondary me-1 mb-1", tag)
              })
            )
          )
        )
      ),

      # LISTA DE SCRIPTS DISPONIBLES
      card(
        card_header("📁 Scripts Disponibles"),

        if (is.null(scripts) || length(scripts) == 0) {
          div(class = "alert alert-info",
              icon("info-circle"),
              " Esta herramienta no tiene scripts configurados.")
        } else {
          # Crear una card por cada script
          script_cards <- lapply(names(scripts), function(script_name) {
            script <- scripts[[script_name]]

            card(
              class = "mb-3 border-primary",
              card_header(
                div(
                  class = "d-flex justify-content-between align-items-center",
                  h6(script_name, class = "mb-0 text-primary"),
                  span(
                    class = paste0("badge ",
                                   switch(script$script_type,
                                          "basic" = "bg-success",
                                          "standard" = "bg-info",
                                          "advanced" = "bg-warning",
                                          "custom" = "bg-dark",
                                          "bg-secondary")),
                    script$script_type
                  )
                )
              ),

              layout_column_wrap(
                width = 1/2,

                # Información del script
                card(
                  tags$dl(
                    class = "row",
                    tags$dt(class = "col-sm-5", "Descripción:"),
                    tags$dd(class = "col-sm-7", script$description),

                    tags$dt(class = "col-sm-5", "Carpeta:"),
                    tags$dd(class = "col-sm-7",
                            tags$code(script$folder, style = "font-size: 0.8em;")),

                    tags$dt(class = "col-sm-5", "Archivo:"),
                    tags$dd(class = "col-sm-7",
                            tags$code(script$module_file, style = "font-size: 0.8em;")),

                    tags$dt(class = "col-sm-5", "Autor:"),
                    tags$dd(class = "col-sm-7", script$author),

                    tags$dt(class = "col-sm-5", "Versión:"),
                    tags$dd(class = "col-sm-7", script$version)
                  )
                ),

                # Configuración y acciones
                card(
                  tags$dl(
                    class = "row",
                    tags$dt(class = "col-sm-5", "Pestaña inicial:"),
                    tags$dd(class = "col-sm-7", script$start_tab),

                    tags$dt(class = "col-sm-5", "Reportes:"),
                    tags$dd(class = "col-sm-7",
                            paste(script$reporting %||% "No especificado", collapse = ", ")),

                    tags$dt(class = "col-sm-5", "Analyzer:"),
                    tags$dd(class = "col-sm-7", script$analyzer),

                    if (!is.null(script$dependencies)) {
                      list(
                        tags$dt(class = "col-sm-5", "Dependencias:"),
                        tags$dd(class = "col-sm-7",
                                paste(script$dependencies, collapse = ", "))
                      )
                    }
                  ),

                  # Botón para cargar este script específico
                  div(
                    class = "mt-3 text-center",
                    actionButton(
                      inputId = paste0("load_script_", tool$id, "_", script_name),
                      label = "Cargar este Script",
                      class = "btn-primary btn-sm",
                      icon = icon("play")
                    )
                  )
                )
              )
            )
          })

          # Retornar todos los cards de scripts
          tagList(script_cards)
        }
      )
    )
  })

  # Observador dinámico para botones de carga de scripts
  observe({
    tool <- selected_tool()
    if (is.null(tool) || is.null(tool$scripts)) return()

    # Crear observers para cada botón de script
    lapply(names(tool$scripts), function(script_name) {
      btn_id <- paste0("load_script_", tool$id, "_", script_name)

      observeEvent(input[[btn_id]], {
        script <- tool$scripts[[script_name]]

        showNotification(
          paste("🚀 Cargando:", tool$label, "-", script_name),
          type = "message", duration = 3
        )

        # Información detallada en consola
        cat("\n" , strrep("=", 60), "\n")
        cat("INICIANDO CARGA DE SCRIPT\n")
        cat("Herramienta ID:", tool$id, "\n")
        cat("Herramienta:", tool$label, "\n")
        cat("Script:", script_name, "\n")
        cat("Tipo:", script$script_type, "\n")
        cat("Carpeta:", script$folder, "\n")
        cat("Archivo:", script$module_file, "\n")
        cat("Pestaña inicial:", script$start_tab, "\n")

        # Verificar si el archivo existe
        full_path <- file.path(script$folder, script$module_file)
        if (file.exists(full_path)) {
          cat("✅ Archivo encontrado:", full_path, "\n")
        } else {
          cat("❌ Archivo NO encontrado:", full_path, "\n")
          showNotification("❌ No se encuentra el archivo del script",
                           type = "error")
        }

        cat(strrep("=", 60), "\n\n")

        # Aquí iría tu lógica completa para:
        # 1. Crear carpeta temporal con timestamp
        # 2. Copiar archivos desde script$folder
        # 3. Hacer source del script$module_file
        # 4. Inicializar el módulo con script$start_tab
      })
    })
  })

  # Exportar lista de herramientas
  output$download_list <- downloadHandler(
    filename = function() {
      paste("herramientas_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(filtered_tools(), file, row.names = FALSE)
    }
  )
}

shinyApp(ui, server)
