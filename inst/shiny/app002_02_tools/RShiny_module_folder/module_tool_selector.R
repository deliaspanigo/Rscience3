module_tool_selector_ui <- function(id) {
  ns <- NS(id)
  tagList(
    tags$head(
      tags$style(HTML(paste0("
        /* Forzar el estado deshabilitado en Selectize */
        .selectize-control.disabled {
          opacity: 1 !important; /* Mantenemos opacidad para que se vea el texto gris */
        }

        .selectize-control.disabled .selectize-input,
        .selectize-control.disabled .selectize-input input {
          background-color: #f2f2f2 !important; /* Gris claro de fondo */
          color: #a1a1a1 !important;           /* Texto gris */
          cursor: not-allowed !important;      /* Cursor de prohibido */
          border-color: #d1d1d1 !important;
          box-shadow: none !important;
        }

        .selectize-control.disabled .selectize-input * {
          cursor: not-allowed !important;
        }
      ")))
    ),

    shinyjs::useShinyjs(),

    # Panel de edición (selectize) - SIEMPRE presente pero oculto/mostrado
    shinyjs::hidden(
      div(
        id = ns("panel_edit"),
        card(
          style = "overflow: visible;",
          card_header("Tools & Scripts"),
          layout_column_wrap(
            width = 1/3,
            # COLUMNA 1: Category
            div(
              selectizeInput(ns("sel_category"), "1. Category", choices = "", width = "100%"),
              tags$small(id = ns("msg_category"), class = "text-muted",
                         style = "display: block; margin-top: -15px; margin-bottom: 10px; min-height: 15px;")
            ),
            # COLUMNA 2: Tool
            div(
              selectizeInput(ns("sel_tool_id"), "2. Tool", choices = "", width = "100%"),
              tags$small(id = ns("msg_tool"), class = "text-muted",
                         style = "display: block; margin-top: -15px; margin-bottom: 10px; min-height: 15px;")
            ),
            # COLUMNA 3: Script
            div(
              selectizeInput(ns("sel_script"), "3. Script", choices = "", width = "100%"),
              tags$small(id = ns("msg_script"), class = "text-muted",
                         style = "display: block; margin-top: -15px; margin-bottom: 10px; min-height: 15px;")
            )
          )
        )
      )
    ),

    # Panel de resumen (texto) - SIEMPRE presente pero oculto/mostrado
    shinyjs::hidden(
      div(
        id = ns("panel_summary"),
        card(
          class = "bg-light border-success shadow-sm",
          card_header("Tools & Scripts (User selection)"),
          card_body(
            style = "padding: 15px;", # Espaciado interno controlado
            uiOutput(ns("summary_tool_ui")) # Aquí se inyectará el texto grande
          )
        )
      )
    ),

    br(),
    uiOutput(ns("tool_control_btns")),
    br(),
    withSpinner(uiOutput(ns("details_display_ui")), type = 8)
  )
}

module_tool_selector_server <- function(id, config_path = "tools_config_DEV.yml") {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # --- 1. CONFIG & STATE ---
    config_data <- reactiveFileReader(1000, session, config_path, yaml::read_yaml)
    tools_list  <- reactive({ unname(config_data()$tools) })
    cat_details <- reactive({ config_data()$cat_code })
    rv          <- reactiveValues(ui_state = "edit", is_done = FALSE)

    # Mostrar panel de edición al inicio
    observe({
      shinyjs::show("panel_edit")
    })

    # --- 2. INITIALIZATION (Conteo Inicial y Bloqueo) ---
    selector01_opts_category <- reactive({
      t_list <- tools_list()
      req(length(t_list) > 0)

      # Extraemos categorías únicas de forma limpia
      cats_code <- sapply(t_list, function(x) x$cat_code)
      cats_name <- sapply(t_list, function(x) x$category)
      unique_indices <- which(!duplicated(cats_code))

      # final_choices <- sapply(unique_indices, function(i) {
      #   code <- cats_code[i]
      #   count <- sum(cats_code == code)
      #   paste0(cats_name[i], " (", count, ")")
      # })
      final_choices <- sapply(unique_indices, function(i) {
        cats_name[i]
      })
      final_cats <- setNames(unname(cats_code[unique_indices]), unname(final_choices))
      final_cats
    })
    observe({

      req(selector01_opts_category())
      final_cats <- selector01_opts_category()
      text01 <- "There is _num_ category available."
      text02 <- "There are _num_ categories available."
      number_of_categories <- length(final_cats)
      text_selected <- ifelse(test = number_of_categories == 1, yes = text01, no = text02)
      text_selected <- sub(pattern = "_num_", replacement = number_of_categories, x = text_selected)



      updateSelectizeInput(session, "sel_category",
                           # label = paste0("1. Category (", number_of_categories, ")"),
                           label = paste0("1. Category"),
                           choices = c("", final_cats),
                           server = TRUE,
                           options = list(placeholder = 'Select Category...', dropdownParent = "body"))
      shinyjs::html("msg_category", text_selected)

      updateSelectizeInput(session, "sel_tool_id", label = "2. Tool", choices = "", server = TRUE,
                           options = list(placeholder = '(Locked - Complete step 1)'))
      shinyjs::disable("sel_tool_id")

      updateSelectizeInput(session, "sel_script", label = "3. Script", choices = "", server = TRUE,
                           options = list(placeholder = '(Locked - Complete step 2)'))
      shinyjs::disable("sel_script")
    })

    # --- 3. CASCADING LOGIC ---
    selector02_opts_tools <- reactive({
      req(input$sel_category)
      filtered <- Filter(function(x) x$cat_code == input$sel_category, tools_list())

      # tool_labels <- sapply(filtered, function(x) {
      #   n_scripts <- length(x$folder_scripts)
      #   paste0(x$name, " (", n_scripts, ")")
      # })
      tool_labels <- sapply(filtered, function(x) {
        x$name
      })
      tool_ids <- sapply(filtered, function(x) x$id)

      tool_choices <- setNames(unname(tool_ids), unname(tool_labels))

      tool_choices
    })

    observeEvent(input$sel_category, {
      if (input$sel_category == "") {
        updateSelectizeInput(session, "sel_tool_id", label = "2. Tool", choices = "", server = TRUE,
                             options = list(placeholder = '(Locked - Complete step 1)'))
        return()
      }

      req(selector02_opts_tools())
      tool_choices <-  selector02_opts_tools()

      text01 <- "There is _num_ tool available."
      text02 <- "There are _num_ tools available."
      number_of_tools <- length(tool_choices)
      text_selected <- ifelse(test = number_of_tools == 1, yes = text01, no = text02)
      text_selected <- sub(pattern = "_num_", replacement = number_of_tools, x = text_selected)

      updateSelectizeInput(session, "sel_tool_id",
                           label = paste0("2. Tool"),
                           choices = c("", tool_choices), server = TRUE,
                           options = list(placeholder = 'Select Tool...', dropdownParent = "body"))

      shinyjs::html("msg_tool", text_selected)
      shinyjs::enable("sel_tool_id")

    }, ignoreInit = TRUE)


    selector03_opts_scritps <- reactive({
      req(input$sel_tool_id)
      matches <- Filter(function(x) x$id == input$sel_tool_id, tools_list())
      req(length(matches) > 0)
      scripts <- names(matches[[1]]$folder_scripts)
      scripts
    })

    observeEvent(input$sel_tool_id, {
      if (input$sel_tool_id == "") {
        updateSelectizeInput(session, "sel_script", label = "3. Script", choices = "", server = TRUE,
                             options = list(placeholder = '(Locked - Complete step 2)'))
        return()
      }

      req(selector03_opts_scritps())
      scripts_choices <- selector03_opts_scritps()

      text01 <- "There is _num_ script available."
      text02 <- "There are _num_ scripts available."
      number_of_scripts <- length(scripts_choices)
      text_selected <- ifelse(test = number_of_scripts == 1, yes = text01, no = text02)
      text_selected <- sub(pattern = "_num_", replacement = number_of_scripts, x = text_selected)

      updateSelectizeInput(session, "sel_script",
                           label = paste0("3. Script"),
                           choices = c("", unname(scripts_choices)), server = TRUE,
                           options = list(placeholder = 'Select Script...', dropdownParent = "body"))

      shinyjs::html("msg_script", text_selected)
      shinyjs::enable("sel_script")

    }, ignoreInit = TRUE)


    output$summary_tool_ui <- renderUI({
      # Solo renderizamos si estamos en estado locked
      req(rv$ui_state == "locked")

      # Buscamos nombres externos (labels)
      cats  <- selector01_opts_category()
      tools <- selector02_opts_tools()

      cat_label  <- names(cats)[cats == input$sel_category]
      tool_label <- names(tools)[tools == input$sel_tool_id]

      div(
        style = "font-size: 1.15rem; line-height: 1.5; color: #2c3e50;",

        div(tags$b("Category: "), span(style = "color: #1a1a1a;", cat_label)),

        div(tags$b("Selected Tool: "), span(style = "color: #1a1a1a;", tool_label)),

        div(tags$b("Active Script: "), span(style = "color: #1a1a1a;", input$sel_script))
        )

    })

    # --- 4. BUTTONS & UI STATE ---
    output$tool_control_btns <- renderUI({
      is_locked <- rv$ui_state == "locked"
      layout_column_wrap(
        width = 1/3, fill = FALSE,
        actionButton(ns("btn_lock_tool"), " Confirm", icon = icon("check"),
                     class = paste("btn-success", if(is_locked) "disabled")),
        actionButton(ns("btn_edit_tool"), " Edit", icon = icon("pen"),
                     class = paste("btn-warning", if(!is_locked) "disabled")),
        actionButton(ns("btn_reset_tool"), " Reset All", icon = icon("rotate"), class = "btn-danger")
      )
    })

    observeEvent(input$btn_lock_tool, {
      if(nzchar(input$sel_script %||% "")) {
        rv$ui_state <- "locked"
        rv$is_done <- TRUE

        # Actualizar los textos del resumen
        if (input$sel_category != "") {
          t_list <- tools_list()
          cats_name <- sapply(t_list, function(x) x$category)
          cats_code <- sapply(t_list, function(x) x$cat_code)
          # Encontrar la categoría correspondiente
          matching_cats <- cats_name[cats_code == input$sel_category]
          if(length(matching_cats) > 0) {
            cat_name <- matching_cats[1]
            shinyjs::html("summary_category", cat_name)
          }
        }

        if (input$sel_tool_id != "") {
          matches <- Filter(function(x) x$id == input$sel_tool_id, tools_list())
          if(length(matches) > 0) {
            shinyjs::html("summary_tool", matches[[1]]$name)
          }
        }

        if (input$sel_script != "") {
          shinyjs::html("summary_script", input$sel_script)
        }

        # Cambiar visibilidad de paneles
        shinyjs::hide("panel_edit")
        shinyjs::show("panel_summary")
      }
    })

    observeEvent(input$btn_edit_tool, {
      rv$ui_state <- "edit"
      rv$is_done <- FALSE

      # Cambiar visibilidad de paneles
      shinyjs::hide("panel_summary")
      shinyjs::show("panel_edit")
    })

    observeEvent(input$btn_reset_tool, {
      # 1. Recuperamos las opciones originales del primer selector
      # para asegurar que el ítem vacío ("") se reconozca correctamente
      initial_cats <- selector01_opts_category()

      # 2. Reseteamos el 1er selector pasando de nuevo las opciones
      updateSelectizeInput(session, "sel_category",
                           choices = c("Select Category..." = "", initial_cats),
                           selected = "")

      # 3. Reseteamos los hijos (estos pueden quedar vacíos)
      updateSelectizeInput(session, "sel_tool_id", selected = "", choices = "")
      updateSelectizeInput(session, "sel_script", selected = "", choices = "")

      # 4. Reset del estado
      rv$ui_state <- "edit"
      rv$is_done <- FALSE

      # 5. Visibilidad
      shinyjs::hide("panel_summary")
      shinyjs::show("panel_edit")

      # Opcional: Si quieres que los hijos vuelvan a estar deshabilitados
      shinyjs::disable("sel_tool_id")
      shinyjs::disable("sel_script")
    })

    # --- 5. DISPLAYS (Details) ---
    output$details_display_ui <- renderUI({
      req(input$sel_category, input$sel_category != "")
      is_locked <- rv$ui_state == "locked"

      # --- 1. DATOS DE CATEGORÍA ---
      cats_opts    <- selector01_opts_category()
      cat_label    <- names(cats_opts)[cats_opts == input$sel_category]
      desc_cat_obj <- cat_details()[[input$sel_category]]

      ui_list <- list(card(
        card_header(
          if(is_locked) span(icon("check-circle"), "Category Confirmed") else "Category Details",
          class = if(is_locked) "bg-success text-white" else "bg-secondary text-white"
        ),
        card_body(
          h3(cat_label, style = "color: #2c3e50; margin-bottom: 15px;"),
          p(tags$b("Internal ID: "), tags$code(input$sel_category)),
          p(tags$b("Short Description: "), desc_cat_obj$description_short %||% "N/A"),
          hr(),
          p(tags$b("Long Description:")),
          p(style = "color: #555;", tags$i(desc_cat_obj$description_long %||% "N/A"))
        )
      ))

      # --- 2. DATOS DE TOOL ---
      if (nzchar(input$sel_tool_id %||% "")) {
        matches <- Filter(function(x) x$id == input$sel_tool_id, tools_list())
        if(length(matches) > 0) {
          node       <- matches[[1]]
          tools_opts <- selector02_opts_tools()
          tool_label <- names(tools_opts)[tools_opts == input$sel_tool_id]

          ui_list[[2]] <- card(
            card_header(
              if(is_locked) span(icon("check-circle"), "Tool Confirmed") else "Tool Details",
              class = if(is_locked) "bg-success text-white" else "bg-secondary text-white"
            ),
            card_body(
              h3(tool_label, style = "color: #2c3e50; margin-bottom: 15px;"),
              p(tags$b("Internal ID: "), tags$code(input$sel_tool_id)),
              p(tags$b("Short Description: "), node$description_short %||% "N/A"),
              hr(),
              p(tags$b("Long Description:")),
              p(style = "color: #555;", tags$i(node$description_long %||% "N/A"))
            )
          )

          # --- 3. DATOS DE SCRIPT ---
          if (nzchar(input$sel_script %||% "")) {
            s_info <- node$folder_scripts[[input$sel_script]]

            ui_list[[3]] <- card(
              card_header(
                if(is_locked) span(icon("check-circle"), "Script Confirmed") else "Script Details",
                class = if(is_locked) "bg-success text-white" else "bg-secondary text-white"
              ),
              card_body(
                # Aquí el "Label Externo" es el ID del script según tu YAML
                h3(input$sel_script, style = "color: #2c3e50; margin-bottom: 15px;"),
                p(tags$b("Internal ID: "), tags$code(input$sel_script)),
                p(tags$b("Short Description: "), s_info$description_short %||% "N/A"),
                hr(),
                p(tags$b("Long Description:")),
                p(style = "color: #555;", tags$i(s_info$description_long %||% "N/A"))
              )
            )
          }
        }
      }

      tagList(ui_list)
    })

    # --- 6. RETURN ---
    return(reactive({
      # Solo devolvemos datos si el usuario confirmó la selección
      if (!rv$is_done) return(list(is_done = FALSE))

      # 1. Obtenemos el nodo completo del YAML para la herramienta seleccionada
      node <- Filter(function(x) x$id == input$sel_tool_id, tools_list())[[1]]
      script_info <- node$folder_scripts[[input$sel_script]]

      # 2. Obtenemos los Labels amigables (External) buscando en nuestros reactivos
      # Para Categoría:
      all_cats <- selector01_opts_category()
      cat_label <- names(all_cats)[all_cats == input$sel_category]

      # Para Herramienta:
      all_tools <- selector02_opts_tools()
      tool_label <- names(all_tools)[all_tools == input$sel_tool_id]

      # 3. Construimos el objeto de salida
      list(
        is_done = TRUE,
        description_short = "Selection for category, tool and script.",
        # Información de Categoría
        category = list(
          internal = input$sel_category, # ej: "descriptive_stats"
          external = cat_label           # ej: "Descriptive Statistics"
        ),
        # Información de Herramienta
        tool = list(
          internal = input$sel_tool_id,   # ej: "tool_001"
          external = tool_label           # ej: "Linear Regression"
        ),
        # Información de Script
        script = list(
          internal = input$sel_script,    # ej: "run_analysis"
          external = input$sel_script     # El script suele ser nombre directo
        ),
        # Metadatos técnicos adicionales
        paths = list(
          folder = script_info$folder_script,
          module = script_info$special_module_file_path
        ),
        yml_node = node # Devolvemos el nodo completo por si el módulo padre necesita algo más
      )
    }))


  })
}
