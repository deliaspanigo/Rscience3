submodule_07_control_ui <- function(id) {
  ns <- shiny::NS(id)

  shiny::tagList(
    bslib::navset_card_underline(
      id = ns("control_tabs"),
      title = shiny::span(shiny::icon("gauge-high"), "Data Audit Control"),

      # --- PESTAÑA 1: RESUMEN GENERAL ---
      bslib::nav_panel(
        title = "General Summary",
        value = "tab_summary",
        icon = shiny::icon("database"),
        bslib::layout_column_wrap(
          width = 1/2,
          bslib::value_box(
            title = "Import Source",
            value = shiny::textOutput(ns("file_info")),
            showcase = shiny::icon("file-import"),
            theme = "primary"
          ),
          bslib::value_box(
            title = "Clean Records",
            value = shiny::textOutput(ns("clean_info")),
            showcase = shiny::icon("filter"),
            theme = "info"
          )
        ),
        bslib::card(
          bslib::card_header("Variable & Statistical Detail"),
          # Usamos un UI dinámico para asegurar que no esté vacío
          shiny::uiOutput(ns("stats_ui"))
        )
      ),

      # --- PESTAÑA 2: VISUALIZACIÓN ---
      bslib::nav_panel(
        title = "Dispersion Plot",
        value = "tab_plot",
        icon = shiny::icon("chart-line"),
        bslib::card(
          full_screen = TRUE,
          plotly::plotlyOutput(ns("dispersion_plot"))
        )
      ),

      # --- PESTAÑA 3: DATASETS ---
      bslib::nav_panel(
        title = "Data Explorer",
        value = "tab_data",
        icon = shiny::icon("table-list"),
        bslib::layout_column_wrap(
          width = 1,
          bslib::card(
            bslib::card_header("Full Imported Dataset"),
            DT::DTOutput(ns("table_full"))
          ),
          bslib::card(
            bslib::card_header("Mini-Dataset (Selected Variable)"),
            DT::DTOutput(ns("table_mini"))
          )
        )
      )
    )
  )
}

submodule_07_control_server <- function(id, OR_01_import_dataset, PACK_central, debug_toggle = reactive({FALSE})) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # --- 1. EXTRACCIÓN DE DATOS CON VALIDACIÓN ---

    # Dataset Base
    raw_df <- shiny::reactive({
      shiny::req(OR_01_import_dataset(), OR_01_import_dataset()$my_dataset)
      OR_01_import_dataset()$my_dataset
    })

    # El Mini-Dataset (Punto crítico)
    mini_df <- shiny::reactive({
      # Validación: Necesitamos que el pack central esté listo
      shiny::req(PACK_central())

      # Extraemos el nombre de la variable (Cuidado con janitor::clean_names)
      var_name <- PACK_central()$selector$var_selector$selected_var
      df <- raw_df()

      shiny::req(var_name %in% names(df))

      # Limpieza: Seleccionamos, quitamos NAs y convertimos a data.frame puro
      sub_df <- df[, var_name, drop = FALSE]
      sub_df <- sub_df[stats::complete.cases(sub_df), , drop = FALSE]

      message("Submodule 07: Mini-dataset creado con variable: ", var_name)
      return(sub_df)
    })

    # --- 2. OUTPUTS ---

    output$file_info <- shiny::renderText({
      bundle <- OR_01_import_dataset()
      shiny::req(bundle)
      paste0(bundle$dataset_name_short, " (", ncol(bundle$my_dataset), " cols)")
    })

    output$clean_info <- shiny::renderText({
      shiny::req(mini_df())
      paste0(nrow(mini_df()), " Valid Rows")
    })

    output$stats_ui <- shiny::renderUI({
      # Si PACK_central() no tiene ready = TRUE, mostramos aviso
      if (!isTRUE(PACK_central()$ready)) {
        return(shiny::div(class="alert alert-warning", "Waiting for variable confirmation in Selector and Settings..."))
      }

      df_mini <- mini_df()
      var_name <- PACK_central()$selector$var_selector$selected_var
      val_vector <- df_mini[[var_name]]

      # Verificación de tipo
      if(!is.numeric(val_vector)) {
        return(shiny::div(class="alert alert-info", "The selected variable is not numeric. Descriptive stats are limited."))
      }

      shiny::tagList(
        shiny::tags$table(class = "table table-bordered table-sm",
                          shiny::tags$tbody(
                            shiny::tags$tr(shiny::tags$th("Variable Name"), shiny::tags$td(var_name)),
                            shiny::tags$tr(shiny::tags$th("Minimum Value"), shiny::tags$td(min(val_vector, na.rm=T))),
                            shiny::tags$tr(shiny::tags$th("Maximum Value"), shiny::tags$td(max(val_vector, na.rm=T)))
                          )
        )
      )
    })

    output$dispersion_plot <- plotly::renderPlotly({
      shiny::req(mini_df(), isTRUE(PACK_central()$ready))

      df_plot <- mini_df()
      var_name <- names(df_plot)[1]
      color_hex <- PACK_central()$settings$color %||% "#0d6efd"

      plotly::plot_ly(df_plot, y = ~get(var_name), type = "scatter", mode = "markers",
                      marker = list(color = color_hex)) %>%
        plotly::layout(title = paste("Analysis of", var_name))
    })

    output$table_full <- DT::renderDT({
      shiny::req(raw_df())
      DT::datatable(raw_df(), options = list(pageLength = 5, scrollX = TRUE))
    })

    output$table_mini <- DT::renderDT({
      shiny::req(mini_df())
      DT::datatable(mini_df(), options = list(pageLength = 5))
    })
  })
}
