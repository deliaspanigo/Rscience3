submodule_04_selector_ui <- function(id) {
  ns <- NS(id)

  tagList(
    tags$head(
      tags$style(HTML(sprintf("
        #%s.hide-tabs .nav-tabs { display: none !important; }
        #%s.hide-tabs .card-header { display: none !important; } /* Opcional: oculta cabecera si usas cards */
        #%s.hide-tabs { border-top: none !important; }
      ", ns("wrapper"), ns("wrapper"), ns("wrapper"))))
    ),
  div(
    id = ns("wrapper"),
    class = "hide-tabs", # Estado inicial: oculto
    bslib::navset_tab(
      id = ns("variable_selector"),
    selected = "the_selector",
    bslib::nav_panel(
      title = "Action", value = "the_selector",
      bslib::layout_column_wrap(
        width = 1/2,
        bslib::card(
          card_header("Analysis Configuration"),
          div(id = ns("selection_controls_wrapper"), uiOutput(ns("var_selector_ui"))),
          hr(),
          bslib::layout_column_wrap(
            width = 1/3,
            actionButton(ns("btn_accept"), "Accept", icon = icon("check"), class = "btn-success"),
            actionButton(ns("btn_edit"),   "Edit",   icon = icon("pen"),   class = "btn-warning"),
            actionButton(ns("btn_reset"),  "Reset",  icon = icon("trash"), class = "btn-danger")
          )
        ),
        bslib::card(
          card_header("Selected Variable Information"),
          uiOutput(ns("selection_detail_display")),
          card_footer(textOutput(ns("selection_status_text")))
        )
      ),
      div(style = "margin-top: 15px;", verbatimTextOutput(ns("debug_text")))
    ),
    bslib::nav_panel(
      title = "Debug Var Selector", value = "var_selector_DEBUG",
      bslib::layout_column_wrap(
        width = 1/2,
        bslib::card(card_header("Raw Logic (OR_var_selection)"), verbatimTextOutput(ns("debug_OR_list"))),
        bslib::card(card_header("Final Master Bundle (PACK)"), verbatimTextOutput(ns("debug_ANCESTRAL_list")))
      )
    )
  )

    )
  )
}

submodule_04_selector_server <- function(id, OR_01_import_dataset, debug_toggle = reactive({FALSE})) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns


    # --- 1. DATA EXTRACTION ---
    # Usamos req() dentro de los reactivos para evitar errores de cascada
    current_bundle <- reactive({
      req(OR_01_import_dataset())
      OR_01_import_dataset()
    })

    actual_df <- reactive({
      req(is.data.frame(current_bundle()$my_dataset))
      current_bundle()$my_dataset
    })
    internal_debug_toggle <- reactive(debug_toggle())
    PACK_master <- reactiveVal(list())

    # Manejo de visibilidad Mode Engineer
    observe({
      # Usamos el valor directamente para que se ejecute en el segundo 0
      is_debug <- isTRUE(internal_debug_toggle())

      if (is_debug) {
        shinyjs::removeClass(id = "wrapper", class = "hide-tabs")
      } else {
        shinyjs::addClass(id = "wrapper", class = "hide-tabs")
        # Aseguramos que la selección vuelva a la pestaña principal
        bslib::nav_select("variable_selector", "the_selector")
      }
    })

    # --- 4. VARIABLE SELECTOR UI ---
    output$var_selector_ui <- renderUI({
      selectizeInput(ns("selected_var"), "Select a variable to analyze:",
                     choices = c("Select one..." = "", colnames(actual_df())),
                     selected = isolate(input$selected_var))
    })

    # --- 5. LOGIC & VALIDATION ---
    OR_var_selection <- reactive({
      df <- actual_df()
      sel <- input$selected_var

      check_sel <- !is.null(sel) && sel != ""
      check_exists <- check_sel && (sel %in% colnames(df))
      check_numeric <- if(check_exists) is.numeric(df[[sel]]) else FALSE

      err_msg <- if(!check_sel) "No variable selected."
      else if(!check_exists) "Selected variable does not exist in dataset."
      else if(!check_numeric) "The selected variable must be numeric for this analysis."
      else ""

      list(
        ready = check_numeric,
        selected_var = sel,
        text_error = err_msg,
        timestamp = Sys.time()
      )
    })

    ALL_READY <- reactive({
      res <- OR_var_selection()
      # Aquí puedes expandir: all(res$ready, res2$ready, ...)
      isTRUE(res$ready)
    })

    # --- 6. BUTTONS (CONTROL MASTER PACK) ---
    observeEvent(input$btn_accept, {
      internal_OR_var_selection <- OR_var_selection()
      ready_to_go <- ALL_READY()

      if(ready_to_go) {
        master_list <- list(
          "ready" = TRUE,
          "timestamp" = Sys.time(),
          "var_selector" = internal_OR_var_selection
        )
        PACK_master(master_list)

        shinyjs::disable("selection_controls_wrapper")
        shinyjs::disable("btn_accept")
        shinyjs::enable("btn_edit")
        showNotification("Selection confirmed and bundled.", type = "message")
      } else {
        showNotification(paste("Error:", internal_OR_var_selection$text_error), type = "error")
      }
    })

    observeEvent(input$btn_edit, {
      PACK_master(list())
      shinyjs::enable("selection_controls_wrapper")
      shinyjs::enable("btn_accept")
      shinyjs::disable("btn_edit")
    })

    observeEvent(input$btn_reset, {
      PACK_master(list())
      updateSelectizeInput(session, "selected_var", selected = "")
      shinyjs::enable("selection_controls_wrapper")
      shinyjs::enable("btn_accept")
      shinyjs::disable("btn_edit")
    })

    # --- 7. HELPER & DEBUG ---
    is_confirmed <- reactive({
      master <- PACK_master()
      # Verificamos que exista la llave correcta
      length(master) > 0 && !is.null(master$var_selector)
    })

    output$debug_ANCESTRAL_list <- renderPrint({ utils::str(PACK_master()) })
    output$debug_OR_list <- renderPrint({ utils::str(OR_var_selection()) })
    output$bundle_print <- renderPrint({ utils::str(current_bundle()) })

    output$debug_text <- renderPrint({
      res <- OR_var_selection()
      validate(need(res$text_error != "" && !is_confirmed(), res$text_error))
    })

    # --- 8. RESULTS RENDERING ---
    output$selection_detail_display <- renderUI({
      if (!is_confirmed()) {
        div(class="alert alert-info", icon("info-circle"), "Waiting for confirmation...")
      } else {
        data_01 <- PACK_master()$var_selector
        df <- actual_df()
        renderTable({
          data.frame(
            Indicator = c("Variable", "Class", "Timestamp"),
            Value = c(data_01$selected_var, class(df[[data_01$selected_var]])[1], format(data_01$timestamp, "%H:%M:%S"))
          )
        }, striped = TRUE, width = "100%")
      }
    })

    output$selection_status_text <- renderText({
      if(is_confirmed()) {
        paste("Confirmed in Master Pack:", PACK_master()$var_selector$selected_var)
      } else {
        "Status: Pending"
      }
    })




    PACK_public <- reactive({
      PACK_master()
    })

    # RETORNO: Devolvemos el objeto reactivo de solo lectura
    return(PACK_public)

  })
}
