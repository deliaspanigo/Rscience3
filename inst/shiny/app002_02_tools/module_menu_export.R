


module_selector_ui <- function(id) {
  ns <- NS(id)
  card(
    card_header("Dynamic Selectors"),
    conditionalPanel(
      condition = sprintf("output['%s-selection_confirmed'] == false", id),
      layout_column_wrap(
        width = 1/3,
        uiOutput(ns("ui_cat")),
        uiOutput(ns("ui_tool")),
        uiOutput(ns("ui_script"))
      )
    ),
    conditionalPanel(
      condition = sprintf("output['%s-selection_confirmed'] == true", id),
      div(
        style = "padding: 20px; border: 1px dashed #2c3e50; border-radius: 5px; background: #f8f9fa;",
        htmlOutput(ns("confirmed_text"))
      )
    ),
    card_footer(
      div(
        class = "d-flex gap-2",
        actionButton(ns("btn_confirm"), "Confirm Selection", class = "btn-primary"),
        actionButton(ns("btn_modify"), "Modify Selection", class = "btn-warning"),
        actionButton(ns("btn_reset"), "Reset All", class = "btn-danger")
      )
    )
  )
}

module_selector_server <- function(id, config) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    state <- reactiveValues(confirmed = FALSE, load_time = NULL)

    output$ui_cat <- renderUI({
      req(config())
      cats <- unique(sapply(config()$tools, function(x) x$category))
      selectInput(ns("category_sel"), "1. Category:", choices = cats)
    })

    output$ui_tool <- renderUI({
      req(input$category_sel, config())
      tools_list <- config()$tools
      matches <- list()
      for (id_key in names(tools_list)) {
        if (tools_list[[id_key]]$category == input$category_sel) {
          matches[[tools_list[[id_key]]$name]] <- id_key
        }
      }
      selectInput(ns("tool_id_sel"), "2. Tool:", choices = matches)
    })

    output$ui_script <- renderUI({
      req(input$tool_id_sel, config())
      folder_list <- config()$tools[[input$tool_id_sel]]$folder_scripts
      selectInput(ns("script_sel"), "3. Script:", choices = folder_list)
    })

    observeEvent(input$btn_confirm, {
      req(input$tool_id_sel, input$script_sel)
      state$confirmed <- TRUE
      state$load_time <- format(Sys.time(), "%H:%M:%S")
    })

    observeEvent(input$btn_modify, { state$confirmed <- FALSE })

    observeEvent(input$btn_reset, {
      state$confirmed <- FALSE
      state$load_time <- NULL
      updateSelectInput(session, "category_sel", selected = character(0))
    })

    output$selection_confirmed <- reactive({ state$confirmed })
    outputOptions(output, "selection_confirmed", suspendWhenHidden = FALSE)

    output$confirmed_text <- renderUI({
      req(state$confirmed)
      tagList(
        p(tags$b("Category: "), input$category_sel),
        p(tags$b("Tool ID: "), input$tool_id_sel),
        p(tags$b("Script Folder: "), input$script_sel),
        p(tags$b("Confirmed at: "), state$load_time)
      )
    })

    # Return values to orchestrator
    return(list(
      state = state,
      choices = reactive({
        list(category = input$category_sel, tool_id = input$tool_id_sel, script = input$script_sel)
      })
    ))
  })
}

