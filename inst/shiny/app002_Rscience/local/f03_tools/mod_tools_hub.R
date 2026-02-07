# mod_tools_hub.R

mod_tools_hub_ui <- function(id) {
  ns <- NS(id)

  tagList(
    tags$head(
      tags$style(HTML(paste0("
        #", ns("wrapper"), ".hide-tabs ul.nav-tabs { display: none !important; }
        #", ns("wrapper"), ".hide-tabs .navset-tab { border-top: none !important; }
        #", ns("wrapper"), ".hide-tabs .tab-content { padding-top: 0px !important; }
      ")))
    ),

    div(
      id = ns("wrapper"),
      class = "hide-tabs",

      bslib::navset_tab(
        id = ns("tools_workflow_steps"),
        selected = "step03_action",

        # --- DEPURACIÓN 01-02 ---
        bslib::nav_panel(title = "01. Check External", value = "step01_check_external", uiOutput(ns("SO_step01_check_external"))),
        bslib::nav_panel(title = "02. Check Pre", value = "step02_check_pre", uiOutput(ns("SO_step02_check_pre"))),

        # --- PANELES DE ACCIÓN 03 ---
        bslib::nav_panel(
          title = "03. Action: tool selection",
          value = "step03_action",
          br(),
          bslib::layout_column_wrap(
            width = 1/2,
            bslib::card(
              card_header("Selection Settings"),
              div(id = ns("selection_controls_wrapper"),
                  selectizeInput(ns("sel_category"), "1. Category", choices = ""),
                  selectizeInput(ns("sel_tool_id"), "2. Tool", choices = ""),
                  selectizeInput(ns("sel_script"), "3. Script", choices = "")
              ),
              hr(),
              bslib::layout_column_wrap(
                width = 1/3,
                actionButton(ns("btn_accept"), "Accept", icon = icon("check"), class = "btn-success"),
                actionButton(ns("btn_edit"), "Edit", icon = icon("pen"), class = "btn-warning"),
                actionButton(ns("btn_reset"), "Reset", icon = icon("trash"), class = "btn-danger")
              )
            ),
            bslib::card(
              card_header("Selected Tool Information"),
              uiOutput(ns("selection_detail_display")),
              card_footer(textOutput(ns("tool_info")))
            )
          )
        ),

        # --- DEPURACIÓN Y MONITOREO 04-08 ---
        bslib::nav_panel(title = "04. Live", value = "step04_live", uiOutput(ns("render_ui_step04_ALL"))),
        bslib::nav_panel(title = "05. View Post", value = "step05_view_post",
                         verbatimTextOutput(ns("render_ui_step05_view_post_A")),
                         verbatimTextOutput(ns("render_ui_step05_view_post_B"))),
        bslib::nav_panel(title = "06. Control Post", value = "step06_check_post", uiOutput(ns("SO_step06_check_post"))),
        bslib::nav_panel(title = "07. Check General", value = "step07_check_general", uiOutput(ns("SO_step07_check_general"))),
        bslib::nav_panel(title = "08. Raw Output", value = "step08_raw_output", verbatimTextOutput(ns("render_ui_step08_RAW")))
      )
    )
  )
}

mod_tools_hub_server <- function(id, config_path = "local/f03_tools/super_menu01.yml",
                                 check_external = reactive({TRUE}),
                                 debug_toggle = reactive({FALSE})) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # 1. Configuración Reactiva
    config_data <- reactiveFileReader(1000, session, config_path, yaml::read_yaml)
    current_tool_bundle <- reactiveVal(NULL)

    # Manejo de visibilidad Mode Engineer
    observeEvent(debug_toggle(), {
      if (isTRUE(debug_toggle())) shinyjs::removeClass(id = "wrapper", class = "hide-tabs")
      else {
        shinyjs::addClass(id = "wrapper", class = "hide-tabs")
        bslib::nav_select("tools_workflow_steps", "step03_action")
      }
    })

    # --- 01. Logic Check External ---
    output$SO_step01_check_external <- renderUI({
      is_ready <- isTRUE(check_external())
      bslib::value_box(
        title = "External Dependencies",
        value = if(is_ready) "Loaded & Ready" else "Missing Dependencies",
        showcase = icon(if(is_ready) "check-double" else "triangle-exclamation"),
        theme = if(is_ready) "success" else "danger"
      )
    })

    # --- 02. Logic Check Pre (YAML) ---
    output$SO_step02_check_pre <- renderUI({
      conf <- config_data()
      is_ok <- !is.null(conf) && !is.null(conf$categories)
      bslib::value_box(
        title = "YAML Configuration",
        value = if(is_ok) "Valid File" else "Invalid / Not Found",
        showcase = icon(if(is_ok) "file-circle-check" else "file-circle-xmark"),
        theme = if(is_ok) "success" else "danger",
        p(paste("Path:", config_path))
      )
    })

    # --- 03. LOGICA DE SELECCIÓN ---
    observe({
      conf <- config_data()
      req(conf$categories)
      cats <- conf$categories
      choices_cat <- setNames(names(cats), sapply(cats, function(x) x$opt$external))
      updateSelectizeInput(session, "sel_category", choices = c("Select category..." = "", choices_cat))
      updateSelectizeInput(session, "sel_tool_id",  choices = c("Waiting..." = ""))
      updateSelectizeInput(session, "sel_script",   choices = c("Waiting..." = ""))

      shinyjs::disable("sel_tool_id")
      shinyjs::disable("sel_script")
    })

    observeEvent(input$sel_category, {
      req(input$sel_category != "")
      conf <- config_data()
      filtered <- purrr::keep(conf$tools, ~input$sel_category %in% .x$vector_name_category)
      choices_tools <- setNames(names(filtered), sapply(filtered, function(x) x$opt$external))
      updateSelectizeInput(session, "sel_tool_id", choices = c("Select tool..." = "", choices_tools))
      shinyjs::enable("sel_tool_id")
    })

    observeEvent(input$sel_tool_id, {
      req(input$sel_tool_id != "")
      scripts <- config_data()$tools[[input$sel_tool_id]]$vector_USC
      updateSelectizeInput(session, "sel_script", choices = c("Select script..." = "", scripts))
      shinyjs::enable("sel_script")
    })

    # Botones
    observeEvent(input$btn_accept, {
      req(input$sel_script != "", input$sel_script != "Select script...")
      conf <- config_data()
      bundle <- list(
        ready = T,
        category_name = conf$categories[[input$sel_category]]$opt$external,
        tool_name     = conf$tools[[input$sel_tool_id]]$opt$external,
        script        = input$sel_script,
        tool_full_info = conf$tools[[input$sel_tool_id]],
        timestamp     = Sys.time()
      )
      current_tool_bundle(bundle)
      shinyjs::disable("selection_controls_wrapper")
      shinyjs::disable("btn_accept")
      shinyjs::enable("btn_edit")
    })

    observeEvent(input$btn_edit, {
      current_tool_bundle(NULL)
      # shinyjs::enable("selection_controls_wrapper");
      if(input$"sel_category" != "") shinyjs::enable("sel_category")
      if(input$"sel_tool_id"  != "") shinyjs::enable("sel_tool_id")
      if(input$"sel_script"   != "") shinyjs::enable("sel_script")

      # shinyjs::disable("sel_tool_id")
      # shinyjs::disable("sel_script")
      shinyjs::enable("btn_accept")
      shinyjs::disable("btn_edit")
    })

    observeEvent(input$btn_reset, {
      current_tool_bundle(NULL)

      conf <- config_data()
      cats <- conf$categories
      choices_cat <- setNames(names(cats), sapply(cats, function(x) x$opt$external))
      updateSelectizeInput(session, "sel_category", choices = c("Select category..." = "", choices_cat))
      updateSelectizeInput(session, "sel_tool_id",  choices = c("Waiting..." = ""))
      updateSelectizeInput(session, "sel_script",   choices = c("Waiting..." = ""))

      shinyjs::enable("sel_category")
      shinyjs::disable("sel_tool_id")
      shinyjs::disable("sel_script")
      shinyjs::enable("btn_accept")
    })

    # --- OUTPUTS DE VISTA 03 ---
    output$selection_detail_display <- renderUI({
      if (is.null(current_tool_bundle())) {
        tagList(div(class = "alert alert-info", "Waiting for selection..."), tableOutput(ns("preview_table_empty")))
      } else tableOutput(ns("preview_table"))
    })

    output$preview_table_empty <- renderTable({
      data.frame(Details = c("Category", "Tool", "Script", "Time Stamp"), Value = rep("Waiting...", 4))
    })

    output$preview_table <- renderTable({
      b <- current_tool_bundle(); req(b)
      data.frame(Field = c("Category", "Tool", "Script", "Time Stamp"),
                 Value = c(b$category_name, b$tool_name, b$script, format(b$timestamp, "%H:%M:%S")))
    })

    output$tool_info <- renderText({
      if(is.null(current_tool_bundle())) "Ready." else paste("Confirmed:", current_tool_bundle()$script)
    })

    # --- PANELES DE DEPURACIÓN (04-08) ---
    output$render_ui_step04_ALL <- renderUI({
      tagList(strong("Inputs:"), verbatimTextOutput(ns("live_inputs")), strong("Bundle:"), verbatimTextOutput(ns("live_bundle")))
    })
    output$live_inputs <- renderPrint({ utils::str(list(cat = input$sel_category, tool = input$sel_tool_id, script = input$sel_script)) })
    output$live_bundle <- renderPrint({ utils::str(current_tool_bundle()) })

    output$render_ui_step05_view_post_A <- renderPrint({ req(current_tool_bundle()); cat("--- SUMMARY ---\n"); print(current_tool_bundle()[1:3]) })
    output$render_ui_step05_view_post_B <- renderPrint({ req(current_tool_bundle()); cat("--- FULL CONFIG ---\n"); utils::str(current_tool_bundle()$tool_full_info) })

    output$SO_step06_check_post <- renderUI({
      ok <- !is.null(current_tool_bundle())
      bslib::value_box(title = "Integrity", value = if(ok) "Valid" else "Invalid", theme = if(ok) "success" else "danger", showcase = icon("shield"))
    })

    output$SO_step07_check_general <- renderUI({
      s1 <- isTRUE(check_external()); s2 <- !is.null(config_data()); s3 <- !is.null(current_tool_bundle())
      bslib::card(bslib::card_header("System Check"), p(if(s1) "✅ Ext" else "❌ Ext"), p(if(s2) "✅ YAML" else "❌ YAML"), p(if(s3) "✅ Tool" else "❌ Tool"))
    })

    output$render_ui_step08_RAW <- renderPrint({ utils::str(current_tool_bundle()) })

    return(current_tool_bundle)
  })
}
