# =============================================================================
# MODULE: Simple Descriptive Statistics (Synced with Import Hub)
# =============================================================================

# Source calls remain the same
source("local/f02_scripts/Rs001_desc_1Q_s01/sub_modules/submodule_01_connection.R")
source("local/f02_scripts/Rs001_desc_1Q_s01/sub_modules/submodule_02_show_dataset.R")
source("local/f02_scripts/Rs001_desc_1Q_s01/sub_modules/submodule_03_tool_theory.R")
source("local/f02_scripts/Rs001_desc_1Q_s01/sub_modules/submodule_04_selector.R")
source("local/f02_scripts/Rs001_desc_1Q_s01/sub_modules/submodule_05_settings.R")
source("local/f02_scripts/Rs001_desc_1Q_s01/sub_modules/submodule_06_central.R")
source("local/f02_scripts/Rs001_desc_1Q_s01/sub_modules/submodule_07_control.R")
source("local/f02_scripts/Rs001_desc_1Q_s01/sub_modules/submodule_08_RUN.R")
source("local/f02_scripts/Rs001_desc_1Q_s01/sub_modules/submodule_09_RUN_ALL.R")


module_ui_menu <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shinyjs::useShinyjs(),
    shiny::tags$head(
      shiny::tags$style(shiny::HTML(sprintf("
        #%s .nav-link[data-value='tab_HIDDEN_lateral'] { display: none !important; }
        .debug-hidden { display: none !important; }
      ", ns("menu_lateral"))))
    ),
    bslib::navset_pill_list(
      id = ns("menu_lateral"),
      well = FALSE,
      widths = c(12, 12),
      # --- DEBUG CONNECTION ---
      bslib::nav_panel(
        title = shiny::span(shiny::icon("bullseye"), "HH01. Connection DEBUG", style = "color: #fd7e14; font-weight: bold;"),
        value = "tab_conexion_DEBUG",
        class = "debug-hidden"
      ),
      # --- SHOW DATASET ---
      bslib::nav_panel(
        title = shiny::span("01. Show Dataset"),
        value = "tab_show_dataset",
        icon = shiny::icon("table")
      ),
      # --- TOOL THEORY ---
      bslib::nav_panel(
        title = shiny::span("02. Tool Theory"),
        value = "tab_tool_theory",
        icon = shiny::icon("book")
      ),
      bslib::nav_item(shiny::div(id = ns("debug_sep_01"), shiny::tags$hr(style = "margin: 10px 0; border-top: 1px solid #000;"))),
      bslib::nav_spacer(),
      # --- VARIABLE SELECTOR ---
      bslib::nav_panel(
        title = shiny::span("03. Selector"),
        value = "tab_selector",
        icon = shiny::icon("sliders")
      ),
      bslib::nav_panel(
        title = shiny::span("03. Settings"),
        value = "tab_settings",
        icon = shiny::icon("sliders")
      ),
      bslib::nav_panel(
        title = shiny::span(shiny::icon("bullseye"), "HH02. Central DEBUG", style = "color: #fd7e14; font-weight: bold;"),
        value = "tab_central_DEBUG",
        class = "debug-hidden"
      ),
      bslib::nav_panel(
        title = shiny::span("03. Control"),
        value = "tab_control",
        icon = shiny::icon("sliders")
      ),
      # --- DEBUG POST-PROCESS ---
      bslib::nav_panel(
        title = shiny::span(shiny::icon("bullseye"), "HH02. Post-Process DEBUG", style = "color: #fd7e14; font-weight: bold;"),
        value = "tab_post_DEBUG",
        class = "debug-hidden"
      ),
      bslib::nav_item(shiny::div(id = ns("debug_sep_02"), shiny::tags$hr(style = "margin: 10px 0; border-top: 1px solid #000;"))),
      bslib::nav_spacer(),
      bslib::nav_panel(
        title = shiny::span("03. Script"),
        value = "tab_script",
        icon = shiny::icon("sliders")
      ),
      bslib::nav_panel(
        title = shiny::span("03. ScriptB"),
        value = "tab_scriptB",
        icon = shiny::icon("sliders")
      ),
      bslib::nav_panel(
        title = shiny::span("03. ScriptC"),
        value = "tab_scriptC",
        icon = shiny::icon("sliders")
      ),
      bslib::nav_panel(
        title = shiny::span("03. Download"),
        value = "tab_download",
        icon = shiny::icon("sliders")
      ),
      # --- RESULTS ---
      bslib::nav_panel(
        title = shiny::span("Results"),
        value = "tab_results",
        icon = shiny::icon("chart-bar")
      ),
      bslib::nav_spacer(),
      bslib::nav_panel("🧹 Clear", value = "tab_HIDDEN_lateral", class = "debug-hidden"),
      bslib::nav_item(shiny::div(id = ns("debug_sep_99"), shiny::tags$hr(style = "margin: 10px 0; border-top: 1px solid #000;")))
    )
  )
}

module_ui_body <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    # HH01. Connection DEBUG
    shiny::conditionalPanel(
      condition = sprintf("input['%s'] == 'tab_conexion_DEBUG'", ns("menu_lateral")),
      bslib::card(
        bslib::card_header("Import Hub Connection Status"),
        submodule_01_connection_ui(id = ns("sub01"))
      )
    ),

    # 01. Show Dataset
    shiny::conditionalPanel(
      condition = sprintf("input['%s'] == 'tab_show_dataset'", ns("menu_lateral")),
      submodule_02_show_dataset_ui(id = ns("sub02"))
    ),

    # 02. Tool Theory
    shiny::conditionalPanel(
      condition = sprintf("input['%s'] == 'tab_tool_theory'", ns("menu_lateral")),
      submodule_03_tool_theory_ui(id = ns("sub03"))
    ),

    # 03. Variables
    shiny::conditionalPanel(
      condition = sprintf("input['%s'] == 'tab_selector'", ns("menu_lateral")),
      submodule_04_selector_ui(id = ns("sub04"))
    ),

    shiny::conditionalPanel(
      condition = sprintf("input['%s'] == 'tab_settings'", ns("menu_lateral")),
      submodule_05_settings_ui(id = ns("sub05"))
    ),

    shiny::conditionalPanel(
      condition = sprintf("input['%s'] == 'tab_central_DEBUG'", ns("menu_lateral")),
      submodule_06_central_ui(id = ns("sub06"))
    ),
    shiny::conditionalPanel(
      condition = sprintf("input['%s'] == 'tab_control'", ns("menu_lateral")),
      submodule_07_control_ui(id = ns("sub07"))
    ),
    shiny::conditionalPanel(
      condition = sprintf("input['%s'] == 'tab_script'", ns("menu_lateral")),
      submodule_08_RUN_ui(id = ns("sub08"))
    ),
    shiny::conditionalPanel(
      condition = sprintf("input['%s'] == 'tab_scriptB'", ns("menu_lateral")),
      submodule_08_RUN_ui(id = ns("sub08B"))
    ),
    shiny::conditionalPanel(
      condition = sprintf("input['%s'] == 'tab_scriptC'", ns("menu_lateral")),
      submodule_08_RUN_ui(id = ns("sub08C"))
    ),
    shiny::conditionalPanel(
      condition = sprintf("input['%s'] == 'tab_download'", ns("menu_lateral")),
      submodule_09_RUN_ALL_ui(id = ns("sub09"))
    ),
    # HH02. Post-Process DEBUG
    shiny::conditionalPanel(
      condition = sprintf("input['%s'] == 'tab_post_DEBUG'", ns("menu_lateral")),
      bslib::card(bslib::card_header("Post-Process Audit"), shiny::verbatimTextOutput(ns("bundle_print")))
    ),

    # Results
    shiny::conditionalPanel(
      condition = sprintf("input['%s'] == 'tab_results'", ns("menu_lateral")),
      shiny::uiOutput(ns("render_results_gate"))
    )
  )
}

module_server <- function(id, OR_01_import_dataset, ORH_02_temporal_FF, debug_toggle = shiny::reactive({FALSE})) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # --- 1. DEBUG VISIBILITY LOGIC ---
    shiny::observe({
      debug_tabs <- c("tab_conexion_DEBUG", "tab_central_DEBUG", "tab_post_DEBUG")
      # Select based on the data-value attribute in bslib pill list
      selector <- paste0("#", ns("menu_lateral"), " .nav-link[data-value='", debug_tabs, "']", collapse = ", ")

      if (isTRUE(debug_toggle())) {
        shinyjs::removeClass(selector = selector, class = "debug-hidden")
        shinyjs::show("debug_sep_999")
      } else {
        shinyjs::addClass(selector = selector, class = "debug-hidden")
        shinyjs::hide("debug_sep_999")
        if (!is.null(input$menu_lateral) && input$menu_lateral %in% debug_tabs) {
          bslib::nav_select("menu_lateral", selected = "tab_selector")
        }
      }
    })

    # --- 2. DATA EXTRACTION ---
    current_bundle <- shiny::reactive({
      shiny::req(OR_01_import_dataset())
      OR_01_import_dataset()
    })

    internal_ORH_02_temporal_FF <- shiny::reactive({
      ORH_02_temporal_FF()
    })

    actual_df <- shiny::reactive({
      shiny::req(is.data.frame(current_bundle()$my_dataset))
      current_bundle()$my_dataset
    })

    # --- 3. SUBMODULE SERVERS ---
    submodule_01_connection_server(
      id = "sub01",
      OR_01_import_dataset = current_bundle,
      debug_toggle = debug_toggle
    )

    submodule_02_show_dataset_server(
      id = "sub02",
      OR_01_import_dataset = current_bundle,
      debug_toggle = debug_toggle
    )

    submodule_03_tool_theory_server(
      id = "sub03",
      OR_01_import_dataset = current_bundle,
      debug_toggle = debug_toggle
    )

    PACK_04_selector <- submodule_04_selector_server(
      id = "sub04",
      OR_01_import_dataset = current_bundle,
      debug_toggle = debug_toggle
    )

    PACK_05_settings <- submodule_05_settings_server(
      id = "sub05",
      OR_01_import_dataset = current_bundle,
      debug_toggle = debug_toggle
    )

    PACK_06_central <- submodule_06_central_server(
      id = "sub06",
      PACK_selector = PACK_04_selector,
      PACK_settings = PACK_05_settings,
      debug_toggle =debug_toggle
    )

    PACK_07_control <- submodule_07_control_server(
      id = "sub07",
      OR_01_import_dataset = OR_01_import_dataset,
      PACK_central = PACK_06_central,
      debug_toggle = debug_toggle
    )


    PACK_08_Rscript <- submodule_08_RUN_server(
      id = "sub08",
      internal_ORH_02_temporal_FF = internal_ORH_02_temporal_FF,
      target_actions = shiny::reactive({c("action02")}),
      debug_toggle = debug_toggle,
      show_viewer = TRUE
    )

    PACK_08B_Rscript <- submodule_08_RUN_server(
      id = "sub08B",
      internal_ORH_02_temporal_FF,
      target_actions = shiny::reactive({c("action02")}),
      debug_toggle = debug_toggle,
      show_viewer = FALSE
    )


    PACK_08C_Rscript <- submodule_08_RUN_server(
      id = "sub08C",
      internal_ORH_02_temporal_FF,
      target_actions = shiny::reactive({c("action07")}),
      debug_toggle = debug_toggle,
      show_viewer = TRUE
    )

    PACK_09_Rscript <- submodule_09_RUN_ALL_server(
      id = "sub09",
      internal_ORH_02_temporal_FF,
      vector_target_actions = shiny::reactive({c("action02", "action03", "action04",
                                                 "action05", "action06", "action07",
                                                 "action08", "action09", "action10",
                                                 "action11")}),
      debug_toggle = debug_toggle,
      show_viewer = FALSE
    )

    # --- 4. VALIDATION LOGIC ---
    is_selection_confirmed <- shiny::reactive({
      res <- PACK_01_master()
      isTRUE(res$ready) && !is.null(res$var_selector$selected_var)
    })

    # --- 5. RESULTS GATEWAY ---
    output$render_results_gate <- shiny::renderUI({
      if (!is_selection_confirmed()) {
        bslib::card(
          shiny::div(class = "alert alert-warning",
                     shiny::icon("lock"),
                     "Please select and confirm a variable in the '03. Variables' tab before viewing results.")
        )
      } else {
        bslib::layout_column_wrap(
          width = 1,
          bslib::card(bslib::card_header("Distribution Plot"), shiny::plotOutput(ns("plot_simple"))),
          bslib::card(bslib::card_header("Summary Statistics"), shiny::verbatimTextOutput(ns("resumen_simple")))
        )
      }
    })

    # --- 6. PLOT & SUMMARY RENDERING ---
    output$plot_simple <- shiny::renderPlot({
      shiny::req(is_selection_confirmed())

      target_var <- PACK_01_master()$var_selector$selected_var
      df <- actual_df()

      hist(df[[target_var]],
                  col = "#0d6efd",
                  border = "white",
                  main = paste("Histogram of", target_var),
                  xlab = target_var)
    })

    output$resumen_simple <- shiny::renderPrint({
      shiny::req(is_selection_confirmed())
      target_var <- PACK_01_master()$var_selector$selected_var
      summary(actual_df()[[target_var]])
    })

    # --- 7. DEBUG AUDIT ---
    output$bundle_print <- shiny::renderPrint({
      utils::str(PACK_01_master())
    })

  })
}
