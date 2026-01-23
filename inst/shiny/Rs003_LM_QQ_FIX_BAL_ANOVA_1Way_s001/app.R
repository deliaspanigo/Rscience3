# app.R - VERSIÓN CORREGIDA

library("shiny")
library("bslib")
library("shinyjs")
library("here")


library("promises")
library("future")
plan(multisession, workers = parallel::detectCores() - 1)


# Esta línea hace MAGIA:
# here::i_am("app.R")

# Cargar módulos
source("R_modules/module01_central/module_buttons.R")
source("R_modules/module02_theory/module_theory.R")
source("R_modules/module03_inputs/module_inputs.R")
source("R_modules/module04_work_table/module_work_table.R")
source("R_modules/module05_render_RShiny_outputs/module_render_RShiny_outputs.R")
source("R_modules/module06_render_ONE_outputs/module_render_ONE_outputs.R")
# source("R_modules/module04_render_outputs/module_download_one.R")
# source("R_modules/module04_render_outputs/module_download_multi.R")

# source("R_modules/module05_analysis/module_analysis_one.R")
# source("R_modules/module05_analysis/module_analysis_multi.R")

ui <- page_sidebar(
  title = NULL,  # Eliminamos el título de arriba
  sidebar = sidebar(
    uiOutput("sidebar_content")
  ),
  # Contenido principal
  uiOutput("main_content"),

  # Pie de página con el título
  tags$footer(
    style = "
      position: fixed;
      bottom: 0;
      left: 0;
      width: 100%;
      background-color: #f8f9fa;
      text-align: center;
      padding: 15px 0;
      border-top: 1px solid #dee2e6;
      z-index: 1000;
    ",
    h2("General Linear Models - Fix Effects - Balanced Tratments - Anova - Anova 1 Way",
       style = "margin: 0; font-weight: bold;")
  )
)


ui <- page_sidebar(
  title =     h3("General Linear Models - Fix Effects - Balanced Tratments - Anova - Anova 1 Way",
                 style = "margin: 0; font-weight: bold;"),
  sidebar = sidebar(
    "Rscience 0.1.5",
    uiOutput("sidebar_content")
  ),
  # TabsetPanel normal y corriente en el UI
  uiOutput("main_content")
)

server <- function(input, output, session) {

  # 1. List buttons (and tabs!)-------------------------------- ---------------------------
  buttons_info <- list(
    info = list(
      btn_id = "btn_info",
      btn_label = "Info",
      btn_enable = TRUE,
      tab_id = "tab_info",
      icon = icon("info-circle"),
      order = 1
    ),
    theory = list(
      btn_id = "btn_theory",
      btn_label = "Theory",
      btn_enable = TRUE,
      tab_id = "tab_theory",
      icon = icon("book"),
      order = 2
    ),
    inputs = list(
      btn_id = "btn_inputs",
      btn_label = "Inputs",
      btn_enable = TRUE,
      tab_id = "tab_inputs",
      icon = icon("upload"),
      order = 3
    ),
    outputs = list(
      btn_id = "btn_outputs",
      btn_label = "Outputs",
      btn_enable = TRUE,
      tab_id = "tab_outputs",
      icon = icon("upload"),
      order = 4
    ),
    outputs_pdf = list(
      btn_id = "btn_pdf",
      btn_label = "Outputs PDF",
      btn_enable = TRUE,
      tab_id = "tab_pdf",
      icon = icon("chart-bar"),
      order = 5
    ),
    outputs_reveal = list(
      btn_id = "btn_reveal",
      btn_label = "Outputs Reveal",
      btn_enable = TRUE,
      tab_id = "tab_reveal",
      icon = icon("chart-bar"),
      order = 6
    ),
    outputs_docx = list(
      btn_id = "btn_docx",
      btn_label = "Outputs docx",
      btn_enable = TRUE,
      tab_id = "tab_docx",
      icon = icon("chart-bar"),
      order = 7
    ),
    outputs_xlsx = list(
      btn_id = "btn_xlsx",
      btn_label = "Outputs xlsx",
      btn_enable = TRUE,
      tab_id = "tab_xlsx",
      icon = icon("chart-bar"),
      order = 8
    ),
    outputs_png = list(
      btn_id = "btn_png",
      btn_label = "Outputs PNG",
      btn_enable = TRUE,
      tab_id = "tab_png",
      icon = icon("chart-bar"),
      order = 9
    ),
    outputs_html_full = list(
      btn_id = "btn_html_full",
      btn_label = "Outputs HTML Full",
      btn_enable = TRUE,
      tab_id = "tab_html_full",
      icon = icon("chart-bar"),
      order = 10
    ),
    outputs_reporting = list(
      btn_id = "btn_reporting",
      btn_label = "Reporting",
      btn_enable = TRUE,
      tab_id = "tab_reporting",
      icon = icon("chart-bar"),
      order = 11
    )
  )

  # 2. Only buttons info -------------------------------------------------------
  buttons_list <- reactive({
    enabled_buttons <- Filter(function(tab) isTRUE(tab$btn_enable), buttons_info)
    ordered_buttons <- enabled_buttons[order(sapply(enabled_buttons, function(x) x$order))]

    lapply(ordered_buttons, function(tab) {
      list(
        id = tab$btn_id,
        label = tab$btn_label,
        icon = tab$icon,
        enabled = tab$btn_enable
      )
    })
  })

  # 3. Buttons data from server ------- ----------------------------------------
  buttons_data <- module_buttons_server(
    id = "navigation",
    buttons_list_reactive = buttons_list,
    initial_active_id = "btn_theory"  # Empezar en Theory
  )

  # 4. SideBar -----------------------------------------------------------------
  output$sidebar_content <- renderUI({
    module_buttons_ui("navigation")
  })

  # 5. Main --------------------------------------------------------------------
  output$main_content <- renderUI({
    tabsetPanel(
      id = "main_tabs",
      type = "hidden", #"tabs",  # Cambiado de "hidden" a "tabs" normal

      # Tab de Theory (con el módulo)
      tabPanel(
        title = "Info",
        value = buttons_info$"info"$"tab_id",
        h3("Info - Contenido simple"),
        p("Este tab puede ser reemplazado por un módulo más adelante")
      ),
      tabPanel(
        title = "Theory",
        value = buttons_info$"theory"$"tab_id",
        module_theory_ui("theory_module")
      ),
      tabPanel(
        title = "Inputs",
        value = buttons_info$"inputs"$"tab_id",
        module_inputs_ui("inputs_module")
      ),
      tabPanel(
        title = "Outputs",
        value = buttons_info$"outputs"$"tab_id",
        module_render_RShiny_outputs_ui("module_render_RShiny_outputs")
      ),
      tabPanel(
        title = "Outputs - PDF",
        value = buttons_info$"outputs_pdf"$"tab_id",
        module_render_ONE_outputs_ui("module_render_ONE_PDF_01")
      ),
      tabPanel(
        title = "Outputs - Revealjs",
        value = buttons_info$"outputs_reveal"$"tab_id",
        module_render_ONE_outputs_ui("module_render_ONE_REVEAL_01")
      ),
      tabPanel(
        title = "Outputs - docx",
        value = buttons_info$"outputs_docx"$"tab_id",
        module_render_ONE_outputs_ui("module_render_ONE_DOCX_01")
      ),
      tabPanel(
        title = "Outputs - xlsx",
        value = buttons_info$"outputs_xlsx"$"tab_id",
        module_render_ONE_outputs_ui("module_render_ONE_XLSX_01")
      ),
      tabPanel(
        title = "Outputs - PNG",
        value = buttons_info$"outputs_png"$"tab_id",
        module_render_ONE_outputs_ui("module_render_ONE_PNG_01")
      ),
      tabPanel(
        title = "Outputs - HTML Full",
        value = buttons_info$"outputs_html_full"$"tab_id",
        module_render_ONE_outputs_ui("module_render_ONE_HTML_FULL_01")
      ),
      tabPanel(
        title = "Outputs - Reporting",
        value = buttons_info$"outputs_reporting"$"tab_id",
        uiOutput("rendering")
        # module_render_ONE_outputs_ui("module_render_ONE_HTML_FULL")
      )
      # tabPanel(
      #   title = "Proc",
      #   value = buttons_info$"proc"$"tab_id",
      #   module_render_outputs_ui(id = "module_render")
      # ),
      # tabPanel(
      #   title = "Analysis",
      #   value = buttons_info$"analysis"$"tab_id",
      #   module_analysis_multi_ui(id = "module_analysis_multi")
      #   # h3("Analysis - Contenido simple"),
      #   # module_analysis_ui(id = "module_analyis")
      # ),

      # tabPanel(
      #   title = "Download",
      #   value = buttons_info$"download"$"tab_id",
      #   module_download_multi_ui(id = "module_download_multi")
      # )
    )
  })

  # 6. Sincronize button → tab -------------------------------------------------
  observe({
    active_id <- buttons_data$active_button()
    if (!is.null(active_id)) {
      # Encontrar el tab_id correspondiente al botón activo
      for(tab_info in buttons_info) {
        if(tab_info$btn_id == active_id) {
          updateTabsetPanel(session, "main_tabs", selected = tab_info$tab_id)
          break
        }
      }
    }
  })

  # 7. Sincronize tab → button ---------------------------
  observeEvent(input$main_tabs, {
    current_tab <- input$main_tabs

    # Buscar el btn_id correspondiente
    btn_id_to_set <- NULL
    for(tab_info in buttons_info) {
      if(tab_info$tab_id == current_tab) {
        btn_id_to_set <- tab_info$btn_id
        break
      }
    }

    if(!is.null(btn_id_to_set)) {
      # Intentar diferentes nombres posibles para la función
      if(!is.null(buttons_data$set_active_button) &&
         is.function(buttons_data$set_active_button)) {
        buttons_data$set_active_button(btn_id_to_set)
      } else if(!is.null(buttons_data$set_active) &&
                is.function(buttons_data$set_active)) {
        buttons_data$set_active(btn_id_to_set)
      } else if(!is.null(buttons_data$update_active) &&
                is.function(buttons_data$update_active)) {
        buttons_data$update_active(btn_id_to_set)
      }
      # Si ninguna funciona, no hacemos nada
    }
  })


  ### SERVER - SERVER - SERVER - SERVER - SERVER - SERVER ----------------------
  ### SERVER - SERVER - SERVER - SERVER - SERVER - SERVER ----------------------
  ### SERVER - SERVER - SERVER - SERVER - SERVER - SERVER ----------------------

  # Server 01. Info ------------------------------------------------------------


  # Server 02. Theory ----------------------------------------------------------
  module_theory_server(id = "theory_module")



  # Server 03. Inputs ----------------------------------------------------------
  app_state_inputs <- module_inputs_server(id = "inputs_module")


  # Server 04. Work Table ------------------------------------------------------
  the_currier_work_table <- reactiveValues(is_running = FALSE,
                                           is_done = FALSE)

  observe({
    req(app_state_inputs()$play$run)
    the_currier_work_table$is_running <- app_state_inputs()$play$run
  })

  app_state_work_table <-  module_work_table_server(
    id = "module_work_table",
    app_state = the_currier_work_table# <-- ¡OBJETO REACTIVO, NO UNA LISTA!
  )





  # Server 05. Render RShiny ------------------------------------------------------
  the_currier_render_RShiny <- reactiveValues(is_running = FALSE,
                                              is_done = FALSE)

  observe({
    req(app_state_work_table$is_done)
    the_currier_render_RShiny$is_running <- app_state_work_table$is_done
    the_currier_render_RShiny$str_temp_work_folder_path <- app_state_work_table$str_temp_work_folder_path

  })

  app_state_render_RShiny <-  module_render_RShiny_outputs_server(
    id = "module_render_RShiny_outputs",
    app_state = the_currier_render_RShiny# <-- ¡OBJETO REACTIVO, NO UNA LISTA!
  )

  ##############################################################################
  update_reactive_values <- function(rv_object, ..., values = list(...)) {
    # 1. Validar que el objeto sea un reactiveValues
    if (!shiny::is.reactivevalues(rv_object)) {
      stop("El objeto proporcionado no es un objeto reactiveValues de Shiny.")
    }

    # 2. Obtener los campos permitidos (los que ya existen en el objeto)
    # Usamos names(reactiveValuesToList()) para obtener las llaves actuales
    fields_allowed <- names(shiny::reactiveValuesToList(rv_object))

    nms <- names(values)

    for (nm in nms) {
      if (nm %in% fields_allowed) {
        rv_object[[nm]] <- values[[nm]]
      } else {
        warning(sprintf("Campo '%s' no permitido para este objeto. Ignorado.", nm))
      }
    }
  }
  ##############################################################################



  # Server 06. Output PDF ------------------------------------------------------
  the_currier_render_PDF <- reactiveValues(start_running = FALSE,
                                          text = list(
                                                 "str_text01_long" = "PDF Report",
                                                 "str_text01_short" = "Rendering PDF Report"
                                               ),
                                          str_temp_work_folder_path = NULL,
                                          str_qmd_file_path = NULL,
                                          str_output_file_path = NULL,
                                          str_download_file_name = NULL,
                                          is_done = FALSE
                                          )

  observe({
    req(app_state_work_table$is_done)

    # Basics
      start_running <- app_state_work_table$is_done

    # Files
      str_qmd_file_name   <- "report02_lab01_render_pdf_HIDDEN_RUNNER.qmd"
      str_output_file_name <- "zzz_output_report02_lab01_render_pdf_STONE.pdf"
      str_download_file_name <- "report02_pdf_Rscience.pdf"

    # Folders
      str_temp_work_folder_path <- app_state_work_table$str_temp_work_folder_path

      str_subfolder_report <- file.path("report02_pdf", "lab01_RUN_pdf")
      str_folder_report_path <- file.path(str_temp_work_folder_path, str_subfolder_report)

      str_subfolder_output <- "zzz_zzz_output"
      str_folder_output_path <- file.path(str_temp_work_folder_path, str_subfolder_output)


    # Paths
      str_qmd_file_path <- file.path(str_folder_report_path, str_qmd_file_name)
      str_output_file_path <- file.path(str_folder_output_path, str_output_file_name)

    # More Basics
      is_done <- file.exists(str_output_file_path)

      update_reactive_values(
        the_currier_render_PDF,
        start_running = start_running,
        str_temp_work_folder_path = str_temp_work_folder_path,
        str_qmd_file_path = str_qmd_file_path,
        str_output_file_path = str_output_file_path,
        str_download_file_name = str_download_file_name,
        is_done = is_done
      )

  })



  app_state_render_PDF_01 <-  module_render_ONE_outputs_server(
    id = "module_render_ONE_PDF_01",
    app_state = the_currier_render_PDF,
    show_output = T# <-- ¡OBJETO REACTIVO, NO UNA LISTA!
  )

  app_state_render_PDF_02 <-  module_render_ONE_outputs_server(
    id = "module_render_ONE_PDF_02",
    app_state = the_currier_render_PDF,
    show_output = F# <-- ¡OBJETO REACTIVO, NO UNA LISTA!
  )

  super_control_pdf <- reactive({
    req(!the_currier_render_PDF$is_done)
    req(the_currier_render_PDF$start_running)

    is_done01 <- app_state_render_PDF_01$is_done()
    is_done02 <- app_state_render_PDF_02$is_done()

    IS_DONE <- is_done01 | is_done02
    IS_DONE
  })

  observe({
    req(super_control_pdf())

    is_done <- super_control_pdf()

      update_reactive_values(
        the_currier_render_PDF,
        is_done = is_done
        )

  })
  ##############################################################################
  ##############################################################################
  ##############################################################################
  ##############################################################################






  # Server 07. Output Revealjs ------------------------------------------------------
  the_currier_render_REVEAL <- reactiveValues(start_running = FALSE,
                                              text = list(
                                                "str_text01_long" = "Revealjs Report",
                                                "str_text01_short" = "Rendering Revealjs Report"
                                              ),
                                              str_temp_work_folder_path = NULL,
                                              str_qmd_file_path = NULL,
                                              str_output_file_path = NULL,
                                              str_download_file_name = NULL,
                                              is_done = FALSE
  )

  observe({
    req(app_state_work_table$is_done)

    # Basics
    start_running <- app_state_work_table$is_done

    # Files
    str_qmd_file_name      <- "report03_lab01_render_revealjs_HIDDEN_RUNNER.qmd"
    str_output_file_name   <- "zzz_output_report03_lab01_render_revealjs_STONE.html"
    str_download_file_name <- "report03_revealjs_Rscience.html"

    # Folders
    str_temp_work_folder_path <- app_state_work_table$str_temp_work_folder_path

    str_subfolder_report <- file.path("report03_revealjs", "lab01_RUN_revealjs")
    str_folder_report_path <- file.path(str_temp_work_folder_path, str_subfolder_report)

    str_subfolder_output <- "zzz_zzz_output"
    str_folder_output_path <- file.path(str_temp_work_folder_path, str_subfolder_output)


    # Paths
    str_qmd_file_path <- file.path(str_folder_report_path, str_qmd_file_name)
    str_output_file_path <- file.path(str_folder_output_path, str_output_file_name)

    # More Basics
    is_done <- file.exists(str_output_file_path)

    update_reactive_values(
      the_currier_render_REVEAL,
      start_running = start_running,
      str_temp_work_folder_path = str_temp_work_folder_path,
      str_qmd_file_path = str_qmd_file_path,
      str_output_file_path = str_output_file_path,
      str_download_file_name = str_download_file_name,
      is_done = is_done
    )


  })



  app_state_render_REVEAL_01 <-  module_render_ONE_outputs_server(
    id = "module_render_ONE_REVEAL_01",
    app_state = the_currier_render_REVEAL,
    show_output = T# <-- ¡OBJETO REACTIVO, NO UNA LISTA!
  )

  app_state_render_REVEAL_02 <-  module_render_ONE_outputs_server(
    id = "module_render_ONE_REVEAL_02",
    app_state = the_currier_render_REVEAL,
    show_output = F# <-- ¡OBJETO REACTIVO, NO UNA LISTA!
  )

  super_control_reveal <- reactive({
    req(!the_currier_render_REVEAL$is_done)
    req(the_currier_render_REVEAL$start_running)

    is_done01 <- app_state_render_REVEAL_01$is_done()
    is_done02 <- app_state_render_REVEAL_02$is_done()

    IS_DONE <- is_done01 | is_done02
    IS_DONE
  })

  observe({
    req(super_control_reveal())

    is_done <- super_control_reveal()

    update_reactive_values(
      the_currier_render_REVEAL,
      is_done = is_done
    )

  })
  ##############################################################################
  ##############################################################################
  ##############################################################################
  ##############################################################################




  # Server 07. Output DOCX ------------------------------------------------------
  the_currier_render_DOCX <- reactiveValues(start_running = FALSE,
                                            text = list(
                                              "str_text01_long" = "Docx Report",
                                              "str_text01_short" = "Rendering Docx Report"
                                            ),
                                            str_temp_work_folder_path = NULL,
                                            str_qmd_file_path = NULL,
                                            str_output_file_path = NULL,
                                            str_download_file_name = NULL,
                                            is_done = FALSE
  )

  observe({
    req(app_state_work_table$is_done)

    # Basics
    start_running <- app_state_work_table$is_done

    # Files
    str_qmd_file_name      <- "report04_lab01_render_docx_HIDDEN_RUNNER.qmd"
    str_output_file_name   <- "zzz_output_report04_lab01_render_docx_STONE.docx"
    str_download_file_name <- "report04_docx_Rscience.docx"

    # Folders
    str_temp_work_folder_path <- app_state_work_table$str_temp_work_folder_path

    str_subfolder_report <- file.path("report04_docx", "lab01_render_docx")
    str_folder_report_path <- file.path(str_temp_work_folder_path, str_subfolder_report)

    str_subfolder_output <- "zzz_zzz_output"
    str_folder_output_path <- file.path(str_temp_work_folder_path, str_subfolder_output)


    # Paths
    str_qmd_file_path <- file.path(str_folder_report_path, str_qmd_file_name)
    str_output_file_path <- file.path(str_folder_output_path, str_output_file_name)

    # More Basics
    is_done <- file.exists(str_output_file_path)

    update_reactive_values(
      the_currier_render_DOCX,
      start_running = start_running,
      str_temp_work_folder_path = str_temp_work_folder_path,
      str_qmd_file_path = str_qmd_file_path,
      str_output_file_path = str_output_file_path,
      str_download_file_name = str_download_file_name,
      is_done = is_done
    )


  })



  app_state_render_DOCX_01 <-  module_render_ONE_outputs_server(
    id = "module_render_ONE_DOCX_01",
    app_state = the_currier_render_DOCX,
    show_output = T# <-- ¡OBJETO REACTIVO, NO UNA LISTA!
  )

  app_state_render_DOCX_02 <-  module_render_ONE_outputs_server(
    id = "module_render_ONE_DOCX_02",
    app_state = the_currier_render_DOCX,
    show_output = F# <-- ¡OBJETO REACTIVO, NO UNA LISTA!
  )

  super_control_docx <- reactive({
    req(!the_currier_render_DOCX$is_done)
    req(the_currier_render_DOCX$start_running)

    is_done01 <- app_state_render_DOCX_01$is_done()
    is_done02 <- app_state_render_DOCX_02$is_done()

    IS_DONE <- is_done01 | is_done02
    IS_DONE
  })

  observe({
    req(super_control_docx())

    is_done <- super_control_docx()

    update_reactive_values(
      the_currier_render_DOCX,
      is_done = is_done
    )

  })

  ##############################################################################
  ##############################################################################
  ##############################################################################
  ##############################################################################




  # Server 08. Output XLSX ------------------------------------------------------
  the_currier_render_XLSX <- reactiveValues(start_running = FALSE,
                                            text = list(
                                              "str_text01_long" = "XLSX Report",
                                              "str_text01_short" = "Rendering XLSX Report"
                                            ),
                                            str_temp_work_folder_path = NULL,
                                            str_qmd_file_path = NULL,
                                            str_output_file_path = NULL,
                                            str_download_file_name = NULL,
                                            is_done = FALSE
  )

  observe({
    req(app_state_work_table$is_done)

    # Basics
    start_running <- app_state_work_table$is_done

    # Files
    str_qmd_file_name      <- "report05_lab01_render_xlsx_HIDDEN_RUNNER.qmd"
    str_output_file_name   <- "zzz_output_report05_lab01_render_xlsx_STONE.xlsx"
    str_download_file_name <- "report05_xlsx_Rscience.xlsx"

    # Folders
    str_temp_work_folder_path <- app_state_work_table$str_temp_work_folder_path

    str_subfolder_report   <- file.path("report05_xlsx", "lab01_render_xlsx")
    str_folder_report_path <- file.path(str_temp_work_folder_path, str_subfolder_report)

    str_subfolder_output   <- "zzz_zzz_output"
    str_folder_output_path <- file.path(str_temp_work_folder_path, str_subfolder_output)


    # Paths
    str_qmd_file_path    <- file.path(str_folder_report_path, str_qmd_file_name)
    str_output_file_path <- file.path(str_folder_output_path, str_output_file_name)

    # More Basics
    is_done <- file.exists(str_output_file_path)

    update_reactive_values(
      the_currier_render_XLSX,
      start_running = start_running,
      str_temp_work_folder_path = str_temp_work_folder_path,
      str_qmd_file_path = str_qmd_file_path,
      str_output_file_path = str_output_file_path,
      str_download_file_name = str_download_file_name,
      is_done = is_done
    )


  })



  app_state_render_XLSX_01 <-  module_render_ONE_outputs_server(
    id = "module_render_ONE_XLSX_01",
    app_state = the_currier_render_XLSX,
    show_output = T# <-- ¡OBJETO REACTIVO, NO UNA LISTA!
  )

  app_state_render_XLSX_02 <-  module_render_ONE_outputs_server(
    id = "module_render_ONE_XLSX_02",
    app_state = the_currier_render_XLSX,
    show_output = F# <-- ¡OBJETO REACTIVO, NO UNA LISTA!
  )

  super_control_xlsx <- reactive({
    req(!the_currier_render_XLSX$is_done)
    req(the_currier_render_XLSX$start_running)

    is_done01 <- app_state_render_XLSX_01$is_done()
    is_done02 <- app_state_render_XLSX_02$is_done()

    IS_DONE <- is_done01 | is_done02
    IS_DONE
  })

  observe({
    req(super_control_xlsx())

    is_done <- super_control_xlsx()

    update_reactive_values(
      the_currier_render_XLSX,
      is_done = is_done
    )

  })

  ##############################################################################
  ##############################################################################
  ##############################################################################
  ##############################################################################


  # Server 08. Output PNG ------------------------------------------------------
  the_currier_render_PNG <- reactiveValues(start_running = FALSE,
                                           text = list(
                                             "str_text01_long" = "PNG Report",
                                             "str_text01_short" = "Rendering PNG Report"
                                           ),
                                           str_temp_work_folder_path = NULL,
                                           str_qmd_file_path = NULL,
                                           str_output_file_path = NULL,
                                           str_download_file_name = NULL,
                                           is_done = FALSE
  )

  observe({
    req(app_state_work_table$is_done)

    # Basics
    start_running <- app_state_work_table$is_done

    # Files
    str_qmd_file_name      <- "report06_lab01_render_png_HIDDEN_RUNNER.qmd"
    str_output_file_name   <- "zzz_output_report06_lab01_render_png.zip"
    str_download_file_name <- "report06_png_Rscience.zip"

    # Folders
    str_temp_work_folder_path <- app_state_work_table$str_temp_work_folder_path

    str_subfolder_report   <- file.path("report06_png", "lab01_render_png")
    str_folder_report_path <- file.path(str_temp_work_folder_path, str_subfolder_report)

    str_subfolder_output   <- "zzz_zzz_output"
    str_folder_output_path <- file.path(str_temp_work_folder_path, str_subfolder_output)


    # Paths
    str_qmd_file_path    <- file.path(str_folder_report_path, str_qmd_file_name)
    str_output_file_path <- file.path(str_folder_output_path, str_output_file_name)

    # More Basics
    is_done <- file.exists(str_output_file_path)

    update_reactive_values(
      the_currier_render_PNG,
      start_running = start_running,
      str_temp_work_folder_path = str_temp_work_folder_path,
      str_qmd_file_path = str_qmd_file_path,
      str_output_file_path = str_output_file_path,
      str_download_file_name = str_download_file_name,
      is_done = is_done
    )


  })



  app_state_render_PNG_01 <-  module_render_ONE_outputs_server(
    id = "module_render_ONE_PNG_01",
    app_state = the_currier_render_PNG,
    show_output = T# <-- ¡OBJETO REACTIVO, NO UNA LISTA!
  )

  app_state_render_PNG_02 <-  module_render_ONE_outputs_server(
    id = "module_render_ONE_PNG_02",
    app_state = the_currier_render_PNG,
    show_output = F# <-- ¡OBJETO REACTIVO, NO UNA LISTA!
  )

  super_control_png <- reactive({
    req(!the_currier_render_PNG$is_done)
    req(the_currier_render_PNG$start_running)

    is_done01 <- app_state_render_PNG_01$is_done()
    is_done02 <- app_state_render_PNG_02$is_done()

    IS_DONE <- is_done01 | is_done02
    IS_DONE
  })

  observe({
    req(super_control_png())

    is_done <- super_control_png()

    update_reactive_values(
      the_currier_render_PNG,
      is_done = is_done
    )

  })

  ##############################################################################
  ##############################################################################
  ##############################################################################
  ##############################################################################



  # Server 08. Output HTML_FULL ------------------------------------------------------
  the_currier_render_HTML_FULL <- reactiveValues(start_running = FALSE,
                                                 text = list(
                                                   "str_text01_long" = "HTML FULL Report",
                                                   "str_text01_short" = "Rendering HTML FULL Report"
                                                 ),
                                                 str_temp_work_folder_path = NULL,
                                                 str_qmd_file_path = NULL,
                                                 str_output_file_path = NULL,
                                                 str_download_file_name = NULL,
                                                 is_done = FALSE
  )

  observe({
    req(app_state_work_table$is_done)

    # Basics
    start_running <- app_state_work_table$is_done

    # Files
    str_qmd_file_name      <- "report99_lab01_render_html_full_HIDDEN_RUNNER.qmd"
    str_output_file_name   <- "zzz_output_report99_lab01_render_html_full_STONE.html"
    str_download_file_name <- "report04_html_full_Rscience.html"

    # Folders
    str_temp_work_folder_path <- app_state_work_table$str_temp_work_folder_path

    str_subfolder_report   <- file.path("report99_html_Rscience", "lab01_RUN_html_full")
    str_folder_report_path <- file.path(str_temp_work_folder_path, str_subfolder_report)

    str_subfolder_output   <- "zzz_zzz_output"
    str_folder_output_path <- file.path(str_temp_work_folder_path, str_subfolder_output)


    # Paths
    str_qmd_file_path    <- file.path(str_folder_report_path, str_qmd_file_name)
    str_output_file_path <- file.path(str_folder_output_path, str_output_file_name)

    # More Basics
    is_done <- file.exists(str_output_file_path)

    update_reactive_values(
      the_currier_render_HTML_FULL,
      start_running = start_running,
      str_temp_work_folder_path = str_temp_work_folder_path,
      str_qmd_file_path = str_qmd_file_path,
      str_output_file_path = str_output_file_path,
      str_download_file_name = str_download_file_name,
      is_done = is_done
    )


  })



  app_state_render_HTML_FULL_01 <-  module_render_ONE_outputs_server(
    id = "module_render_ONE_HTML_FULL_01",
    app_state = the_currier_render_HTML_FULL,
    show_output = T# <-- ¡OBJETO REACTIVO, NO UNA LISTA!
  )

  app_state_render_HTML_FULL_02 <-  module_render_ONE_outputs_server(
    id = "module_render_ONE_HTML_FULL_02",
    app_state = the_currier_render_HTML_FULL,
    show_output = F# <-- ¡OBJETO REACTIVO, NO UNA LISTA!
  )

  super_control_html_full <- reactive({
    req(!the_currier_render_HTML_FULL$is_done)
    req(the_currier_render_HTML_FULL$start_running)

    is_done01 <- app_state_render_HTML_FULL_01$is_done()
    is_done02 <- app_state_render_HTML_FULL_02$is_done()

    IS_DONE <- is_done01 | is_done02
    IS_DONE
  })

  observe({
    req(super_control_html_full())

    is_done <- super_control_html_full()

    update_reactive_values(
      the_currier_render_HTML_FULL,
      is_done = is_done
    )

  })

  ##############################################################################
  ##############################################################################
  ##############################################################################
  ##############################################################################




  # Server12


  output$"rendering" <- renderUI({
    div(module_render_ONE_outputs_ui("module_render_ONE_PDF_02"),
        module_render_ONE_outputs_ui("module_render_ONE_REVEAL_02"),
        module_render_ONE_outputs_ui("module_render_ONE_DOCX_02"),
        module_render_ONE_outputs_ui("module_render_ONE_XLSX_02"),
        module_render_ONE_outputs_ui("module_render_ONE_PNG_02"),
        module_render_ONE_outputs_ui("module_render_ONE_HTML_FULL_02"))
  })

}

shinyApp(ui, server)
