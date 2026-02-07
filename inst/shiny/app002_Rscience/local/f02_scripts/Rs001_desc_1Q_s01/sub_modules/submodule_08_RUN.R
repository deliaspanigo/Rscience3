# =============================================================================
# MODULE: Pipeline Runner (Single Instance - Pure Row Mode)
# =============================================================================

submodule_08_RUN_ui <- function(id) {
  ns <- shiny::NS(id)

  shiny::tagList(
    shinyjs::useShinyjs(),

    shiny::tags$style(shiny::HTML(paste0("
      /* Estilo para que sea SOLO una linea */
      .pipeline-row-mode {
        margin-bottom: 0px !important;
        border-radius: 0px !important;
        box-shadow: none !important;
        border-top: none !important;
      }
      .pipeline-row-mode .card-header {
        padding: 4px 12px !important;
        background-color: #ffffff !important;
      }
      .btn-disabled { opacity: 0.5; pointer-events: none; }
      .compact-card { border: 1px solid #ddd; }
    "))),

    bslib::card(
      id = ns("main_card"),
      class = "compact-card",
      bslib::card_header(
        class = "d-flex justify-content-between align-items-center",
        shiny::span(
          shiny::icon("rocket", style = "color: #555; font-size: 0.9em;"),
          shiny::tags$span(shiny::textOutput(ns("card_title"), inline = TRUE),
                           style = "margin-left: 8px; font-weight: 500; font-size: 0.9rem;")
        ),
        shiny::div(
          class = "btn-group",
          shiny::actionButton(ns("run_pipeline"), "Run", icon = shiny::icon("play"), class = "btn-warning btn-sm"),
          shiny::actionButton(ns("view_report"), "View", icon = shiny::icon("binoculars"), class = "btn-info btn-sm"),
          shiny::downloadButton(ns("download_file"), "Get", class = "btn-secondary btn-sm"),
          shiny::actionButton(ns("diagnostic"), "Diag", icon = shiny::icon("bug"), class = "btn-secondary btn-sm")
        )
      ),
      # UI Condicional: Si show_viewer es FALSE, esto es NULL y no ocupa espacio
      shiny::uiOutput(ns("conditional_body"))
    )
  )
}

submodule_08_RUN_server <- function(id, internal_ORH_02_temporal_FF,
                                    target_actions = shiny::reactive({c("action02")}),
                                    debug_toggle = shiny::reactive({FALSE}),
                                    show_viewer = TRUE) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    module_state <- shiny::reactiveValues(
      execution_log = "System Ready...",
      is_running = FALSE,
      file_exists = FALSE,
      has_output_value = FALSE,
      file_path = NULL,
      file_is_viewable = FALSE,
      downloaded = FALSE
    )

    current_action_id <- shiny::reactive({
      shiny::req(target_actions())
      target_actions()[1]
    })

    # --- HELPERS (Inalterados) ---
    find_file_recursively <- function(root, pattern) {
      if (!fs::dir_exists(root)) return(NULL)
      files <- fs::dir_ls(root, recurse = TRUE, regexp = pattern)
      if (length(files) == 0) return(NULL)
      return(files[1])
    }

    get_all_dependencies <- function(action_id, pipe, visited = NULL) {
      if (is.null(visited)) visited <- c()
      if (action_id %in% visited) return(visited)
      visited <- c(visited, action_id)
      task <- pipe[[action_id]]
      if (!is.null(task$dependencies)) {
        for (dep in task$dependencies) { visited <- get_all_dependencies(dep, pipe, visited) }
      }
      return(visited)
    }

    topological_sort <- function(actions, pipe) {
      all_deps <- c()
      for (action_id in actions) { all_deps <- unique(c(all_deps, get_all_dependencies(action_id, pipe))) }
      sorted_actions <- c(); remaining <- all_deps
      while (length(remaining) > 0) {
        ready <- Filter(function(a) {
          deps <- pipe[[a]]$dependencies
          is.null(deps) || all(deps %in% sorted_actions)
        }, remaining)
        if (length(ready) == 0) break
        sorted_actions <- c(sorted_actions, ready); remaining <- setdiff(remaining, ready)
      }
      return(sorted_actions)
    }

    # --- PIPELINE DEFINITION ---
    pipeline_list <- shiny::reactive({
      shiny::req(internal_ORH_02_temporal_FF())
      root <- internal_ORH_02_temporal_FF()$temp_copying_FF$temp_script_folder$str_path
      list(
        "action01" = list(description = "Copy & Mod QMD",           qmd_file = find_file_recursively(root, "f00_02_action_copy_mod/action01_STONE\\.qmd$"), output_pattern = NULL, dependencies = NULL),
        "action02" = list(description = "HTML Report Generation",   qmd_file = find_file_recursively(root, "f01_RQuarto/STONE\\.qmd$"), output_pattern = "zzz_zzz_output/zzz_output_f00_01_RQuarto\\.html$", export_name = "RQuarto_Rcode.html", dependencies = c("action01")),
        "action03" = list(description = "R-Script Analysis",        qmd_file = find_file_recursively(root, "f02_Rscript/STONE\\.qmd$"), output_pattern = "zzz_zzz_output/zzz_output_f02_gen_Rscript_external\\.R$", export_name = "Rscript_Rcode.R", dependencies = c("action01")),
        "action04" = list(description = "Data Environment (RData)", qmd_file = find_file_recursively(root, "f03_RData/STONE\\.qmd$"), output_pattern = "zzz_zzz_output/zzz_output_f03_RData\\.RData$", export_name = "RData_ENV.RData", dependencies = c("action03")),
        "action05" = list(description = "Rscript HTML",             qmd_file = find_file_recursively(root, "f04_Rscript_html/STONE\\.qmd$"), output_pattern = "zzz_zzz_output/zzz_output_f04_RUNNER\\.html$", export_name = "External.html", dependencies = c("action03")),
        "action06" = list(description = "Report PDF",               qmd_file = find_file_recursively(root, "f05_gen_pdf/STONE\\.qmd$"), output_pattern = "zzz_zzz_output/zzz_output_f05_RUNNER\\.pdf$", export_name = "PDF_Report.pdf", dependencies = c("action04")),
        "action07" = list(description = "Report Revealjs",          qmd_file = find_file_recursively(root, "f06_gen_reveajs/STONE\\.qmd$"), output_pattern = "zzz_zzz_output/zzz_output_f06_RUNNER\\.html$", export_name = "Revealjs_Report.html", dependencies = c("action04")),
        "action08" = list(description = "Report docx",              qmd_file = find_file_recursively(root, "f07_gen_docx/STONE\\.qmd$"), output_pattern = "zzz_zzz_output/zzz_output_f07_RUNNER\\.docx$", export_name = "docx_Report.docx", dependencies = c("action04")),
        "action09" = list(description = "Report xlsx",              qmd_file = find_file_recursively(root, "f08_gen_xlsx/STONE\\.qmd$"), output_pattern = "zzz_zzz_output/zzz_output_f08_RUNNER\\.xlsx$", export_name = "xlsx_Report.xlsx", dependencies = c("action04")),
        "action10" = list(description = "PNG zipped",               qmd_file = find_file_recursively(root, "f09_gen_png/STONE\\.qmd$"), output_pattern = "zzz_zzz_output/zzz_output_f09_RUNNER\\.zip$", export_name = "png_files.zip", dependencies = c("action04")),
        "action11" = list(description = "Report HTML",              qmd_file = find_file_recursively(root, "f10_gen_shiny/STONE\\.qmd$"), output_pattern = "zzz_zzz_output/zzz_output_f10_RUNNER\\.html$", export_name = "html_Report.html", dependencies = c("action04"))


      )
    })

    # --- TÍTULO LIMPIO (Solo descripción) ---
    output$card_title <- shiny::renderText({
      shiny::req(pipeline_list(), current_action_id())
      pipeline_list()[[current_action_id()]]$description
    })

    # --- UI CONDICIONAL ---
    output$conditional_body <- shiny::renderUI({
      if (!show_viewer) {
        shinyjs::addClass("main_card", "pipeline-row-mode")
        return(NULL)
      }
      shiny::div(style = "padding: 15px;",
                 shiny::conditionalPanel(
                   condition = paste0("output['", ns("has_output"), "'] == true"),
                   bslib::navset_card_underline(
                     id = ns("report_tabs"),
                     bslib::nav_panel("Visualizer", icon = shiny::icon("eye"), shiny::uiOutput(ns("content_viewer_ui"))),
                     bslib::nav_panel("Logs", icon = shiny::icon("terminal"), shiny::verbatimTextOutput(ns("stdout")))
                   )
                 )
      )
    })

    # --- LOGICA DE BOTONES Y COLORES ---
    check_file_exists <- function() {
      shiny::req(internal_ORH_02_temporal_FF(), pipeline_list(), current_action_id())
      root <- internal_ORH_02_temporal_FF()$temp_copying_FF$temp_script_folder$str_path
      task <- pipeline_list()[[current_action_id()]]
      if (!is.null(task$output_pattern)) {
        path <- find_file_recursively(root, task$output_pattern)
        exists <- !is.null(path) && fs::file_exists(path)
        if (exists) {
          module_state$file_path <- path
          module_state$file_is_viewable <- tolower(fs::path_ext(path)) %in% c("html", "pdf")
        }
        module_state$has_output_value <- exists
        return(exists)
      }
      FALSE
    }

    shiny::observe({
      exists <- check_file_exists()
      # Boton RUN
      if (!module_state$is_running) {
        if (exists) {
          shinyjs::disable("run_pipeline"); shinyjs::addClass(id="run_pipeline", class="btn-success"); shinyjs::removeClass(id="run_pipeline", class="btn-warning")
        } else {
          shinyjs::enable("run_pipeline"); shinyjs::addClass(id="run_pipeline", class="btn-warning"); shinyjs::removeClass(id="run_pipeline", class="btn-success")
        }
      }
      # Boton GET (Download)
      if (exists) {
        shinyjs::enable("download_file")
        if (module_state$downloaded) {
          shinyjs::removeClass(id="download_file", class="btn-secondary"); shinyjs::removeClass(id="download_file", class="btn-warning"); shinyjs::addClass(id="download_file", class="btn-success")
        } else {
          shinyjs::removeClass(id="download_file", class="btn-secondary"); shinyjs::removeClass(id="download_file", class="btn-success"); shinyjs::addClass(id="download_file", class="btn-warning")
        }
      } else {
        shinyjs::disable("download_file"); shinyjs::addClass(id="download_file", class="btn-secondary"); shinyjs::removeClass(id="download_file", class="btn-warning"); shinyjs::removeClass(id="download_file", class="btn-success")
      }
      # Boton VIEW
      if (exists && module_state$file_is_viewable) shinyjs::enable("view_report") else shinyjs::disable("view_report")
      shiny::invalidateLater(2000)
    })

    # --- VIEW (Pestaña nueva) ---
    shiny::observeEvent(input$view_report, {
      shiny::req(module_state$file_path, module_state$file_is_viewable)
      resource_name <- paste0("res_", id)
      shiny::addResourcePath(resource_name, fs::path_dir(module_state$file_path))
      file_url <- paste0(resource_name, "/", fs::path_file(module_state$file_path))
      shinyjs::runjs(sprintf("window.open('%s', '_blank');", file_url))
    })

    # --- RUN ---
    shiny::observeEvent(input$run_pipeline, {
      shiny::req(pipeline_list(), internal_ORH_02_temporal_FF())
      module_state$is_running <- TRUE; module_state$downloaded <- FALSE; shinyjs::disable("run_pipeline")
      root <- internal_ORH_02_temporal_FF()$temp_copying_FF$temp_script_folder$str_path
      tryCatch({
        pipe <- pipeline_list(); all_actions <- topological_sort(current_action_id(), pipe)
        for (act in all_actions) {
          task <- pipe[[act]]
          out <- if(!is.null(task$output_pattern)) find_file_recursively(root, task$output_pattern) else NULL
          if (is.null(out)) {
            # print(task$qmd_file)
            quarto::quarto_render(input=task$qmd_file,
                                  execute_dir=fs::path_dir(task$qmd_file),
                                  quiet=TRUE,
                                  as_job = FALSE) }
        }
      }, error = function(e) { message("Error333: ", e$message) })
      module_state$is_running <- FALSE; check_file_exists()
    })

    # --- VIEWER Y DOWNLOAD ---
    output$content_viewer_ui <- shiny::renderUI({
      shiny::req(module_state$file_path, module_state$file_is_viewable)
      resource_name <- paste0("res_int_", id)
      shiny::addResourcePath(resource_name, fs::path_dir(module_state$file_path))
      shiny::tags$iframe(src=paste0(resource_name, "/", fs::path_file(module_state$file_path)), style="width:100%; height:600px; border:none;")
    })

    output$stdout <- shiny::renderText({ module_state$execution_log })

    output$download_file <- shiny::downloadHandler(
      filename = function() {
        task <- pipeline_list()[[current_action_id()]]
        if(!is.null(task$export_name)) task$export_name else "output.file"
      },
      content = function(file) { module_state$downloaded <- TRUE; fs::file_copy(module_state$file_path, file) }
    )

    output$has_output <- shiny::reactive({ module_state$has_output_value })
    shiny::outputOptions(output, "has_output", suspendWhenHidden = FALSE)
  })
}
