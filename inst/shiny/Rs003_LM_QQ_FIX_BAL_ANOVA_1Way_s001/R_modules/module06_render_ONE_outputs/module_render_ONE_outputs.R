library(shiny)
library(promises)
library(future)
library(shinyjs)

# Background plan configuration
plan(multisession)

module_render_ONE_outputs_ui <- function(id) {
  ns <- NS(id)
  uiOutput(ns("main_render_RShiny"))
}

module_render_ONE_outputs_server <- function(id, app_state, show_output = FALSE) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Special fn
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

    # --- 1. State Variables ---
    start_running    <- reactiveVal(FALSE)
    is_done          <- reactiveVal(FALSE)
    play_clicked     <- reactiveVal(FALSE)
    download_clicked <- reactiveVal(FALSE)
    open_clicked     <- reactiveVal(FALSE)
    is_pdf_or_html   <- reactiveVal(FALSE)

    # Unique prefix for this module (Avoids conflicts between multiple instances)
    str_file_prefix <- ns("file_vault")
    tiempo_estimado <- 45

    observe({
      req(app_state$is_running)
      start_running(TRUE)
    })

    # --- 2. Resource Path Registration ---
    # Executes once when the app path is available
    observe({
      req(app_state$str_output_file_path)
      str_output_file_path <- app_state$str_output_file_path
      str_ext <- tools::file_ext(str_output_file_path)

      check_pdf_or_html <- str_ext == "pdf" | str_ext == "html"
      is_pdf_or_html(check_pdf_or_html)

      # Register the physical folder under this module's alias
      addResourcePath(
        prefix = str_file_prefix,
        directoryPath = dirname(app_state$str_output_file_path)
      )
    })

    # --- 2.5 Auto-Detección de Archivo Existente ---
    observe({
      req(app_state$str_output_file_path)
      path <- app_state$str_output_file_path

      if (file.exists(path)) {
        is_done(TRUE)
        play_clicked(TRUE) # <--- IMPORTANTE: Esto deshabilita el botón Play

        # Sincronizamos al Currier global para que otras instancias se enteren
        if(!is.null(app_state$is_done) && !app_state$is_done) {
          update_reactive_values(app_state, is_done = TRUE)
        }
      }
    })

    # --- 2.7 Sincronización desde el Estado Global ---
    observe({
      # Escuchamos el is_done del app_state (Currier)
      req(app_state$is_done)

      # Si el global es TRUE, actualizamos los locales de este módulo
      if(app_state$is_done) {
        is_done(TRUE)
        play_clicked(TRUE)
      }
    })

    # --- 3. Dynamic Modal UI ---
    output$modal_content_ui <- renderUI({
      str_text01_short <- "Render file report."
      list_text <- app_state$text
      if(!is.null(list_text$str_text01_short)) str_text01_short <- list_text$str_text01_short

      if (!is_done()) {
        # STATE: PROCESSING
        tagList(
          tags$script(HTML(sprintf("
            setTimeout(function() {
              window.currentPdfSeconds = 0;
              var estimado = %s;
              var display = document.getElementById('%s');
              var warning = document.getElementById('%s');
              if (window.pdfTimer) clearInterval(window.pdfTimer);
              window.pdfTimer = setInterval(function() {
                window.currentPdfSeconds++;
                var m = Math.floor(window.currentPdfSeconds / 60);
                var s = window.currentPdfSeconds %% 60;
                var timeStr = (m < 10 ? '0' + m : m) + ':' + (s < 10 ? '0' + s : s);
                if (display) { display.innerText = ' ' + timeStr; }
                if (window.currentPdfSeconds > estimado && warning) { warning.style.display = 'block'; }
                window.lastPdfTimeStr = timeStr;
              }, 1000);
            }, 200);
          ", tiempo_estimado, ns("timer_display"), ns("timer_warning")))),
          div(style = "text-align: center; min-height: 280px; padding: 10px;",
              icon("cog", class = "fa-spin fa-5x", style = "color: #0d6efd; margin-bottom: 20px;"),
              h3(str_text01_short, style = "color: #0d6efd; font-weight: bold;"),
              p(tags$b(paste("Expected time:", tiempo_estimado, "seconds.")), style="color: #666;"),
              div(style = "font-size: 35px; font-family: 'Courier New', monospace; font-weight: bold; color: #333; margin-bottom: 10px;",
                  icon("clock"), span(id = ns("timer_display"), " 00:00")),
              div(id = ns("timer_warning"),
                  style = "display: none; color: #d9534f; font-weight: bold; margin-bottom: 15px; background: #f9f2f2; padding: 10px; border-radius: 5px; border: 1px solid #ebccd1;",
                  icon("exclamation-triangle"), "Please continue waiting. File in process..."),
              div(class = "progress", style = "height: 12px; margin: 0 20px;",
                  div(class = "progress-bar progress-bar-striped progress-bar-animated", style = "width: 100%;"))
          )
        )
      } else {
        # STATE: COMPLETED
        tagList(
          tags$script(HTML(sprintf("
            setTimeout(function() {
              if(window.pdfTimer) clearInterval(window.pdfTimer);
              var finalDisplay = document.getElementById('%s');
              if(finalDisplay && window.lastPdfTimeStr) { finalDisplay.innerText = window.lastPdfTimeStr; }
            }, 100);
          ", ns("final_time_span")))),
          div(style = "text-align: center; padding: 20px;",
              icon("check-circle", class = "fa-5x", style = "color: #198754; margin-bottom: 20px;"),
              h3("Process Completed!", style = "color: #198754; font-weight: bold;"),
              h4(tags$b("Total time: ", span(id = ns("final_time_span"), "...")) , style="color: #333; margin: 15px 0;"),
              p("The report has been successfully generated.", style = "color: #666;"),
              br(),
              actionButton(ns("btn_close_modal"), "Finish and View Report",
                           class = "btn-success btn-lg", style = "width: 100%; font-weight: bold;")
          )
        )
      }
    })

    # --- 4. Play Button Action with Autodestruction ---
    observeEvent(input$btn_play, {
      req(!play_clicked())
      is_done(FALSE)
      showModal(modalDialog(title = NULL, footer = NULL, easyClose = FALSE, size = "m", uiOutput(ns("modal_content_ui"))))

      # Capture the observer in a variable to destroy it when finished
      obs_render <- observe({
        str_qmd_file_path <- app_state$str_qmd_file_path

        shinyjs::delay(1000, {
          future_promise({
            library("quarto")
            quarto::quarto_render(
              input = str_qmd_file_path,
              execute_dir = dirname(str_qmd_file_path)
            )
            TRUE
          }) %...>% {
            is_done(TRUE)
            play_clicked(TRUE)
            obs_render$destroy() # AUTO-DESTRUCTION
            print(">>> Quarto Observer successfully DESTROYED.")
          } %...!% {
            removeModal()
            showNotification("Error during Quarto rendering.", type = "error")
            obs_render$destroy() # AUTO-DESTRUCTION
            print(">>> Quarto Observer DESTROYED after error.")
          }
        })
        isolate(NULL)
      })
    })

    # --- 5. Button Events ---
    observeEvent(input$btn_close_modal, {
      shinyjs::runjs("if(window.pdfTimer) clearInterval(window.pdfTimer);")
      removeModal()
    })

    observeEvent(input$btn_open, {
      open_clicked(TRUE)
      # Use the registered prefix for the URL
      url <- sprintf("%s/%s", str_file_prefix, basename(app_state$str_output_file_path))
      shinyjs::runjs(sprintf("window.open('%s', '_blank');", url))
    })

    output$btn_download <- downloadHandler(
      filename = function() { app_state$str_download_file_name },
      content = function(file) {
        download_clicked(TRUE)
        file.copy(app_state$str_output_file_path, file)
      }
    )

    # --- 6. Interface Rendering ---
    output$set_btn <- renderUI({
      disabled_style <- "pointer-events: none; cursor: not-allowed; opacity: 0.5; filter: grayscale(1);"
      class_p <- if(play_clicked()) "btn-success" else "btn-primary"
      btn_p <- actionButton(ns("btn_play"), NULL, icon("play", class="fa-2x"), class=paste(class_p, "btn-sm"))
      if(play_clicked()) btn_p <- tagAppendAttributes(btn_p, style=disabled_style)

      class_d <- if(!is_done()) "btn-danger" else if(download_clicked()) "btn-success" else "btn-warning"
      btn_d <- downloadButton(ns("btn_download"), NULL, class=paste(class_d, "btn-sm"))
      btn_d$children[[1]] <- icon("download", class="fa-2x")
      if(!is_done()) btn_d <- tagAppendAttributes(btn_d, style=disabled_style)

      class_o <- if(!is_done()) "btn-danger" else if(open_clicked()) "btn-success" else "btn-warning"
      btn_o <- actionButton(ns("btn_open"), NULL, icon("binoculars", class="fa-2x"), class=paste(class_o, "btn-sm"))
      if(!is_done()) btn_o <- tagAppendAttributes(btn_o, style=disabled_style)

      div(style="display: flex; gap: 8px;", btn_p, btn_d, btn_o)
    })

    output$main_render_RShiny <- renderUI({
      str_text01_long <- app_state$text$str_text01_long %||% "Rendering"

      # Cabecera siempre presente
      header <- fluidRow(
        column(6, h4(str_text01_long)),
        column(6, div(uiOutput(ns("set_btn")), style = "float: right;"))
      )

      # Si NO se debe mostrar el output, solo retornamos la cabecera
      if (!show_output) {
        return(tagList(header))
      }

      # Si SÍ se debe mostrar, agregamos el visor o el placeholder
      tagList(
        header,
        hr(),
        if(is_done()){
          uiOutput(ns("viewer_html_pdf"))
        } else {
          div(style="text-align:center; padding: 80px; color: #ddd; border: 2px dashed #eee; border-radius: 15px;",
              icon("file-pdf", class="fa-5x"),
              p("The file viewer will be activated automatically upon completion."))
        }
      )
    })

    output$viewer_html_pdf <- renderUI({
      req(show_output, is_done(), is_pdf_or_html())
      # Build URL using namespace prefix
      str_output_file_name <- basename(app_state$str_output_file_path)
      pdf_url <- sprintf("%s/%s?t=%s", str_file_prefix, str_output_file_name, as.numeric(Sys.time()))
      tags$iframe(src = pdf_url, style="width:100%; height:850px; border:1px solid #eee; border-radius: 10px;")
    })

    return(reactiveValues(is_done = is_done))
  })
}
