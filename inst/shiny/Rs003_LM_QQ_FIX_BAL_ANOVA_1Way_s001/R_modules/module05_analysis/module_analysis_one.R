library(shiny)
library(shinyjs)
library(tools)
library(digest)

# ==============================================================================
# UI Module
# ==============================================================================
module_analysis_one_ui <- function(id, title) {
  ns <- NS(id)

  div(
    fluidRow(
      column(2, strong(title)),
      column(8),
      column(2, div(uiOutput(ns("set_btn"))))
    ),
    fluidRow(
      # Contenedor principal CON altura fija y UN solo scroll
      div(
        style = "width: 100%; height: calc(100vh - 200px); overflow: auto;",
        uiOutput(ns("the_view"))
      )
    )
  )
}

# ==============================================================================
# Server Module
# ==============================================================================
module_analysis_one_server <- function(id, r_file_path) {

  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Estados reactivos iniciales
    super_btn_download <- reactiveValues(
      class = "btn-danger btn-sm",
      is_disabled = TRUE
    )
    super_btn_open <- reactiveValues(
      class = "btn-danger btn-sm",
      is_disabled = TRUE
    )

    # Reactive value para manejar recursos
    resource_path <- reactiveVal(NULL)

    # Estado para la barra de herramientas del PDF
    show_pdf_toolbar <- reactiveVal(TRUE)

    disabled_style <- "pointer-events: none; cursor: not-allowed; opacity: 0.5; filter: grayscale(50%);"

    # --- Renderizado de Botones ---
    output$"set_btn" <- renderUI({

      # 1. Botón de descarga
      btn_down <- downloadButton(
        outputId = ns("btn_download"),
        label = NULL,
        icon = icon("download", class = "fa-2x"),
        class = super_btn_download$class
      )

      # 2. Botón de apertura
      btn_open <- actionButton(
        inputId = ns("btn_open"),
        label = NULL,
        icon = icon("binoculars", class = "fa-2x"),
        class = super_btn_open$class
      )

      # 3. Botón para alternar barra de herramientas del PDF
      file_ext <- if (!is.null(r_file_path()) && file.exists(r_file_path())) {
        tolower(tools::file_ext(r_file_path()))
      } else {
        NULL
      }

      pdf_toolbar_btn <- NULL
      if (!is.null(file_ext) && file_ext == "pdf") {
        pdf_toolbar_btn <- actionButton(
          inputId = ns("btn_toggle_toolbar"),
          label = NULL,
          icon = if (show_pdf_toolbar()) icon("eye-slash") else icon("eye"),
          class = "btn-info btn-sm",
          title = if (show_pdf_toolbar()) "Ocultar barra" else "Mostrar barra"
        )
      }

      # Aplicar estilo deshabilitado
      if (super_btn_download$is_disabled) {
        btn_down <- tagAppendAttributes(
          btn_down,
          class = "disabled",
          style = disabled_style
        )
      }

      if (super_btn_open$is_disabled) {
        btn_open <- tagAppendAttributes(
          btn_open,
          class = "disabled",
          style = disabled_style
        )
      }

      # Organizar botones
      if (!is.null(pdf_toolbar_btn)) {
        div(btn_down, btn_open, pdf_toolbar_btn)
      } else {
        div(btn_down, btn_open)
      }
    })

    # --- Observador para manejar recursos del archivo ---
    observeEvent(r_file_path(), {
      req(r_file_path())

      file_path <- r_file_path()

      # Limpiar recursos anteriores si existen
      if (!is.null(resource_path())) {
        shiny::removeResourcePath(resource_path()$id)
      }

      # Reiniciar estado de barra de herramientas
      show_pdf_toolbar(TRUE)

      if (!file.exists(file_path)) {
        super_btn_download$class       <- "btn-danger btn-sm"
        super_btn_download$is_disabled <- TRUE
        super_btn_open$class           <- "btn-danger btn-sm"
        super_btn_open$is_disabled     <- TRUE
        return()
      }

      file_ext <- tolower(tools::file_ext(file_path))
      file_dir <- dirname(file_path)
      file_name <- basename(file_path)

      # Crear recurso para el archivo
      resource_id <- paste0("file_", digest::digest(file_path, algo = "md5"))
      shiny::addResourcePath(resource_id, file_dir)

      # Guardar información del recurso
      resource_path(list(
        id = resource_id,
        dir = file_dir,
        name = file_name,
        url = file.path(resource_id, file_name)
      ))

      # Actualizar estado de botones
      super_btn_download$class       <- "btn-warning btn-sm"
      super_btn_download$is_disabled <- FALSE

      if (file_ext %in% c("html", "pdf")) {
        super_btn_open$class         <- "btn-warning btn-sm"
        super_btn_open$is_disabled   <- FALSE
      } else {
        super_btn_open$class         <- "btn-warning btn-sm"
        super_btn_open$is_disabled   <- TRUE
      }
    }, ignoreNULL = FALSE, ignoreInit = FALSE)

    # --- Botón para alternar barra de herramientas del PDF ---
    observeEvent(input$btn_toggle_toolbar, {
      show_pdf_toolbar(!show_pdf_toolbar())
    })

    # --- Renderizado de la vista ---
    output$the_view <- renderUI({
      req(r_file_path(), resource_path())

      file_path <- r_file_path()
      resource_info <- resource_path()

      if (!file.exists(file_path)) {
        return(
          tags$div(
            class = "alert alert-danger",
            icon("exclamation-triangle"),
            " Archivo no encontrado: ",
            tags$code(basename(file_path))
          )
        )
      }

      file_ext <- tolower(tools::file_ext(file_path))

      # Parámetros para PDF
      pdf_params <- if (show_pdf_toolbar()) {
        ""  # Barra de herramientas visible
      } else {
        "#toolbar=0"  # Sin barra de herramientas
      }

      if (file_ext == "html") {
        # HTML - SIN scroll interno, todo el contenido fluye
        if (exists("fn_app_show_my_html")) {
          html_content <- fn_app_show_my_html(resource_info$dir, resource_info$name)
          tags$div(
            style = "width: 100%; min-height: 100%;",  # Sin overflow, contenido natural
            HTML(html_content)
          )
        } else {
          # Iframe SIN scrollbars - ocupa todo el espacio disponible
          tags$iframe(
            src = resource_info$url,
            style = "width: 100%; height: 100%; border: none;",  # 100% del contenedor
            sandbox = "allow-same-origin allow-scripts",
            scrolling = "no"  # Desactiva scroll interno
          )
        }

      } else if (file_ext == "pdf") {
        # PDF - Altura completa SIN scroll interno
        tags$div(
          style = "width: 100%; height: 100%;",  # 100% del contenedor padre
          tags$iframe(
            src = paste0(resource_info$url, pdf_params),
            style = "width: 100%; height: 100%; border: none;",  # 100% del div contenedor
            type = "application/pdf"
          )
        )

      } else {
        # Formato no soportado
        tags$div(
          class = "alert alert-warning",
          icon("exclamation-circle"),
          " Formato no soportado: ", tags$code(paste(".", file_ext)),
          tags$br(),
          "Solo se pueden visualizar archivos HTML y PDF."
        )
      }
    })

    # --- Lógica del botón Abrir (btn_open) ---
    observeEvent(input$btn_open, {
      req(r_file_path(), resource_path())

      file_path <- r_file_path()
      resource_info <- resource_path()

      if (!file.exists(file_path)) return()

      file_ext <- tolower(tools::file_ext(file_path))

      if (file_ext %in% c("html", "pdf")) {
        # Cambio visual al hacer clic
        shinyjs::removeClass("btn_open", "btn-warning")
        shinyjs::addClass("btn_open", "btn-success")

        # Determinar parámetros para PDF
        pdf_params <- if (file_ext == "pdf") {
          if (show_pdf_toolbar()) "" else "#toolbar=0"
        } else ""

        # Abrir en nueva ventana
        js_code <- sprintf("window.open('%s%s', '_blank');",
                           resource_info$url,
                           if (file_ext == "pdf") pdf_params else "")
        shinyjs::runjs(js_code)
      }
    })

    # --- Lógica del botón Descargar (btn_download) ---
    output$btn_download <- downloadHandler(
      filename = function() {
        full_path <- r_file_path()
        if (is.null(full_path) || !file.exists(full_path)) {
          return("file.html")
        }
        basename(full_path)
      },
      content = function(file) {
        file_to_download <- r_file_path()
        if (!is.null(file_to_download) && file.exists(file_to_download)) {
          # Cambio visual al descargar
          shinyjs::runjs(sprintf("$('#%s').removeClass('btn-warning').addClass('btn-success');",
                                 ns("btn_download")))
          file.copy(file_to_download, file)
        }
      }
    )

    # --- Limpieza al destruir el módulo ---
    onStop(function() {
      if (!is.null(resource_path())) {
        shiny::removeResourcePath(resource_path()$id)
      }
    })
  })
}
