library(shiny)
library(shinyjs)
library(tools)
library(digest)

# ==============================================================================
# UI Module
# ==============================================================================
module_download_one_ui <- function(id, title) {
  ns <- NS(id)
  
  fluidRow(
    column(4, strong(title)),
    column(8,
           div(uiOutput(ns("set_btn")))
    )
  )
}

# ==============================================================================
# Server Module
# ==============================================================================
module_download_one_server <- function(id, r_file_path) {
  
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Estados reactivos iniciales: ROJO y BLOQUEADO
    super_btn_download <- reactiveValues(
      class = "btn-danger btn-sm",
      is_disabled = TRUE
    )
    super_btn_open <- reactiveValues(
      class = "btn-danger btn-sm",
      is_disabled = TRUE
    )
    
    # Estilo CSS común para el estado bloqueado
    # Esto garantiza que ambos se vean idénticos
    disabled_style <- "pointer-events: none; cursor: not-allowed; opacity: 0.5; filter: grayscale(50%);"
    
    # --- Renderizado de Botones ---
    output$"set_btn" <- renderUI({
      
      # 1. Crear el botón de descarga
      btn_down <- downloadButton(
        outputId = ns("btn_download"),
        label = NULL,
        icon = icon("download", class = "fa-2x"),
        class = super_btn_download$class
      )
      
      # 2. Crear el botón de apertura
      btn_open <- actionButton(
        inputId = ns("btn_open"),
        label = NULL,
        icon = icon("binoculars", class = "fa-2x"),
        class = super_btn_open$class
      )
      
      # --- APLICAR MISMO EFECTO VISUAL A AMBOS ---
      
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
      
      div(btn_down, btn_open)
    })
    
    # --- Observador de cambios en la ruta del archivo ---
    observeEvent(r_file_path(), {
      path <- r_file_path()
      
      if (is.null(path) || !file.exists(path)) {
        super_btn_download$class       <- "btn-danger btn-sm"
        super_btn_download$is_disabled <- TRUE
        
        super_btn_open$class           <- "btn-danger btn-sm"
        super_btn_open$is_disabled     <- TRUE
        
      } else {
        the_file_ext <- tolower(tools::file_ext(path))
        
        # Habilitar descarga
        super_btn_download$class       <- "btn-warning btn-sm"
        super_btn_download$is_disabled <- FALSE
        
        # Habilitar apertura solo si es HTML/PDF
        if (the_file_ext %in% c("html", "pdf")) {
          super_btn_open$class         <- "btn-warning btn-sm"
          super_btn_open$is_disabled   <- FALSE
        } else {
          # Si existe pero no es HTML/PDF, se queda bloqueado pero en color naranja (warning)
          super_btn_open$class         <- "btn-warning btn-sm"
          super_btn_open$is_disabled   <- TRUE 
        }
      }
    }, ignoreNULL = FALSE, ignoreInit = FALSE)
    
    # --- Lógica del botón Abrir (btn_open) ---
    observeEvent(input$btn_open, {
      html_path <- r_file_path()
      req(html_path, file.exists(html_path))
      
      # Cambio visual al hacer clic (éxito)
      shinyjs::removeClass("btn_open", "btn-warning")
      shinyjs::addClass("btn_open", "btn-success")
      
      html_dir <- dirname(html_path)
      html_filename <- basename(html_path)
      the_file_ext <- tolower(tools::file_ext(html_path))
      
      if (the_file_ext %in% c("html", "pdf")) {
        resource_id <- digest::digest(html_dir, algo = "md5")
        shiny::addResourcePath(resource_id, html_dir)
        html_url <- file.path(resource_id, html_filename)
        shinyjs::runjs(paste0("window.open('", html_url, "', '_blank');"))
      }
    })
    
    # --- Lógica del botón Descargar (btn_download) ---
    output$btn_download <- downloadHandler(
      filename = function() {
        full_path <- r_file_path()
        basename(ifelse(!is.null(full_path), full_path, "file.html"))
      },
      content = function(file) {
        file_to_download <- r_file_path()
        if (!is.null(file_to_download) && file.exists(file_to_download)) {
          # Cambio visual al descargar
          shinyjs::runjs(paste0("$('#", ns("btn_download"), "').removeClass('btn-warning').addClass('btn-success');"))
          file.copy(file_to_download, file)
        }
      }
    )
  })
}