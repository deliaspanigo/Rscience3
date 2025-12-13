# app.R - VERSIÓN CORREGIDA

library("shiny")
library("bslib")
library("shinyjs")
library("here")


library("promises")
library("future")
plan(multisession, workers = parallel::detectCores() - 1)


# Esta línea hace MAGIA:
here::i_am("app.R")

# Cargar módulos
source("R_modules/modules00_central/module_buttons.R")
source("R_modules/modules01_theory/module_theory.R")
source("R_modules/modules02_inputs/module_inputs.R")
source("R_modules/modules03_render_outputs/module_render_outputs.R")
source("R_modules/modules03_render_outputs/module_download_one.R")
source("R_modules/modules03_render_outputs/module_download_multi.R")


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
    "Rscience 0.1.0",
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
    proc = list(
      btn_id = "btn_proc",
      btn_label = "Proccessing", 
      btn_enable = TRUE,
      tab_id = "tab_proc",
      icon = icon("chart-bar"),
      order = 4
    ),
    analysis = list(
      btn_id = "btn_analysis",
      btn_label = "Analysis", 
      btn_enable = TRUE,
      tab_id = "tab_analysis",
      icon = icon("chart-bar"),
      order = 5
    ),
    download = list(
      btn_id = "btn_download",
      btn_label = "Download",
      btn_enable = TRUE,
      tab_id = "tab_download",
      icon = icon("download"),
      order = 6
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
        title = "Proc",
        value = buttons_info$"proc"$"tab_id",
        module_render_outputs_ui(id = "module_render")
      ),
      tabPanel(
        title = "Analysis",
        value = buttons_info$"analysis"$"tab_id",
        # h3("Analysis - Contenido simple"),
        uiOutput("main_analysis")
      ),
      
      tabPanel(
        title = "Download",
        value = buttons_info$"download"$"tab_id",
        module_download_multi_ui(id = "module_download_multi")
      )
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
  
  
  
  # Server 03. Inputs -----------------------------------------------------------
  app_state_inputs <- module_inputs_server(id = "inputs_module")
  
  # observe({
  #   req(app_state_inputs)
  #   print(app_state_inputs())
  # })
  # Server Hide - Proccesing
  # Currier 02 

  
  

  
  

  
  # Server 04 - Rendering files ------------------------------------------------
  the_currier_proccesing <- reactiveValues(is_running = FALSE,
                                           is_done = FALSE)
  
  observe({
    req( app_state_inputs()$play$run)
    the_currier_proccesing$is_running <- app_state_inputs()$play$run
    # the_currier_proccesing$is_done    <- TRUE
  })
  
  app_state_render <-  module_render_outputs_server(
    id = "module_render", 
    app_state = the_currier_proccesing# <-- ¡OBJETO REACTIVO, NO UNA LISTA!
  )
  
  # --- Sincronización: Al terminar el proceso, saltar a Analysis ---
  observeEvent(app_state_render$is_done, {
    # Validamos que el proceso efectivamente haya terminado y sea TRUE
    req(app_state_render$is_done)
    
    # 1. Cambiamos el tabsetPanel principal a la solapa de Análisis
    updateTabsetPanel(session, "main_tabs", selected = buttons_info$analysis$tab_id)
    
    # 2. (Opcional) Notificación para el usuario
    showNotification("Proceso completado. Mostrando análisis.", type = "message")
  })
  
  # Server 05 - Analysis -------------------------------------------------------
  # Server 05 - Analysis -------------------------------------------------------
  app_state_analysis <- reactive({
    
    # 1. Acceso directo: Esto crea una dependencia únicamente de esta variable
    # y no de todo el objeto reactiveValues.
    path_ready <- app_state_render$temp_output_folder_path
    
    # 2. Validación silenciosa: Si es NULL o vacío, req() detiene la ejecución
    # de este bloque sin lanzar errores en la consola.
    req(path_ready)
    
    # 3. Lógica de construcción
    str_file_name <- "zzz_output_report99_Rscience_Report_00.html"
    str_file_path <- file.path(path_ready, str_file_name)
    
    output_list <- list(
      str_file_path = str_file_path
    )
    
    # Debugging informativo
    message("--- GENERANDO APP_STATE_ANALYSIS ---")
    print(output_list)
    
    return(output_list)    
  })
  
  output$"analysis_view_html" <- renderText({
    
    # Initial message (user just entered)
    initial_message <- HTML('
      <div style="
        display: flex;
        flex-direction: column;
        align-items: center;
        justify-content: center;
        height: 70vh;
        text-align: center;
        padding: 40px;
        background: linear-gradient(135deg, #f0f8ff 0%, #e6f2ff 100%);
        border-radius: 12px;
        margin: 20px;
        border: 2px dashed #4dabf7;
        box-shadow: 0 4px 12px rgba(77, 171, 247, 0.1);
      ">
        <div style="margin-bottom: 30px; position: relative;">
          <i class="fas fa-chart-line fa-4x" style="color: #339af0;"></i>
          <!-- Pulsing blue dot -->
          <div style="
            position: absolute;
            top: 0;
            right: -10px;
            width: 20px;
            height: 20px;
            background-color: #339af0;
            border-radius: 50%;
            animation: pulse 2s infinite;
            box-shadow: 0 0 10px #339af0;
          "></div>
        </div>
        <h3 style="color: #1971c2; margin-bottom: 15px; font-weight: 600;">Analysis</h3>
        <p style="color: #4263eb; max-width: 500px; margin-bottom: 25px; font-size: 16px;">
          To begin, please load your dataset and configure the analysis parameters.
        </p>
        <div style="
          background: rgba(66, 99, 235, 0.1);
          padding: 18px;
          border-radius: 10px;
          max-width: 400px;
          text-align: left;
          margin-top: 20px;
          border-left: 4px solid #339af0;
        ">
          <p style="margin: 8px 0; color: #1971c2;"><i class="fas fa-database" style="color: #339af0; margin-right: 10px;"></i> Load your dataset</p>
          <p style="margin: 8px 0; color: #1971c2;"><i class="fas fa-sliders-h" style="color: #339af0; margin-right: 10px;"></i> Configure parameters</p>
          <p style="margin: 8px 0; color: #1971c2;"><i class="fas fa-play-circle" style="color: #339af0; margin-right: 10px;"></i> Run the analysis</p>
        </div>
        <!-- Floating dots -->
        <div style="position: absolute; bottom: 20px; left: 50px; animation: float 3s infinite ease-in-out;">
          <div style="width: 8px; height: 8px; background-color: #4dabf7; border-radius: 50%; opacity: 0.7;"></div>
        </div>
        <div style="position: absolute; top: 50px; right: 50px; animation: float 4s infinite ease-in-out reverse;">
          <div style="width: 6px; height: 6px; background-color: #74c0fc; border-radius: 50%; opacity: 0.5;"></div>
        </div>
      </div>
      <style>
        @keyframes pulse {
          0% { transform: scale(0.8); opacity: 0.7; }
          50% { transform: scale(1.1); opacity: 1; }
          100% { transform: scale(0.8); opacity: 0.7; }
        }
        @keyframes float {
          0%, 100% { transform: translateY(0); }
          50% { transform: translateY(-10px); }
        }
      </style>
    ')
    
    # Loading message (when processing)
    loading_message <- HTML('
      <div style="
        display: flex;
        flex-direction: column;
        align-items: center;
        justify-content: center;
        height: 70vh;
        text-align: center;
        padding: 40px;
        background: linear-gradient(135deg, #e3f2fd 0%, #bbdefb 100%);
        border-radius: 12px;
        margin: 20px;
        border: 2px solid #1c7ed6;
        box-shadow: 0 4px 15px rgba(28, 126, 214, 0.2);
      ">
        <!-- Lighthouse animation -->
        <div style="margin-bottom: 40px; position: relative; width: 80px; height: 80px;">
          <div style="
            position: absolute;
            bottom: 0;
            left: 50%;
            transform: translateX(-50%);
            width: 20px;
            height: 40px;
            background: linear-gradient(to bottom, #495057 0%, #212529 100%);
            border-radius: 4px;
          "></div>
          <div style="
            position: absolute;
            bottom: 40px;
            left: 50%;
            transform: translateX(-50%);
            width: 40px;
            height: 30px;
            background: linear-gradient(135deg, #ffd43b 0%, #fcc419 100%);
            border-radius: 20px 20px 0 0;
          "></div>
          <!-- Light beam -->
          <div style="
            position: absolute;
            bottom: 60px;
            left: 50%;
            width: 2px;
            height: 60px;
            background: linear-gradient(to top, rgba(255, 212, 59, 0.8), transparent);
            transform-origin: bottom center;
            animation: beam 4s infinite linear;
            filter: blur(1px);
          "></div>
        </div>
        
        <!-- Blinking dots -->
        <div style="display: flex; justify-content: center; margin-bottom: 30px; gap: 15px;">
          <div style="
            width: 12px;
            height: 12px;
            background-color: #339af0;
            border-radius: 50%;
            animation: blink 1.4s infinite;
            animation-delay: 0s;
          "></div>
          <div style="
            width: 12px;
            height: 12px;
            background-color: #4263eb;
            border-radius: 50%;
            animation: blink 1.4s infinite;
            animation-delay: 0.2s;
          "></div>
          <div style="
            width: 12px;
            height: 12px;
            background-color: #4dabf7;
            border-radius: 50%;
            animation: blink 1.4s infinite;
            animation-delay: 0.4s;
          "></div>
        </div>
        
        <h3 style="color: #1c7ed6; margin-bottom: 15px; font-weight: 600;">Generating Analysis...</h3>
        <p style="color: #1971c2; max-width: 500px; margin-bottom: 30px; font-size: 16px;">
          Your report is being generated. This may take a few moments.
          Please wait while we process your data.
        </p>
        
        <!-- Animated progress bar -->
        <div style="
          width: 300px;
          height: 8px;
          background-color: rgba(28, 126, 214, 0.2);
          border-radius: 4px;
          overflow: hidden;
          margin-top: 20px;
        ">
          <div style="
            width: 70%;
            height: 100%;
            background: linear-gradient(90deg, #339af0, #4dabf7);
            border-radius: 4px;
            animation: loading 2s infinite ease-in-out, shimmer 3s infinite;
          "></div>
        </div>
        
        <!-- Floating bubbles -->
        <div style="position: absolute; bottom: 30px; left: 30px; animation: bubble 5s infinite ease-in-out;">
          <div style="width: 15px; height: 15px; background-color: rgba(77, 171, 247, 0.4); border-radius: 50%;"></div>
        </div>
        <div style="position: absolute; top: 30px; right: 30px; animation: bubble 6s infinite ease-in-out reverse;">
          <div style="width: 12px; height: 12px; background-color: rgba(66, 99, 235, 0.3); border-radius: 50%;"></div>
        </div>
      </div>
      <style>
        @keyframes beam {
          0% { transform: translateX(-50%) rotate(0deg); }
          25% { transform: translateX(-50%) rotate(90deg); }
          50% { transform: translateX(-50%) rotate(180deg); }
          75% { transform: translateX(-50%) rotate(270deg); }
          100% { transform: translateX(-50%) rotate(360deg); }
        }
        @keyframes blink {
          0%, 100% { opacity: 0.2; transform: scale(0.8); }
          50% { opacity: 1; transform: scale(1.1); }
        }
        @keyframes loading {
          0% { transform: translateX(-100%); }
          100% { transform: translateX(400%); }
        }
        @keyframes shimmer {
          0% { background-position: -200px 0; }
          100% { background-position: 200px 0; }
        }
        @keyframes bubble {
          0%, 100% { transform: translateY(0) scale(1); opacity: 0.3; }
          50% { transform: translateY(-20px) scale(1.2); opacity: 0.7; }
        }
      </style>
    ')
    
    # 1. Check if reactive object exists
    if (is.null(app_state_render)) {
      return(initial_message)  # User just entered
    }
    
    # 2. Check if str_temp_work_folder_path property exists
    if (is.null(app_state_render$str_temp_work_folder_path)) {
      return(initial_message)  # User just entered
    }
    
    str_temp_work_folder_path <- app_state_render$str_temp_work_folder_path
    str_temp_output_folder_path <- app_state_render$str_temp_output_folder_path
    
    # 3. Check if paths exist
    if (is.null(str_temp_output_folder_path) || str_temp_output_folder_path == "") {
      return(initial_message)  # User hasn\'t started analysis yet
    }
    
    str_file_name <- "zzz_output_report99_Rscience_Report_00.html"
    str_file_path <- file.path(str_temp_output_folder_path, str_file_name)
    
    html_path <- str_file_path
    
    # 4. If HTML file doesn\'t exist, show loading message
    if (!file.exists(html_path)) {
      return(loading_message)  # Analysis in progress
    }
    
    # 5. If file exists, show it
    html_dir <- dirname(html_path)
    html_filename <- basename(html_path)
    
    # Define and register resource
    resource_id <- digest::digest(html_dir, algo = "md5")
    shiny::addResourcePath(resource_id, html_dir)
    
    # Build URL with unique resource ID
    html_url <- paste0("/", file.path(resource_id, html_filename))
    
    # Create iframe
    armado_v <- paste('<div style="height: 100%; width: 100%; "><iframe style="height: 100%; width:100%; border: none;" src="', html_url, '"></iframe></div>', sep = "")
    
    return(armado_v)
  })
  
  output$"main_analysis" <- renderUI({
    bslib::card(
      id = "output-main-card",
      
      # [CAMBIO] Usamos bslib::card_header() para forzar el título.
      bslib::card_header(
        style = "height: 60px; overflow: hidden;",
        fluidRow(
          column(3, tags$h4("Output - ShowRoom")),
          column(7)#,
          # column(2, uiOutput("botonera_html"))
        )
      ),
      
      card_body(
        class = "p-0",
        tags$div(
          # style = "flex-grow: 1; overflow-y: auto;",
          style = "flex-grow: 1; overflow-y: auto; height: 80vh; width: 100%; overflow: hidden;", # Asegurar que el contenedor tenga altura suficiente
          
          # Contenido que deseas mostrar dentro de la tarjeta
          htmlOutput("analysis_view_html")
        )
      )
    )
    
    
    
    
    
  })
  
  # Server 06 - Download -------------------------------------------------------
  module_download_multi_server(
    id = "module_download_multi", 
    app_state_render = app_state_render# <-- ¡OBJETO REACTIVO, NO UNA LISTA!
  )
  
  
  
}

shinyApp(ui, server)