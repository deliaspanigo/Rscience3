module_download_multi_ui <- function(id) {
  # Create a namespace using the 'id' to ensure element IDs are unique
  ns <- NS(id)
  
  uiOutput(ns("download_main"))
}

module_download_multi_server <- function(id, app_state_render) {
  # Note: The 'shinyjs' and 'digest' packages are required for this module.
  
  moduleServer(id, function(input, output, session) {
    
    # Get the namespace function for use inside the server
    ns <- session$ns
    
    
    # ReactiveValues con estructura de archivos
    TOTEM_file_path <- reactiveValues(
      files = list(
        file01 = list(
          id = "file01",
          file_name = "zzz_output_report01_RQuarto_00.html",
          show_label = "File 01 - RQuarto - Code and super comments.",
          file_path = NULL,
          check = NULL,
          output_file_name = NULL
        ),
        file02 = list(
          id = "file02",
          file_name = "zzz_output_report01_RQuarto_00_Env.RData",
          show_label = "File 02 - R objects from analysis.",
          file_path = NULL,
          check = NULL,
          output_file_name = NULL
        ),
        file03 = list(
          id = "file03",
          file_name = "zzz_output_report02_R_png_folder.zip",
          show_label = "File 03 - All plots as png files ziped.",
          file_path = NULL,
          check = NULL,
          output_file_name = NULL
        ),
        file04 = list(
          id = "file04",
          file_name = "zzz_output_report02_R_script.R",
          show_label = "File 04 - R script.",
          file_path = NULL,
          check = NULL,
          output_file_name = NULL
        ),
        file05 = list(
          id = "file05",
          file_name = "zzz_output_report03_pdf_00.pdf",
          show_label = "File 05 - PDF file report.",
          file_path = NULL,
          check = NULL,
          output_file_name = NULL
        ),
        file06 = list(
          id = "file06",
          file_name = "zzz_output_report04_docx_00.docx",
          show_label = "File 06 - Docx file report.",
          file_path = NULL,
          check = NULL,
          output_file_name = NULL
        ),
        file07 = list(
          id = "file07",
          file_name = "zzz_output_report05_xlsx_00.xlsx",
          show_label = "File 07 - xlsx file report.",
          file_path = NULL,
          check = NULL,
          output_file_name = NULL
        ),
        file08 = list(
          id = "file08",
          file_name = "zzz_output_report06_revealjs_00.html",
          show_label = "File 08 - Presentation report.",
          file_path = NULL,
          check = NULL,
          output_file_name = NULL
        ),
        file09 = list(
          id = "file09",
          file_name = "zzz_output_report99_Rscience_Report_00.html",
          show_label = "File 09 - HTML Rscience Full Report",
          file_path = NULL,
          check = NULL,
          output_file_name = NULL
        )
      )
    )
    
    # Observer que verifica TODOS los archivos
    observe({
      # 1. Obtener paths de app_state_render
      req(app_state_render)  # Asegurar que existe
      req(app_state_render$"is_running")
      
      str_temp_work_folder_path <- app_state_render$str_temp_work_folder_path
      str_temp_output_folder_path <- app_state_render$str_temp_output_folder_path
      
      # 2. Si no tenemos el output path, salir
      req(!is.null(str_temp_output_folder_path))
      req(dir.exists(str_temp_output_folder_path))
      
      cat("Verificando archivos en:", str_temp_output_folder_path, "\n")
      
      # 3. Usar lapply para procesar TODOS los archivos
      lapply(names(TOTEM_file_path$files), function(file_id) {
        
        # Obtener info del archivo actual
        file_info <- TOTEM_file_path$files[[file_id]]
        the_file_name <- file_info$file_name
        
        # Construir path completo
        the_file_path <- file.path(str_temp_output_folder_path, the_file_name)
        
        # Verificar existencia
        the_check <- file.exists(the_file_path)
        
        # Actualizar reactiveValues
        TOTEM_file_path$files[[file_id]]$file_path <- the_file_path
        TOTEM_file_path$files[[file_id]]$check <- the_check
        TOTEM_file_path$files[[file_id]]$output_file_name <- the_file_name
        
        # Debug (opcional)
        if (the_check) {
          cat(paste0("✓ ", file_id, ": ", the_file_name, "\n"))
        } else {
          cat(paste0("✗ ", file_id, ": ", the_file_name, "\n"))
        }
      })
      
      # 4. Resumen
      total_files <- length(TOTEM_file_path$files)
      existing_files <- sum(sapply(TOTEM_file_path$files, function(x) x$check))
      cat(sprintf("Archivos existentes: %d/%d\n", existing_files, total_files))
    })
    
    
    # Inicializar TODOS los módulos de descarga
    # ✅ CORREGIDO:
    observe({
      # Solo cuando TOTEM_file_path esté disponible
      req(TOTEM_file_path)
      
      # Inicializar TODOS los módulos de descarga
      lapply(names(TOTEM_file_path$files), function(file_key) {
        file_info <- TOTEM_file_path$files[[file_key]]
        
        local({
          current_key <- file_key
          
          file_path <- TOTEM_file_path$files[[current_key]]$file_path
          
          
          module_download_one_server(
            id = file_info$id,
            r_file_path = reactive(file_path)
          )
        })
      })
    })
    
    # Todso los UI    
    output$all_download_modules_ui <- renderUI({
      
      # Verificar que tenemos la estructura
      req(TOTEM_file_path)
      req(length(TOTEM_file_path$files) > 0)
      
      # Crear lista de UIs de módulos
      module_uis <- lapply(names(TOTEM_file_path$files), function(file_key) {
        
        file_info <- TOTEM_file_path$files[[file_key]]
        
        # SOLO el UI del módulo
        module_download_one_ui(
          id = ns(file_info$id),
          title = file_info$show_label
        )
      })
      
      # Retornar todos los UIs
      tagList(module_uis)
    })
    
    # Final output
    output$"download_main" <- renderUI({
      
      
      bslib::navset_card_tab(
        # Puedes mantener un header para toda la tarjeta si quieres, o omitirlo
        title = tags$h4("Output - Download"),
        height = "87vh", 
        
        bslib::nav_panel(
          title = "nueva_descarga",
          # uiOutput("special01"),
          uiOutput(ns("all_download_modules_ui"))
          
          
        ),
        bslib::nav_panel(
          title = "folder_files",
          "Despues aca el path y los files."
        )
      )
      
    })
    
    
    
    
  })
}
