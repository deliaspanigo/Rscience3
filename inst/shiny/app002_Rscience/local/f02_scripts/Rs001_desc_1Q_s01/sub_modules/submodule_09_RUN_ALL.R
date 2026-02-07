# =============================================================================
# MODULE: Bulk Pipeline Runner (Dynamic UI)
# =============================================================================

submodule_09_RUN_ALL_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    # Contenedor donde se volcarán los N módulos hijos
    shiny::uiOutput(ns("dynamic_pipelines_ui"))
  )
}

submodule_09_RUN_ALL_server <- function(id,
                                        internal_ORH_02_temporal_FF,
                                        vector_target_actions, # Debe ser un reactive que retorne c("act1", "act2")
                                        debug_toggle = shiny::reactive({FALSE}),
                                        show_viewer = TRUE) {

  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # 1. Registro interno para no duplicar servidores
    # Guardamos los IDs de los módulos que ya han sido lanzados
    instantiated_modules <- shiny::reactiveVal(c())

    # --- 2. RENDERIZADO DE UI DINÁMICA ---
    output$dynamic_pipelines_ui <- shiny::renderUI({
      actions <- vector_target_actions()
      shiny::req(length(actions) > 0)

      # Creamos la interfaz para cada elemento del vector
      pipeline_uis <- lapply(seq_along(actions), function(i) {
        # ID único basado en la posición
        child_id <- paste0("pipe_", i)

        shiny::div(
          style = "margin-bottom: 20px; padding: 15px; border: 1px solid #ddd; border-radius: 8px; background-color: #f9f9f9;",
          shiny::tags$h4(paste("Pipeline:", actions[i]), style = "margin-top: 0; color: #2c3e50;"),

          # Llamada a la UI del módulo hijo (08)
          submodule_08_RUN_ui(ns(child_id))
        )
      })

      shiny::tagList(pipeline_uis)
    })

    # --- 3. INSTANCIACIÓN DE SERVIDORES (Lógica Central) ---
    shiny::observe({
      actions <- vector_target_actions()
      shiny::req(length(actions) > 0)

      current_list <- instantiated_modules()

      for (i in seq_along(actions)) {
        child_id <- paste0("pipe_", i)

        # Si este ID de módulo no ha sido creado todavía, lo creamos
        if (!(child_id %in% current_list)) {

          # 'local' es vital para que cada módulo capture su propio índice 'i'
          local({
            idx <- i

            # Creamos un reactive que entrega SOLO la acción que le toca a este hijo
            this_action_reactive <- shiny::reactive({
              current_v <- vector_target_actions()
              if (length(current_v) >= idx) {
                return(current_v[idx])
              } else {
                return(NULL)
              }
            })

            # Llamamos al servidor del módulo hijo (08)
            submodule_08_RUN_server(
              id = child_id,
              internal_ORH_02_temporal_FF = internal_ORH_02_temporal_FF,
              target_actions = this_action_reactive, # El hijo recibe su acción individual
              debug_toggle = debug_toggle,
              show_viewer = show_viewer
            )
          })

          # Actualizamos el registro de módulos creados
          current_list <- c(current_list, child_id)
          instantiated_modules(current_list)
        }
      }
    })
  })
}
