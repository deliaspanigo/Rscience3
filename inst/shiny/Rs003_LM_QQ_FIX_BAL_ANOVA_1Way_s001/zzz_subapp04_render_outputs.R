# Main Application
library(shiny)
library(bslib)
library(shinyjs)
library(promises) # <--- AÑADIR
library(future)   # <--- AÑADIR

# Source the modules (if they're in separate files)
source("R_modules/modules04_render_outputs/module_render_outputs.R")
source("R_modules/modules04_render_outputs/module_silenced_button.R")
source("R_modules/modules04_render_outputs/module_download_one.R")
source("R_modules/modules04_render_outputs/module_download_multi.R")

# UI
ui <- page_sidebar(

  title = "Render Module",
  # includeScript("www/scroll_handler.js"),
  sidebar = sidebar(
    useShinyjs(),
    "SubModule App",
    # El botón que depende del reactiveValues
    module_silenced_button_ui(id = "button_control")
  ),
  uiOutput("general_main")


)

# Server
server <- function(input, output, session) {

  # [NUEVO] Configuración para ejecutar tareas pesadas fuera del hilo principal
  # plan(sequential) <---- ESTE NOOOOOOOO!!!!!!!!!!
  plan(multisession) # Puedes usar plan(multicore) en Linux/macOS, pero multisession es más compatible.

  output$"general_main" <- renderUI({


    bslib::navset_card_tab(
      # Puedes mantener un header para toda la tarjeta si quieres, o omitirlo
      title = tags$h4("Output - Download"),

      bslib::nav_panel(
        title = "proc",
        module_render_outputs_ui(id = "module_render")
      ),
      bslib::nav_panel(
        title = "download",
        module_download_multi_ui(id = "module_download_multi")
      )
    )

  })


  # Server 01 - Button ---------------------------------------------------------
  # ReactiveValues que controla TODO el estado
  app_state <-  module_silenced_button_server(id = "button_control")

  # Currier 01
  active_run <- reactiveValues(is_running = FALSE,
                               is_done = FALSE)

  # Aduana 01 - Checking permit for run ----------------------------------------
  observeEvent(app_state$"is_running", {
    req(app_state$"is_running")
    active_run$"is_running" <- TRUE
  })

  #######-----------------------------------------------------------------------
  #######-----------------------------------------------------------------------
  #######-----------------------------------------------------------------------



  # Server 02 - Rendering files ------------------------------------------------
  app_state_render <-  module_render_outputs_server(
    id = "module_render",
    app_state = active_run# <-- ¡OBJETO REACTIVO, NO UNA LISTA!
  )

  # Currier 02
  the_currier02 <- reactiveValues(is_running = FALSE,
                                     is_done = FALSE)


  # Aduana 01 - Checking permit for run ----------------------------------------
  observeEvent(app_state_render$"is_running", {
    req(app_state_render$"is_running")
    the_currier02$"is_running" <- TRUE
  })

  #######-----------------------------------------------------------------------
  #######-----------------------------------------------------------------------
  #######-----------------------------------------------------------------------

  module_download_multi_server(
    id = "module_download_multi",
    app_state_render = app_state_render# <-- ¡OBJETO REACTIVO, NO UNA LISTA!
  )



}

# Run the application
shinyApp(ui = ui, server = server)

