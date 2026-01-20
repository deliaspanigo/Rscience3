module_buttons_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    useShinyjs(),
    uiOutput(ns("dynamic_buttons"))
  )
}

module_buttons_server <- function(id, buttons_list_reactive, initial_active_id = NULL) {
  moduleServer(id, function(input, output, session) {
    
    ns <- session$ns
    
    # Reactive value for active button
    active_button <- reactiveVal(NULL)
    
    # 🌟 Modified observer to initialize active button 🌟
    observe({
      btns <- buttons_list_reactive()
      
      # 1. Check if there's already an active button or no buttons
      if (length(btns) == 0 || !is.null(active_button())) {
        return()
      }
      
      btn_ids <- sapply(btns, function(x) x$id)
      
      # 2. Try to use the provided initial ID
      if (!is.null(initial_active_id) && initial_active_id %in% btn_ids) {
        active_button(initial_active_id)
        
        # 3. If no valid initial ID, select the first button
      } else {
        active_button(btns[[1]]$id)
      }
    })
    
    # Generate buttons dynamically (regenerates when buttons_list changes)
    output$dynamic_buttons <- renderUI({
      btns <- buttons_list_reactive()
      
      if (length(btns) == 0) {
        return(tags$p("No buttons available", style = "color: gray; padding: 10px;"))
      }
      
      # Determine which button is currently active
      current_active <- active_button()
      
      tagList(
        lapply(btns, function(btn) {
          is_active <- btn$id == current_active
          
          actionButton(
            inputId = ns(btn$id),
            label = tagList(
              if (!is.null(btn$icon)) btn$icon,
              btn$label
            ),
            class = paste("btn-block", ifelse(is_active, "btn-success", "btn-primary")),
            width = "100%",
            style = "margin-bottom: 5px; text-align: left;",
            disabled = isFALSE(btn$enabled)  # If 'enabled' field exists
          )
        })
      )
    })
    
    # Function to update button classes (Keeping JS logic)
    update_button_classes <- function(active_btn_id) {
      btns <- buttons_list_reactive()
      
      if (length(btns) == 0) return()
      
      js_code <- ""
      for (btn in btns) {
        full_id <- paste0("#", ns(btn$id))
        
        if (btn$id == active_btn_id) {
          # Active button -> green
          js_code <- paste0(js_code, 
                            "$('#", full_id, "').removeClass('btn-primary').addClass('btn-success');")
        } else {
          # Inactive button -> blue
          js_code <- paste0(js_code, 
                            "$('#", full_id, "').removeClass('btn-success').addClass('btn-primary');")
        }
      }
      
      if (nzchar(js_code)) {
        shinyjs::runjs(js_code)
      }
    }
    
    # Observers for each button - recreated when list changes
    observe({
      btns <- buttons_list_reactive()
      
      # Create new observers for each button
      lapply(btns, function(btn) {
        btn_id <- btn$id
        
        observeEvent(input[[btn_id]], {
          # Only process if the button is enabled
          if (is.null(btn$enabled) || isTRUE(btn$enabled)) {
            active_button(btn_id)
            # No longer necessary to call update_button_classes here, 
            # the observer below handles it when active_button() changes
          }
        }, ignoreInit = TRUE)
      })
    })
    
    # Observer to update classes when active button changes
    # 🚀 This replaces the call to update_button_classes within observeEvent
    observe({
      current_active <- active_button()
      # We need a conditional to ensure the button exists 
      # before attempting to update classes.
      if (!is.null(current_active)) {
        # Needs a dependency on 'output$dynamic_buttons' or a delay 
        # to ensure UI buttons have been rendered before 
        # attempting to manipulate their classes with JS.
        # In practice, with the renderUI structure, it's generally safe 
        # here, but if it fails, one could use a 'req(buttons_list_reactive())' 
        # and a slight delay with 'invalidateLater(100)'.
        update_button_classes(current_active)
      }
    })
    
    # Observer for when button list changes (maintains re-selection logic)
    observeEvent(buttons_list_reactive(), {
      btns <- buttons_list_reactive()
      
      # If current active button is no longer in the list, select the first one
      if (!is.null(active_button()) && 
          !active_button() %in% sapply(btns, function(x) x$id)) {
        if (length(btns) > 0) {
          active_button(btns[[1]]$id)
        } else {
          active_button(NULL)
        }
      }
      
      # If active button is in the list, ensure its classes are correct
      # (especially if only the 'enabled' state changed).
      if (!is.null(active_button())) {
        update_button_classes(active_button())
      }
    })
    
    # Return reactive values for external use (no necessary changes here)
    return(
      list(
        # Active button ID
        active_button = reactive(active_button()),
        
        # Complete info about active button
        active_button_info = reactive({
          btns <- buttons_list_reactive()
          active_id <- active_button()
          
          if (!is.null(active_id) && length(btns) > 0) {
            # Use match instead of which with sapply to be more robust
            match_index <- match(active_id, sapply(btns, function(x) x$id))
            if (!is.na(match_index)) {
              btns[[match_index]]
            } else {
              NULL
            }
          } else {
            NULL
          }
        }),
        
        # Statistics for all buttons
        button_stats = reactive({
          btns <- buttons_list_reactive()
          stats <- list()
          
          for (btn in btns) {
            stats[[btn$id]] <- list(
              label = btn$label,
              clicks = input[[btn$id]] %||% 0,
              is_active = active_button() == btn$id,
              enabled = btn$enabled %||% TRUE
            )
          }
          return(stats)
        }),
        
        # Current buttons list
        buttons_info = reactive({
          btns <- buttons_list_reactive()
          lapply(btns, function(btn) {
            list(
              id = btn$id,
              label = btn$label,
              icon = btn$icon,
              enabled = btn$enabled %||% TRUE,
              is_active = active_button() == btn$id
            )
          })
        }),
        
        # Function to programmatically set active button
        set_active = function(button_id) {
          btns <- buttons_list_reactive()
          if (button_id %in% sapply(btns, function(x) x$id)) {
            active_button(button_id)
          }
        }
      )
    )
  })
}