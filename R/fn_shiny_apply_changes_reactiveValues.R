#' @export
fn_shiny_apply_changes_reactiveValues <- function(rv, changes_list) {
  # Temporarily disable reactions
  isolate({
    # Apply all changes within the isolated environment
    for (name in names(changes_list)) {
      rv[[name]] <- changes_list[[name]]
    }
  })
  # When exiting isolate, a single reaction will be triggered
}
