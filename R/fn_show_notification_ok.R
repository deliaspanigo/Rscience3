
#' @export
fn_show_notification_ok <- function(the_message) {
  showNotification(
    ui = tags$div(
      style = "background-color: #d1e7dd; color: #0f5132; font-size: 15px; font-weight: bold; padding: 10px; border-radius: 4px; border-left: 5px solid #0f5132; display: flex; align-items: center;",
      tags$i(class = "fa fa-check-circle", style = "font-size: 20px; margin-right: 5px;"),
      the_message
    ),
    duration = 2,
    closeButton = TRUE
  )
}