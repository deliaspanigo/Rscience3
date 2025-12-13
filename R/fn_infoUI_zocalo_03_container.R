#' Función auxiliar para generar el contenedor UI del zócalo de metadatos.
#'
#' @param data_obj Objeto de datos con la información a mostrar
#' @param width Ancho del contenedor (ej: "100%", "500px", "50vw")
#' @param height Alto del contenedor (ej: "100%", "300px", "50vh")
#' @return Un objeto de tagList (código HTML/Shiny UI).
fn_infoUI_zocalo_03_container <- function(data_obj, width = "100%", height = "100%") {

  # Tamaños de letra separados
  font_size_title = "20px"    # Tamaño para el título
  font_size_text = "20px"     # Tamaño para el texto
  font_family = "Arial, sans-serif"

  list_safe <- tryCatch(data_obj, error = function(e) NULL)
  if (is.null(list_safe)) return("Sin datos.")

  info_status <- list_safe$"info_status"
  info_check_go_forward <- list_safe$"info_check_go_forward"
  info_color <- list_safe$"info_color"
  shiny_obj_name <- list_safe$"shiny_obj_name"

  # checking...
  check_ok <- (sum(!is.na(unlist(list_safe))) == 0) && info_check_go_forward

  # Colors
  the_selected_color <- info_color
  gen_color_rgba <- function(hex_color, opacity = 1) {
    rgb_col <- col2rgb(hex_color)
    return(sprintf("rgba(%d, %d, %d, %s)",
                   rgb_col[1], rgb_col[2], rgb_col[3], opacity))
  }
  the_bg_color  <- gen_color_rgba(hex_color = the_selected_color, opacity = 0.05)

  # Estilos base con tipografía
  base_style_text <- paste0("font-size: ", font_size_text, "; font-family: ", font_family, ";")
  base_style_title <- paste0("font-size: ", font_size_title, "; font-family: ", font_family, "; font-weight: bold;")

  str_style_border <- paste0("background-color: ", the_bg_color, ";
                             border-left: 4px solid ", the_selected_color, ";
                             ", base_style_text)

  str_style_btn <- paste0("padding-left: 10px; color: ", the_selected_color, ";")

  str_style_title <- paste0("color: ", the_selected_color, "; ", base_style_title)

  div(
    class = "p-3 rounded shadow-sm",
    style = paste0(str_style_border, "; width: ", width, "; height: ", height, ";"),

    div(
      style = "height: 100%; display: flex; flex-direction: column;",

      h5(style = str_style_title,
         tagList(icon(name = "sliders",
                      style = str_style_btn,
                      class = "me-2")),
         "Special Settings"),

      # Contenedor para el plotly con altura flexible
      div(
        style = "flex: 1; min-height: 0; border: 1px solid #ffc107; padding: 5px;",
        plotly::plotlyOutput(shiny_obj_name, width = "100%", height = "100%")
      )
    )
  )
}
