#' @export
fn_infoUI_zocalo_01_dataset <- function(data_obj) {
  req(data_obj)

  # Tamaños de letra separados
  font_size_title = "20px"    # Tamaño para el título
  font_size_text = "20px"     # Tamaño para el texto
  font_family = "Arial, sans-serif"

  list_safe <- tryCatch(data_obj, error = function(e) NULL)
  if (is.null(list_safe)) return("Sin datos.")

  # Basics
  the_source <- list_safe$"source"
  the_file <- list_safe$"file"
  str_shape <- list_safe$"str_shape"
  info_status <- list_safe$"info_status"
  info_check_go_forward <- list_safe$"info_check_go_forward"
  info_color <- list_safe$"info_color"

  # Status
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
                             ", base_style_text)  # Usa tamaño de texto para el contenedor

  str_style_btn <- paste0("padding-left: 10px; color: ", the_selected_color, ";")

  str_style_title <- paste0("color: ", the_selected_color, "; ", base_style_title)  # Título con su tamaño
  str_style_text <- paste0("color: ", the_selected_color, "; ", base_style_text)    # Texto con su tamaño

  # Estilo para los textos monospace
  str_style_monospace <- paste0("font-family: 'Courier New', monospace;
                                font-size: ", font_size_text, ";")  # Usa tamaño de texto

  div(
    class = "p-3 rounded shadow-sm",
    style = str_style_border,
    # style = paste0("background: linear-gradient(to right, #f8f9fa, #ffffff); ", base_style_text),  # Tamaño texto para el contenedor principal

    div(
      # class = "mb-3 p-2 rounded",
      # style = str_style_border,

      h5(style = str_style_title,  # ← Título con tamaño grande
         tagList(icon(name = "database",
                      style = str_style_btn,
                      class = "me-2")),
         "User file - dataset"),

      div(
        class = "d-flex flex-wrap gap-3",  # ← Claves flexbox
        div(class = "me-4 mb-2",
            tags$b(style = paste0("padding-left: 10px; ", base_style_text), "Source: "),  # Texto normal
            span(the_source, style = str_style_monospace)),

        div(class = "me-4 mb-2",
            tags$b(style = paste0("padding-left: 10px; ", base_style_text), "File: "),    # Texto normal
            span(the_file, style = str_style_monospace)),

        div(class = "me-4 mb-2",
            tags$b(style = paste0("padding-left: 10px; ", base_style_text), "Shape: "),   # Texto normal
            span(str_shape, style = str_style_monospace))
      )
    )
  )
}
