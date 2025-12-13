#' @export
fn_infoUI_zocalo_02_VarSelection <- function(data_obj) {
  req(data_obj)

  # Tamaños de letra separados
  font_size_title = "20px"    # Tamaño para el título
  font_size_text = "20px"     # Tamaño para el texto
  font_family = "Arial, sans-serif"

  list_safe <- tryCatch(data_obj, error = function(e) NULL)
  if (is.null(list_safe)) return("Sin datos.")

  var_name_factor <- list_safe$"var_name_factor"
  var_name_rv <- list_safe$"var_name_rv"
  str_shape <- list_safe$"str_shape"
  alpha_value <- list_safe$"alpha_value"

  # Colors
  the_color_blue  <- "#0d6efd"
  the_color_green <- "#198754"
  the_selected_color <- ifelse(test = is.na(var_name_factor), yes = the_color_blue, no = the_color_green)

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
  str_style_text <- paste0("color: ", the_selected_color, "; ", base_style_text)

  # Estilo para los textos monospace
  str_style_monospace <- paste0("font-family: 'Courier New', monospace;
                                font-size: ", font_size_text, ";")

  div(
    class = "p-3 rounded shadow-sm",
    style = str_style_border,
    # style = paste0("background: linear-gradient(to right, #f8f9fa, #ffffff); ", base_style_text),

    div(
      # class = "mb-3 p-2 rounded",
      # style = str_style_border,

      h5(style = str_style_title,
         tagList(icon(name = "filter",
                      style = str_style_btn,
                      class = "me-2")),
         "Variable Selection - minidataset"),

      div(
        class = "d-flex flex-wrap gap-3",  # ← Claves flexbox
        div(class = "me-4 mb-2",
            tags$b(style = paste0("padding-left: 10px; ", base_style_text), "Factor: "),
            span(var_name_factor, style = str_style_monospace)),

        div(class = "me-4 mb-2",
            tags$b(style = paste0("padding-left: 10px; ", base_style_text), "Response Variable: "),
            span(var_name_rv, style = str_style_monospace)),

        div(class = "me-4 mb-2",
            tags$b(style = paste0("padding-left: 10px; ", base_style_text), "Shape: "),
            span(str_shape, style = str_style_monospace)),

        div(class = "me-4 mb-2",
            tags$b(style = paste0("padding-left: 10px; ", base_style_text), "Alpha value: "),
            span(alpha_value, style = str_style_monospace))
      )
    )
  )
}
