#' Data Schema Constructor
#' @description Creates the skeleton with fixed types to avoid logical NAs
#' where timestamps or characters are expected.
#' @return A nested list with predefined categories.
#' @export
fn3_IMPORT_new_import_struct <- function() {
  list(
    "is_done" = FALSE,
    "orquestator_import" = list(
      "name_internal" = NA_character_,
      "name_external" = NA_character_,
      "module"        = NA_character_,
      "info"          = "This info is inside orquestator for import dataset."
    ),
    "importer" = list(
      "name_internal" = NA_character_,
      "name_external" = NA_character_,
      "module"        = NA_character_,
      "info"          = "This info is inside each import module."
    ),
    "dataset" = list(
      "is_done"            = FALSE,
      "is_data_frame"      = FALSE,
      "my_dataset"         = NULL,
      "rows"               = 0L,
      "cols"               = 0L,
      "file_name"          = NA_character_,
      "label_file_name"    = NA_character_,
      "sheet_name"         = NA_character_,
      "sheet_pos"          = 0L,
      "file_path_external" = NA_character_,
      "file_path_internal" = NA_character_,
      "init_time"          = as.POSIXct(NA),
      "end_time"           = as.POSIXct(NA),
      "time_secs"          = NA_real_
    ),
    "extra" = list(
      "description" = NA,
      "extra_list"  = list()
    )
  )
}

#' Strict Hierarchical Data Setter
#' @description Updates a single field in a specific category with dynamic validation.
#' @param current_list The list to be updated.
#' @param category The target category (dynamically validated against constructor).
#' @param field The specific field name to update.
#' @param value The new value to assign.
#' @return The updated list.
#' @export
#' Strict Hierarchical Data Setter
#' @description Updates a field in a specific category or at the root level.
#' @param current_list The list to be updated.
#' @param category The target category. Use NULL or "" to update root fields (like 'is_done').
#' @param field The specific field name to update.
#' @param value The new value to assign.
#' @export
fn3_IMPORT_set_import_data <- function(current_list, category, field, value) {

  # 0. Safety Check
  if (is.null(current_list)) {
    stop("STRICT ERROR: 'current_list' is NULL. Initialize it first.")
  }

  # --- CASO A: Actualización en la RAÍZ (ej. is_done) ---
  if (is.null(category) || category == "") {
    valid_root_fields <- names(current_list)

    if (!(field %in% valid_root_fields)) {
      stop(sprintf("STRICT ERROR: Field '%s' does not exist at root level.", field))
    }

    current_list[[field]] <- value
    return(current_list)
  }

  # --- CASO B: Actualización en CATEGORÍA (Lógica original) ---

  # 1. Validate Category
  valid_cats <- names(fn3_IMPORT_new_import_struct())
  if (!(category %in% valid_cats)) {
    stop(sprintf("STRICT ERROR: Category '%s' is not valid.", category))
  }

  # 2. Validate Field (except for 'extra')
  if (category != "extra") {
    valid_fields <- names(current_list[[category]])
    if (!(field %in% valid_fields)) {
      stop(sprintf("STRICT ERROR: Field '%s' does not exist in category '%s'.",
                   field, category))
    }
  }

  # 3. Perform the update
  current_list[[category]][[field]] <- value

  return(current_list)
}
