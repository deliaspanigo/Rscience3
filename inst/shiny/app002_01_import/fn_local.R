#' Data Schema Constructor
#' @description Creates the skeleton with fixed types to avoid logical NAs
#' where timestamps or characters are expected.
#' @return A nested list with predefined categories.
#' @export
fn3_IMPORT_new_import_struct <- function() {
  list(
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
fn3_IMPORT_set_import_data <- function(current_list, category, field, value) {

  # 0. Safety Check: Does the list exist?
  if (is.null(current_list)) {
    stop("STRICT ERROR: 'current_list' is NULL. Initialize it with fn3_IMPORT_new_import_struct() first.")
  }

  # 1. Validate Category (Dynamic based on Constructor)
  valid_cats <- names(fn3_IMPORT_new_import_struct())
  if (!(category %in% valid_cats)) {
    stop(sprintf("STRICT ERROR: Category '%s' is not valid. Available categories: %s.",
                 category, paste(valid_cats, collapse = ", ")))
  }

  # 2. Validate Field (Strict check for all categories except 'extra')
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
