

### 1.3.3 Helper function to render HTML content

fn_app_show_my_html <- function(str_source_folder_full_path, html_file_name) {
  
  # Validate inputs
  if (!is.character(str_source_folder_full_path) || length(str_source_folder_full_path) != 1) {
    return(
      '<div class="alert alert-danger">
         <i class="fas fa-exclamation-circle"></i>
         Error: Invalid source folder path parameter.
       </div>'
    )
  }
  
  if (!is.character(html_file_name) || length(html_file_name) != 1) {
    return(
      '<div class="alert alert-danger">
         <i class="fas fa-exclamation-circle"></i>
         Error: Invalid HTML file name parameter.
       </div>'
    )
  }
  
  # Check if source folder exists
  if (!dir.exists(str_source_folder_full_path)) {
    return(paste0(
      '<div class="alert alert-warning">
         <i class="fas fa-exclamation-triangle"></i>
         Error: Source folder does not exist.<br>
         Path: "', str_source_folder_full_path, '"
       </div>'
    ))
  }
  
  # Build full path to HTML file
  full_html_path <- file.path(str_source_folder_full_path, html_file_name)
  
  # Check if HTML file exists
  if (!file.exists(full_html_path)) {
    
    # List available HTML files to help user
    available_files <- list.files(
      path = str_source_folder_full_path,
      pattern = "\\.html$",
      ignore.case = TRUE,
      full.names = FALSE
    )
    
    if (length(available_files) == 0) {
      available_files_msg <- "No HTML files found in the specified folder."
    } else {
      available_files_msg <- paste0(
        "Available HTML files:<br>",
        paste0("• ", available_files, collapse = "<br>")
      )
    }
    
    return(paste0(
      '<div class="alert alert-warning">
         <i class="fas fa-exclamation-triangle"></i>
         Error: HTML file not found.<br>
         Expected file: "', html_file_name, '"<br>
         Search path: "', full_html_path, '"<br><br>
         ', available_files_msg, '
       </div>'
    ))
  }
  
  # Check for multiple files with same name (case-insensitive search)
  all_files <- list.files(
    path = str_source_folder_full_path,
    pattern = "\\.html$",
    ignore.case = TRUE,
    full.names = FALSE
  )
  
  # Case-insensitive matching
  matching_files <- all_files[tolower(all_files) == tolower(html_file_name)]
  
  if (length(matching_files) > 1) {
    return(paste0(
      '<div class="alert alert-danger">
         <i class="fas fa-exclamation-circle"></i>
         Error: Multiple files found with similar names.<br>
         Requested file: "', html_file_name, '"<br>
         Found files:<br>',
      paste0("• ", matching_files, collapse = "<br>"), '<br><br>
         Please ensure the file name matches exactly (including case sensitivity).
       </div>'
    ))
  }
  
  # Verify file is actually an HTML file (by extension and content)
  if (!grepl("\\.html$", html_file_name, ignore.case = TRUE)) {
    return(paste0(
      '<div class="alert alert-warning">
         <i class="fas fa-exclamation-triangle"></i>
         Warning: File does not have .html extension.<br>
         File: "', html_file_name, '"<br>
         Loading anyway as requested.
       </div>'
    ))
  }
  
  # Create unique resource ID based on folder path and file name
  # This ensures different folders/files get different resource paths
  resource_id_input <- paste0(str_source_folder_full_path, ":", html_file_name)
  resource_id <- paste0("res_", digest::digest(resource_id_input, algo = "md5"))
  
  # Add resource path if not already added
  current_paths <- shiny::resourcePaths()
  if (!resource_id %in% names(current_paths)) {
    shiny::addResourcePath(resource_id, str_source_folder_full_path)
  }
  
  # Construct URL for the iframe
  html_url <- paste0("/", file.path(resource_id, html_file_name))
  
  # Optional: Log successful loading (for debugging)
  # message(paste(
  #   "Successfully loaded HTML file:",
  #   html_file_name,
  #   "from:",
  #   str_source_folder_full_path,
  #   "URL:", html_url
  # ))
  
  # Build iframe HTML with responsive design and fallback
  armado_v <- paste0(
    '<div class="html-container" style="height: 100vh; width: 100%; position: relative;">
       <iframe 
         id="theory-frame"
         src="', html_url, '"
         style="height: 100%; width: 100%; border: none;"
         title="Theory Content: ', htmltools::htmlEscape(html_file_name), '"
         aria-label="Theory content from ', htmltools::htmlEscape(html_file_name), '"
         onload="this.style.opacity=\'1\';"
         onerror="this.onerror=null; this.src=\'about:blank\';
                  this.parentNode.innerHTML=\'<div class=\\\'alert alert-danger\\\'>Failed to load content. Please check if the file exists and is accessible.</div>\';"
       >
         <div class="alert alert-info">
           <i class="fas fa-info-circle"></i>
           Your browser does not support iframes.<br>
           Please <a href="', html_url, '" target="_blank">click here</a> to open the content in a new tab.
         </div>
       </iframe>
       <div id="frame-loading" style="position: absolute; top: 10px; right: 10px; background: rgba(0,0,0,0.7); color: white; padding: 5px 10px; border-radius: 3px; display: none;">
         <i class="fas fa-spinner fa-spin"></i> Loading...
       </div>
     </div>
     <script>
       // Show loading indicator
       document.addEventListener("DOMContentLoaded", function() {
         var frame = document.getElementById("theory-frame");
         var loading = document.getElementById("frame-loading");
         
         if (frame && loading) {
           frame.style.opacity = "0";
           frame.style.transition = "opacity 0.3s";
           loading.style.display = "block";
           
           frame.onload = function() {
             loading.style.display = "none";
             frame.style.opacity = "1";
           };
           
           // Hide loading after 10 seconds max
           setTimeout(function() {
             loading.style.display = "none";
           }, 10000);
         }
       });
     </script>'
  )
  
  return(armado_v)
}
