# utils_ui.R

# Function to inject the "Backdoor" keyboard listener
tags_hidden_engineer <- function(secret_word = "gokuvegeta", target_id = "#hidden_engineer_zone") {
  tags$script(HTML(paste0("
    var secretCode = '", secret_word, "';
    var inputBuffer = '';
    var timer;

    $(document).on('keydown', function(e) {
      // Ignore if user is typing in a real input field
      if ($(e.target).is('input, textarea, select')) return;

      // Clear buffer if more than 3 seconds pass between keystrokes
      clearTimeout(timer);
      timer = setTimeout(function() {
        inputBuffer = '';
      }, 3000);

      inputBuffer += e.key.toLowerCase();

      // Check if the sequence is matched
      if (inputBuffer.includes(secretCode)) {
        $('", target_id, "').slideToggle('slow');
        inputBuffer = '';
      }

      if (inputBuffer.length > 20) {
        inputBuffer = inputBuffer.substring(inputBuffer.length - 20);
      }
    });
  ")))
}
