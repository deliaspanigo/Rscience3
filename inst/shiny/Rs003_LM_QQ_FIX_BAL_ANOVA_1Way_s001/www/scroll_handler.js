// Variable to store the last known scroll position
Shiny.setInputValue("scroll_pos_data", null, { priority: "event" });

// 1. Handler to GET the scroll position
Shiny.addCustomMessageHandler('get_scroll_pos', function(message) {
  var element = document.getElementById(message.id);
  if (element) {
    // Send the current scroll position back to Shiny
    // We send it back using a generic input ID
    Shiny.setInputValue("scroll_pos_data", element.scrollTop, { priority: "event" });
  }
});

// 2. Handler to SET the scroll position
Shiny.addCustomMessageHandler('set_scroll_pos', function(message) {
  var element = document.getElementById(message.id);
  if (element) {
    // Restore the previously captured scroll position
    element.scrollTop = message.pos;
  }
});