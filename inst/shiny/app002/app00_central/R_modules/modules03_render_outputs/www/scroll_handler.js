Shiny.setInputValue("steps_scroll_pos", null, { priority: "event" });

// 1. Handler to GET the scroll position
Shiny.addCustomMessageHandler('get_scroll_pos', function(message) {
  var element = document.getElementById(message.id);
  if (element) {
    // Send the current scroll position back to Shiny
    Shiny.setInputValue("steps_scroll_pos", element.scrollTop, { priority: "event" });
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