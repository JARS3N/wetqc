session_end_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    session$onSessionEnded(function() {
      message("Advancing to the rear.")
      stopApp()
    })
  })
}
