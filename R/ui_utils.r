check.ui = function(ui, create.app=TRUE) {
  if (create.app)
    app = eventsApp()
  app$ui = fluidPage(
    ui
  )
  runEventsApp(app, launch.browser = rstudio::viewer)
}
