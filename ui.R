shinyUI(function(request) {tagList(
  fluidPage(
    sidebarLayout(
      sidebarPanel = sidebarPanel(
        prepara_ui("cargar_datos")
      ),
      mainPanel = mainPanel()
    )
  )
)})