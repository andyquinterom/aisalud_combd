shinyUI(function(request) {tagList(
  fluidPage(
    tags$br(),
    sidebarLayout(
      sidebarPanel = sidebarPanel(
        prepara_ui("cargar_datos")
      ),
      mainPanel = mainPanel(
        tabsetPanel(
          tabPanel(title = "Columnas",
                   columnas_ui("convertir_columnas")),
          tabPanel(title = "Agrupadores"),
          tabPanel(title = "Filtros")
        )
      )
    )
  )
)})