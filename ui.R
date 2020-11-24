shinyUI(function(request) {tagList(
  fluidPage(
    tags$br(),
    sidebarLayout(
      sidebarPanel = sidebarPanel(
        tabsetPanel(
          tabPanel("Cargar datos",
                   prepara_ui("cargar_datos")),
          tabPanel("Nube",
                   nube_ui("nube_datos"))
        )
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