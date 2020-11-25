shinyUI(function(request) {tagList(
  fluidPage(
    tags$br(),
    sidebarLayout(
      sidebarPanel = sidebarPanel(
        tabsetPanel(
          tabPanel(
            "Datos",
            prepara_ui("cargar_datos")
          ),
          tabPanel(
            "Agrupadores",
            prepara_ui("cargar_agrupadores")
          )
        )
      ),
      mainPanel = mainPanel(
        tabsetPanel(
          tabPanel(title = "Columnas",
                   columnas_ui("convertir_columnas")),
          tabPanel(title = "Agrupadores",
                   agrupadores_ui("agrupadores"),
                   columnas_ui("agrupadores_columnas")),
          tabPanel(title = "Filtros")
        )
      )
    )
  )
)})