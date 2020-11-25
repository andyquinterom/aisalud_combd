shinyUI(function(request) {tagList(
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "style.css"),
    tags$script(src = "document_ready.js"),
    tags$script(src = "ResizeSensor.js")
  ),
  fluidPage(
    tags$div(
      class = "header_combd"
    ),
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
          ),
          tabPanel(
            "Descargar",
            tags$br(),
            descargar_ui("descargar_datos")
          ),
          tabPanel(
            "nube",
            tags$br(),
            nube_ui("nube_datos")
          )
        )
      ),
      mainPanel = mainPanel(
        tabsetPanel(
          tabPanel(title = "Columnas",
                   columnas_ui("convertir_columnas")),
          tabPanel(title = "Agrupadores",
                   tags$br(),
                   agrupadores_ui("agrupadores"),
                   columnas_ui("agrupadores_columnas")),
          tabPanel(title = "Filtros",
                   fluidRow(
                     column(width = 12,
                            filtros_ui("filtros")
                            )
                   ))
        )
      )
    )
  )
)})