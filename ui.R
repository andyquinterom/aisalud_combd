shinyUI(function(request) {tagList(
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "style.css"),
    tags$script(src = "document_ready.js"),
    tags$script(src = "ResizeSensor.js")
  ),
  fluidPage(
    tags$br(),
    sidebarLayout(
      sidebarPanel = sidebarPanel(
        tabsetPanel(
          tabPanel(
            "Datos",
            tags$br(),
            prepara_ui("cargar_datos")
          ),
          tabPanel(
            "Agrupadores",
            tags$br(),
            prepara_ui("cargar_agrupadores")
          ),
          # tabPanel(
          #   "Descargar",
          #   tags$br(),
          #   descargar_ui("descargar_datos")
          # ),
          tabPanel(
            "Nube",
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
                   )),
          tabPanel(title = "Preview",
                   fluidRow(
                     column(width = 12,
                            preview_ui("preview"))
                   ))
          # tabPanel(title = "Otras funciones",
          #          fluidRow(
          #            column(width = 12,
          #                   otras_funciones_ui("otras_funciones"))
          #          ))
        )
      )
    )
  )
)})
