descargar_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    fluidRow(
      column(
        width = 12,
        div(
          class = "download_buttons",
          downloadButton(
            outputId = ns("descargar_feather"),
            label = "Feather"
          ),
          tags$br(),
          tags$br(),
          downloadButton(
            outputId = ns("descargar_csv"),
            label = "CSV"
          ),
          tags$br(),
          tags$br(),
          downloadButton(
            outputId = ns("descargar_xlsx"),
            label = "Excel"
          )
        )
      )
    )
  )
}

descargar_server <- function(input, output, session, datos) {
  
  output$descargar_feather <- downloadHandler(
    filename = function() {
      paste("base_de_datos",
            ".feather", sep="")
    },
    content = function(file) {
      write_feather(
        x = datos$data_table,
        path = file)
    }, 
    contentType = "feather"
  )
  
  output$descargar_csv <- downloadHandler(
    filename = function() {
      paste("base_de_datos",
            ".csv", sep="")
    },
    content = function(file) {
      readr::write_csv(
        x = datos$data_table,
        file = file)
    }, 
    contentType = "text/csv"
  )
  
  output$descargar_xlsx <- downloadHandler(
    filename = function() {
      paste("base_de_datos",
            ".xlsx", sep="")
    },
    content = function(file) {
      writexl::write_xlsx(
        x = datos$data_table,
        path = file)
    }, 
    contentType = "xlsx"
  )
  
}