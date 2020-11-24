columnas_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    fluidRow(
      column(
        width = 5,
        tags$br(),
        tags$div(
          class = "columnas_lista_div",
          DT::dataTableOutput(outputId = ns("columnas"))
        )
      ),
      column(
        width = 7,
        tags$br(),
        tags$div(
          class = "resumen_columna",
          verbatimTextOutput(
            outputId = ns("resumen_columna"),
            placeholder = TRUE
          ),
          fluidRow(
            column(width = 4, actionButton(
              inputId = ns("convertir_caracter"),
              width = "100%",
              "Caracter"
            )),
            column(width = 4, actionButton(
              inputId = ns("convertir_numerico"),
              width = "100%",
              "Numérico"
            )),
            column(width = 4, actionButton(
              inputId = ns("remover"),
              width = "100%",
              "Remover"
            ))
          )
        )
      )
    )
  )
}

columnas_server <- function(input, output, session, datos) {
  
  observeEvent(datos$colnames, {
    if (!is.null(datos$colnames)) {
      output$columnas <- DT::renderDataTable({
        datatable(
          data = data.table("Columnas" = datos$colnames),
          rownames = FALSE,
          editable = TRUE,
          selection = "single",
          options = list(
            ordering = F,
            pageLength = length(datos$colnames),
            scrollY = "400px",
            scrollX = TRUE,
            dom = "t"
          )
        )
      })
    }
  })
  
  observeEvent(input$columnas_cell_edit, {
    setnames(
      x = datos$data_table,
      input$columnas_cell_edit$row,
      input$columnas_cell_edit$value
    )
    datos$colnames <- names(datos$data_table)
  })
  
}