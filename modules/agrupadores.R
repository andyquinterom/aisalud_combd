agrupadores_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    fluidRow(
      column(
        width = 6,
        selectizeInput(
          inputId = ns("llave_primaria"),
          multiple = TRUE,
          label = "Llave primaria:",
          choices = "Ninguno",
          width = "100%"
        )
      ),
      column(
        width = 6,
        selectizeInput(
          inputId = ns("llave_foranea"),
          multiple = TRUE,
          label = "Llave foranea:",
          choices = "Ninguno",
          width = "100%"
        )
      )
    ),
    fluidRow(
      column(
        width = 12,
        selectizeInput(
          inputId = ns("agrupadores"),
          multiple = TRUE,
          label = "Agrupadores:",
          choices = "Ninguno",
          width = "100%"
        ),
        actionButton(
          inputId = ns("ejecutar_agrupadores"),
          label = "Agrupar"
        )
      )
    )
  )
}

agrupadores_server <- function(input, output, session, datos, agrupadores) {
  
  observeEvent(datos$colnames, {
    updateSelectizeInput(
      session = session,
      inputId = "llave_primaria",
      choices = datos$colnames
    )
  })
  
  observeEvent(agrupadores$colnames, {
    updateSelectizeInput(
      session = session,
      inputId = "llave_foranea",
      choices = agrupadores$colnames
    )
    updateSelectizeInput(
      session = session,
      inputId = "agrupadores",
      choices = agrupadores$colnames
    )
  })
  
  observeEvent(input$ejecutar_agrupadores, {
    columnas_y <- c(input$llave_foranea, input$agrupadores)
    datos_agrupados <- merge.data.table(
      x = datos$data_table,
      y = agrupadores$data_table[, c(columnas_y), with = FALSE],
      by.x = input$llave_primaria,
      by.y = input$llave_foranea,
      all.x = TRUE
    )
    
    datos$data_original <- datos_agrupados
    datos$data_table <- datos_agrupados
    datos$colnames <- NULL
    datos$colnames <- colnames(datos$data_table)
    datos$colnames_num <- NULL
    columnas_num <- unlist(lapply(datos$data_table[1,], is.numeric))
    datos$colnames_num <- datos$colnames[columnas_num]
    
  })
  
  
  
}