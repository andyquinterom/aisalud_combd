otras_funciones_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    tags$br(),
    fluidRow(
      column(
        width = 4,
        actionButton(
          inputId = ns("fechas"),
          label = "Fechas",
          width = "100%"
        )
      )
    )
  )
}

otras_funciones_server <- function(id, datos) {
  moduleServer(
    id = id,
    module = function(input, output, session) {
      ns <- NS(id)
      
      observeEvent(input$fechas, {
        showModal(
          session = session,
          ui = modalDialog(
            title = "Utilidades de fecha",
            easyClose = TRUE,
            fade = TRUE,
            fluidRow(
              column(width = 8,
                     selectizeInput(
                       inputId = ns("col_fecha"), 
                       width = "100%",
                       label = "Columna de fecha",
                       choices = datos$colnames)),
              column(width = 4,
                     textInput(
                       inputId = ns("col_fecha_formato"),
                       value = "%d/%m/%Y",
                       label = "Formato",
                       width = "100%"))
            ),
            fluidRow(
              column(width = 12,
                     checkboxGroupButtons(
                       inputId = ns("fechas_columnas_selected"),
                       label = "Label",
                       width = "100%",
                       choices = c(
                         "Mes #",
                         "Mes",
                         "Año",
                         "Mes + Año"
                       ),
                       status = "primary",
                       checkIcon = list(
                         yes = icon("ok", 
                                    lib = "glyphicon"),
                         no = icon("remove",
                                   lib = "glyphicon"))
                     ))
            ),
            footer = actionButton(
              inputId = ns("fechas_crear_columnas"),
              label = "Crear columnas")
          )
        )
      })
      
      observeEvent(input$fechas_crear_columnas, {
        tryCatch(
          expr = {
            showNotification("Iniciado creación")
            print(input$fechas_columnas_selected)
            print("Mes #" %in% input$fechas_columnas_selected)
            if("Mes #" %in% input$fechas_columnas_selected) {
              datos$data_original[, "mes_numero_nuevo" := month(as.Date(
                get(input$col_fecha), format = input$col_fecha_formato))]
            }
            if("Mes" %in% input$fechas_columnas_selected) {
              datos$data_original[, "mes_nuevo" := mes_spanish(month(as.Date(
                get(input$col_fecha), format = input$col_fecha_formato)))]
            }
            if("Año" %in% input$fechas_columnas_selected) {
              datos$data_original[, "anio_nuevo" := year(as.Date(
                get(input$col_fecha), format = input$col_fecha_formato))]
            }
            if("Mes + Año" %in% input$fechas_columnas_selected) {
              datos$data_original[, "mes_anio_nuevo" := paste(
                year(as.Date(
                  get(input$col_fecha), format = input$col_fecha_formato)),
                mes_spanish(month(as.Date(
                  get(input$col_fecha), format = input$col_fecha_formato))),
                sep = " - "
              )]
            }
            datos$data_table <- NULL
            datos$data_table <- copy(datos$data_original)
            datos$valores_unicos <- lapply(datos$data_table, unique)
            datos$colnames <- NULL
            datos$colnames <- colnames(datos$data_table)
            columnas_num <- unlist(lapply(datos$data_table[1,], is.numeric))
            datos$colnames_num <- NULL
            datos$colnames_num <- datos$colnames[columnas_num]
            showNotification("Nuevas columnas creadas")
          },
          error = function(e) {
            print(e)
            sendSweetAlert(
              session = session,
              title = "Error",
              text = e[1],
              type = "error"
            )
          }
        )
      })
      
    }
  )
}