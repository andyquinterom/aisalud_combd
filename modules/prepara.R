prepara_ui <- function(id) {
  
  ns <- NS(id)
  
  tagList(
    fileInput(
      inputId = ns("file"),
      label = "", 
      buttonLabel = "Subir archivo",
      placeholder = "Ningún archivo"),
    radioButtons(
      inputId = ns("file_type"),
      label = "Tipo de archivo",
      inline = TRUE, 
      choices = c("csv", "feather")),
    fluidRow(
      column(width = 6, actionButton(
        inputId = ns("file_options_open"),
        label = "Opciones", 
        width = "100%")),
      column(width = 6, actionButton(
        inputId = ns("file_load"), 
        label = "Aplicar", 
        width = "100%")),
      column(width = 12, 
             tags$br(),
             actionButton(ns("undo"), "undo"),
             div(verbatimTextOutput(ns("logs")),
                 class = "error_logs"))
    )
    )
}

prepara_server <- function(id, opciones, validar_fecha = FALSE) {
  
  ns <- NS(id)
  
  moduleServer(
    id = id,
    module = function(input, output, session) {
      
      opciones_prepara <- reactiveValues(
        "value_decimal" = ".",
        "value_delimitador" = ",",
        "value_sheet" = NULL,
        "value_range" = NULL
      )
      
      observeEvent(input$file_options_open, {
        showModal(
          session = session,
          ui = modalDialog(
            title = "Opciones archivo",
            easyClose = TRUE,
            fade = TRUE,
            datos_opciones_ui(
              id = id,
              file_type = input$file_type,
              value_decimal = opciones_prepara$value_decimal,
              value_delimitador = opciones_prepara$value_delimitador,
              value_range = opciones_prepara$value_range,
              value_sheet = opciones_prepara$value_sheet,
              value_file = opciones_prepara$value_file),
            footer = actionButton(
              inputId = ns("datos_opciones_guardar"),
              label = "Guardar")
          )
        )
      })
      
      observeEvent(input$datos_opciones_guardar, {
        opciones_prepara$value_decimal <- input$value_decimal
        opciones_prepara$value_delimitador <- input$value_delimitador
        opciones_prepara$value_sheet <- input$value_sheet
        opciones_prepara$value_range <- input$value_range
        opciones_prepara$value_file <- input$value_file
        removeModal(session = session)
      })
      
      observeEvent(input$file_load, {
        tryCatch(
          expr = {
            if (!is.null(input$file)) {
              opciones$data_original <- data.frame()
              opciones$validar_fecha <- !validar_fecha
              file_name <- sub(
                ".csv$|.feather$|.txt$|.xlsx$",
                "",
                basename(input$file$name))
              if (input$file_type == "csv") {
                value_delimitador <- ifelse(
                  test = opciones_prepara$value_delimitador == "Espacios",
                  yes = "\t",
                  no = opciones_prepara$value_delimitador
                )
                opciones$data_original <- read_delim(
                  file = input$file$datapath, 
                  delim = value_delimitador, 
                  col_types = cols(.default = col_character()),
                  locale = locale(
                    encoding = guess_encoding(input$file$datapath)[[1]][1],
                    decimal_mark = opciones_prepara$value_decimal))
              } 
              if (input$file_type == "feather") {
                opciones$data_original <- read_feather(
                  path = input$file$datapath)
              }
              
              opciones$nombre_tabla <- paste(
                sep = "_",
                "temporal", file_name,
                round(runif(1, 100, 999), 0))
              
              opciones$data_original <- opciones$data_original %>% 
                rename_with(tolower)
              
              
            }
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
      
      observeEvent(opciones$data_original, {
        if (nrow(opciones$data_original > 0)) {
          if (!opciones$validar_fecha) {
            showModal(
              session = session,
              ui = modalDialog(
                title = "Columna de fecha",
                fluidRow(
                  column(
                    width = 4,
                    selectInput(
                      inputId = ns("fecha_columna"),
                      label = "Columna de fecha:",
                      choices = as.character(colnames(opciones$data_original)),
                      selected = "fecha_prestacion"
                    )
                  ),
                  column(
                    width = 4,
                    textInput(
                      inputId = ns("fecha_formato"),
                      label = "Formato de fecha",
                      value = "%d/%m/%Y"
                    )
                  )
                ),
                footer = actionButton(
                  inputId = ns("confirmar_fecha"),
                  label = "Validar columna de fecha")
              )
            )
          } else {
            withProgress(message = "Cargando datos", {
              dbWriteTable(
                conn = conn,
                name = opciones$nombre_tabla,
                value = opciones$data_original,
                temporary = TRUE)
              
              opciones$tabla_original <- tbl(conn, opciones$nombre_tabla)
              
              opciones$tabla <- opciones$tabla_original
              
              opciones$cambios <- list()
              
              opciones$data_original <- NULL
              gc(full = TRUE)
            })
          }
        }
      })
      
      observeEvent(input$confirmar_fecha, {
        fecha_columna <- input$fecha_columna
        fecha_formato <- input$fecha_formato
        
        tryCatch(
          expr = {
            opciones$data_original <- opciones$data_original %>% 
              mutate(!!fecha_columna := as.Date(!!as.name(fecha_columna),
                                                format = fecha_formato))
            
            if (class(pull(opciones$data_original, !!as.name(fecha_columna)))
                == "Date") {
              opciones$validar_fecha = TRUE
              removeModal()
            } else {
              showNotification("Columna no fue convertida a fecha.",
                               type = "warning")
            }
          },
          error = function(e) {
            print(e)
            showNotification(
              ui = "No se pudo convertir a fecha",
              type = "error",
              session = session
            )
          }
        )
        
      })
      
      observe({
        opciones$tabla <- map_func(opciones$tabla_original, opciones$cambios)
      })
     
      output$logs <- renderText({
        paste(names(opciones$cambios), collapse = "\n")
      })
      
      observeEvent(input$undo, {
        n_cambios <- length(opciones$cambios)
        opciones$cambios <- opciones$cambios[-n_cambios]
      })
       
    }
  )
  
}

# Funciones --------------------------------------------------------------------

datos_opciones_ui <- function(
  id, file_type, value_decimal, value_delimitador, value_sheet, value_range,
  value_file) {
  
  if (file_type == "csv") {
    return(
      datos_opciones_csv_ui(
        id = id,
        value_decimal = value_decimal,
        value_delimitador = value_delimitador)
    )
  }
  
  if (file_type == "datos didacticos") {
    return(
      datos_opciones_cloud_ui(
        id = id,
        value_file = value_file
      )
    )
  }
  
}

datos_opciones_csv_ui <- function(id, value_delimitador, value_decimal) {
  ns <- NS(id)
  
  tagList(
    radioButtons(
      inputId = ns("value_delimitador"),
      choices = c(",", ";", "|", "Espacios"),
      label = "Delimitador",
      inline = TRUE,
      selected = value_delimitador
    ),
    
    radioButtons(
      inputId = ns("value_decimal"),
      choiceNames = c("Punto", "Coma"),
      choiceValues = c(".", ","),
      label = "Separador decimal",
      inline = TRUE,
      selected = value_decimal
    )
  )
  
}

datos_opciones_cloud_ui <- function(id, value_file) {
  ns <- NS(id)
  
  tagList(
    selectizeInput(
      inputId = ns("value_file"),
      choices = list.files("datos/didacticos/"),
      label = "Archivo:",
      selected = value_file
    )
  )
  
}