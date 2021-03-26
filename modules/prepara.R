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

prepara_server <- function(id, opciones) {
  
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
              data_original <- data.frame()
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
                data_original <- read_delim(
                  file = input$file$datapath, 
                  delim = value_delimitador, 
                  col_types = cols(.default = col_character()),
                  locale = locale(
                    encoding = guess_encoding(input$file$datapath)[[1]][1],
                    decimal_mark = opciones_prepara$value_decimal))
              } 
              if (input$file_type == "feather") {
                data_original <- read_feather(
                  path = input$file$datapath)
              }
              nombre_tabla_temporal <- paste(
                "temporal", file_name,
                round(runif(1, 100, 999), 0))
              
              if (nrow(data_original) > 0) {
                dbWriteTable(
                  conn = conn,
                  name = nombre_tabla_temporal,
                  value = {data_original %>% rename_with(tolower)},
                  temporary = TRUE)
                
                opciones$tabla_original <- tbl(conn, nombre_tabla_temporal)
                
                opciones$tabla <- opciones$tabla_original
                
                opciones$cambios <- list()
                
                rm(data_original)
                gc(full = TRUE)
                
              }
              
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