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
      choices = c("feather", "csv", "xlsx", "datos didacticos")),
    fluidRow(
      column(width = 6, actionButton(
        inputId = ns("file_options_open"),
        label = "Opciones", 
        width = "100%")),
      column(width = 6, actionButton(
        inputId = ns("file_load"), 
        label = "Aplicar", 
        width = "100%"))
    )
    )
}

prepara_server <- function(input, output, session, nombre_id) {
  
  id <- nombre_id
  ns <- NS(id)
  
  opciones_prepara <- reactiveValues(
    "value_decimal" = ".",
    "value_delimitador" = ",",
    "value_sheet" = NULL,
    "value_range" = NULL
  )
  
  datos <- reactiveValues(
    "data_table" = data.table(),
    "data_original" = data.table(),
    "colnames" = NULL
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
    if (input$file_type == "datos didacticos" &&
        !is.null(opciones_prepara$value_file)) {
      datos$file_name <- opciones_prepara$value_file
      datos$data_original <- as.data.table(
        read_feather(
          path = paste0("datos/didacticos/", opciones_prepara$value_file))
      )
      setnames(datos$data_original, tolower(colnames(datos$data_original)))
      datos$data_table <- datos$data_original
      datos$valores_unicos <- lapply(datos$data_table, unique)
      datos$colnames <- colnames(datos$data_table)
      columnas_num <- unlist(lapply(datos$data_table[1,], is.numeric))
      datos$colnames_num <- datos$colnames[columnas_num]
    }
    if (!is.null(input$file)) {
      datos$colnames <- NULL
      datos$colnames_num <- NULL
      datos$file_name <- sub(
        ".csv$|.feather$|.txt$|.xlsx$",
        "",
        basename(input$file$name))
      if (input$file_type == "csv") {
        datos$data_original <- fread(
          input = input$file$datapath, 
          sep = opciones_prepara$value_delimitador, 
          dec = opciones_prepara$value_decimal,
          data.table = TRUE)
        setnames(datos$data_original, tolower(colnames(datos$data_original)))
        datos$data_table <- datos$data_original
        datos$valores_unicos <- lapply(datos$data_table, unique)
        datos$colnames <- colnames(datos$data_table)
        columnas_num <- unlist(lapply(datos$data_table[1,], is.numeric))
        datos$colnames_num <- datos$colnames[columnas_num]
      } 
      if (input$file_type == "feather") {
        datos$data_original <- as.data.table(
          read_feather(
            path = input$file$datapath)
        )
        setnames(datos$data_original, tolower(colnames(datos$data_original)))
        datos$data_table <- datos$data_original
        datos$valores_unicos <- lapply(datos$data_table, unique)
        datos$colnames <- colnames(datos$data_table)
        columnas_num <- unlist(lapply(datos$data_table[1,], is.numeric))
        datos$colnames_num <- datos$colnames[columnas_num]
      }
      if (input$file_type == "xlsx") {
        datos$data_original <- as.data.table(
          read_excel(
            path = input$file$datapath, 
            sheet = opciones_prepara$value_sheet, 
            range = opciones_prepara$value_range)
        )
        setnames(datos$data_original, tolower(colnames(datos$data_original)))
        datos$data_table <- datos$data_original
        datos$valores_unicos <- lapply(datos$data_table, unique)
        datos$colnames <- colnames(datos$data_table)
        columnas_num <- unlist(lapply(datos$data_table[1,], is.numeric))
        datos$colnames_num <- datos$colnames[columnas_num]
      }
    }
  })
  
  observeEvent(datos$colnames, {
    updateSelectizeInput(
      session = session,
      inputId = "columna_valor",
      choices = datos$colnames_num,
      selected = "valor"
    )
  })
  
  output$preview <- DT::renderDataTable({
    if (is.null(datos$colnames)) {
      data.table()
    } else {
      tryCatch(
        expr = {
          columnas <- intersect(
            x = c(
              "nro_identificacion",
              "fecha_prestacion",
              "valor"),
            y = names(datos$data_original[1])
          )
          DT::datatable(
            data = datos$data_original[
              1:5,
              columnas,
              with = FALSE],
            rownames = FALSE,
            options = list(
              columnDefs = list(
                list(
                  className = 'dt-center',
                  targets = "_all")),
              dom = 't',
              pageLength = 5,
              ordering = FALSE,
              language = list(
                url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Spanish.json')
            )) %>%
            DT::formatStyle(
              columns = 1:length(columnas),
              valueColumns = 1,
              backgroundColor = "white")
        },
        error = function(e) {
          print(e[1])
          sendSweetAlert(
            session = session,
            title = "Error",
            text = e[1],
            type = "error"
          )
        }
      )
    }
  })
  
  return(datos)
  
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
  
  if (file_type == "xlsx") {
    return(
      datos_opciones_xlsx_ui(
        id = id,
        value_sheet = value_sheet,
        value_range = value_range
      )
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

datos_opciones_xlsx_ui <- function(id, value_sheet, value_range) {
  ns <- NS(id)
  
  tagList(
    textInput(
      inputId = ns("value_sheet"),
      label = "Nombre de la hoja",
      placeholder = "Sheet1",
      value = value_sheet
    ),
    
    textInput(
      inputId = ns("value_range"),
      label = "Rango",
      placeholder = "A1:A1",
      value = value_range
    ),
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