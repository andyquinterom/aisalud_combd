nube_ui <- function(id) {
  ns <- NS(id) 
  
  tagList(
    fluidRow(
      tags$br(),
      actionButton(
        inputId = ns("abrir_modal_conectar"),
        label = "Concetar con la el servicio",
        width = "100%"),
      progressBar(
        id = ns("almacenamiento_percent"),
        value = 0, 
        display_pct = TRUE,
        striped = TRUE)
    ),
    fluidRow(
      actionButton(
        inputId = ns("subir_tabla"),
        "Subir tabla"
      )
    )
  )
}

nube_server <- function(input, output, session, datos, nombre_id) {
  
  ns <- NS(nombre_id)
  
  opciones_nube <- reactiveValues()
  
  observeEvent(input$abrir_modal_conectar, {
    showModal(
      session = session,
      ui = modalDialog(
        title = "Clave de API",
        easyClose = TRUE,
        fade = TRUE,
        textInput(
          inputId = ns("api_key"),
          label = "Clave del API",
          width = "100%",
          value = opciones_nube$api_key
        ),
        textInput(
          inputId = ns("request_link"),
          label = "Enlace del API",
          width = "100%",
          value = opciones_nube$api_uri
        ),
        footer = actionButton(
          inputId = ns("conectar_al_servicio"),
          label = "Conectar"
        )
      )
    )
  })
  
  observeEvent(input$conectar_al_servicio, {
    opciones_nube$api_key <-input$api_key
    opciones_nube$api_uri <- input$request_link
    response_index <- httr::GET(
      paste0(opciones_nube$api_uri, "almacenamiento"),
      query = list(),
      add_headers(Authorization = paste0("Key ", opciones_nube$api_key)))
    opciones_nube$status_code <- response_index %>%
      status_code()
    if (opciones_nube$status_code == 200) {
      showNotification(
        ui = "Status 200. Conectado satisfactoriamente.",
        duration = 10
      )
      opciones_nube$almacenamiento <- response_index %>%
        content() %>%
        unlist() %>%
        as.numeric()
      opciones_nube$almacenamiento_total <- httr::GET(
        paste0(opciones_nube$api_uri, "almacenamiento_total"),
        query = list(),
        add_headers(Authorization = paste0(
          "Key ", opciones_nube$api_key))) %>%
        content() %>%
        unlist() %>%
        as.numeric()
      opciones_nube$tablas <- httr::GET(
        paste0(opciones_nube$api_uri, "tablas"),
        query = list(),
        add_headers(Authorization = paste0(
          "Key ", opciones_nube$api_key))) %>%
        content() %>%
        unlist()
      
      print(opciones_nube$almacenamiento)
      print(opciones_nube$almacenamiento_total)
      print(opciones_nube$almacenamiento / 
              opciones_nube$almacenamiento_total)
      print(opciones_nube$tablas)
    } else {
      showNotification(
        ui = "No se pude conectar",
        type = "error",
        duration = 10
      )
    }
  })
  
  observeEvent(input$subir_tabla, {
    table_size <- round(as.numeric(object.size(datos$data_table)) / 1048576)
    print(table_size)
    print(table_size + opciones_nube$almacenamiento)
    if (table_size + opciones_nube$almacenamiento <
        opciones_nube$almacenamiento_total) {
      withProgress({
        httr::POST(
          paste0(opciones_nube$api_uri, "subir"),
          body = list(
            "nombre" = "tabla test",
            "datos" = toJSON(datos$data_table)
          ),
          add_headers(Authorization = paste0(
            "Key ", opciones_nube$api_key))
        )
      })
    }
  })
  
  observeEvent(opciones_nube$almacenamiento, {
    uso <- (opciones_nube$almacenamiento / 
              opciones_nube$almacenamiento_total) * 100
    if (uso <= 75) {
      status <- "success"
    } else if (uso < 90) {
      status <- "warning"
    } else {
      status <- "danger"
    }
    updateProgressBar(
      session = session,
      id = "almacenamiento_percent",
      value = uso,
      status = status
    )
  })

  
}

# curl -X GET "https://users.indexmic.com/content/29/almacenamiento" -H  "accept: */*"