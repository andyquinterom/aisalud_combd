nube_ui <- function(id) {
  ns <- NS(id) 
  
  tagList(
    fluidRow(
      tags$br(),
      actionButton(
        inputId = ns("abrir_modal_conectar"),
        label = "Concetar con la el servicio",
        width = "100%"
      )
    ),
    fluidRow()
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
    if (status_code(response_index) == 200) {
      showNotification(
        ui = "Status 200. Conectado satisfactoriamente.",
        duration = 10
      )
      opciones_nube$almacenamiento <- as.numeric(
        content(response_index)[[1]])
      almacenamiento_total <- httr::GET(
        paste0(opciones_nube$api_uri, "almacenamiento_total"),
        query = list(),
        add_headers(Authorization = paste0(
          "Key ", opciones_nube$api_key)))
      tablas <- httr::GET(
        paste0(opciones_nube$api_uri, "tablas"),
        query = list(),
        add_headers(Authorization = paste0(
          "Key ", opciones_nube$api_key)))
      
      opciones_nube$almacenamiento_total <- as.numeric(
        content(almacenamiento_total)[[1]])
      opciones_nube$tablas <- unlist(content(tablas))
      
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

  
}

# curl -X GET "https://users.indexmic.com/content/29/almacenamiento" -H  "accept: */*"