nube_ui <- function(id) {
  ns <- NS(id) 
  
  tagList(
    fluidRow(
      tags$br(),
      actionButton(
        inputId = ns("conectar_al_servicio"),
        label = "Concetar con la el servicio",
        width = "100%"),
      progressBar(
        id = ns("almacenamiento_percent"),
        value = 100, 
        display_pct = TRUE,
        striped = TRUE)
    ),
    uiOutput(ns("ui_conectado"))
  )
}

nube_server <- function(input, output, session, datos, nombre_id) {
  
  ns <- NS(nombre_id)
  
  opciones_nube <- reactiveValues()
  
  update_almacenamiento <- reactive({
    list(input$conectar_al_servicio, opciones_nube$resultados_subidas,
         opciones_nube$resultado_eliminacion)
  })
  
  observeEvent(update_almacenamiento(), {
    if (!is.null(base_de_datos_con)) {
      opciones_nube$tablas_almacenadas <- dbGetQuery(
        base_de_datos_con,
        paste0("SELECT table_name FROM information_schema.tables
      WHERE table_schema='", Sys.getenv("DATABASE_SCHEMA"), "'")) %>%
        unlist() %>%
        unname()
      
      opciones_nube$almacenamiento <- check_schema_size(
        con = base_de_datos_con,
        schema = Sys.getenv("DATABASE_SCHEMA")
      ) %>% ifelse(is.na(.),
                   yes = 0,
                   no = .)
      
      opciones_nube$almacenamiento_total <- Sys.getenv("DATABASE_MAX_STORAGE") %>%
        as.numeric()
      
      opciones_nube$tablas_almacenadas <- dbGetQuery(
        base_de_datos_con,
        paste0("SELECT table_name FROM information_schema.tables
               WHERE table_schema='", 
               Sys.getenv("DATABASE_SCHEMA"), "'")) %>%
        unlist() %>%
        unname()  
    }
  })
  
  observeEvent(input$conectar_al_servicio, {
    if (is.null(base_de_datos_con)) {
      tryCatch(
        expr = {
          opciones_nube$tablas_almacenadas <- dbGetQuery(
            base_de_datos_con,
            paste0("SELECT table_name FROM information_schema.tables
                   WHERE table_schema='",
                   Sys.getenv("DATABASE_SCHEMA"), "'")) %>%
            unlist() %>%
            unname()
          
          opciones_nube$almacenamiento <- check_schema_size(
            con = base_de_datos_con,
            schema = Sys.getenv("DATABASE_SCHEMA")
          ) %>% ifelse(is.na(.),
                       yes = 0,
                       no = .)
          
          opciones_nube$almacenamiento_total <- 
            Sys.getenv("DATABASE_MAX_STORAGE") %>%
            as.numeric()
          
          opciones_nube$tablas_almacenadas <- dbGetQuery(
            base_de_datos_con,
            paste0("SELECT table_name FROM information_schema.tables
                   WHERE table_schema='", 
                   Sys.getenv("DATABASE_SCHEMA"), "'")) %>%
            unlist() %>%
            unname()  
          
          
        },
        error = function(e) {
          base_de_datos_con <<- NULL
          opciones_nube$almacenamiento_total <- NULL
          print(e)
          showNotification(
            ui = "Error subiendo la base de datos.",
            type = "error",
            session = session
          )
        } 
      )
    }
  })
  
  observeEvent(input$subir_tabla, {
    if (!is.null(datos$colnames)) {
      table_size <- round(feather_size_est(datos$data_table) / 1048576)
      print(table_size)
      print(table_size + opciones_nube$almacenamiento)
      if (table_size + opciones_nube$almacenamiento <
          opciones_nube$almacenamiento_total) {
        showModal(
          session = session,
          ui = modalDialog(
            title = "Nombre de la tabla",
            textInput(
              inputId = ns("subir_tabla_nombre"),
              label = "", 
              width = "100%"
            ),
            footer = actionButton(
              inputId = ns("subir_tabla_confirmar"),
              label = "Subir tabla")
          )
        )
      } else {
        sendSweetAlert(
          session = session,
          title = "Archivo demasiado grande.",
          text = paste0(
            "Los datos que esta intentando subir son demasiado extensos. \n",
            "Se supera el límite de ", opciones_nube$almacenamiento_total, 
            " MB.\n", "Por favor borrar alguna tabla para continuar."),
          type = "error"
        )
      }
    }
  })
  
  observeEvent(input$subir_tabla_confirmar, {
    if (input$subir_tabla_nombre != "") {
      removeModal(session = session)
      table_size <- round(feather_size_est(datos$data_table) / 1048576)
      print(table_size)
      print(table_size + opciones_nube$almacenamiento)
      if (table_size + opciones_nube$almacenamiento <
          opciones_nube$almacenamiento_total) {
        nombre_tabla <- tolower(input$subir_tabla_nombre)
        tryCatch(
          expr = {
            withProgress(
              message = "Subiendo base de datos a la nube.",
              expr = {
              opciones_nube$resultados_subidas <- dbWriteTable(
                con = base_de_datos_con,
                schema = Sys.getenv("DATABASE_SCHEMA"),
                name = nombre_tabla,
                value = datos$data_table
              )
            })
          },
          error = function(e) {
            print(e)
            showNotification(
              ui = "Error subiendo la base de datos.",
              type = "error",
              session = session
            )
          }
        )
      }
      print(opciones_nube$resultados_subidas)
      opciones_nube$resultados_subidas <- NULL
    } else {
      showNotification(
        ui = "El nombre no puede ser vacio.",
        type = "error",
        session = session
      )
    }
  })
  
  observeEvent(opciones_nube$almacenamiento, {
    if (!is.na(opciones_nube$almacenamiento)) {
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
    } else {
      updateProgressBar(
        session = session,
        id = "almacenamiento_percent",
        value = 0,
        status = "success"
      )
    }
  })
  
  output$tablas_lista <- DT::renderDataTable({
    datatable(
      data = data.frame("Tablas" = opciones_nube$tablas_almacenadas),
      rownames = FALSE,
      selection = "single",
      options = list(
        ordering = F,
        pageLength = length(opciones_nube$tablas_almacenadas),
        scrollY = "200px",
        scrollX = TRUE,
        dom = "t"
      )
    )
  })
  
  observeEvent(input$remover_tabla_seleccionada, {
    if (!is.null(input$tablas_lista_rows_selected)) {
      confirmSweetAlert(
        session = session,
        inputId = ns("remover_tabla_seleccionada_confirmar"),
        title = "Confirmar eliminación", 
        text = "Este cambio no es reversible.",
        btn_labels = c("Cancelar", "Confirmar")
      )
    }
  })
  
  observeEvent(input$remover_tabla_seleccionada_confirmar, {
    if (!is.null(input$tablas_lista_rows_selected) && 
        input$remover_tabla_seleccionada_confirmar) {
      tryCatch(
        expr = {
          withProgress({
            dbRemoveTable(
              con = base_de_datos_con,
              schema = Sys.getenv("DATABASE_SCHEMA"),
              name = opciones_nube$tablas_almacenadas[
                input$tablas_lista_rows_selected]
            )
            opciones_nube$resultado_eliminacion <- NULL
            opciones_nube$resultado_eliminacion <- TRUE
          })
        },
        error = function(e) {
          print(e)
          showNotification(
            ui = "Error subiendo la base de datos.",
            type = "error",
            session = session
          )
        }
      )
    }
  })
  
  output$ui_conectado <- renderUI({
    if (!is.null(opciones_nube$almacenamiento_total)) {
      tagList(
        fluidRow(
          DT::dataTableOutput(outputId = ns("tablas_lista")),
          actionButton(
            inputId = ns("remover_tabla_seleccionada"),
            label = "Eliminar",
            width = "100%"
          )
        ),
        tags$br(),
        fluidRow(
          actionButton(
            inputId = ns("subir_tabla"),
            "Subir tabla",
            width = "100%"
          )
        )
      )
    }
  })

  
}

# curl -X GET "https://users.indexmic.com/content/29/almacenamiento" -H  "accept: */*"