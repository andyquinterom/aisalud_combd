columnas_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    fluidRow(
      column(
        width = 5,
        tags$br(),
        tags$div(
          class = "columnas_lista_div well",
          DT::dataTableOutput(outputId = ns("columnas")) %>% withSpinner()
        )
      ),
      column(
        width = 7,
        tags$br(),
        tags$div(
          class = "well opciones_columnas",
          verbatimTextOutput(
            outputId = ns("resumen_columna"),
            placeholder = TRUE) %>% withSpinner(),
          fluidRow(
            div(class = "botones_convertir_fila_1",
              column(width = 4, actionButton(
                inputId = ns("convertir_caracter"),
                width = "100%",
                "Carácter"
              )),
              column(width = 4, actionButton(
                inputId = ns("convertir_numerico"),
                width = "100%",
                "Numérico"
              )),
              column(width = 4, actionButton(
                inputId = ns("reemplazar_na"),
                width = "100%",
                "Llenar vacios"
              ))
          )),
          tags$br(),
          fluidRow(
            div(class = "botones_convertir_fila_2",
              column(width = 6, actionButton(
                inputId = ns("quitar_duplicados"),
                width = "100%",
                "Quitar duplicados"
              )),
              column(width = 6, actionButton(
                inputId = ns("remover"),
                width = "100%",
                "Remover"
              ))
          ))
          
        )
      )
    )
  )
}

columnas_server <- function(input, output, session, datos, nombre_id) {
  
  ns <- NS(nombre_id)
  
  opciones_columnas <- reactiveValues()
  
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
        ) %>%
          formatStyle(
            columns = 1,
            backgroundColor = "white",
            fontSize = '95%',
            "white-space"="nowrap"
          )
      })
    }
  })
  
  observeEvent(input$columnas_cell_edit, {
    setnames(
      x = datos$data_original,
      input$columnas_cell_edit$row,
      input$columnas_cell_edit$value
    )
    setnames(
      x = datos$data_table,
      input$columnas_cell_edit$row,
      input$columnas_cell_edit$value
    )
    datos$colnames <- names(datos$data_table)
  })
  
  # Resumen de la columna seleccionada
  
  actualizar_resumen <- reactive({
    list(input$columnas_rows_selected, 
         input$convertir_caracter_confirmar,
         input$convertir_numerico_confirmar,
         input$quitar_duplicados_confirmar,
         datos$filtros_aplicados,
         datos$colnames)
  })
  
  output$resumen_columna <- renderText({
    if (!is.null(input$columnas_rows_selected)) {
      opciones_columnas$resumen
    }
  })
  
  observeEvent(actualizar_resumen(), {
    if (!is.null(input$columnas_rows_selected)) {
      tryCatch(
        expr = {
          opciones_columnas$resumen <- resumen(
            datos$data_table[[input$columnas_rows_selected]])
        },
        error = function(e) {}
      )
    }
  })
  
  # Convertir a carácter
  
  observeEvent(input$convertir_caracter, {
    if (!is.null(input$columnas_rows_selected)) {
      confirmSweetAlert(
        session = session,
        inputId = ns("convertir_caracter_confirmar"),
        title = "Confirmar conversión", 
        text = "Este cambio no es reversible.",
        btn_labels = c("Cancelar", "Confirmar")
      )
    }
  })
  
  observeEvent(input$convertir_caracter_confirmar, {
    if (input$convertir_caracter_confirmar && 
        !is.null(input$columnas_rows_selected)) {
      datos$data_original[, datos$colnames[input$columnas_rows_selected] := 
                         as.character(
                           get(datos$colnames[input$columnas_rows_selected]))]
      datos$data_table <- copy(datos$data_original)
    }
  })
  
  # Convertir a numerico
  
  observeEvent(input$convertir_numerico, {
    if (!is.null(input$columnas_rows_selected)) {
      confirmSweetAlert(
        session = session,
        inputId = ns("convertir_numerico_confirmar"),
        title = "Confirmar conversión", 
        text = "Este cambio no es reversible.",
        btn_labels = c("Cancelar", "Confirmar")
      )
    }
  })
  
  observeEvent(input$convertir_numerico_confirmar, {
    if (input$convertir_numerico_confirmar && 
        !is.null(input$columnas_rows_selected)) {
      datos$data_original[, datos$colnames[input$columnas_rows_selected] := 
                         as.numeric(as.character(
                           get(datos$colnames[input$columnas_rows_selected])))]
      datos$data_table <- copy(datos$data_original)
    }
  })
  
  # Quitar duplicados
  
  observeEvent(input$quitar_duplicados, {
    if (!is.null(input$columnas_rows_selected)) {
      confirmSweetAlert(
        session = session,
        inputId = ns("quitar_duplicados_confirmar"),
        title = "Confirmar eliminación", 
        text = "Este cambio no es reversible.",
        btn_labels = c("Cancelar", "Confirmar")
      )
    }
  })
  
  observeEvent(input$quitar_duplicados_confirmar, {
    if (input$quitar_duplicados_confirmar && 
        !is.null(input$columnas_rows_selected)) {
      datos$data_table <- copy(datos$data_table)[
        !duplicated(get(datos$colnames[input$columnas_rows_selected]))
      ]
    }
  })
  
  # Quitar NA
  
  observeEvent(input$reemplazar_na, {
    if (!is.null(input$columnas_rows_selected)) {
      confirmSweetAlert(
        session = session,
        inputId = ns("reemplazar_na_confirmar"),
        title = "Confirmar eliminación", 
        text = "Este cambio no es reversible.",
        btn_labels = c("Cancelar", "Confirmar")
      )
    }
  })
  
  observeEvent(input$reemplazar_na_confirmar, {
    if (input$reemplazar_na_confirmar && 
        !is.null(input$columnas_rows_selected)) {
      datos$data_original[is.na(get(datos$colnames[input$columnas_rows_selected])),
                       datos$colnames[input$columnas_rows_selected] := "NULO"]
      datos$data_table <- copy(datos$data_original)
    }
  })
  
  # Remover columnas
  
  observeEvent(input$remover, {
    if (!is.null(input$columnas_rows_selected)) {
      confirmSweetAlert(
        session = session,
        inputId = ns("remover_confirmar"),
        title = "Remover columna", 
        text = "¿Seguro que desea remover la columna seleccionada?",
        btn_labels = c("Cancelar", "Confirmar")
      )
    }
  })
  
  observeEvent(input$remover_confirmar, {
    if (input$remover_confirmar && 
        !is.null(input$columnas_rows_selected)) {
      datos$data_original[, datos$colnames[input$columnas_rows_selected] := 
                         NULL]
      datos$data_table <- copy(datos$data_original)
      datos$colnames <- NULL
      datos$colnames <- colnames(datos$data_table)
      columnas_num <- unlist(lapply(datos$data_table[1,], is.numeric))
      datos$colnames_num <- NULL
      datos$colnames_num <- datos$colnames[columnas_num]
    }
  })
  
}