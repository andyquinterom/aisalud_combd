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
              column(width = 6, actionButton(
                inputId = ns("convertir_caracter"),
                width = "100%",
                "Carácter"
              )),
              column(width = 6, actionButton(
                inputId = ns("convertir_numerico"),
                width = "100%",
                "Numérico"
              ))
          )),
          tags$br(),
          fluidRow(
            column(width = 12, actionButton(
              inputId = ns("reemplazar_na"),
              width = "100%",
              "Llenar vacios"
            ))
          ),
          tags$br(),
          fluidRow(
            div(class = "botones_convertir_fila_2",
              column(width = 12, actionButton(
                inputId = ns("quitar_duplicados"),
                width = "100%",
                "Quitar duplicados"
              ))
          )),
          uiOutput(ns("muestreo"))
        )
      )
    )
  )
}

columnas_server <- function(id, opciones) {
  
  ns <- NS(id)
  
  moduleServer(
    id = id,
    module = function(input, output, session) {
      
      columnas <- reactiveValues(tabla = c())
      
      observe({
        if (!is.null(opciones$tabla_original)) {
          tryCatch(
            expr = {
              opciones$colnames <- opciones$tabla %>% 
                colnames()
              
              opciones$coltypes <- opciones$tabla %>% 
                head(0) %>% 
                collect() %>% 
                summarise_all(class)
              
              columnas$tabla <- data.frame(
                "Columnas" = as.character(opciones$colnames))
            },
            error = function(e) {
              print(e)
              sendSweetAlert(
                session = session,
                title = "Error",
                text = "Valide que la tabla aun exista.",
                type = "error"
              )
            }
          )
          
        }
      })
      
      observeEvent(opciones$colnames, {
        if (!is.null(opciones$colnames)) {
          output$columnas <- DT::renderDataTable(server = FALSE, {
            datatable(
              data = columnas$tabla,
              rownames = FALSE,
              editable = TRUE,
              selection = "single",
              options = list(
                ordering = F,
                pageLength = length(opciones$colnames),
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
        
        tryCatch(
          expr = {
            columna_cambio <- input$columnas_cell_edit$row
            viejo_nombre <- opciones$colnames[columna_cambio]
            nuevo_nombre <- input$columnas_cell_edit$value %>% 
              stri_trans_general(id = "Latin-ASCII") %>% 
              str_replace_all("\\s", "_") %>% 
              str_replace_all('[^0-9a-zA-Z]+', "_")
            n_cambios <- length(opciones$cambios) + 1
            
            if (nchar(nuevo_nombre) == 0) {
              nombre_cambio <- paste(n_cambios, "-", "eliminar", viejo_nombre)
              opciones$cambios[[nombre_cambio]] <- 
                function(x) {select(x, -c(columna_cambio))}
            } else {
              nombre_cambio <- paste(n_cambios, "-", viejo_nombre, "a", nuevo_nombre)
              opciones$cambios[[nombre_cambio]] <- 
                function(x) {rename(x, !!nuevo_nombre := columna_cambio)}
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

      # Resumen de la columna seleccionada

      output$resumen_columna <- renderText({
        if (!is.null(input$columnas_rows_selected)) {
          opciones$coltypes[[input$columnas_rows_selected]]
        }
      })

      # Convertir a carácter
      
      observeEvent(input$convertir_caracter, {
        if (!is.null(input$columnas_rows_selected)) {
          col_mod <- opciones$colnames[input$columnas_rows_selected]
          col_mod_type <- opciones$coltypes[[input$columnas_rows_selected]]
          n_cambios <- length(opciones$cambios) + 1
          
          nombre_cambio <- paste(
            n_cambios, "-", col_mod, "de", col_mod_type, "a", "character")
          
          if (col_mod_type != "character") {
            opciones$cambios[[nombre_cambio]] <- 
              function(x) {
                mutate(
                  .data = x,
                  !!col_mod := as.character(!!as.name(col_mod)))
              }
          }
          
        }
      })

      # Convertir a numerico

      observeEvent(input$convertir_numerico, {
        if (!is.null(input$columnas_rows_selected)) {
          col_mod <- opciones$colnames[input$columnas_rows_selected]
          col_mod_type <- opciones$coltypes[[input$columnas_rows_selected]]
          n_cambios <- length(opciones$cambios) + 1
          
          nombre_cambio <- paste(
            n_cambios, "-", col_mod, "de", col_mod_type, "a", "numeric")
          
          if (col_mod_type != "numeric") {
            opciones$cambios[[nombre_cambio]] <- 
              function(x) {
                if (opciones$sep_decimal == ",") {
                  mutate(
                    .data = x,
                    !!col_mod := as.numeric(
                      str_replace(!!as.name(col_mod), ",", ".")))
                } else {
                  mutate(
                    .data = x,
                    !!col_mod := as.numeric(!!as.name(col_mod)))
                }
              }
          }
          
        }
      })

      # Quitar duplicados

      observeEvent(input$quitar_duplicados, {
        showModal(
          session = session,
          ui = modalDialog(
            title = "Quitar duplicados",
            easyClose = TRUE,
            fade = TRUE,
            selectizeInput(
              inputId = ns("duplicados_llaves"),
              width = "100%",
              label = "Llaves primarias",
              choices = opciones$colnames,
              multiple = TRUE
            ),
            footer = actionButton(
              inputId = ns("duplicados_confirmar"),
              label = "Quitar duplicados")
          )
        )
      })
      
      observeEvent(input$duplicados_confirmar, {
        if (!("" %in% input$duplicados_llaves)) {
          col_mod <- input$duplicados_llaves
          n_cambios <- length(opciones$cambios) + 1
          
          nombre_cambio <- paste(
            n_cambios, "-", "Quitar duplicados por", paste(
              input$duplicados_llaves, collapse = ", "))
          
          opciones$cambios[[nombre_cambio]] <- 
            function(x) {
              group_by(.data = x, !!!rlang::syms(col_mod)) %>% 
                mutate(across(.cols = everything(), .fns = first)) %>% 
                ungroup() %>% 
                distinct()
            }
        }
      })

      # Opcion de muestreo

      # output$muestreo <- renderUI({
      #   if (muestreo) {
      #     tagList(
      #       tags$br(),
      #       fluidRow(
      #         div(class = "botones_convertir_fila_2",
      #             column(width = 12, actionButton(
      #               inputId = ns("muestreo"),
      #               width = "100%",
      #               "Muestreo"
      #             ))
      #         ))
      #     )
      #   }
      # })

      # observeEvent(input$muestreo, {
      #   n_unicos <- uniqueN(
      #     x = datos$data_original[[datos$colnames[input$columnas_rows_selected]]])
      #   showModal(
      #     session = session,
      #     ui = modalDialog(
      #       title = "Muestreo",
      #       easyClose = TRUE,
      #       fade = TRUE,
      #       tags$p("Esta opción ayuda a generar muestreos aleatorios de los datos."),
      #       tags$p("Esta funcionalidad no es reversible."),
      #       numericInput(inputId = ns("seed"), label = "Semilla", value = 100),
      #       numericInput(inputId = ns("n_values"), 
      #                    label = "Valor únicos",
      #                    value = round(n_unicos/5),
      #                    max = n_unicos,
      #                    min = 1),
      #       footer = actionButton(
      #         inputId = ns("muestreo_confirmar"),
      #         label = "Ejecutar muestreo")
      #     )
      #   )
      # })

      # observeEvent(input$muestreo_confirmar, {
      #   set.seed(100)
      #   muestreo_datos <- sample(
      #     x = {
      #       datos$data_original[[datos$colnames[input$columnas_rows_selected]]] %>%
      #         unique()
      #     },
      #     size = input$n_values,
      #     replace = FALSE)
      #   
      #   datos$data_original <- 
      #     datos$data_original[get(datos$colnames[input$columnas_rows_selected])
      #                         %in% muestreo_datos,]
      #   
      #   datos$data_table <- copy(datos$data_original)
      #   datos$actualizar_resumen <- NULL
      #   datos$actualizar_resumen <- TRUE
      #   finalizado()
      #   
      # })

      # Quitar NA

      observeEvent(input$reemplazar_na, {
        if (!is.null(input$columnas_rows_selected)) {

          col_mod <- opciones$colnames[input$columnas_rows_selected]
          col_mod_type <- opciones$coltypes[[input$columnas_rows_selected]]
          n_cambios <- length(opciones$cambios) + 1
          
          nombre_cambio <- paste(
            n_cambios, "-", "Llenar vacios en", col_mod)
          
          if (col_mod_type == "numeric") {
            opciones$cambios[[nombre_cambio]] <- 
              function(x) {
                mutate(
                  .data = x,
                  !!col_mod := case_when(
                    is.na(!!as.name(col_mod)) ~ 0,
                    TRUE ~ !!as.name(col_mod)
                  ))
              }
          } else if (col_mod_type == "character") {
            opciones$cambios[[nombre_cambio]] <- 
              function(x) {
                mutate(
                  .data = x,
                  !!col_mod := case_when(
                    is.na(!!as.name(col_mod)) ~ "DATO VACIO",
                    TRUE ~ !!as.name(col_mod)
                  ))
              }
          } else {
            showNotification(
              "No es posible llenar vacios para la columna."
            )
          }
          
        }
      })

    }
  )
   
}