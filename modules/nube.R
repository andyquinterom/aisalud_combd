nube_ui <- function(id) {
  ns <- NS(id)

  tagList(
    fluidRow(
      tags$br(),
      shinyWidgets::progressBar(
        id = ns("almacenamiento_percent"),
        value = 100,
        display_pct = TRUE,
        striped = TRUE)
    ),
    uiOutput(ns("ui_conectado")) %>% withSpinner()
  )
}

nube_server <- function(id, opciones, opciones_agrupadores) {

  ns <- NS(id)
  contador <- counter()

  moduleServer(
    id = id,
    module = function(input, output, session) {

      opciones_nube <- reactiveValues()

      update_almacenamiento <- reactive({
        list(input$conectar_al_servicio, opciones_nube$resultados_subidas,
             opciones_nube$resultado_eliminacion)
      })

      opciones$actualizar_tablas <- reactiveTimer()

      observe({
        opciones$actualizar_tablas()
        if (!is.null(conn)) {
          opciones_nube$tablas_almacenadas <- dbListTables(conn) %>% {
            if (identical(character(0), .)) {NULL} else {.}}

          opciones_nube$almacenamiento <- check_db_size(
            con = conn,
            database = Sys.getenv("DATABASE_NAME")
          ) %>%
            ifelse(is.null(.),
              yes = 0,
              no = .)

          opciones_nube$almacenamiento_total <- Sys.getenv(
            "DATABASE_MAX_STORAGE"
          ) %>%
            as.numeric()

          opciones_nube$tablas_almacenadas <- dbListTables(conn) %>% {
            if (identical(character(0), .)) {NULL} else {.}}
        }
      })

      observeEvent(input$subir_tabla, {
        if (!is.null(opciones$colnames)) {
          if (opciones_nube$almacenamiento <
              opciones_nube$almacenamiento_total) {
            fecha_incluida <- "Date" %in%
              opciones$coltypes[["fecha_prestacion"]]
            showModal(
              session = session,
              ui = modalDialog(
                easyClose = TRUE,
                title = "Nombre de la tabla",
                textInput(
                  inputId = ns("subir_tabla_nombre"),
                  label = "",
                  width = "100%"
                ),
                if (!fecha_incluida) {
                  fluidRow(
                    column(
                      width = 6,
                      selectInput(
                        inputId = ns("subir_tabla_fecha_columna"),
                        label = "Columna de fecha:",
                        choices = as.character(opciones$colnames),
                        selected = "fecha_prestacion"
                      )
                    ),
                    column(
                      width = 6,
                      textInput(
                        inputId = ns("subir_tabla_fecha_formato"),
                        label = "Formato de fecha",
                        value = "DD/MM/YYYY"
                      )
                    )
                  )
                },
                fluidRow(
                  column(
                    width = 6,
                    selectInput(
                      inputId = ns("subir_tabla_nro_identificacion"),
                      label = "Columna de número de identificacion:",
                      choices = as.character(opciones$colnames),
                      selected = "nro_identificacion"
                    )
                  ),
                  column(
                    width = 6,
                    selectInput(
                      inputId = ns("subir_tabla_valor_columna"),
                      label = "Columna de valor:",
                      choices = as.character(opciones$colnames),
                      selected = "valor"
                    )
                  )
                ),
                fluidRow(
                  column(
                    width = 6,
                    selectInput(
                      inputId = ns("subir_tabla_cantidad"),
                      label = "Columna de número de cantidad:",
                      choices = as.character(opciones$colnames),
                      selected = "cantidad"
                    )
                  )
                ),
                includeMarkdown("markdown/subir_tabla.md"),
                DT::dataTableOutput(
                  outputId = ns("preview")
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

      output$preview <- DT::renderDataTable({
        opciones$tabla %>%
          select(!!!rlang::syms(c(
            input$subir_tabla_nro_identificacion,
            input$subir_tabla_valor_columna,
            input$subir_tabla_cantidad,
            ifelse(
              test = !is.null(input$subir_tabla_fecha_columna),
              yes = input$subir_tabla_fecha_columna,
              no = "fecha_prestacion"
            )))) %>%
          head(3) %>%
          collect() %>%
          datatable(
            options = list(
              language = list(url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Spanish.json'),
              dom = 't'
            )
          )
      })

      observeEvent(input$subir_tabla_confirmar, {
        if (input$subir_tabla_nombre != "") {
          fecha_incluida <- "Date" %in% opciones$coltypes[["fecha_prestacion"]]
          columna_nro_identificacion <- input$subir_tabla_nro_identificacion
          columna_valor <- input$subir_tabla_valor_columna
          columna_cantidad <- input$subir_tabla_cantidad
          if (!fecha_incluida) {
            columna_fecha <- input$subir_tabla_fecha_columna
            columna_fecha_formato <- input$subir_tabla_fecha_formato
            fecha_incluida <-
              "Date" %in% opciones$coltypes[[columna_fecha]]
          } else {
            columna_fecha <- "fecha_prestacion"
          }
          sep_decimal_coma <- opciones$sep_decimal == ","

          columna_valor_numerica <-
            "numeric" %in% opciones$coltypes[[columna_valor]]
          columna_cantidad_numerica <-
            "numeric" %in% opciones$coltypes[[columna_cantidad]]

          removeModal(session = session)
          if (opciones_nube$almacenamiento <
            opciones_nube$almacenamiento_total) {
            nombre_tabla <- paste0("ais_", tolower(input$subir_tabla_nombre))
            overwrite <- !identical(opciones$nombre_tabla, nombre_tabla)
            tryCatch(
              expr = {
                withProgress(
                  message = "Subiendo base de datos a la nube.",
                  expr = {
                    if (!"temporal_meses" %in% dbListTables(conn)) {
                      dbWriteTable(
                        conn = conn,
                        name = "temporal_meses",
                        value = data.frame(
                          ais_mes = 1:12,
                          ais_mes_nombre = c(
                            "Enero",
                            "Febrero",
                            "Marzo",
                            "Abril",
                            "Mayo",
                            "Junio",
                            "Julio",
                            "Agosto",
                            "Septiembre",
                            "Octubre",
                            "Noviembre",
                            "Diciembre"
                          )
                        )
                      )
                    }
                    opciones$tabla %>%
                      rename(
                        fecha_prestacion = !!as.name(columna_fecha),
                        valor = !!as.name(columna_valor),
                        cantidad = !!as.name(columna_cantidad),
                        nro_identificacion =
                          !!as.name(columna_nro_identificacion)) %>%
                      {if (!fecha_incluida) {
                        mutate(., fecha_prestacion = TO_DATE(
                          fecha_prestacion, columna_fecha_formato))
                      } else {.}} %>%
                      {if (sep_decimal_coma &&
                           (!columna_valor_numerica) &&
                           (!columna_cantidad_numerica)) {
                        mutate(
                          .data = .,
                          valor = str_replace(valor, ",", "."),
                          cantidad = str_replace(cantidad, ",", "."))
                      } else {.}} %>%
                      mutate(
                        valor = as.numeric(valor),
                        cantidad = as.numeric(cantidad),
                        ais_mes = month(fecha_prestacion),
                        ais_anio = year(fecha_prestacion)) %>%
                      left_join(tbl(conn, "temporal_meses")) %>%
                      lazy_to_postgres(
                        nombre = nombre_tabla,
                        conn = conn,
                        overwrite = overwrite
                      )

                    index_query <- str_replace_all(
                      'CREATE INDEX ON "#tabla#" USING btree
                      (fecha_prestacion ASC NULLS LAST)
                      INCLUDE(fecha_prestacion)',
                      pattern = "#tabla#",
                      replacement = nombre_tabla)

                    dbExecute(
                      conn = conn,
                      statement = index_query
                    )

                  })
              },
              error = function(e) {
                print(e)
                showNotification(
                  ui = paste(
                    "Error subiendo la base de datos.", e, sep = "\n"),
                  type = "error",
                  session = session
                )
              }
            )
          } else {
            showNotification(
              type = "warning",
              ui = "Almacenamiento lleno."
            )
          }
          opciones_nube$resultados_subidas <- NULL
        } else {
          showNotification(
            ui = "El nombre no puede ser vacio.",
            type = "error",
            session = session
          )
        }
      })

      observeEvent(input$subir_agrupadores, {
        if (!is.null(opciones_agrupadores$colnames)) {
          if (opciones_nube$almacenamiento <
              opciones_nube$almacenamiento_total) {
            showModal(
              session = session,
              ui = modalDialog(
                title = "Nombre de la tabla de agrupadores",
                textInput(
                  inputId = ns("subir_agrupadores_nombre"),
                  label = "",
                  width = "100%"
                ),
                footer = actionButton(
                  inputId = ns("subir_agrupadores_confirmar"),
                  label = "Subir agrupadores")
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

      observeEvent(input$subir_agrupadores_confirmar, {
        if (input$subir_agrupadores_nombre != "") {
          removeModal(session = session)

          if (opciones_nube$almacenamiento < opciones_nube$almacenamiento_total) {
            nombre_tabla <- paste0(
              "agrupador_",
              tolower(input$subir_agrupadores_nombre))
            tryCatch(
              expr = {
                withProgress(
                  message = "Subiendo base de datos a la nube.",
                  expr = {

                    opciones_agrupadores$tabla %>%
                      lazy_to_postgres(
                        nombre = nombre_tabla,
                        conn = conn
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
          } else {
            showNotification(
              type = "warning",
              ui = "Almacenamiento lleno."
            )
          }
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

      output$tablas_lista <- DT::renderDataTable(server = FALSE, {
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
                if (!str_detect(
                    opciones_nube$tablas_almacenadas[
                      input$tablas_lista_rows_selected],
                    "(^temporal_|^ais_BASE)")) {
                  dbRemoveTable(
                    con = conn,
                    name = opciones_nube$tablas_almacenadas[
                      input$tablas_lista_rows_selected]
                  )
                  opciones_nube$resultado_eliminacion <- contador()
                } else {
                  showNotification("Las tablas temporales no se puede eliminar.")
                }
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
        validar_temporal <- str_detect(
          opciones$nombre_tabla,
          "(^temporal|^ais_BASE)"
        )
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
              actionGroupButtons(
                inputIds = ns(c(
                  "subir_tabla",
                  if (!validar_temporal) "reemplazar_tabla",
                  "subir_agrupadores"
                )),
                labels = c(
                  "Subir tabla",
                  if (!validar_temporal) "Actualizar tabla",
                  "Subir agrupadores"
                ),
                fullwidth = TRUE
              )
            )
          )
        }
      })

      observe({
        confirmSweetAlert(
          session = session,
          inputId = ns("reemplazar_tabla_confirmar"),
          title = "Confirmar actualización",
          text = "Este cambio no es reversible. Perderá la opción de undo.",
          btn_labels = c("Cancelar", "Confirmar")
        )
      }) %>%
        bindEvent(input$reemplazar_tabla)

      observe({
        nombre_tabla <- opciones$nombre_tabla
        nombre_hash <- digest::digest(
          object = nombre_tabla,
          algo = "xxhash32",
          seed = 1
        )
        nombre_tabla_hash <- paste0("temporal_", nombre_hash)
        tryCatch(
          expr = {
            withProgress(
              message = "Subiendo base de datos a la nube.",
              expr = {
                if (!"temporal_meses" %in% dbListTables(conn)) {
                  dbWriteTable(
                    conn = conn,
                    name = "temporal_meses",
                    value = data.frame(
                      ais_mes = 1:12,
                      ais_mes_nombre = c(
                        "Enero",
                        "Febrero",
                        "Marzo",
                        "Abril",
                        "Mayo",
                        "Junio",
                        "Julio",
                        "Agosto",
                        "Septiembre",
                        "Octubre",
                        "Noviembre",
                        "Diciembre"
                      )
                    )
                  )
                }
                opciones$tabla %>%
                  mutate(
                    valor = as.numeric(valor),
                    cantidad = as.numeric(cantidad),
                    ais_mes = month(fecha_prestacion),
                    ais_anio = year(fecha_prestacion)) %>%
                  left_join(tbl(conn, "temporal_meses")) %>%
                  lazy_to_postgres(
                    nombre = nombre_tabla_hash,
                    conn = conn
                  )

                dbRemoveTable(
                  conn = conn,
                  name = nombre_tabla
                )

                rename_query <- str_replace_all(
                  'ALTER TABLE "#hash#"
                  RENAME TO "#nombre_tabla#"',
                  pattern = "#hash#",
                  replacement = nombre_tabla_hash
                ) %>%
                  str_replace_all(
                    pattern = "#nombre_tabla#",
                    replacement = nombre_tabla
                  )

                dbExecute(
                  conn = conn,
                  statement = rename_query
                )

                index_query <- str_replace_all(
                  'CREATE INDEX ON "#tabla#" USING btree
                  (fecha_prestacion ASC NULLS LAST)
                  INCLUDE(fecha_prestacion)',
                  pattern = "#tabla#",
                  replacement = nombre_tabla
                )

                dbExecute(
                  conn = conn,
                  statement = index_query
                )

                opciones$tabla_actualizada <- contador()

              })
          },
          error = function(e) {
            print(e)
            showNotification(
              ui = paste(
                "Error subiendo la base de datos.", e, sep = "\n"),
              type = "error",
              session = session
            )
          }
        )
        opciones_nube$resultados_subidas <- NULL
      }) %>%
        bindEvent(input$reemplazar_tabla_confirmar)

    }
  )

}



# curl -X GET "https://users.indexmic.com/content/29/almacenamiento" -H  "accept: */*"
