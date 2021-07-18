prepara_ui <- function(id) {

  ns <- NS(id)

  tagList(
    tabsetPanel(
      tabPanel(
        title = "Seleccionar datos",
        tags$br(),
        selectInput(
          inputId = ns("nube_tablas"),
          label = "Tabla",
          choices = "Ninguno",
          width = "100%"
        ),
        tags$br(),
        actionGroupButtons(
          inputIds = ns(c("undo", "export_sql")),
          labels = c("Undo", "Exportar SQL"),
          fullwidth = TRUE
        ),
        div(verbatimTextOutput(ns("logs")),
             class = "error_logs")
      ),
      tabPanel(
        title = "Subir datos",
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
            label = "Opciones archivo",
            width = "100%")),
          column(width = 6, actionButton(
            inputId = ns("file_load"),
            label = "Aplicar",
            width = "100%")))
      ),
      tabPanel(
        title = "Opciones",
        tags$br(),
        radioButtons(
          inputId = ns("value_decimal"),
          choiceNames = c("Punto", "Coma"),
          choiceValues = c(".", ","),
          label = "Separador decimal",
          inline = TRUE,
          selected = "."
        )
      )
    )
  )
}

prepara_server <- function(id, opciones, prefix = "ais_") {

  ns <- NS(id)

  moduleServer(
    id = id,
    module = function(input, output, session) {

      observe({
        opciones$sep_decimal <- input$value_decimal
      })

      observe({
        tablas_query <- dbListTables(conn = conn) %>%
          unlist() %>%
          unname()

        tablas <- tablas_query[str_starts(
          string = tablas_query, paste(
            paste0("temporal_", prefix), prefix, sep = "|"
          ))]

        if (identical(character(0), tablas)) {
          tablas <- NULL
          updateSelectizeInput(
            session = session,
            inputId = "nube_tablas",
            choices = "Ninguno"
          )
        } else {
          updateSelectizeInput(
            session = session,
            inputId = "nube_tablas",
            choices = c("Ninguno", tablas)
          )
        }
      })

      opciones_prepara <- reactiveValues(
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
        opciones_prepara$value_delimitador <- input$value_delimitador
        opciones_prepara$value_sheet <- input$value_sheet
        opciones_prepara$value_range <- input$value_range
        opciones_prepara$value_file <- input$value_file
        removeModal(session = session)
      })

      observeEvent(input$file_load, {
        tryCatch(
          expr = {
            withProgress(
              min = 0,
              max = 1,
              value = 0.2,
              message = "Cargando datos...",
              expr = {
                if (!is.null(input$file)) {
                  opciones$data_original <- data.frame()
                  file_name <- sub(
                    ".csv$|.feather$|.txt$|.xlsx$",
                    "",
                    basename(input$file$name))
                  if (input$file_type == "csv") {
                    value_delimitador <- ifelse(
                      test = opciones_prepara$value_delimitador == "Tabulado",
                      yes = "\t",
                      no = opciones_prepara$value_delimitador
                    )
                    opciones$data_original <- read_delim(
                      file = input$file$datapath,
                      delim = value_delimitador,
                      col_types = cols(.default = col_character()),
                      locale = locale(
                        encoding = guess_encoding(input$file$datapath)[[1]][1]))
                  }
                  if (input$file_type == "feather") {
                    opciones$data_original <- read_feather(
                      path = input$file$datapath)
                  }

                  incProgress(amount = 0.4)

                  opciones$nombre_tabla <- paste(
                    sep = "",
                    "temporal_", prefix, file_name,
                    round(runif(1, 100, 999), 0))

                  opciones$data_original <- opciones$data_original %>%
                    rename_with(tolower) %>%
                    rename_with(.fn = function(x) {
                      x %>%
                        stri_trans_general(id = "Latin-ASCII") %>%
                        str_replace_all("\\s", "_") %>%
                        str_replace_all('[^0-9a-zA-Z]+', "_")
                    })

                  dbWriteTable(
                    conn = conn,
                    name = opciones$nombre_tabla,
                    value = opciones$data_original,
                    temporary = TRUE)

                  incProgress(amount = 0.2)

                  tablas_query <- dbListTables(conn = conn) %>%
                    unlist() %>%
                    unname()

                  tablas <- tablas_query[str_starts(
                    string = tablas_query, paste(
                      paste0("temporal_", prefix), prefix, sep = "|"
                    ))]

                  if (identical(character(0), tablas)) {
                    tablas <- NULL
                    updateSelectizeInput(
                      session = session,
                      inputId = "nube_tablas",
                      choices = "Ninguno"
                    )
                  } else {
                    updateSelectizeInput(
                      session = session,
                      inputId = "nube_tablas",
                      selected = opciones$nombre_tabla,
                      choices = c("Ninguno", tablas)
                    )
                  }

                  opciones$data_original <- NULL
                  gc(full = TRUE)

                  incProgress(amount = 0.2)
                }
              }
            )
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

      observeEvent(input$nube_tablas, {
        if (input$nube_tablas != "Ninguno") {

          opciones$tabla_original <- tbl(conn, input$nube_tablas)

          opciones$tabla <- opciones$tabla_original

          opciones$cambios <- list()

        }
      })

      observe({
        tryCatch(
          expr = {
            opciones$tabla <- map_func(opciones$tabla_original, opciones$cambios)
          },
          error = function(e) {
            print(e)
            showNotification(
              ui = "No se trabajar con la tabla seleccionada. Valide que aun exista.",
              type = "error",
              session = session
            )
          }
        )
      })

      output$logs <- renderText({
        paste(names(opciones$cambios), collapse = "\n")
      })

      observeEvent(input$undo, {
        n_cambios <- length(opciones$cambios)
        opciones$cambios <- opciones$cambios[-n_cambios]
      })

      observeEvent(input$export_sql, {
        if (!is.null(opciones$tabla)) {
          showModal(
            ui = modalDialog(
              easyClose = TRUE,
              footer = NULL,
              shinyAce::aceEditor(
                ns("export_sql_ace"),
                value = opciones$tabla %>% dbplyr::sql_render() %>% str_remove("<SQL>"),
                mode = "pgsql",
                wordWrap = TRUE
              )
            )
          )
        } else {
          showNotification(
            ui = "No hay una tabla seleccionada",
            type = "warning"
          )
        }
      })

    }
  )

}

# Funciones --------------------------------------------------------------------

datos_opciones_ui <- function(
  id, file_type, value_delimitador, value_sheet, value_range,
  value_file) {

  if (file_type == "csv") {
    return(
      datos_opciones_csv_ui(
        id = id,
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

datos_opciones_csv_ui <- function(id, value_delimitador) {
  ns <- NS(id)

  tagList(
    radioButtons(
      inputId = ns("value_delimitador"),
      choices = c(",", ";", "|", "Tabulado"),
      label = "Delimitador",
      inline = TRUE,
      selected = value_delimitador
    )
  )

}
