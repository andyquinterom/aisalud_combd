agrupadores_ui <- function(id) {
  ns <- NS(id)

  tagList(
    fluidRow(
      column(
        width = 6,
        selectizeInput(
          inputId = ns("llave_foranea"),
          multiple = TRUE,
          label = "Llave foranea:",
          choices = "Ninguno",
          width = "100%"
        )
      ),
      column(
        width = 6,
        selectizeInput(
          inputId = ns("llave_primaria"),
          multiple = TRUE,
          label = "Llave primaria:",
          choices = "Ninguno",
          width = "100%"
        )
      )
    ),
    fluidRow(
      column(
        width = 12,
        selectizeInput(
          inputId = ns("agrupadores"),
          multiple = TRUE,
          label = "Agrupadores:",
          choices = "Ninguno",
          width = "100%"
        ),
        actionGroupButtons(
          inputIds = ns(c("agrupar_left", "agrupar_inner")),
          labels = c("Left join", "Inner Join"),
          direction = "horizontal"
        )
      )
    )
  )
}

agrupadores_server <- function(id, opciones, opciones_agrupadores) {

  ns <- NS(id)

  moduleServer(
    id = id,
    module = function(input, output, session) {

      observe({
        updateSelectizeInput(
          session = session,
          inputId = "llave_foranea",
          choices = opciones$colnames
        )
      })

      observe({
        updateSelectizeInput(
          session = session,
          inputId = "llave_primaria",
          choices = opciones_agrupadores$colnames
        )
        updateSelectizeInput(
          session = session,
          inputId = "agrupadores",
          choices = opciones_agrupadores$colnames
        )
      })

      observeEvent(input$agrupar_left, {
        tryCatch(
          expr = {
            if (!is.null(input$llave_foranea) &&
                !is.null(input$llave_primaria) &&
                !is.null(input$agrupadores)) {
              llave_foranea <- input$llave_foranea
              llave_primaria <- input$llave_primaria
              agrupadores <- input$agrupadores

              tabla_temp_agrupadores <- opciones_agrupadores$tabla %>%
                select(!!!rlang::syms(c(llave_primaria, agrupadores)))

              coltypes_agrupador <- opciones_agrupadores$coltypes

              llave_primaria_named <- llave_primaria
              names(llave_primaria_named) <- llave_foranea

              n_cambios <- length(opciones$cambios) + 1

              nombre_cambio <- paste(
                n_cambios, "-", "Left join por",
                paste(llave_foranea, collapse = ", "))

              agrupadores_char <- purrr::map(
                .x = agrupadores,
                .f = function(y) {
                  tipo_columna <- coltypes_agrupador[[y]]
                  if (identical(tipo_columna, "character")) {
                    return(y)
                  } else {
                    return(NULL)
                  }
                }
              ) %>% unlist()

              opciones$cambios[[nombre_cambio]] <-
                function(x) {
                  left_join(x = x, y = tabla_temp_agrupadores,
                            by = llave_primaria_named,
                            suffix = c(".datos", ".agrupadores")) %>%
                    {if (!is.null(agrupadores_char)) {
                      mutate(.data = ., across(
                        .cols = starts_with(!! agrupadores_char),
                        .fns = ~ case_when(
                          is.na(.) ~ "DATO NO AGRUPADO",
                          TRUE ~ .
                        )))
                    } else {.}}
                }
            }
          },
          error = function(e) {
            print(e)
            showNotification(
              ui = "Error agrupando.",
              type = "error",
              session = session
            )
          }
        )
      })

      observeEvent(input$agrupar_inner, {
        tryCatch(
          expr = {
            if (!is.null(input$llave_foranea) &&
                !is.null(input$llave_primaria) &&
                !is.null(input$agrupadores)) {
              llave_foranea <- input$llave_foranea
              llave_primaria <- input$llave_primaria
              agrupadores <- input$agrupadores

              tabla_temp_agrupadores <- opciones_agrupadores$tabla %>%
                select(!!!rlang::syms(c(llave_primaria, agrupadores)))

              coltypes_agrupador <- opciones_agrupadores$coltypes

              llave_primaria_named <- llave_primaria
              names(llave_primaria_named) <- llave_foranea

              n_cambios <- length(opciones$cambios) + 1

              nombre_cambio <- paste(
                n_cambios, "-", "Inner join por",
                paste(llave_foranea, collapse = ", "))

              agrupadores_char <- purrr::map(
                .x = agrupadores,
                .f = function(y) {
                  tipo_columna <- coltypes_agrupador[[y]]
                  if (identical(tipo_columna, "character")) {
                    return(y)
                  } else {
                    return(NULL)
                  }
                }
              ) %>% unlist()

              opciones$cambios[[nombre_cambio]] <-
                function(x) {
                  inner_join(x = x, y = tabla_temp_agrupadores,
                             by = llave_primaria_named,
                             suffix = c(".datos", ".agrupadores"))
                }
            }
          },
          error = function(e) {
            print(e)
            showNotification(
              ui = "Error agrupando.",
              type = "error",
              session = session
            )
          }
        )
      })


    }
  )

}

