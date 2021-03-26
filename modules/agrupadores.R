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
        if (!("" %in% input$llave_foranea) &&
            !("" %in% input$llave_primaria) &&
            !("" %in% input$agrupadores)) {
          llave_foranea <- input$llave_foranea
          llave_primaria <- input$llave_primaria
          agrupadores <- input$agrupadores
          
          tabla_temp_agrupadores <- opciones_agrupadores$tabla %>% 
            select(!!!rlang::syms(c(llave_primaria, agrupadores)))
          
          llave_primaria_named <- llave_primaria
          names(llave_primaria_named) <- llave_foranea
          
          n_cambios <- length(opciones$cambios) + 1
          
          nombre_cambio <- paste(
            n_cambios, "-", "Left join por", 
            paste(llave_foranea, collapse = ", "))
          
          opciones$cambios[[nombre_cambio]] <- 
            function(x) {
              left_join(x = x, y = tabla_temp_agrupadores,
                        by = llave_primaria_named,
                        suffix = c(".datos", ".agrupadores")) %>%
                mutate(across(
                  .cols = starts_with(!! agrupadores),
                  .fns = ~ case_when(
                    is.na(.) ~ "DATO NO AGRUPADO",
                    TRUE ~ .
                  )))
            }
        }
      })
      
      # observeEvent(input$ejecutar_agrupadores, {
      #   tryCatch(
      #     expr = {
      #       columnas_y <- c(input$llave_primaria, input$agrupadores)
      #       datos_agrupados <- merge.data.table(
      #         x = datos$data_table,
      #         y = agrupadores$data_table[, c(columnas_y), with = FALSE],
      #         by.x = input$llave_foranea,
      #         by.y = input$llave_primaria,
      #         all.x = TRUE
      #       )
      #       
      #       datos$data_original <- datos_agrupados
      #       datos$data_table <- datos_agrupados
      #       datos$colnames <- NULL
      #       datos$colnames <- colnames(datos$data_table)
      #       datos$colnames_num <- NULL
      #       columnas_num <- unlist(lapply(datos$data_table[1,], is.numeric))
      #       datos$colnames_num <- datos$colnames[columnas_num]
      #       showNotification(
      #         ui = "Agrupación finalizada.",
      #         session = session
      #       )
      #     },
      #     error = function(e) {
      #       print(e)
      #       showNotification(
      #         ui = "Error agrupando.",
      #         type = "error",
      #         session = session
      #       )
      #     }
      #   )
      #   
      # })
      
      
      
    }
  )
  
}
  
