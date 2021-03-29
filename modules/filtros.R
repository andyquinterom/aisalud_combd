filtros_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    tags$div(
      class = "filtros",
      fluidRow(
        column(
          width = 5,
          tags$h3("Variables")
        ),
        column(
          width = 2
        ),
        column(
          width = 5,
          tags$h3("Valores")
        )
      ),
      tags$div(
        class = "filtros_char",
          filtro_discreto_ui_insert(ns = ns, n = 5)
        ),
      actionButton(ns("aplicar_filtros"), "Aplicar")
    )
  )
}

filtros_server <- function(input, output, session, opciones) {

  n_num = 3
  n_char = 5
  
  selected_cache <- reactiveValues(
    "num" = rep("Ninguno", n_num),
    "char" = rep("Ninguno", n_char)
  )
  
  observeEvent(opciones$colnames, {
    if (!is.null(opciones$colnames)) {
      lapply(
        X = 1:n_char,
        FUN = function(x) {
          updateSelectizeInput(
            session = session,
            inputId = paste("filtro_char_columna", x, sep = "_"),
            choices = c("Ninguno", opciones$colnames),
            selected = ifelse(
              test = selected_cache$char[x] %in% c("Ninguno", opciones$colnames),
              yes = selected_cache$char[x],
              no = {selected_cache$char[x] <- "Ninguno"
              "Ninguno"})
          )
        }
      )
    }
  })
  
  lapply(
    X = 1:n_char,
    FUN = function(i) {
      observeEvent(input[[paste0("filtro_char_columna_", i)]], {
        columna_selected <- input[[paste0("filtro_char_columna_", i)]]
        
        if (!any(c("", "Ninguno") %in% columna_selected)) {
          updateSelectizeInput(
            session = session,
            inputId = paste0("filtro_char_valor_", i),
            server = TRUE,
            choices = opciones$tabla %>%
              group_by(!!rlang::sym(columna_selected)) %>%
              count() %>% 
              pull(!!rlang::sym(columna_selected))
          )
        }
      
      })
    }
  )

  observeEvent(input$aplicar_filtros, {

    inputs_filtros_char <- c()

    lapply(
      X = 1:n_char,
      FUN = function(i) {
        selected_cache$char[i] <- input[[paste0("filtro_char_columna_", i)]]
        
        if (!any(c("Ninguno", "") %in% 
                input[[paste0("filtro_char_columna_", i)]])) {
          
          n_cambios <- length(opciones$cambios) + 1
          columna_filtro <- input[[paste0("filtro_char_columna_", i)]]
          columna_filtro_val <- input[[paste0("filtro_char_valor_", i)]]
          columna_filtro_incluir <- input[[paste0("filtro_char_incluir_", i)]]
          nombre_filtro <- paste(
            n_cambios, "-", 
            "Filtro de",
            ifelse(test = columna_filtro_incluir,
                   yes = "inclusion", no = "exclusion"),
            "en", columna_filtro)
          
          opciones$cambios[[nombre_filtro]] <- 
            function(x) {
              if (columna_filtro_incluir) {
                filter(x, !!as.name(columna_filtro) %in% columna_filtro_val)
              } else {
                filter(x, !(!!as.name(columna_filtro) %in% columna_filtro_val))
              }
            }
        }
      }
    )

  })
  
}


# Funciones ----------------

filtro_discreto_ui_fila <- function(ns, position = 1) {
  fluidRow(
    column(
      width = 5,
      selectizeInput(
        inputId = ns(paste("filtro_char_columna", position, sep = "_")),
        label = NULL,
        choices = "Ninguno",
        selected = "Ninguno",
        multiple = FALSE,
        width = "100%"
      )),
    column(
      width = 2,
      shinyWidgets::switchInput(
        inputId = ns(paste("filtro_char_incluir", position, sep = "_")),
        onLabel = "Incluir",
        offLabel = "Excluir",
        value = TRUE,
        width = "100%"
      )
    ),
    column(
      width = 5,
      selectizeInput(
        inputId = ns(paste("filtro_char_valor", position, sep = "_")),
        label = NULL,
        choices = "Ninguno",
        selected = "Ninguno",
        multiple = TRUE,
        width = "100%"
      ))
  )
}

filtro_discreto_ui_insert <- function(ns, n) {
  
  filtros_filas <- list()
  
  for (i in 1:n) {
    filtros_filas[[i]] <- filtro_discreto_ui_fila(
      ns = ns, 
      position = i)
  }
  
  return(filtros_filas)
  
}

filtro_numerico_ui_fila <- function(ns, position = 1) {
  fluidRow(
    column(
      width = 5,
      pickerInput(
        inputId = ns(paste("filtro_num_columna", position, sep = "_")),
        choices = c("Ninguno"),
        width = "100%")),
    column(
      width = 2
    ),
    column(
      width = 5,
      fluidRow(
        column(
          width = 6,
          numericInput(
            inputId = ns(paste("filtro_num_min", position, sep = "_")),
            label = NULL,
            value = 0,
            min = 0,
            max = 0, 
            width = "100%")),
        column(
          width = 6,
          numericInput(
            inputId = ns(paste("filtro_num_max", position, sep = "_")),
            label = NULL, 
            value = 0,
            min = 0,
            max = 0,
            width = "100%"))
      )
    )
    )
}

filtro_numerico_ui_insert <- function(ns, n) {
  
  filtros_filas <- list()
  
  filtros_filas[[1]] <-  fluidRow(
    column(width = 7),
    column(
      width = 5,
      fluidRow(
        column(
          width = 6,
          tags$p("Mínimo", style = "font-size: 100%;")),
        column(
          width = 6,
          tags$p("Máximo", style = "font-size: 100%;"))
      )
    )
  )
    
  
  for (i in 1:n + 1) {
    filtros_filas[[i]] <- filtro_numerico_ui_fila(
      ns = ns, 
      position = i - 1)
  }
  
  return(filtros_filas)
  
}

filtros_pacientes_ui_fila <- function(ns) {
  fluidRow(
    column(
      width = 3,
      pickerInput(
        inputId = ns("filtro_paciente"),
        label = NULL,
        choices = "Pacientes",
        selected = "Pacientes",
        multiple = FALSE
      )),
    column(
      width = 2,
      actionButton(
        inputId = ns("filtro_paciente_vaciar"),
        label = "Vaciar",
        width = "100%"
      )
    ),
    column(
      width = 2,
      shinyWidgets::switchInput(
        inputId = ns("filtro_paciente_incluir"),
        onLabel = "Incluir",
        offLabel = "Excluir",
        value = FALSE
      )
    ),
    column(
      width = 5,
      selectizeInput(
        inputId = ns("filtros_paciente_valor"),
        label = NULL,
        choices = NULL,
        selected = NULL,
        multiple = TRUE
      )
    )
  )
}