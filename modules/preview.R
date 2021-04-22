preview_ui <- function(id) {
  
  ns <- NS(id)
  
  tagList(
    tags$br(),
    DT::dataTableOutput(
      outputId = ns("preview_tabla"),
      width = "100%"
    )
  )
  
}

preview_server <- function(id, opciones) {
  
  ns <- NS(id)
  
  moduleServer(
    id = id,
    module = function(input, output, session) {
      
      output$preview_tabla <- DT::renderDataTable({
        if (!is.null(opciones$tabla)) {
          opciones$tabla %>% 
            head(3) %>% 
            collect() %>% 
            datatable(
              rownames = FALSE, 
              options = list(
                language = list(url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Spanish.json'),
                scrollX = TRUE
              )
            )
        }
        
      })
      
    }
  )
  
}