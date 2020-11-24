shinyServer(function(input, output, session) {
  
  # Modulo para cargar datps
  
  datos <- callModule(
    module = prepara_server,
    id = "cargar_datos",
    nombre_id = "cargar_datos"
  )
  
  
})