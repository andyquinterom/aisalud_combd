shinyServer(function(input, output, session) {
  
  # Modulo para cargar datps
  
  datos <- callModule(
    module = prepara_server,
    id = "cargar_datos",
    nombre_id = "cargar_datos"
  )
  
  # Modulo de columnas
  
  callModule(
    module = columnas_server,
    id = "convertir_columnas",
    datos = datos
  )
  
  # Modulo de datos en la nube
  
  callModule(
    module = nube_server,
    id = "nube_datos",
    nombre_id = "nube_datos",
    datos = datos
  )
  
  
})