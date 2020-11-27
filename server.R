shinyServer(function(input, output, session) {
  
  session$onSessionEnded(function() {
    dbDisconnect(base_de_datos_con)
  })
  
  # Modulo para cargar datos
  
  datos <- callModule(
    module = prepara_server,
    id = "cargar_datos",
    nombre_id = "cargar_datos"
  )
  
  agrupadores <- callModule(
    module = prepara_server,
    id = "cargar_agrupadores",
    nombre_id = "cargar_agrupadores"
  )
  
  # Modulo de columnas
  
  callModule(
    module = columnas_server,
    id = "convertir_columnas",
    datos = datos,
    nombre_id = "convertir_columnas"
  )
  
  callModule(
    module = columnas_server,
    id = "agrupadores_columnas",
    datos = agrupadores,
    nombre_id = "agrupadores_columnas"
  )
  
  # Modulo de datos en la nube
  
  callModule(
    module = nube_server,
    id = "nube_datos",
    nombre_id = "nube_datos",
    datos = datos
  )
  
  # Modulo de agrupadores
  
  callModule(
    module = agrupadores_server,
    id = "agrupadores",
    datos = datos,
    agrupadores = agrupadores
  )
  
  # Modulo de filtros
  
  callModule(
    module = filtros_server,
    id = "filtros",
    datos = datos
  )
  
  # Modulo de descargas
  
  callModule(
    module = descargar_server,
    id = "descargar_datos",
    datos = datos
  )
  
})