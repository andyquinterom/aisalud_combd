shinyServer(function(input, output, session) {
  
  opciones <- reactiveValues(
    cambios = list()
  )
  
  opciones_agrupadores <- reactiveValues(
    cambios = list()
  )
  
  prepara_server(
    id = "cargar_datos",
    opciones = opciones,
    validar_fecha = TRUE
  )
  
  columnas_server(
    id = "convertir_columnas",
    opciones = opciones
  )
  
  # Modulo para cargar datos
  
  prepara_server(
    id = "cargar_agrupadores",
    opciones = opciones_agrupadores
  )
  
  columnas_server(
    id = "agrupadores_columnas",
    opciones = opciones_agrupadores
  )
  
  # # Modulo de datos en la nube

  nube_server(
    id = "nube_datos",
    opciones = opciones,
    opciones_agrupadores = opciones_agrupadores
  )
  # callModule(
  #   module = nube_server,
  #   id = "nube_datos",
  #   nombre_id = "nube_datos",
  #   datos = datos
  # )

  # # Modulo de agrupadores

  agrupadores_server(
    id = "agrupadores",
    opciones = opciones,
    opciones_agrupadores = opciones_agrupadores
  )
  
  # callModule(
  #   module = agrupadores_server,
  #   id = "agrupadores",
  #   datos = datos,
  #   agrupadores = agrupadores
  # )

  # # Modulo de filtros
  # 
  # callModule(
  #   module = filtros_server,
  #   id = "filtros",
  #   datos = datos
  # )
  # 
  # # Modulo de descargas
  # 
  # callModule(
  #   module = descargar_server,
  #   id = "descargar_datos",
  #   datos = datos
  # )
  # 
  # otras_funciones_server("otras_funciones", datos)
  # 
})