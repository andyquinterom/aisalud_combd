shinyServer(function(input, output, session) {
  
  opciones <- reactiveValues(
    cambios = list(),
    sep_decimal = ".",
    nombre_tabla = "temporal"
  )
  
  opciones_agrupadores <- reactiveValues(
    cambios = list()
  )
  
  prepara_server(
    id = "cargar_datos",
    opciones = opciones
  )
  
  columnas_server(
    id = "convertir_columnas",
    opciones = opciones
  )
  
  # Modulo para cargar datos
  
  prepara_server(
    id = "cargar_agrupadores",
    opciones = opciones_agrupadores,
    prefix = "agrupador_"
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

  filtros_server(id = "filtros", opciones = opciones)

  preview_server(id = "preview", opciones = opciones)
  
})
