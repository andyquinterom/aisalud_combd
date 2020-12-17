consolidar_rips <- function(prestadores = NULL, ac, af, ah, am, ap, at, au, us,
                            cups = NULL) {
  
  lista_archivos <- c(
    ac = !is.null(ac),
    ap = !is.null(ap),
    at = !is.null(at),
    am = !is.null(am)
  )
  
  # Los diferentes archivos se transforman a un objecto de tipo data.table
  # Se filtran para solo obtener datos de los prestadores seleccionados
  if (!is.null(prestadores)) {
    ac <- no_null(copy(ac), FUN = function(x) x[cod_prestador %in% prestadores])
    af <- no_null(copy(af), FUN = function(x) x[cod_prestador %in% prestadores])
    ah <- no_null(copy(ah), FUN = function(x) x[cod_prestador %in% prestadores])
    am <- no_null(copy(am), FUN = function(x) x[cod_prestador %in% prestadores])
    ap <- no_null(copy(ap), FUN = function(x) x[cod_prestador %in% prestadores])
    at <- no_null(copy(at), FUN = function(x) x[cod_prestador %in% prestadores])
    au <- no_null(copy(au), FUN = function(x) x[cod_prestador %in% prestadores])
  } else {
    ac <- copy(ac)
    af <- copy(af)
    ah <- copy(ah)
    am <- copy(am)
    ap <- copy(ap)
    at <- copy(at)
    au <- copy(au)
  }
  
 
  # Modificacion al archivo AP
   
  if (lista_archivos[["ap"]]) {
    ap[, 'ambito' := NULL]
    ap[, cantidad := 1]
    ap[, nombre_prestacion := ""]
    ap[, 'tipo_servicio' := "Procedimientos"]
    setnames(ap, 7, "cod_prestacion")
  }
  
  # Modificacion al archivo AC
  
  if (lista_archivos[["ac"]]) {
    ac[, 'tipo_servicio' := "Consultas"]
    ac[, cantidad := 1]
    ac[, nombre_prestacion := ""]
    setnames(ac, 7, "cod_prestacion")
  }
  
  # Modificacion al archivo AM
  
  if (lista_archivos[["am"]]) {
    am[, 'tipo_servicio' := tipo_medicamento]
    setnames(am, 6, "cod_prestacion")
    setnames(am, 8, "nombre_prestacion")
  }
  
  # Modificacion al archivo AT
  
  if (lista_archivos[["at"]]) {
    setnames(at, 7, "cod_prestacion")
    setnames(at, 8, "nombre_prestacion")
    setnames(at, 6, "tipo_servicio")
  }
  
  # Se eliminan columnas innecesarias
  ah$tipo_identificacion <- NULL
  au$tipo_identificacion <- NULL
  us$tipo_identificacion <- NULL
  us$cod_eapb            <- NULL
  
  # Conversión de tipo de servicio númerico a valor correspondiente
  
  tryCatch(
    expr = {
      at$tipo_servicio <- conversion_tiposervicio(at$tipo_servicio)
    },
    error = function(e) {
      print(e)
    }
  )
  
  # Conversión de tipo de medicamento a valor correspondiente
  
  tryCatch(
    expr = {
      am$tipo_servicio <- conversion_tipomedicamento(am$tipo_servicio)
    },
    error = function(e) {
      print(e)
    }
  )
  
  # Se hace merge entre el archivo de transacciones y los de prestaciones
  
  columnas_merge_af <- c("nro_factura",
                         "nro_identificacion",
                         "valor", "cod_prestacion",
                         "nombre_prestacion",
                         "tipo_servicio",
                         "cantidad")
  
  lista_merge <- lapply(
    X = list(ac, ap, at, am),
    FUN = function(x) {
      no_null(x, FUN = function(x) {
        merge(af, x[, columnas_merge_af, with = FALSE],
              by = "nro_factura")
      })
    }
  )
  
  merge_af <- rbindlist(lista_merge)
  
  # Encontrar duplicados y eliminar
  
  tryCatch(
    expr = {
      au <- quitar_duplicados(au)
    },
    error = function(e) {
      print(e)
    }
  )
  
  tryCatch(
    expr = {
      ah <- quitar_duplicados(ah)
    },
    error = function(e) {
      print(e)
    }
  )
  
  tryCatch(
    expr = {
      us <- quitar_usuarios_duplicados(us)
    },
    error = function(e) {
      print(e)
    }
  )
  
  # Quitar fechas innecesarias
  
  au$fecha_prestacion <- NULL
  ah$fecha_prestacion <- NULL
  
  # Merge archivo de urgencias y hospotilización con el de prestaciones
  
  # Si hay datos para hospitalización pero no para urgencias
  if (nrow(au) == 0 & nrow(ah) != 0) {
    merge_prestaciones <- merge.data.table(x = merge_af,
                                           y = ah,
                                           by = c("nro_factura",
                                                  "nro_identificacion",
                                                  "cod_prestador"),
                                           all.x = TRUE
                                           )
  }
  
  # Si hay datos para urgencias pero no para hospitalización
  if (nrow(au) != 0 & nrow(ah) == 0) {
    merge_prestaciones <- merge.data.table(x = merge_af,
                                           y = au,
                                           by = c("nro_factura",
                                                  "nro_identificacion",
                                                  "cod_prestador"),
                                           all.x = TRUE
    )
  }
  
  # Si noy hay datos para urgencias ni para hospitalización
  if (nrow(au) == 0 & nrow(ah) == 0) {
    merge_prestaciones <- merge_af
  }
  
  # Si hay datos para urgencias y hospitalización
  if (nrow(au) != 0 & nrow(ah) != 0) {
    # Prestaciones de hospitalización
    merge_prestaciones_ah <- merge.data.table(x = merge_af,
                                             y = ah,
                                             by = c("nro_factura",
                                                    "nro_identificacion",
                                                    "cod_prestador"))
    
    # Prestaciones que no son hospitalizaciones
    merge_prestaciones_sin_ah <- ajoin(x = merge_af,
                                       y = merge_prestaciones_ah,
                                       by = c("nro_factura",
                                              "nro_identificacion",
                                              "cod_prestador"))
    
    # Prestaciones de urgencias que no se encuentran en hospitalizaciones
    merge_prestaciones_au <- merge.data.table(x = merge_prestaciones_sin_ah,
                                              y = au,
                                              by = c("nro_factura",
                                                     "nro_identificacion",
                                                     "cod_prestador"),
                                              all.x = TRUE)
    
    # Juntar hospitalizaciones con urgencias
    merge_prestaciones <- rbind(merge_prestaciones_ah,
                                merge_prestaciones_au,
                                fill = TRUE)
    
  }
  
  # Juntar datos de usuarios con las prestaciones
  if (nrow(us) != 0) {
    
    merge_prestaciones <- merge(x = merge_prestaciones,
                                y = us,
                                by = "nro_identificacion",
                                all.x = TRUE
                                )
    
    # Conversión de tipo de usuario a su valor correspondiente
    merge_prestaciones$tipo_usuario <- 
      conversion_tipousuario(merge_prestaciones$tipo_usuario)
    
  }
  
  # Generar nombres de prestación faltantes
  
  # Encontrar y marcar los valores solo numéricos
  prestacion_numerica <- !grepl("\\D", merge_prestaciones$cod_prestacion)
  
  # Convertir de character a numerico y devuelta
  cod_prestacion_num <- merge_prestaciones$cod_prestacion[prestacion_numerica]
  cod_prestacion_num <- as.character(
    as.numeric(as.character(cod_prestacion_num))
  )
  
  merge_prestaciones$cod_prestacion[prestacion_numerica] <- cod_prestacion_num
  
  # Encontrar el ambito de la factura
  
  facturas_hospitalizacion <- ah[, list("ambito" = "Hospitalizacion"),
                                 by = "nro_factura"]
  
  facturas_urgencias <- au[nro_factura %notin% 
                             facturas_hospitalizacion$nro_factura,
                           list("ambito" = "Urgencias"),
                           by = "nro_factura"]
  
  facturas_ambito <- rbind(facturas_hospitalizacion, facturas_urgencias)
  
  merge_prestaciones <- merge.data.table(
    x = merge_prestaciones,
    y = facturas_ambito,
    all.x = TRUE,
    by = "nro_factura")
  
  merge_prestaciones[is.na(ambito), 'ambito' := "Ambulatorio"]
  
  # Merge codigo de prestación con indice de cups

  if (!is.null(cups)) {
    merge_prestaciones <- rbind(
      merge(
        merge_prestaciones[nombre_prestacion == ""][, -c("nombre_prestacion")],
        cups,
        by = c("cod_prestacion"),
        all.x = TRUE
      )[, union(names(merge_prestaciones), names(cups)), with = FALSE],
      merge_prestaciones[nombre_prestacion != ""]
    )
  }
  
  merge_prestaciones[, "mes" := lubridate::month(
      as.Date(fecha_prestacion, format = "%d/%m/%Y"))]
  
  merge_prestaciones[, "anio" := lubridate::year(
    as.Date(fecha_prestacion, format = "%d/%m/%Y"))]
  
  merge_prestaciones[, "mes_anio" := paste(anio, mes_spanish(mes),
    sep = " - ")]
  
  return(merge_prestaciones)
  
}

mes_spanish <- function(x) {
  meses <- c(
    "Enero",
    "Febrero",
    "Marzo",
    "Abril",
    "Mayo",
    "Junio",
    "Julio",
    "Agosto",
    "Septiembre",
    "Octubre",
    "Noviembre",
    "Diciembre"
  )
  return(
    meses[x]
  )
  
}


no_null <- function(x, FUN, ...) {
  if (!is.null(x)) {
    x %>%
      FUN(...)
  } else {
    NULL
  }
}

simple_if <- function(condition, FUN, ...) {
  if (condition) {
    FUN(...)
  }
}
