un_zip_rips <- function(path, folder_unzip, session) {
  # Colnames para US -------------------------
  
  cols_us <- cols(
    "tipo_identificacion" = col_character(),
    "nro_identificacion" = col_character(),
    "cod_eapb" = col_character(),
    "tipo_usuario" = col_character(),
    "primer_apellido" = col_character(),
    "segundo_apellido" = col_character(),
    "primer_nombre" = col_character(),
    "segundo_nombre" = col_character(),
    "edad" = col_character(),
    "unidad_edad" = col_character(),
    "sexo" = col_character(),
    "codigo_departamento" = col_character(),
    "codigo_municipio" = col_character(),
    "zone_residencial"  = col_character()
  )
  
  # Colnames para AF -------------------------
  
  cols_af <- cols(
    "cod_prestador" = col_character(),
    "nombre_prestador" = col_character(),
    "tipo_identificacion_prestador" = col_character(),
    "nro_identificacion_prestador" = col_character(),
    "nro_factura" = col_character(),
    "fecha_prestacion" = col_character(),
    "fecha_inicio" = col_character(),
    "fecha_final" = col_character(),
    "codigo_eapb" = col_character(),
    "nombre_eapb" = col_character(),
    "nro_contrato" = col_character(),
    "plan_beneficios" = col_character(),
    "nro_poliza" = col_character(),
    "valor_copago" = col_character(),
    "valor_comision" = col_character(),
    "valor_descuentos" = col_character(),
    "valor_factura"  = col_character()
  )
  
  # Colnames para AC -------------------------
  
  cols_ac <- cols(
    "nro_factura" = col_character(),
    "cod_prestador" = col_character(),
    "tipo_identificacion" = col_character(),
    "nro_identificacion" = col_character(),
    "fecha_prestacion" = col_character(),
    "nro_autorizacion" = col_character(),
    "cod_consulta" = col_character(),
    "finalidad_consulta" = col_character(),
    "causa_externa" = col_character(),
    "cod_dx_ppal" = col_character(),
    "cod_dx_1" = col_character(),
    "cod_dx_2" = col_character(),
    "cod_dx_3" = col_character(),
    "dx_tipo" = col_character(),
    "valor_consulta" = col_character(),
    "valor_cuota" = col_character(),
    "valor" = col_character()
  )
  
  # Colnames para AP -------------------------
  
  cols_ap <- cols(
    "nro_factura" = col_character(),
    "cod_prestador" = col_character(),
    "tipo_identificacion" = col_character(),
    "nro_identificacion" = col_character(),
    "fecha_prestacion" = col_character(),
    "nro_autorizacion" = col_character(),
    "cod_cups" = col_character(),
    "ambito" = col_character(),
    "finalidad" = col_character(),
    "personal" = col_character(),
    "cod_dx_ppal" = col_character(),
    "cod_dx_rel" = col_character(),
    "cod_complicacion" = col_character(),
    "realizacion_quir" = col_character(),
    "valor" = col_character()
  )
  
  # Colnames para AU -------------------------
  
  cols_au <- cols(
    "nro_factura" = col_character(),
    "cod_prestador" = col_character(),
    "tipo_identificacion" = col_character(),
    "nro_identificacion" = col_character(),
    "fecha_prestacion" = col_character(),
    "hora_prestacion" = col_character(),
    "nro_autorizacion" = col_character(),
    "causa_externa" = col_character(),
    "cod_dx_egreso" = col_character(),
    "cod_dx_rel_1" = col_character(),
    "cod_dx_rel_2" = col_character(),
    "cod_dx_rel_3" = col_character(),
    "destino" = col_character(),
    "estado_egreso" = col_character(),
    "causa_muerte" = col_character(),
    "fecha_egreso" = col_character(),
    "hora_egreso"  = col_character()
  )
  
  # Colnames para AH -------------------------
  
  cols_ah <- cols(
    "nro_factura" = col_character(),
    "cod_prestador" = col_character(),
    "tipo_identificacion" = col_character(),
    "nro_identificacion" = col_character(),
    "via_ingreso" = col_character(),
    "fecha_prestacion" = col_character(),
    "hora_prestacion" = col_character(),
    "nro_autorizacion" = col_character(),
    "causa_externa" = col_character(),
    "cod_dx_ppal" = col_character(),
    "cod_dx_egreso" = col_character(),
    "cod_dx_rel_1" = col_character(),
    "cod_dx_rel_2" = col_character(),
    "cod_dx_rel_3" = col_character(),
    "cod_complicacion" = col_character(),
    "estado_salida" = col_character(),
    "causa_muerte" = col_character(),
    "fecha_egreso" = col_character(),
    "hora_egreso" = col_character()
  )
  
  # Colnames para AN -------------------------
  
  cols_an <- cols(
    "nro_factura" = col_character(),
    "cod_prestador" = col_character(),
    "tipo_identificacion" = col_character(),
    "nro_identificacion" = col_character(),
    "fecha_prestacion" = col_character(),
    "hora_nacimiento" = col_character(),
    "edad" = col_character(),
    "control_prenatal" = col_character(),
    "sexo" = col_character(),
    "peso" = col_character(),
    "cod_dx_ppal" = col_character(),
    "causa_muerte" = col_character(),
    "fecha_muerte" = col_character(),
    "hora_muerte" = col_character()
  )
  
  # Colnames para AM -------------------------
  
  cols_am <- cols(
    "nro_factura" = col_character(),
    "cod_prestador" = col_character(),
    "tipo_identificacion" = col_character(),
    "nro_identificacion" = col_character(),
    "nro_autorizacion" = col_character(),
    "cums" = col_character(),
    "tipo_medicamento" = col_character(),
    "nombre_medicamento" = col_character(),
    "forma_farmaceutica" = col_character(),
    "concentracion_medicamento" = col_character(),
    "unidad_medida" = col_character(),
    "cantidad" = col_character(),
    "valor_unitario" = col_character(),
    "valor" = col_character()
  )
  

  # Colnames para AT -------------------------
  
  cols_at <- cols(
    "nro_factura" = col_character(), 
    "cod_prestador" = col_character(),
    "tipo_identificacion" = col_character(),
    "nro_identificacion" = col_character(),
    "nro_autorizacion" = col_character(),
    "tipo_servicio" = col_character(),
    "codigo_servicio" = col_character(),
    "nombre_servicio" = col_character(),
    "cantidad" = col_character(),
    "valor_unitario" = col_character(),
    "valor" = col_character()
  )
  
  datos_en_lista <- list(
    "us" = list("colnames" = cols_us, 
                "prefix" = "US"),
    "af" = list("colnames" = cols_af, 
                "prefix" = "AF"),
    "ac" = list("colnames" = cols_ac, 
                "prefix" = "AC"),
    "ap" = list("colnames" = cols_ap, 
                "prefix" = "AP"),
    "au" = list("colnames" = cols_au, 
                "prefix" = "AU"),
    "ah" = list("colnames" = cols_ah, 
                "prefix" = "AH"),
    "an" = list("colnames" = cols_an, 
                "prefix" = "AN"),
    "am" = list("colnames" = cols_am, 
                "prefix" = "AM"),
    "at" = list("colnames" = cols_at, 
                "prefix" = "AT")
  )
  
  # Leer los datos -------------------------------------------
  
  unzip(path, exdir = folder_unzip)
  
  datos <- lapply(
    X = datos_en_lista,
    folder_unzip = folder_unzip,
    FUN = function(x, folder_unzip) {
      leer_rips_tabla(
        folder_unzip = folder_unzip,
        col_classes = x$colnames,
        prefix = x$prefix,
        session = NULL
      )
    }
  )
  
  datos$warnings <- c(
    datos$us$warnings,
    datos$af$warnings,
    datos$ac$warnings,
    datos$ap$warnings,
    datos$au$warnings,
    datos$ah$warnings,
    datos$am$warnings,
    datos$an$warnings,
    datos$at$warnings
  )
  datos$warnings <- c(
    datos$warnings,
    datos$us$errores,
    datos$af$errores,
    datos$ac$errores,
    datos$ap$errores,
    datos$au$errores,
    datos$ah$errores,
    datos$am$errores,
    datos$an$errores,
    datos$at$errores
  )
  
  datos$warnings <- paste(
    datos$warnings,
    collapse = "\n"
  )
  
  return(
    datos
  )
}

leer_rips_todos <- function(datos, prestadores, cups, session = NULL) {
  
  us <- datos$us
  af <- datos$af
  ac <- datos$ac
  ap <- datos$ap
  au <- datos$au
  ah <- datos$ah
  an <- datos$an
  am <- datos$am
  at <- datos$at
  
  if (!is.null(session)) {
    setProgress(
      value = 0.9,
      message = "Consolidando y juntando tablas.",
      session = session
    )
  }
  consolidados <- consolidar_rips(
    prestadores = prestadores, ac = ac[["tabla"]], af = af[["tabla"]],
    ah = ah[["tabla"]], am = am[["tabla"]], ap = ap[["tabla"]],
    at = at[["tabla"]], au = au[["tabla"]], us = us[["tabla"]]
  )
  
  consolidados[, "valor" := as.numeric(valor)]
  consolidados[, "valor_factura" := as.numeric(valor_factura)]
  consolidados[, "cantidad" := as.numeric(cantidad)]
  
  return(consolidados)
  
}

leer_rips_tabla <- function(folder_unzip, col_classes, prefix = "US",
                            session = NULL) {
  folders <- list.files(path = folder_unzip)
  
  file_pos <- 0
  tabla <- data.table()
  folder_num <- 1
  warning_count <- c()
  error_count <- c()
  
  folders_total <- length(list.files(folder_unzip))
  
  lapply(
    X = folders,
    FUN = function(i) {
      if(prefix %in% substr(
        list.files(path = paste0(folder_unzip, "/", i)), 1, 2)) {
        file_pos <- which(substr(
          list.files(path = paste0(folder_unzip, "/", i)), 1, 2) == prefix)[1]
        if (!identical(file_pos, integer(0))) {
          tryCatch(
            expr = {
              file <- paste0(
                folder_unzip, "/", i, "/", 
                list.files(
                  path = paste0(folder_unzip, "/", i))[file_pos])
                  tabla <<- rbind(
                    tabla,
                    read_delim(
                      locale = locale(encoding = guess_encoding(file)[[1]][1]),
                      file = file,
                      delim = ",",
                      col_types = col_classes,
                      col_names = names(col_classes[["cols"]])
                    )
                  )
            },
            warning = function(w) {
              file <- paste0(
                folder_unzip, "/", i, "/", 
                list.files(
                  path = paste0(folder_unzip, "/", i))[file_pos])
              tabla <<- rbind(
                tabla,
                read_delim(
                  locale = locale(encoding = guess_encoding(file)[[1]][1]),
                  file = paste0(
                    folder_unzip, "/", i, "/", 
                    list.files(
                      path = paste0(folder_unzip, "/", i))[file_pos]),
                  delim = ",",
                  col_types = col_classes,
                  col_names = names(col_classes[["cols"]])
                )
              )
              warning <- NULL
              warning <- str_replace_all(pattern = "\n", replacement = "AAAAA",
                                         string = w$message)
              warning <- str_replace_all(pattern = folder_unzip, replacement = "",
                                         string = warning)
              warning <- gsub("\\s+", " ", warning)
              warning <- str_replace_all(pattern = "AAAAA", replacement = "\n",
                                         string = warning)
              warning_count <<- c(
                warning,
                warning_count)
            },
            error = function(e) {
              error_count <<- c(error_count, i)
            }
          )
        }
        
        # leyendo_archivo_progress(
        #   session = session,
        #   i = i,
        #   folder_num = folder_num, 
        #   folders_total = folders_total,
        #   prefix = prefix)
        
        folder_num <<- folder_num + 1
      }
    }
  )
  
  return(
    list(
      tabla = as.data.table(tabla),
      warnings = warning_count,
      errores = error_count
    )
  )
}

leyendo_archivo_progress <- function(session = NULL, i, folder_num, prefix,
                                     folders_total) {
  if (!is.null(session)) {
    if (folder_num == 1) {
      setProgress(
        value = 0,
        message = paste("Leyendo carpeta", i),
        detail = prefix,
        session = session)
    } else {
      incProgress(
        amount = 1/folders_total,
        message = paste("Leyendo carpeta", i),
        detail = prefix,
        session = session)
    }
  }
}