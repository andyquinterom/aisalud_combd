un_zip_rips_2020 <- function(path, folder_unzip, session) {
  # Colnames para US -------------------------
  
  col_names_us <- c(
    "tipo_identificacion",
    "nro_identificacion",
    "cod_eapb",
    "tipo_usuario",
    "primer_apellido",
    "segundo_apellido",
    "primer_nombre",
    "segundo_nombre",
    "fecha_nacimiento",
    "sexo",
    "residencia",
    "zone_residencial"
  )
  
  col_classes_us <- list(
    character = c(1:12)
  )
  
  # Colnames para AF -------------------------
  
  col_names_af <- c(
    "cod_prestador",
    "nombre_prestador",
    "tipo_identificacion_prestador",
    "nro_identificacion_prestador",
    "nro_factura",
    "fecha_prestacion",
    "fecha_inicio",
    "fecha_final",
    "codigo_eapb",
    "nombre_eapb",
    "nro_contrato",
    "modalidad_contratacion",
    "plan_beneficios",
    "nro_poliza",
    "valor_copago",
    "valor_cuota_moderadora",
    "valor_cuota_recuperacion",
    "valor_pagos_compartidos",
    "valor_descuentos",
    "valor_factura"
  )
  
  col_classes_af <- list(
    character = c(1:14),
    numeric = c(15:20)
  )
  
  # Colnames para AC -------------------------
  
  col_names_ac <- c(
    "nro_factura",
    "cod_prestador",
    "tipo_identificacion",
    "nro_identificacion",
    "fecha_prestacion",
    "nro_autorizacion",
    "cod_consulta",
    "modalidad",
    "tipo_servicio",
    "finalidad_consulta",
    "causa_externa",
    "cod_dx_1",
    "cod_dx_2",
    "cod_dx_3",
    "cod_dx_ppal",
    "tipo_identificacion_personal",
    "nro_identificacion_personal",
    "valor_consulta",
    "valor_cuota_moderadora",
    "valor_pagos_compartidos",
    "valor"
  )
  
  col_classes_ac <- list(
    character = c(1:17),
    numeric = c(18:21)
  )
  
  # Colnames para AP -------------------------
  
  col_names_ap <- c(
    "nro_factura",
    "cod_prestador",
    "tipo_identificacion",
    "nro_identificacion",
    "fecha_prestacion",
    "nro_autorizacion",
    "cod_cups",
    "via_ingreso",
    "modalidad",
    "tipo_servicio",
    "finalidad",
    "tipo_identificacion_personal",
    "nro_identificacion_personal",
    "cod_dx_ppal",
    "cod_dx_rel",
    "cod_complicacion",
    "valor"
  )
  
  col_classes_ap <- list(
    character = c(1:16),
    numeric = c(17)
  )
  
  # Colnames para AU -------------------------
  
  col_names_au <- c(
    "nro_factura",
    "cod_prestador",
    "tipo_identificacion",
    "nro_identificacion",
    "fecha_prestacion",
    "causa_externa",
    "cod_dx_ppal",
    "cod_dx_egreso",
    "cod_dx_rel_1",
    "cod_dx_rel_2",
    "cod_dx_rel_3",
    "estado_egreso",
    "cod_dx_muerte",
    "fecha_egreso"
  )
  
  col_classes_au <- list(
    character = c(1:14)
  )
  
  # Colnames para AH -------------------------
  
  col_names_ah <- c(
    "nro_factura",
    "cod_prestador",
    "tipo_identificacion",
    "nro_identificacion",
    "via_ingreso",
    "fecha_prestacion",
    "nro_autorizacion",
    "causa_externa",
    "cod_dx_ppal",
    "cod_dx_egreso",
    "cod_dx_rel_1",
    "cod_dx_rel_2",
    "cod_dx_rel_3",
    "cod_complicacion",
    "estado_egreso",
    "cod_dx_muerte",
    "fecha_egreso"
  )
  
  col_classes_ah <- list(
    character = c(1:18)
  )
  
  # Colnames para AN -------------------------
  
  col_names_an <- c(
    "nro_factura",
    "cod_prestador",
    "tipo_identificacion",
    "nro_identificacion",
    "fecha_prestacion",
    "semanas_gestacion",
    "control_prenatal",
    "sexo",
    "peso",
    "cod_dx_ppal",
    "estado_egreso",
    "causa_muerte",
    "fecha_egreso"
  )
  
  col_classes_an <- list(
    character = c(1:13)
  )
  
  # Colnames para AM -------------------------
  
  col_names_am <- c(
    "nro_factura",
    "cod_prestador",
    "tipo_identificacion",
    "nro_identificacion",
    "nro_autorizacion",
    "fecha_prestacion",
    "cod_dx_ppal",
    "cod_dx_rel_1",
    "tipo_medicamento",
    "cums",
    "nombre_medicamento",
    "concentracion_medicamento",
    "unidad_medida",
    "forma_farmaceutica",
    "unidad_minima",
    "cantidad",
    "dias_tratamiento",
    "tipo_identificacion_personal",
    "nro_identificacion_personal",
    "valor_unitario",
    "valor",
    "valor_cuota_moderadora"
  )
  
  col_classes_am <- list(
    character = c(1:19),
    numeric = c(20:22)
  )
  
  # Colnames para AT -------------------------
  
  col_names_at <- c(
    "nro_factura",
    "cod_prestador",
    "tipo_identificacion",
    "nro_identificacion",
    "nro_autorizacion",
    "fecha_prestacion",
    "tipo_servicio",
    "codigo_servicio",
    "nombre_servicio",
    "cantidad",
    "valor_unitario",
    "valor"
  )
  
  col_classes_at <- list(
    character = c(1:9),
    numeric = c(10:12)
  )
  
  datos_en_lista <- list(
    "us" = list("colnames" = col_names_us, 
                "col_classes" = col_classes_us,
                "prefix" = "US"),
    "af" = list("colnames" = col_names_af, 
                "col_classes" = col_classes_af,
                "prefix" = "AF"),
    "ac" = list("colnames" = col_names_ac, 
                "col_classes" = col_classes_ac,
                "prefix" = "AC"),
    "ap" = list("colnames" = col_names_ap, 
                "col_classes" = col_classes_ap,
                "prefix" = "AP"),
    "au" = list("colnames" = col_names_au, 
                "col_classes" = col_classes_au,
                "prefix" = "AU"),
    "ah" = list("colnames" = col_names_ah, 
                "col_classes" = col_classes_ah,
                "prefix" = "AH"),
    "an" = list("colnames" = col_names_an, 
                "col_classes" = col_classes_an,
                "prefix" = "AN"),
    "am" = list("colnames" = col_names_am, 
                "col_classes" = col_classes_am,
                "prefix" = "AM"),
    "at" = list("colnames" = col_names_at, 
                "col_classes" = col_classes_at,
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
        col_names = x$colnames,
        col_classes = x$col_classes,
        prefix = x$prefix,
        session = NULL
      )
    }
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

leer_rips_tabla <- function(folder_unzip, col_names, col_classes, prefix = "US",
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
          list.files(path = paste0(folder_unzip, "/", i)), 1, 2) == prefix)
        if (!identical(file_pos, integer(0))) {
          # tryCatch(
          #   expr = {
          print(paste0(
            folder_unzip, "/", i, "/", 
            list.files(path = paste0(folder_unzip, "/", i))[file_pos]))
              tabla <<- rbind(
                tabla,
                fread(
                  file = paste0(
                    folder_unzip, "/", i, "/", 
                    list.files(path = paste0(folder_unzip, "/", i))[file_pos]),
                  col.names = col_names,
                  colClasses = col_classes
                )
              )
            # },
            # warning = function(w) {
            #   warning_count <<- c(warning_count, i)
            #   tabla <<- rbind(
            #     tabla,
            #     fread(
            #       file = paste0(
            #         folder_unzip, "/", i, "/",
            #         list.files(path = paste0(folder_unzip, "/", i))[file_pos]),
            #       col.names = col_names,
            #       colClasses = col_classes
            #     )
            #   )
            # },
          #   error = function(e) {
          #     error_count <<- c(error_count, i)
          #     print(e)
          #   }
          # )
        }
        leyendo_archivo_progress(
          session = session,
          i = i,
          folder_num = folder_num, 
          folders_total = folders_total,
          prefix = prefix)
        
        folder_num <<- folder_num + 1
      }
    }
  )
  
  print(names(tabla))
  
  return(
    list(
      tabla = tabla,
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