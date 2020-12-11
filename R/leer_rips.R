un_zip_rips <- function(path, folder_unzip, session) {
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
    "edad",
    "unidad_edad",
    "sexo",
    "codigo_departamento",
    "codigo_municipio",
    "zone_residencial"
  )
  
  col_classes_us <- list(
    character = c(1:8, 10:14),
    numeric = c(9)
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
    "plan_beneficios",
    "nro_poliza",
    "valor_copago",
    "valor_comision",
    "valor_descuentos",
    "valor_factura"
  )
  
  col_classes_af <- list(
    character = c(1:13),
    numeric = c(14:17)
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
    "finalidad_consulta",
    "causa_externa",
    "cod_dx_ppal",
    "cod_dx_1",
    "cod_dx_2",
    "cod_dx_3",
    "dx_tipo",
    "valor_consulta",
    "valor_cuota",
    "valor"
  )
  
  col_classes_ac <- list(
    character = c(1:14),
    numeric = c(15:17)
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
    "ambito",
    "finalidad",
    "personal",
    "cod_dx_ppal",
    "cod_dx_rel",
    "cod_complicacion",
    "realizacion_quir",
    "valor"
  )
  
  col_classes_ap <- list(
    character = c(1:14),
    numeric = c(15)
  )
  
  # Colnames para AU -------------------------
  
  col_names_au <- c(
    "nro_factura",
    "cod_prestador",
    "tipo_identificacion",
    "nro_identificacion",
    "fecha_prestacion",
    "hora_prestacion",
    "nro_autorizacion",
    "causa_externa",
    "cod_dx_egreso",
    "cod_dx_rel_1",
    "cod_dx_rel_2",
    "cod_dx_rel_3",
    "destino",
    "estado_egreso",
    "causa_muerte",
    "fecha_egreso",
    "hora_egreso"
  )
  
  col_classes_au <- list(
    character = c(1:17)
  )
  
  # Colnames para AH -------------------------
  
  col_names_ah <- c(
    "nro_factura",
    "cod_prestador",
    "tipo_identificacion",
    "nro_identificacion",
    "via_ingreso",
    "fecha_prestacion",
    "hora_prestacion",
    "nro_autorizacion",
    "causa_externa",
    "cod_dx_ppal",
    "cod_dx_egreso",
    "cod_dx_rel_1",
    "cod_dx_rel_2",
    "cod_dx_rel_3",
    "cod_complicacion",
    "estado_salida",
    "causa_muerte",
    "fecha_egreso",
    "hora_egreso"
  )
  
  col_classes_ah <- list(
    character = c(1:19)
  )
  
  # Colnames para AN -------------------------
  
  col_names_an <- c(
    "nro_factura",
    "cod_prestador",
    "tipo_identificacion",
    "nro_identificacion",
    "fecha_prestacion",
    "hora_nacimiento",
    "edad",
    "control_prenatal",
    "sexo",
    "peso",
    "cod_dx_ppal",
    "causa_muerte",
    "fecha_muerte",
    "hora_muerte"
  )
  
  col_classes_an <- list(
    character = c(1:14)
  )
  
  # Colnames para AM -------------------------
  
  col_names_am <- c(
    "nro_factura",
    "cod_prestador",
    "tipo_identificacion",
    "nro_identificacion",
    "nro_autorizacion",
    "cums",
    "tipo_medicamento",
    "nombre_medicamento",
    "forma_farmaceutica",
    "concentracion_medicamento",
    "unidad_medida",
    "cantidad",
    "valor_unitario",
    "valor"
  )
  
  col_classes_am <- list(
    character = c(1:12),
    numeric = c(13:14)
  )
  
  # Colnames para AT -------------------------
  
  col_names_at <- c(
    "nro_factura",
    "cod_prestador",
    "tipo_identificacion",
    "nro_identificacion",
    "nro_autorizacion",
    "tipo_servicio",
    "codigo_servicio",
    "nombre_servicio",
    "cantidad",
    "valor_unitario",
    "valor"
  )
  
  col_classes_at <- list(
    character = c(1:9),
    numeric = c(10:11)
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
  
  datos <- mclapply(
    X = datos_en_lista,
    mc.cores = parallel::detectCores(),
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
        tryCatch(
          expr = {
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
          },
          warning = function(w) {
            warning_count <<- c(warning_count, i)
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
          },
          error = function(e) {
            error_count <<- c(error_count, i)
            print(e)
          }
        )
        
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