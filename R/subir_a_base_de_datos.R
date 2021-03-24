subir_a_base_de_datos <- function(nombre, datos, con, schema = "public") {
  dbWriteTable(conn = con, 
               Id(schema = schema, table = nombre),
               value = datos)
}

borrar_tabla <- function(nombre, con, schema = "public") {
  dbRemoveTable(
    conn = con,
    Id(schema = schema, table = nombre)
  )
}

feather_size_est <- function(obj, frac=1) {
  tf <- tempfile(tmpdir = tempdir(check =  TRUE))
  n <- ceiling(nrow(obj) * frac)
  write_feather(obj[seq_len(n),], path = tf)
  size <- 1/frac * file.info(tf)$size
  unlink(tf)
  return(size)
}

set_utf8 <- function(x) {
  # Declare UTF-8 encoding on all character columns:
  chr <- sapply(x, is.character)
  x[, chr] <- lapply(x[, chr, drop = FALSE], function(z) {
    print(Encoding(z))
    return(stri_encode(z, "", "UTF-8"))
  })
  # Same on column names:
  # Encoding(names(x)) <- "UTF-8"
  x
}

lazy_to_postgres <- function(datos, nombre, conn, schema = "public") {
  query <- paste0(
      'SELECT * 
      INTO #tabla#
      FROM ( ',
      dbplyr::sql_render(datos),
      " ) AS alias") %>%
    str_replace_all(
      pattern = "#tabla#", 
      replacement = dbQuoteIdentifier(conn, x = nombre)
    )
  
  dbExecute(conn, query)
  
}
