subir_a_base_de_datos <- function(nombre, datos, con, schema) {
  dbWriteTable(conn = con,
               Id(schema = schema, table = nombre),
               datos)
}

borrar_tabla <- function(nombre, con, schema) {
  dbRemoveTable(
    conn = con,
    Id(schema = schema, table = nombre)
  )
}

feather_size_est <- function(obj, frac=1) {
  tf <- tempfile(tmpdir = tempdir(check =  TRUE))
  n <- ceiling(nrow(obj) * frac)
  write_feather(obj[seq_len(n),], path = tf)
  1/frac * file.info(tf)$size
  unlink(tf)
}