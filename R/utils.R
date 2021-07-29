lazy_to_postgres <- function(datos, nombre, conn, overwrite = FALSE) {
  if (overwrite) {
    dbRemoveTable(
      conn = conn,
      name = nombre,
      fail_if_missing = FALSE
    )
  }
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

`%notin%` <- Negate(`%in%`)

check_db_size <- function(con, database) {
  query_string <- "SELECT pg_size_pretty(pg_database_size('######'));"
  query_send <- dbGetQuery(
    con,
    str_replace_all(query_string, "######", database)) %>%
    unname() %>%
    str_split(" ")
  size_info <- query_send[[1]]
  if (is.na(size_info[1])) size_info <- c(0, "MB")
  if (size_info[2] == "MB") {
    return(as.numeric(size_info[1]))
  } else if (size_info[2] == "GB") {
    return(as.numeric(size_info[1]) * 1024)
  }
}

map_func <- function(object, functions) {
  temp_obj <- object

  lapply(
    X = functions,
    FUN = function(x) {
      temp_obj <<- temp_obj %>%
        x()
    }
  )

  return(temp_obj)
}
