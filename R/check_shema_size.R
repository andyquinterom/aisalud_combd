check_schema_size <- function(con, schema = "public") {
  query_string <- "select pg_size_pretty(pg_schema_size('######'::text));"
  query_send <- dbGetQuery(
    con, 
    str_replace(query_string, "######", schema), append = TRUE)
  return(
    gsub("[^0-9.-]", "", query_send[[1]]) %>%
      as.numeric()
  )
}
