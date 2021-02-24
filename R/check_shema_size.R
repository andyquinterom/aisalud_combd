check_schema_size <- function(con, schema = "public") {
  query_string <- "select pg_size_pretty(pg_schema_size('######'::text));"
  query_send <- dbGetQuery(
    con, 
    str_replace_all(query_string, "######", schema)) %>%
    unname() %>%
    str_split(" ")
  size_info <- query_send[[1]]
  if (is.na(size_info)) size_info <- c(0, "MB")
  if (size_info[2] == "MB") {
    return(as.numeric(size_info[1]))
  } else if (size_info[2] == "GB") {
    return(as.numeric(size_info[1]) * 1024)
  }
}
