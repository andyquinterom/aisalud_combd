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
