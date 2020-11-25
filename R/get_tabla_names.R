get_table_names <- function(con, schema) {
  dbListTables(con, table_schema=schema)
}