resumen <- function(x) {
  tipo_de_dato <- class(x)
  
  if (tipo_de_dato == "character") {
    resumen_tabla <- paste0(
      "Reusmen: \n",
      "Tipo: discreto \n",
      "Tamaño: ", length(x), "\n",
      "Valores únicos: ", uniqueN(x), "\n",
      "Campos en blanco: ", sum(is.na(x)), "\n",
      "Valores duplicados: ", sum(duplicated(x))
    )
  } else {
    resumen_tabla <- paste0(
      "Reusmen: \n",
      "Tipo: numérico \n",
      "Tamaño: ", length(x), "\n",
      "Valores únicos: ", uniqueN(x), "\n",
      "Campos en blanco: ", sum(is.na(x)), "\n",
      "Mínimo: ", min(x, na.rm = TRUE), "\n",
      "Máximo: ", max(x, na.rm = TRUE), "\n"
    )
  }
  
}