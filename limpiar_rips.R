limpiar_ap(path) {
  tx <- readLines(path)
  tx2 <- str_replace_all(tx,
                         c("1" = "Contributivo",
                           "2" = "Subsidiado",
                           "3" = "Vinculado",
                           "4" = "Particular",
                           "5" = "Otro",
                           "6" = "Victima con afiliacion al Regimen Contributivo",
                           "7" = "Victima con afiliacion al Regimen Subsidiado",
                           "8" = "Victima no asegurado"))
}