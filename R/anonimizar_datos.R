anonimizar_datos <- function(datos, variable, longitud=31, semilla=NULL){
    columnas <- dplyr::select(datos, variable)
    ids <- unname(apply(columnas, 1, paste, collapse = ""))
    ids <- as.integer(factor(ids))
    n <- length(unique(ids))
    caracteres_especiales <- rawToChar(as.raw(1:255), multiple = TRUE)
    set.seed(semilla)
    anonimos <- replicate(n, paste(sample(caracteres_especiales, longitud, replace = TRUE), collapse = ""))
    list(base=eval(parse(text = paste("dplyr::mutate(datos,", variable, "=anonimos[ids])"))),
         diccionario=unique(do.call(cbind, list(dplyr::select(datos, variable), anonimos=as.character(anonimos[ids])))))

}
