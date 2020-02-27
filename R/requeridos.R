requeridos <- function(...){
    paq <- as.list(match.call())[-1]
    paq <- sapply(paq, paste)
    paq.nuevo <- paq[!(paq %in% installed.packages()[, "Package"])]
    if(length(paq.nuevo))  
        install.packages(paq.nuevo, dependencies = TRUE, quiet = TRUE)
    suppressPackageStartupMessages(sapply(paq, require, character.only = TRUE)) 
}