limpiar_texto <- function(texto, excluir_palabras=NULL, links=FALSE, retweet=FALSE, hashtag=FALSE, signos_puntuacion=FALSE,
                     tildes=FALSE, numeros=FALSE, emoticones=FALSE, palabras_vacias=FALSE, minusculas=TRUE, mayusculas=FALSE){

    #Stopwords
    stopwords<-tm::stopwords("es")
    stopwords<-c(stopwords,"por","que","x","xq", "ser", "parte", "tener", "debe", excluir_palabras)
    stopwords<-stringi::stri_trans_general(stopwords,"Latin-ASCII")

    if(!links){
        texto <- stringr::str_replace_all(texto,"(f|ht)(tp)(s?)(://)(.*)[.|/](.*)", " ")
    }

    if(!retweet){
        texto <- stringr::str_replace_all(texto,"(RT|via)((?:\\b\\W*@\\w+)+)", " ")
    }

    if(!hashtag){
        texto <- stringr::str_replace_all(texto,"#\\w+", " ")
    }



    if(!signos_puntuacion){
        texto <- stringr::str_replace_all(texto,"[[:punct:]]", " ")
    }

    if(!tildes){
        texto<-stringi::stri_trans_general(texto,"Latin-ASCII")
    }

    if(!numeros){
        texto <- stringr::str_replace_all(texto,"[[:digit:]]", " ")
    }

    if(!emoticones){
        texto <- iconv(from = "latin1", to = "ASCII", sub="",texto)
    }

    if(!palabras_vacias){
        texto <- tm::removeWords(texto,stopwords)
    }

    if(minusculas){
        texto <- tolower(as.character(texto))
    }

    if(mayusculas){
        texto <- toupper(as.character(texto))
    }

    # Elimina espacios innecesarios
    texto <- stringr::str_replace_all(texto,"[ \t]{2,}", " ")
    texto <- stringr::str_replace_all(texto,"^\\s+|\\s+$", "")
    texto <- stringr::str_replace_all(texto,"\\n", "")

    texto
}
