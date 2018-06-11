#' @title Retorna tokens comuns entre dois arrays informados nos argumentos
#' @name arrayover
#' @description Esta função recebe dois arrays de tokens e retorna um novo array contendo
#' todos os tokens comuns. Isto é, retorna a intercessão dos conjuntos passados como argumentos.
#' @param vetor1
#' @param vetor2
#' @return Retorna um vetor de strings contendo os tokens em comum entre os dois conjuntos recebidos
#' como argumentos
#' @author Jones Quito
#' @examples 
#' vetor1 <- c('jo','on', 'ne', 'es')
#' vetor2 <- c('jh', 'ho', 'on', ne', 'es')
#' overlap(vetor1, vetor2)
arrayover <- function(vetor1, vetor2){
  over = {c(0:1)}
  
  i = 1
  for(token in vetor1){
    
    if(token %in% vetor2){
      #print(token)
      over[i] = token
      i = i + 1
    }
  }
  return(over)
}