#' @title Calcula a sobreposição de dois vetores
#' @name overlap
#' @description Função que calcula a sobreposição de dois vetores passados como argumento
#' @param vetor1 vetor de tokens
#' @param vetor2 vetor de tokens
#' @return Retorna um inteiro indicando a sobreposição dos elementos de vetor1 e vetor2
#' @author Jones Quito
#' @examples 
#' vetor1 <- c('jo','on', 'ne', 'es')
#' vetor2 <- c('jh', 'ho', 'on', ne', 'es')
#' overlap(vetor1, vetor2)
#' 
#' 
overlap <- function(vetor1, vetor2){
  over = 0
  for(token in vetor1){
    if(token %in% vetor2){
      over = over + 1
    }
  }
  return(over)
}