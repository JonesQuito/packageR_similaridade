#' @title Gera tokens a partir de uma string
#' @name tokenizer
#' @description Função que recebe uma string e um número inteiro e gera tokens do tamanho indicado pelo inteiro recebido.
#' @param texto A string a ser tokenizada
#' @param q Valor inteiro que indica o tamanho dos tokens a ser gerados
#' @return Um vetor de strings com os tokens gerados
#' @author Jones Quito
#' @examples 
#' tokenizer('Um texto qualquer', 2)
#' @export
tokenizer <- function(texto, q){
  nome <- paste(paste('*',texto, sep = ''), '*', sep = '')
  tam = nchar(nome) - (q-1)
  
  vetor = c(1:1)
  
  for(i in 1:tam){
    vetor[i] = substr(nome, i, i+(q-1))
  }
  return(vetor)
}