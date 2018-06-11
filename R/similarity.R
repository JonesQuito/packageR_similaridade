#' @title Calcula a similaridade de duas strings
#' @name similarity
#' @description Esta função retorna um valor de ponto flutuante que repesenta a similaridade entre
#' duas strings informadas como argumentos. A similaridade é calculada de acordo com o algorítimo
#' informado com argumento.
#' @param s1 é a primeira de duas strings a ser comparadas
#' @param s2 é a segunda de duas strings a ser comparadas
#' @param q o tamanho dos tokens a ser gerados para calculo de similaridade - dois(2) é um tamanho bom
#' @param simFunc um inteiro que representa a função de similaridad a ser usada. As opções são:
#' 1 - Similaridade de Jaccard
#' 2 - Similaridade Dice
#' 3 - Similaridade Cosine
#' 
#' @return Retorna um valor float entre 0 e 1 representando a porcentagem de similaridade entre as strings.
#' @examples 
#' texto1 <- 'Felipe de Sousa'
#' texto2 <- 'Felipe de Souza'
#' 
#' sim <- similarity(texto1, texto2, 3, 1)
#' 
#' @author Jones Quito
#' @export
#' 
similarity <- function(s1, s2, q, simFunc){
  
  s1_card = nchar(s1) + q - 1;
  s2_card = nchar(s2) + q - 1;
  
  array_s1 = tokenizer(s1, q)
  array_s2 = tokenizer(s2, q)
  
  array_over = arrayover(array_s1, array_s2)
  overlap = overlap(array_s1, array_s2)
  
  sim = 0
  
  if(simFunc == 1){
    sim = overlap/(s1_card + s2_card - overlap)
  }else if(simFunc == 2){
    sim = (2 * overlap)/(s1_card + s2_card)
  }else if(simFunc == 3){
    sim = overlap/sqrt(s1_card * s2_card)
  }else{
    sim = overlap/(s1_card + s2_card - overlap)
  }
  
  return(sim)
}