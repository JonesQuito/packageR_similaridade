% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/tokenizer.R
\name{tokenizer}
\alias{tokenizer}
\title{Gera tokens a partir de uma string}
\usage{
tokenizer(texto, q)
}
\arguments{
\item{texto}{A string a ser tokenizada}

\item{q}{Valor inteiro que indica o tamanho dos tokens a ser gerados}
}
\value{
Um vetor de strings com os tokens gerados
}
\description{
Função que recebe uma string e um número inteiro e gera tokens do tamanho indicado pelo inteiro recebido.
}
\examples{
tokenizer('Um texto qualquer', 2)
}
\author{
Jones Quito
}
