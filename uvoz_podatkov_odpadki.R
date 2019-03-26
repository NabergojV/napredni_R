library(knitr)
library(dplyr)
library(gsubfn)
library(ggplot2)
library(rvest)
library(XML)

# uvoz tabele vrste odpadkov
stolpci1 <- c("Vrsta", "Nastanek", "Leto", "Stevilo [tona]")
odpadki_vrste <- read.csv2("vrste_odpadkov.csv", skip=3, fileEncoding = "UTF-8")[1:4733]

# s funkcijo uredu uredimo tabelo v tidy data
uredi <- function(tabela, x, y, z, max = nrow(tabela)) {
  s <- seq(x, max, z+1)
  tabela[t(matrix(x:max, ncol=length(s))), y] <- tabela[s, y]
  return(tabela)
}

odpadki_vrste <- uredi(odpadki_vrste, 1,1,5)
# zbrišemo vsako šesto vrstico
odpadki_vrste <- odpadki_vrste[-seq(1,nrow(odpadki_vrste),6),]

odpadki_vrste <- read.csv2("odpadki_regije.csv", skip=3, fileEncoding = "UTF-8")
