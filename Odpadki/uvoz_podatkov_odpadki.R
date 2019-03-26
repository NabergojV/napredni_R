library(knitr)
library(dplyr)
library(gsubfn)
library(ggplot2)
library(rvest)
library(XML)
library(readr)

# uvoz tabele vrste odpadkov

stolpci1 <- c("Vrsta", "Nastanek", "Leto", "Kolicina_tona")
odpadki_vrste <- read.csv2("vrste_odpadkov.csv", 
                           skip = 3,
                           na.strings = "-",
                           header = FALSE,
                           col.names = stolpci1,
                           fileEncoding = "UTF-8")

# s funkcijo uredu uredimo tabelo v tidy data
uredi <- function(tabela, x, y, z, max = nrow(tabela)) {
  s <- seq(x, max, z+1)
  tabela[t(matrix(x:max, ncol=length(s))), y] <- tabela[s, y]
  return(tabela)
}

odpadki_vrste <- uredi(odpadki_vrste, 1,1,85)
odpadki_vrste <- odpadki_vrste[-seq(1,nrow(odpadki_vrste),86),]
odpadki_vrste <- uredi(odpadki_vrste, 1,2,16)
odpadki_vrste <- odpadki_vrste[-seq(1,nrow(odpadki_vrste),17),]

# uredimo imena
odpadki_vrste$Vrsta <- gsub("^\\d+", "", odpadki_vrste$Vrsta)
odpadki_vrste$Vrsta <- gsub("^\\s", "", odpadki_vrste$Vrsta)

vrste_odpadkov <- unique(odpadki_vrste$Vrsta)
nastanek_odpadkov <- unique(odpadki_vrste$Nastanek)

# https://pxweb.stat.si/pxweb/Dialog/SaveShow.asp

# uvoz tabele odpadki po regijah
stolpci2 <- c("Regija", "Leto", "Odpadki", "Kolicina_tona")
odpadki_regije <- read.csv2("odpadki_regije.csv", 
                           skip = 3, 
                           header = FALSE,
                           na.strings = "-",
                           col.names = stolpci2,
                           fileEncoding = "UTF-8")

odpadki_regije <- uredi(odpadki_regije, 1,1,32)
odpadki_regije <- odpadki_regije[-seq(1,nrow(odpadki_regije),33),]
odpadki_regije <- uredi(odpadki_regije, 1,2,3)
odpadki_regije <- odpadki_regije[-seq(1,nrow(odpadki_regije),4),]

# uredimo imena
#odpadki_regije$Odpadki <- gsub("\\s*\\w*$", "", odpadki_regije$Odpadki)


# uvoz tabele odpadki EU
stolpci3 <- c("TIME","GEO","UNIT","HAZARD","WASTE","NACE_R2","Value")
odpadki_EU <- read.csv2("odpadki_EU.csv", 
                        header = TRUE,
                        sep = ",",
                        na.strings = "-",
                        col.names = stolpci3,
                        fileEncoding = "UTF-8")[,-c(3,6)]

# http://appsso.eurostat.ec.europa.eu/nui/submitViewTableAction.do




