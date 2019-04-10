source("lib.R")
source("uvozi_zemljevid.R", encoding = "UTF-8")

# uvoz tabele stevilo prebivalcev po regijah

stolpci_preb <- c("Regija", "Leto", "Stevilo")
prebivalci <- read.csv2("stevilo_prebivalcev.csv", 
                         skip = 2,
                         header = FALSE,
                         col.names = stolpci_preb,
                         fileEncoding = "UTF-8")

# s funkcijo uredu uredimo tabelo v tidy data
uredi <- function(tabela, x, y, z, max = nrow(tabela)) {
  s <- seq(x, max, z+1)
  tabela[t(matrix(x:max, ncol=length(s))), y] <- tabela[s, y]
  return(tabela)
}

# uredimo tabelo
prebivalci <- uredi(prebivalci, 1, 1, 16)
prebivalci <- prebivalci[-seq(1, nrow(prebivalci), 17),]

# spremenimmo nekatera imena regij, da se bodo ujemala z zemljevidom
prebivalci$Regija <- revalue(prebivalci$Regija, 
                                 revalue(c("Primorsko-notranjska" = "Notranjsko-kraska",
                                           "Posavska" = "Spodnjeposavska")))

# naredimo dve tabeli, ena za celotno Slovenijo, druga za regije
preb_slo <- prebivalci[which(prebivalci$Regija == "SLOVENIJA"), c(2,3)]

preb_reg <- prebivalci[which(prebivalci$Regija != "SLOVENIJA" & 
                             prebivalci$Regija !="Zahodna Slovenija" &
                             prebivalci$Regija != "Vzhodna Slovenija" &
                             prebivalci$Leto %in% seq(2012,2017,1)),]

# uredimo po abecednem vrstnem redu glede na regije
preb_reg <- preb_reg[order(preb_reg$Regija),]

#---------------------------------------------------------------------

# uvoz tabele vrste odpadkov

stolpci1 <- c("Vrsta", "Nastanek", "Leto", "Kolicina_tona")
odpadki_vrste <- read.csv2("vrste_odpadkov.csv", 
                           skip = 3,
                           na.strings = "-",
                           header = FALSE,
                           col.names = stolpci1,
                           fileEncoding = "UTF-8")

odpadki_vrste <- uredi(odpadki_vrste, 1,1,85)
odpadki_vrste <- odpadki_vrste[-seq(1,nrow(odpadki_vrste),86),]
odpadki_vrste <- uredi(odpadki_vrste, 1,2,16)
odpadki_vrste <- odpadki_vrste[-seq(1,nrow(odpadki_vrste),17),]

# uredimo imena
odpadki_vrste$Vrsta <- gsub("^\\d+", "", odpadki_vrste$Vrsta)
odpadki_vrste$Vrsta <- gsub("^\\s", "", odpadki_vrste$Vrsta)

vrste_odpadkov <- unique(odpadki_vrste$Vrsta)
nastanek_odpadkov <- unique(odpadki_vrste$Nastanek)

# združimo tabelo vrste odpadkov in tabelo stevilo preb. v Slo
odpadki_vrste <- merge(odpadki_vrste, preb_slo, by = c("Leto"))

# dodamo stolpec kolicina smeti (v kilogramih) na prebivalca
odpadki_vrste$"Kolicina_kg/Prebivalec" <- round(odpadki_vrste$Kolicina_tona*1000/odpadki_vrste$Stevilo, 4)

# odstranimo stolpec s številom prebivalcev
odpadki_vrste <- odpadki_vrste[, c(1:4,6)]

#-----------------------------------------------------------------------

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

# uredimo imena (odstranimo besedo (tone) na koncu)
odpadki_regije$Odpadki <- sapply(strsplit(as.character(odpadki_regije$Odpadki), split=" (", fixed=TRUE), 
                                 function(x) x[1])

odpadki_regije <- odpadki_regije[which(odpadki_regije$Odpadki == "Nastali komunalni odpadki"),]

# spremenimo poimenovanje regij
odpadki_regije$Regija <- revalue(odpadki_regije$Regija, 
                                 revalue(c("Primorsko-notranjska" = "Notranjsko-kraska",
                                           "Posavska" = "Spodnjeposavska")))

odpadki_regije$Regija <- as.character(odpadki_regije$Regija)
odpadki_regije <- odpadki_regije[order(odpadki_regije$Regija),]

odpadki_regije2 <- odpadki_regije %>% filter(Regija != "Zahodna Slovenija" &
                                               Regija != "Vzhodna Slovenija")

odpadki_regije2 <- merge(odpadki_regije2, prebivalci, by = c("Regija", "Leto"))
odpadki_regije2$'Kolicina_kg/Prebivalec' <- round(odpadki_regije2$Kolicina_tona*1000/odpadki_regije2$Stevilo, 6)

# naredimo tabele, ki jih bomo združili z zemljevidom
tabela_zemljevid <- odpadki_regije[which(odpadki_regije$Regija != "SLOVENIJA" & 
                                         odpadki_regije$Regija !="Zahodna Slovenija" &
                                         odpadki_regije$Regija != "Vzhodna Slovenija" &
                                         odpadki_regije$Leto != 2010 &
                                         odpadki_regije$Leto != 2011),]

regije <- unique(odpadki_regije$Regija)

# združimo tabela_zemlljevid s tabelo preb_reg
tabela_zemljevid <- merge(tabela_zemljevid, preb_reg, by = c("Regija", "Leto"))

# dodamo stolpec kolicina smeti (v kilogramih) na prebivalca
tabela_zemljevid$"Kolicina_kg/Prebivalec" <- round(tabela_zemljevid$Kolicina_tona*1000/tabela_zemljevid$Stevilo, 6)

# odstranimo stolpec s številom prebivalcev
tabela_zemljevid <- tabela_zemljevid[, c(1:4,6)]


#-----------------------------------------------------------------------

# uvoz tabele odpadki EU

stolpci3 <- c("Leto","Drzava","UNIT","Nevarnost","Vrsta","NACE_R2","Kolicina")
odpadki_EU <- read.csv2("odpadki_EU.csv", 
                        header = TRUE,
                        sep = ",",
                        na.strings = ":",
                        col.names = stolpci3,
                        fileEncoding = "UTF-8")[,-c(3,6)]

# odpravimo vejice pri tisočicah
odpadki_EU$Kolicina <- as.numeric(gsub(",", "", odpadki_EU$Kolicina, fixed = TRUE)) 



