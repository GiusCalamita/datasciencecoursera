######################################################################
# 18/11/2010 Giuseppe Calamita
# sto provando a mettere un p? di ordine
#####################################################################

########################
# OPERAZIONI PRELIMINARI
########################

#### Read data in ----
  dati.tutti <- read.csv2(file= "dati/misure-tdr.csv") 
 	
#### Nomi variabili ----
  names(dati.tutti)  # colnames()
  names(dati.tutti) <- tolower(names(dati.tutti)) 
  names(dati.tutti)[3] <- "res"

#### Aggiungo una variabile: log(res)
 dati.tutti$logres <- log10(dati.tutti$res)


#####################
# Statistica descrittiva
#####################

### quali sono i 10 valori pi? alti di sm e di res ?
  head(sort(dati.tutti$res, decreasing= T), n = 10) # mostrami i 10 valori di res pi? elevati
  head(order(dati.tutti$res, decreasing=T), n = 10) # mostra le posizioni dei valori di res piu elevati
  head(dati.tutti[order(dati.tutti$res, decreasing= T), ], n = 15) # mostra i valori piu alti di res e tutti gli altri valori associati
### si vede che i valori pi? ALTI di resistivit? sono concentrati a gennaio, con qualche eccezione

### e quelli pi? BASSI di sm?
  head(dati.tutti[order(dati.tutti$tdr), ], n = 15)  # mostra i valori piu alti di res e tutti gli altri valori associati


### tabella riassuntiva ------
### ordina le date
 	# table(dati.tutti$site, dati.tutti$date)
dati.tutti$date <- 
  factor(dati.tutti$date,
         levels = c("jan", "feb", "mar", "jun", "oct"))      #riordino i livelli
table(dati.tutti$site, dati.tutti$date)                                       

# se mi interessa quando ho campionato un certo sito..
	unique(dati.tutti$date[dati.tutti$site == "MOL"])

## PREPARIAMO I DATI IN MANIERA PI? COMODA (ALMENO PER QUANTO NE CAPISCA IO!!!!)
## creo dei data frame contenenti i dati suddivisi per data di campionamento:
## pi? comodi da maneggiare e codici pi brevi
## posso crearli almeno in due diversi modi cosi:

	jan <- dati.tutti[dati.tutti$date == "jan",]; dim(jan)
	feb <- dati.tutti[dati.tutti$date == "feb", ]; dim(feb)
	mar <- dati.tutti[dati.tutti$date == "mar", ]; dim(mar)
	jun <- dati.tutti[dati.tutti$date == "jun",]; dim(jun)
#	jul <- dati.tutti[dati.tutti$date == "jul",]; dim(jul)
	oct <- dati.tutti[dati.tutti$date == "oct",]; dim(oct)

#...o cosi ( possibile scegliere solo le colonne che mi interessano):
#	jan <- subset(dati.tutti, date == "jan", select =  c("site", "date", "res", "tdr"))
#	feb <- subset(dati.tutti, date == "feb", select =  c("site", "date", "res", "tdr"))
#	mar <- subset(dati.tutti, date == "mar", select =  c("site", "date", "res", "tdr"))
#	jun <- subset(dati.tutti, date == "jun", select =  c("site", "date", "res", "tdr"))
#	oct <- subset(dati.tutti, date == "oct", select =  c("site", "date", "res", "tdr"))


#se voglio eliminare i dati di luglio
# dati.nolugl <- dati.tutti[dati.tutti$site != "jul"]
#creo i vettori
	CBE <- subset(dati.tutti, site == "CBE", select = c("site", "date", "res", "logres", "tdr"))
	COL <- subset(dati.tutti, site == "COL", select = c("site", "date", "res", "logres", "tdr"))
	CON <- subset(dati.tutti, site == "CON", select = c("site", "date", "res", "logres", "tdr"))
	CRI  <- subset(dati.tutti, site == "CRI", select = c("site", "date", "res", "logres", "tdr"))
	LEC  <- subset(dati.tutti, site == "LEC", select = c("site", "date", "res", "logres", "tdr"))
	MOL  <- subset(dati.tutti, site == "MOL", select = c("site", "date", "res", "logres", "tdr"))
	PRE  <- subset(dati.tutti, site == "PRE", select = c("site", "date", "res", "logres", "tdr"))
	VRO  <- subset(dati.tutti, site == "VRO", select = c("site", "date", "res", "logres", "tdr"))
	ls()


######################
# medie areali
######################
## ho preparato un file csv contente gia tutte le medie e le deviazioni standard 
	medie.areali <- read.csv2(file="dati/medie_areali.csv")
 	medie.areali$date <- factor(medie.areali$date, 
                              levels = c("jan", "mar", "jun", "oct"))    #riordino i livelli delle date
	medie_fdr <- read.csv2(file="dati/medie_fdr.csv")

## infine le sonde FDR
  sondefisse <- read.csv2(file= "dati/fdr.csv")