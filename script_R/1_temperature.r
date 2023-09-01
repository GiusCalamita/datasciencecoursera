######################################################################
## Date of creation: 28/05/2010 
## Author: Giuseppe Calamita
## Carica i dati di ER-SM e di Temp. aria/suolo
## applica funzione per la corr. temperatura a dati ER
## confronta correlazione e grafico di ER vs SM per dati corretti e non
#####################################################################

########################
# #read data in 
########################
  
  dati.tutti <- read.csv2(file="dati/misure-tdr.csv") 
	temperatures <- read.csv("dati/Temperature.csv")


########################
# explore data sets 
########################

# dati.tutti 
 dim(dati.tutti)    
 str(dati.tutti)


#nomi variabili  
  names(dati.tutti)  # colnames()
  names(dati.tutti) <- tolower(names(dati.tutti)) 
  names(dati.tutti)[3] <- "res"

# cambio anche il nome di 'date' per evitare ambiguit?
# infatti nel file temperatures le date esplicite (es 2008-31-12) si chiamano 'date' ... e questo mi servirà sia per un eventuale merge sia per incrociare i dati
  names(dati.tutti)[1] <- "month" 


# riordino i livelli
levels(dati.tutti$month)
dati.tutti$month <- factor(dati.tutti$month,
                            levels = c("jan", "feb", "mar", "jun", "oct")) 


# temperature
 dim(temperatures)    
 str(temperatures)

# cambio nome da 'data a date' 
names(temperatures)[1]<- "date"
#converto la variabile data in date class
class(temperatures$date)
temperatures$date <- as.Date(temperatures$date, format= "%d-%b-%y")
range(temperatures$date)

# estraggo le temperature per le date che mi interessano
T_vallaccia <- rbind(temperatures[temperatures$date=="2008-01-09",], 
temperatures[temperatures$date=="2008-02-20",], 
temperatures[temperatures$date=="2008-03-12",], 
temperatures[temperatures$date=="2008-06-05",], 
temperatures[temperatures$date=="2008-10-10",]
)


#aggiungo una colonna (che mi servir? dopo) in comune con dati tutti
T_vallaccia$month<- levels(dati.tutti$month)


#creo un unico data set
dati.tutti <-
 merge(dati.tutti, T_vallaccia, sort= F)


########################
# load data 
########################
# save.image("dati/dati_res_T")


################################################################################
##########		Fine operazioni preliminari		################
################################################################################


########################
# load data 
########################
load("dati/dati_res_T")

########################
# load functions from external source file 
########################
source("sheets_hendrickx_correction.r")

########################
# 
########################
#da sistemare meglio
(temp_names <- names(temperatures)[-1])
(n <- length(temp_names))
res_corrected <- vector("list")
for(i in temp_names) res_corrected[[i]] <- 
			sheets.hendrickx(dati.tutti$res, dati.tutti[i])
rm(i,n)

#correlazione tra resistività corrette e TDR
for(i in 1:4)print(cor(dati.tutti$tdr, res_corrected[[i]]))

#scelgo la migliore e aggiungo la colonna a dati.tutti
dati.tutti[,10]  <- res_corrected[[1]]
names(dati.tutti)[10] <- "res_cor_TSF1"


################################
# scatter plot tutti i dati 
# EVIDENZIANDO gruppi DATA o SITO
##################################
par(mfrow= c(1,2))

#dati originali
plot(tdr ~ res, dati.tutti, pch= 20, cex=1.4,
	col= "aquamarine3",
	xlab= "resistivity [Ohm m]", 
	ylab= "soil moisture [%vol/vol]",
	type="n")

attach(dati.tutti)
#evidenzio le Date
 points(tdr[month== "jan"] ~ res[month== "jan"],  
 	pch=1, col= 2, cex=1.3) #"red"
 points(tdr[month== "feb"] ~ res[month== "feb"],
 	pch=2, col= 3, cex=1.3)
 points(tdr[month== "mar"] ~ res[month== "mar"],
 	pch=4, col= 4, cex=1.3)
 points(tdr[month== "jun"] ~ res[month== "jun"],
 	pch=5, col= 5, cex=1.3)
 points(tdr[month== "oct"] ~ res[month== "oct"],
 	pch=6, col= 7, cex=1.3)

#inseriamo la legenda
 legend("topright", legend= levels(dati.tutti$month), 
 	col=c(2,3,4,5,7), pch=c(1,2,4,5,6),
 	cex= 1.5, inset=0.02)
 	
 text(100, 55, paste("r= ", round(cor(tdr,res),2)), cex=2)

 detach(dati.tutti)
 

#dati corretti
plot(tdr ~ res_cor_TSF1, dati.tutti, pch= 20, cex=1.4,
	col= "aquamarine3",
	xlab= "resistivity [Ohm m]", 
	ylab= "soil moisture [%vol/vol]",
	type="n")

attach(dati.tutti)
#evidenzio le Date
 points(tdr[month== "jan"] ~ res_cor_TSF1[month== "jan"],  
 	pch=1, col= 2, cex=1.3) #"red"
 points(tdr[month== "feb"] ~ res_cor_TSF1[month== "feb"],
 	pch=2, col= 3, cex=1.3)
 points(tdr[month== "mar"] ~ res_cor_TSF1[month== "mar"],
 	pch=4, col= 4, cex=1.3)
 points(tdr[month== "jun"] ~ res_cor_TSF1[month== "jun"],
 	pch=5, col= 5, cex=1.3)
 points(tdr[month== "oct"] ~ res_cor_TSF1[month== "oct"],
 	pch=6, col= 7, cex=1.3)

#inseriamo la legenda
 legend("topright", legend= levels(dati.tutti$month), 
 	col=c(2,3,4,5,7), pch=c(1,2,4,5,6),
 	cex= 1.5, inset=0.02)
 	
 text(100, 55, paste("r= ", round(cor(tdr,res_cor_TSF1),2)), cex=2)

 detach(dati.tutti)

