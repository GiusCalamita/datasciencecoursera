######################################################################
## Date of creation: #18/11/2010
## Author: Giuseppe Calamita
## Carica i dati che sono stati salvati con lo script "operazioni_preliminari"
##  
## Contains: Approfondimenti su COLORSO NEW e MOLINO
##
## EDU: manipolazioni legenda in xyplot
#####################################################################

load("dati.RData")

################################################################################
# mi interssa capire perch? CON e MOL vengono cosi male
# calcolo le medie tdr per sito
(M=	medie <- tapply(dati.tutti$tdr, dati.tutti$site, mean))
# e le dev.std
(SD=	std.dev <- tapply(dati.tutti$tdr, dati.tutti$site, sd)))
# e il coeff. di variazione
CV <- SD/M

# altri descrittori:
	m <- tapply(dati.tutti$tdr, dati.tutti$site, min)
	M <- tapply(dati.tutti$tdr, dati.tutti$site, max)
	iqr <- tapply(dati.tutti$tdr, dati.tutti$site, IQR)
#creo una lista
	tutto <- list(medie, std.dev, CV, m, M, iqr)

#e poi li copio su un file .csv cosi potro farci delle tabelle in excel
# 	write.csv2(tutto), file="stat.csv")

#altri descrittori:da copiare a mano
	tapply(dati.tutti$tdr, dati.tutti$site, summary)

# e faccio lo stesso per le resistività
	#calcolo le medie res per sito
	medie <- tapply(dati.tutti$res, dati.tutti$site, mean)
#e le dev.std
	std.dev <- tapply(dati.tutti$res, dati.tutti$site, sd)
#e il coeff. di variazione
	CV  <- std.dev / medie
	m   <- tapply(dati.tutti$res, dati.tutti$site, min)
	med <- tapply(dati.tutti$res, dati.tutti$site, median)
	M   <- tapply(dati.tutti$res, dati.tutti$site, max)
	iqr <- tapply(dati.tutti$res, dati.tutti$site, IQR)

	tutto <- list(medie, std.dev, CV, m, med, M, iqr)
# 	write.csv2(tutto, file="stat.csv", append= F)

#altri descrittori:da copiare a mano
	tapply(dati.tutti$tdr, dati.tutti$site, summary)

# write.csv2(list=("std.dev","medie","CV"))
#e poi li copio su un file .csv cosi potro farci delle tabelle in excel
# 	write.csv2(tapply(dati.tutti$res, dati.tutti$site, mean), file="stat.csv")
# 	write.csv2(tapply(dati.tutti$res, dati.tutti$site, sd), file="stat.csv", append=T)
# 	write.csv2(CV, file="stat.csv", append=T)

###rifacciamo il confronto tra i due tipi di modelli questa volta usando i dati raggrupapti per SITO

################################
  ## Lo stano caso di COLORSO new: 
  ## tutti insieme i dati spot hanno un valore di r>0 !!!
  ## separati per data ...vengono benissimo!
  ################################

#test bilaterale
  cor.test(~ res + tdr, data=CON) #tutti insieme
	cor.test(~ res + tdr, data=CON, subset=(date=="jan"))
	cor.test(~ res + tdr, data=CON, subset=(date=="mar"))
	cor.test(~ res + tdr, data=CON, subset=(date=="jun"))
#)test unilaterale
	cor.test(~ res + tdr, data=CON, alternative="l") #tutti insieme
	cor.test(~ res + tdr, data=CON, subset=(date=="jun"), alternative="l")
	cor.test(~ res + tdr, data=CON, subset=(date=="mar"), alternative="l")
	cor.test(~ res + tdr, data=CON, subset=(date=="jun"), alternative="l")
#poich? ho pochi dati per confermare l'esistenza di una correlazione calcolo i coeff. di Spearman
	cor.test(~ res + tdr, data=CON, 				method="s")
	cor.test(~ res + tdr, data=CON, subset=(date=="jan"), method="s", exact=F)
	cor.test(~ res + tdr, data=CON, subset=(date=="mar"), method="s", exact=F)
	cor.test(~ res + tdr, data=CON, subset=(date=="jun"), method="s", exact=F)
#per dare un occhiata ai dati	
	require(lattice)
  xyplot(tdr~res, groups=date, data=CON, xlim=c(0,250),ylim=c(10,60))

###posso fare lo stesso per MOL
	cor.test(~ res + tdr, data=MOL, alternative="l")
	cor.test(~ res + tdr, data=MOL, alternative="l",method="s")
	
  cor.test(~ res + tdr, data=MOL, subset=(date=="jan"), alternative="l")
	cor.test(~ res + tdr, data=MOL, subset=(date=="jan"), alternative="l", method="s", exact=F)
	
  cor.test(~ res + tdr, data=MOL, subset=(date=="mar"), alternative="l")
	cor.test(~ res + tdr, data=MOL, subset=(date=="mar"), alternative="l", method="s", exact=F)
	
  cor.test(~ res + tdr, data=MOL, subset=(date=="jun"), alternative="l")
	cor.test(~ res + tdr, data=MOL, subset=(date=="jun"), alternative="l", method="s", exact=F)

#per dare un occhiata ai dati
	xyplot(tdr~res, groups=date, data=MOL, xlim=c(0,250),ylim=c(10,60), auto.key=T)

#################################################################################

#immagini

#se voglio fare solo per COLN e MOL
 xyplot(tdr ~ res| site, data= dati.tutti,subset=(site=="CON" | site=="MOL"), xlab= "resistivity Ohm*m", ylab= "%H2O m3/m3")

#posso anche invertire site*date e date*site
xyplot(tdr ~ res| site*date, data= dati.tutti,subset=(site=="CON" | site=="MOL"),  xlim= c(0,200), ylim=c(10,50), xlab= "resistivity Ohm*m", ylab= "%H2O m3/m3")

#creo un data.frame contenente solo COLN e MOL ...cosi per rendermi la vita pi? semplice
	COLNMOL <- rbind(CON,MOL)
	str(COLNMOL)
#il parametro groups controlla i gruppi date
	xyplot(tdr ~ res|site, data=COLNMOL, groups= date)
	 aaa <- simpleKey(text=c("jan","mar","jun")) #creo la legenda

	str(COLNMOL) #le date hanno 5 livelli...devo eliminare quelli che nn mi servono perch? mi
  #danno fastidio nel fare la legenda
	COLNMOL$date <- factor(COLNMOL$date, levels=c("jan","mar", "jun"),
  exclude=(date=="feb" | date=="oct"))
 	str(CON) #controlliamo
#per la legenda posso fare cosi
	xyplot(tdr ~ res|site, data=COLNMOL, groups= date, auto.key=T)
#oppure
	legenda <- simpleKey(levels(COLNMOL$date)) #creo la legenda
	legenda #la analizzo
	legenda$points$pch #visualizzo i formati di punti
	legenda$points$pch <- 1:3 #li cambio
	legenda$points$pch #controllo
#faccio lo stesso con i colori
	legenda$points$col
	legenda$points$col <- 2:4
	legenda$points$col
#ET VOIL?!!!!!!!!!!!!!
	xyplot(tdr ~ res|site, data= COLNMOL, groups= date, 
         xlim= c(0,200), ylim=c(10,50),
      xlab= "resistivity Ohm*m", ylab= "%H2O m3/m3", pch=1:3, col=2:4,
      key=legenda)

#faccio gli scatter plot di COLN e MOL insieme
	par(mfrow=c(1,2))
###	COLN	###
	plot(CON$res, MOL$tdr, pch=1, cex= 0.8,  xlab="", ylab="", type="n", xlim=c(0,200), ylim=c(10,60))
	title(main= "COLN", xlab= "resistivity Ohm*m", ylab= "%H2Ov  m3/m3")
	attach(CON)
	points(tdr[date== "jan"] ~ res[date== "jan"],  pch=1, col= "red")
	points(tdr[date== "mar"] ~ res[date== "mar"], pch=1, col= "blue")
	points(tdr[date== "jun"] ~ res[date== "jun"],  pch=1, col= "darkseagreen4")
	detach()
legend("bottomright", legend= levels(CON$date)[-c(2,5)], col=c("red", "blue", "darkseagreen4"), pch=c(1,1,1))
###	MOLINO	###
	plot(MOL$res, MOL$tdr, pch=1, cex= 0.8,  xlab="", ylab="", type="n", xlim=c(0,200), ylim=c(10,60))
	title(main= "MOL", xlab= "resistivity Ohm*m", ylab= "%H2Ov  m3/m3")
	attach(MOL)
	points(tdr[date== "jan"] ~ res[date== "jan"],  pch=1, col= "red")
	points(tdr[date== "mar"] ~ res[date== "mar"], pch=1, col= "blue")
	points(tdr[date== "jun"] ~ res[date== "jun"],  pch=1, col= "darkseagreen4")
	detach()
search()
#inseriamo la legenda
	legend("bottomright", legend= levels(MOL$date)[-c(2,5)], col=c("red", "blue", "darkseagreen4"), pch=c(1,1,1), btyn)

################################################################################
######    LO STRaNO CASO DI COLORSO NEW E MOLINO
#### tecniche grafiche (pi?) avanzate
################################################################################
#se voglio farli solo per COLN e MOL

xyplot(tdr ~ res| site, data= dati.tutti, subset=(site=="CON" | site=="MOL"),   #SCATTER PLOT SOLI DATI DI COLN E MOL,MOLTO SEMPLICE E POVERO
 xlab= "resistivity Ohm*m", ylab= "%H2O m3/m3")
 
#SUDDIVIDO LE MISURE PER DATE E SITO: posso anche invertire site*date e date*site
xyplot(tdr ~ res| site*date, data= dati.tutti, 
      subset=(site=="CON" | site=="MOL"),  xlim= c(0,200), ylim=c(10,50), 
      xlab= "resistivity Ohm*m", ylab= "%H2O m3/m3")

#creo un data.frame contenente solo COLN e MOL ...cosi per rendermi la vita pi? semplice
	COLNMOL <- rbind(CON,MOL)
	str(COLNMOL) #le date hanno 5 livelli...devo eliminare quelli che nn mi servono perch? mi danno fastidio nel fare la legenda
 	COLNMOL$date <- factor(COLNMOL$date, levels=c("jan","mar", "jun"), 
   exclude=(COLNMOL$date=="feb" | COLNMOL$date=="oct"))
 	str(CON) #controlliamO

xyplot(tdr ~ res|site, data=COLNMOL, groups= date, auto.key=T)                  #il parametro groups controlla i gruppi date
		
#se voglio metttere la legenda altrove uso il parametro space in auto.key
xyplot(tdr ~ res | site, data=COLNMOL, groups= date, 
  auto.key= list(space="right"),
  ylab= list("soil moisture [%]", cex=1.3), 
  xlab=list("electrical resistivity [Ohm m]", cex=1.3))

#oppure
	legenda <- simpleKey(levels(COLNMOL$date)) #creo la legenda
	legenda #la analizzo
	legenda$points$pch #visualizzo i formati di punti
	legenda$points$pch <- 1:3 #li cambio
	legenda$points$pch #controllo
#faccio lo stesso con i colori	
	legenda$points$col
	legenda$points$col <- 2:4
	legenda$points$col
#ET VOIL?!!!!!!!!!!!!!
xyplot(tdr ~ res|site, data=COLNMOL, groups= date, xlim= c(0,200), ylim=c(10,50), 
      xlab="resistivity Ohm*m", ylab="%H2O m3/m3", pch=1:3, col=2:4, key=legenda))