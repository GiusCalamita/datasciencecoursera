######################################################################
## Date of creation: 27/11/2010
## Author: Giuseppe Calamita
## Carica dati che sono stati salvati con lo script "operazioni_preliminari"
## 
## Contains: Analisi correlazione ER-SM sulle mdie areali
## corr. Lineare-NonLineare
##  
## EDU: 
## weighted.mean()
##  
#####################################################################

#caricare il worksapce salvato dal file "dati.RData"
#contiene tutti i dati necessari: gi? svolte le operazioni preliminari

load("dati.RData")

##############
### modifica data set: calcola SD of Mean  ####
##############
# Dal Taylor ho scoperto che interessa è:
 # la media e sua dev.std (SDOM: st. dev. of mean nota anche come errore standard)
 # non la dev.std del set di dati (questa rappresenta l'errore medio associato a ciasucna misura) 
 
# quindi devo modificare il data set

# rapida verifica 
 by(jan$tdr, jan$site, sd)
 by(jan$tdr, jan$site, mean)
 names(medie.areali)

#quello che devo fare è:
 #sostituire ai valori di sd_res (e sd_tdr) quelli della sd_res/16
 medie.areali$es_tdr <- medie.areali$sd_tdr/sqrt(16)
 medie.areali$es_res <- medie.areali$sd_res/sqrt(16)
 
########################
# creo vettori per ciascun mese x comodità                       
########################

  jan.m <- medie.areali[medie.areali$date=="jan",]
  mar.m <- medie.areali[medie.areali$date=="mar",]
  jun.m <- medie.areali[medie.areali$date=="jun",]
  oct.m <- medie.areali[medie.areali$date=="oct",]
  
#oppure leggiamo solo i dati che ci interessano
  #medie.areali <- read.csv2(file="dati/medie_areali.csv")
  # str(medie.areali)

#################################
#### scatter plot delle MEDIE AREALI: pckg "graphs" ####
#################################
# per un controllo più fine su colori e simboli guardare le dispense di Rositter

# colori diversi per ciascun sito..
	plot(medie.areali$res, medie.areali$tdr, 
       xlim= c(0,250), ylim= c(0,60),       #uso il pacchetto graphics di base
       xlab= "", ylab= "", type= "n", cex.axis= 1.5)                                    #fammi lo scatter plot ...ma non plottare
	title(main= "Vallaccia area - spatial averages", cex.main= 2,                  #altro titolo :Vallaccia area
    xlab= list("apparent resistivity  (Ohm m)", cex=1.5),
    ylab= list("soil moisture (%)", cex=1.5))

# segmenti delle dev. std...VOIL?!!!!!!                                         #help.search("error bar")
#le metto prima cosi i punti si vedono meglio dopo
	segments(medie.areali$res-medie.areali$sd_res, medie.areali$tdr,              #fatte a mano
  medie.areali$res + medie.areali$sd_res, medie.areali$tdr)
	segments(medie.areali$res, medie.areali$tdr-medie.areali$sd_tdr,
  medie.areali$res, medie.areali$tdr + medie.areali$sd_tdr)                     #vedi funzione 'plotCI()'

#a questo punto posso decidere se evidenziare i siti o le date

#siti
 attach(medie.areali)
	points(res[site== "CBE"], tdr[site=="CBE"],  pch= 16, col= "red2")
	points(res[site== "CON"], tdr[site== "CON"], pch= 16, col= "green2")
	points(res[site== "COL"], tdr[site== "COL"], pch=16,  col= "blue2")
	points(res[site== "CRI"], tdr[site== "CRI"], pch=16,  col= "goldenrod2")
	points(res[site== "LEC"], tdr[site== "LEC"], pch=16,  col= "cyan2")
	points(res[site== "MOL"], tdr[site== "MOL"], pch=16,  col= "yellow")
	points(res[site== "VRO"], tdr[site== "VRO"], pch=16,  col= "darkseagreen2")
	points(res[site== "PRE"], tdr[site== "PRE"], pch=16,  col= "pink")
detach()
search()
#inseriamo la legenda
	legend("topright", inset= c(0.05, 0.05), legend= levels(medie.areali$site), 
          col=c("red2", "green2", "blue2", "goldenrod2", "cyan2", "yellow", 
          "darkseagreen2", "pink"), 
          pch=c(16,16,16,16,16,16,16,16), btyn)

#date
attach(medie.areali)
	points(res[date== "jan"], tdr[date=="jan"],  pch= 15, col= "red2")
	points(res[date== "mar"], tdr[date=="mar"],  pch= 16, col= "cyan")
	points(res[date== "jun"], tdr[date=="jun"],  pch= 17, col= "blue2")
	points(res[date== "oct"], tdr[date=="oct"],  pch= 18, col= "goldenrod2")
#	points(res[date== "FDR"], tdr[date=="FDR"],  pch= 19, col= "lightgreen")
detach()

#inseriamo la legenda
	legend("topright", inset= c(0.05, 0.05), legend= levels(medie.areali$date),
         col=c("red2", "cyan", "blue2", "goldenrod2"), 
         pch=c(15,16,17,18))  


###################################
#### inserisco le curva della biliografia... #####
###################################
#clay soils
curve(132.25*x^-0.61, 0.1, 750, add=T, col= "green", lwd=2, lty= 2)
#suoli msiti
curve(126.17*x^-0.45, 0.1, 750, add=T, col= "red", lwd=2, lty= 2)
#sandy soils
curve(654.01*x^-0.71, 0.1, 750, add=T, col= "black", lwd=2, lty= 2)

# savePlot("F:/cnr/perugia/cartelladilavoro/immagini/scatt-date-fdr-biblio",     #ridefinire il path a seconda dei casi
#          type="jpeg")

###################################
#...oppure di ciascuna data...
###################################
curve(66.717*x^-0.172, 0.1, add=T, lty=3, col="red2",       lwd=3)
curve(72.642*x^-0.217, 0.1, add=T, lty=3, col="cyan",       lwd=3)
curve(73.384*x^-0.292, 0.1, add=T, lty=3, col="blue2",      lwd=3)
curve(62.214*x^-0.212, 0.1, add=T, lty=3, col="goldenrod2", lwd=3)

abline(h= mean(jan$tdr), lty=2, lwd=2, col="red2")
abline(h= mean(mar$tdr), lty=2, lwd=2, col="cyan")
abline(h= mean(jun$tdr), lty=2, lwd=2, col="blue2")
abline(h= mean(oct$tdr), lty=2, lwd=2, col="goldenrod2")

savePlot("/data/vallaccia/immagini/scatt_medie", type="jpeg")

#e siccom s? ffort assai, mo ti faccio pure la retta di regressione...ohps ..la curva scusa :P

	tt <- seq(0, 300, by= 0.1)
	poppo <- nls(tdr ~ a*res^b, data=medie.areali)                                #stimo i parametri del modello e li salvo in poppo
	predict(poppo)
	predict(poppo, list(res = tt))                                                #giusto per capire come funziona il predict
	lines(tt, predict(poppo, list(res = tt)), col="darkseagreen4")
	lines(tt, predict(poppo, list(res = tt)), col="red")                          #se ti piace unaltro colore YEAH!!!

#posso pensare di fare lo stesso scatter plot con qualche variazione su tipo di punti e colori
#####################
###### SIMBOLI diversi...PER? SI VEDR? MALE!!! ######
#####################

plot(medie.areali$res, medie.areali$tdr, xlim= c(0,270), ylim= c(10,65),
  xlab="", ylab="", type="n", cex.axis= 1.2) #fammi lo scatter plot ...ma non plottare
	title(main= "TDR vs Resistivity", xlab= "apparent resistivity  Ohm*m",
  ylab= "soil moisture %")
	segments(medie.areali$res-medie.areali$sd_res, medie.areali$tdr,
  medie.areali$res + medie.areali$sd_res, medie.areali$tdr)
	segments(medie.areali$res, medie.areali$tdr-medie.areali$sd_tdr,
  medie.areali$res, medie.areali$tdr + medie.areali$sd_tdr)
attach(medie.areali)
	points(res[site== "CBE"], tdr[site=="CBE"],  pch= 1 )
	points(res[site== "CON"], tdr[site== "CON"], pch= 2 )
	points(res[site== "COL"], tdr[site== "COL"], pch= 3 )
	points(res[site== "CRI"], tdr[site== "CRI"], pch= 4 )
	points(res[site== "LEC"], tdr[site== "LEC"], pch= 5 )
	points(res[site== "MOL"], tdr[site== "MOL"], pch= 6 )
	points(res[site== "VRO"], tdr[site== "VRO"], pch= 7 )
	points(res[site== "PRE"], tdr[site== "PRE"], pch= 8 )
detach()
search()
#inseriamo la legenda
	legend("topright", inset= c(0.05, 0.05), legend= levels(medie.areali$site), 
          pch=c(1:8), btyn)

#####################
##### COLORI DIVERSI #####
#####################
plot(medie.areali$res, medie.areali$tdr, xlim= c(0,270), ylim= c(10,65),
  xlab="", ylab="", type="n", cex.axis= 1.2)                                     #fammi lo scatter plot ...ma non plottare
	title(main= "TDR vs Resistivity", xlab= "apparent resistivity  Ohm*m", ylab= "soil moisture %")
	segments(medie.areali$res-medie.areali$sd_res, medie.areali$tdr, medie.areali$res + medie.areali$sd_res, medie.areali$tdr)
	segments(medie.areali$res, medie.areali$tdr-medie.areali$sd_tdr,  medie.areali$res, medie.areali$tdr + medie.areali$sd_tdr)
attach(medie.areali)
	points(res[site== "CBE"], tdr[site=="CBE"],  pch=16, col= 1)
	points(res[site== "CON"], tdr[site== "CON"], pch=16, col= 2)
	points(res[site== "COL"], tdr[site== "COL"], pch=16, col= 3)
	points(res[site== "CRI"], tdr[site== "CRI"], pch=16, col= 4)
	points(res[site== "LEC"], tdr[site== "LEC"], pch=16, col= 5)
	points(res[site== "MOL"], tdr[site== "MOL"], pch=16, col= 6)
	points(res[site== "VRO"], tdr[site== "VRO"], pch=16, col= 7)
	points(res[site== "PRE"], tdr[site== "PRE"], pch=16, col= 8)
detach()
search()

#inseriamo la legenda
	legend("topright", inset= c(0.05, 0.05), legend= levels(medie.areali$site),
          col=c(1:8), pch=c(16,16,16,16,16,16,16,16), btyn)

################################################################################

################
##### CORRELAZIONE ####
###############
# occhio alla significatività: in alcuni casi, nonostante gli alti valori di r, non si ottiene
 #la significatività con p<0.05. Tuttavia, posso pensare  che con un numero maggiore dipunti
 #la cosa si risolverebbe
 
 cor.test(~ tdr + res, data= medie.areali)
cor.test(~ tdr + res, data= medie.areali, method="s")

 cor.test(~ tdr + res, data= medie.areali, subset= (date=="jan"))
	cor.test(~ tdr + res, data= medie.areali, subset= (date=="jan"), method="s")

  cor.test(~ tdr + res, data= medie.areali, subset= (date=="mar"))
	cor.test(~ tdr + res, data= medie.areali, subset= (date=="mar"), method="s")

	cor.test(~ tdr + res, data= medie.areali, subset= (date=="jun"))
	cor.test(~ tdr + res, data= medie.areali, subset= (date=="jun"), method="s")

	cor.test(~ tdr + res, data= medie.areali, subset= (date=="oct"))
	cor.test(~ tdr + res, data= medie.areali, subset= (date=="oct"), method="s")

 
# può essere interessante vedere cosa accade con una trasformazione di variabili
cor.test(~ tdr + log10(res), data= medie.areali)
cor.test(~ tdr + log10(res), data= medie.areali, method="s")
	
 cor.test(~ tdr + log10(res), data= medie.areali, subset= (date=="jan"))
  cor.test(~ tdr + log10(res), data= medie.areali, subset= (date=="jan"), method="s")
	
 cor.test(~ tdr + log10(res), data= medie.areali, subset= (date=="mar"))
cor.test(~ tdr + log10(res), data= medie.areali, subset= (date=="mar"), method="s")

 cor.test(~ tdr + log10(res), data= medie.areali, subset= (date=="jun"))
cor.test(~ tdr + log10(res), data= medie.areali, subset= (date=="jun"), method="s")

 cor.test(~ tdr + log10(res), data= medie.areali, subset= (date=="oct"))
 cor.test(~ tdr + log10(res), data= medie.areali, subset= (date=="oct"), method="s")

################
#### CORRELAZIONE: come funziona ####
###############
## ATTENZIONE: le sd_res e sd_tdr del file medie.areali vanno divise per
## il numero di dati utilizzati per calcolare la media


 cor(medie.areali$tdr,medie.areali$res)
#calcolo basato sulla formula teorica
 attach(medie.areali)
   (codevianza<- sum((tdr - mean(tdr)) * (res - mean(res))))                
   (devianza_tdr<- sum((tdr - mean(tdr))^2))
   (devianza_res<- sum((res - mean(res))^2))
   (codevianza /sqrt(devianza_tdr * devianza_res))              #correlazione

#correlazione: formula empirica 
 (codevianza_emp <- (sum(tdr*res) - sum(tdr)*sum(res))*28)
 #? equivalente a questa
 #sum(tdr*res)- sum(tdr)*sum(res)/28)
 (devianza_emp_tdr<- sum(tdr^2)-sum(tdr)^2/28)
 (devianza_emp_res<- sum(res^2)-sum(res)^2/28)
 codevianza_emp/sqrt(devianza_emp_tdr*devianza_emp_res)
detach()

#####################
#### correlazione pesata ############
#####################

#prima calcolo la media pesata di ciascuna variabile
attach(medie.areali)
 wmean_tdr<- weighted.mean(tdr, 1/es_tdr)
 wmean_res<- weighted.mean(res, 1/es_res)

 (wcodevianza<- sum((tdr-wmean_tdr)*(res-wmean_res)))
 (wdevianza_tdr<- sum((tdr - wmean_tdr)^2))
 (wdevianza_res<- sum((res - wmean_res)^2))
  wcodevianza/ sqrt(wdevianza_tdr*wdevianza_res)
detach()

 sum(jan$tdr)/8                                                               #media
 sum((jan$tdr)*(1/jan$sd_tdr))/sum(1/jan$sd_tdr)                          #media pesata sulla dev.std del tdr
#ora che abbiamo capito, possiamo utilizzare la funzione

weighted.mean(jan.m$tdr, 1/jan.m$sd_tdr)
weighted.mean(mar.m$tdr, 1/mar.m$sd_tdr)
weighted.mean(jun.m$tdr, 1/jun.m$sd_tdr)
weighted.mean(oct.m$tdr, 1/oct.m$sd_tdr)
weighted.mean(jan.m$res, 1/jan.m$sd_res)
weighted.mean(mar.m$res, 1/mar.m$sd_res)
weighted.mean(jun.m$res, 1/jun.m$sd_res)
weighted.mean(oct.m$res, 1/oct.m$sd_res)

#devo ancora capire bene: cisono varie possibilit?
  cov.wt()
require(boot)
  corr(jan.m[,c("res","tdr")], 1/jan.m$sd_res)
  corr(mar.m[,3:4], 1/mar.m$sd_res)
  corr(jun.m[,3:4], 1/jun.m$sd_res)
  corr(oct.m[,3:4], 1/oct.m$sd_res)                                             #package boot


#per vedere che succede sui logaritmi devo estrarre le matrici
pippo <- function(x) {
 a <- x[,3:4]
 a$lres <- log10(a[,2])
 a$sd_lres <- sd(a$lres)
 w <- 1/a$sd_lres
 corr(a[,c(1,3)],w)
 }

pippo(jan.m)
pippo(mar.m)
pippo(jun.m)
pippo(oct.m)
pippo(medie.areali)
#questa funzione usa solo un peso per entrambe le variabili... 
#per verificare digita 
corr                                                                            #senza parentesi

##################################
##### REGRESSIONE SULLE MEDIE AREALI #####
##################################

#cominciamo con la lineare
	lin.jan.mean <- lm(tdr ~ res, data= jan.m)
	linear.mar.mean <- lm(tdr ~ res, data= mar.m)
	linear.jun.mean <- lm(tdr ~ res, data= june.m)
	linear.oct.mean <- lm(tdr ~ res, data= oct.m)

#passiamo alla power...
  pmjan.mean <- nls(tdr ~ a*res^b, data= jan.m, start= list(a= 1, b= 0.1))        #pm=power model
  pmmar.mean <- nls(tdr ~ a*res^b, data= mar.m, start= list(a= 1, b= 0.1))
  pmjun.mean <- nls(tdr ~ a*res^b, data= jun.m, start= list(a= 1, b= 0.1))
  pmoct.mean <- nls(tdr ~ a*res^b, data= oct.m, start= list(a= 1, b= 0.1))

#infine per confrontarli  utile fare  cosi:
summary(lin.jan.mean);
summary(pmjan.mean)
summary(linear.mar.mean);
summary(pmmar.mean)
summary(linear.jun.mean);
summary(pmjun.mean)
summary(linear.oct.mean);
summary(pmoct.mean)

#calcoliamo il coeff.di detrminazione R^2 passando per i residui
#costruisco una funzione da apllicare a tutti i data set
R2 <- function (model,dataset) {
  RSS.p <- sum(residuals(model)^2)                                              #Residuals sum of squares: la p finale sta per power
  TSS   <- sum((dataset[, "tdr"] - mean(dataset[, "tdr"]))^2)                   #Total Sum of squares
  R2 <- 1 - RSS.p/TSS
  print(paste("RSS =", RSS.p))
  print(paste("TSS= ", TSS))
  print(paste("R2= ", R2))
  }                                           
R2(pmjan.mean, jan.m)                                                           #esempio di uso

confint(power.jan.mean)
confint(power.mar.mean)
confint(power.jun.mean)
confint(power.oct.mean)

#per calcolare le deviazioni std            
	lapply(january.mean, sd)
	lapply(march.mean, sd)
	lapply(june.mean, sd, na.rm= T)
	lapply(october.mean, sd, na.rm= T)

 
###############################################################################
###############################################################################
#### Esecizio di stile #####
# ATTENZIONE: quella che segue ? una procedure in vari passaggi per ottenere
# dei data frame contenti medie areali di res e tdr per ciascun sito.
#  un esercizio (di stile) che facevo agli inizi, quando cominciavo ad usare R

# voglio creare un data frame di struttura analoga a "dati.tutti" contenente le medie areali

 names(dati.tutti)
# se mi servono le medie TDR per Sito...
	tdr.meanSite <- tapply(dati.tutti$tdr, dati.tutti$site, mean)
 	tdr.meanData <- tapply(dati.tutti$tdr, dati.tutti$date, mean)# ...e per Data

#faccio lo stesso per le resistivit?
 res.meanSite <- tapply(dati.tutti$res, dati.tutti$site, mean)
 res.meanData <- tapply(dati.tutti$res, dati.tutti$date, mean) #la funzione tapply restituisce i risultati in un array

#in realt? mi serve di pi? creare una tabella a doppia entrata (siti e date) e avere le medie sito per sito in ogni data
 	medie.tdr <- tapply(dati.tutti$tdr, list(dati.tutti$site, dati.tutti$date), mean)
    medie.tdr
	medie.res <- tapply(dati.tutti$res, list(dati.tutti$site, dati.tutti$date), mean)
   medie.res  #restituisce array(matrice), per verificare
	class(medie.tdr)

#comincia la creazione vera e propria del nuovo data.frame:
	pippo <- expand.grid(medie.tdr) #expand.grid trasforma l'array in forma di data.frame
	pippa <- expand.grid(medie.res)
	medie.areali <- cbind(pippo, pippa) #uniamoli in un unico data.frame

#creo un vettore con i nomi dei siti da inserire nel nuovo data.frame
	siti <- c("CBE", "COL", "COLN", "CRI", "LEC", "MOL", "PRE", "VRO")

#l'aggiungo al data.frame
	medie.areali$site <- siti

#lo trasformo in fattore
	medie.areali$site <- factor(medie_areali$site)

#creo anche un vettore con i nomi dei mesi da inserire nel nuovo data.frame
	date <- c(rep("Jan",8), rep("Feb",8), rep("Mar",8), rep("Jun",8), rep("Oct",8))
# l'aggiungo
	medie.areali$date <- date
#lo fattorizzo
	medie.areali$date <- factor(medie_areali$date, levels= c("Jan", "Feb", "Mar", "Jun", "Oct"))

#infine impostiamo nomi delle variabili
	names(medie.areali) <- c("tdr", "res", "site", "date")
#controllo il nuovo data.frame
	str(medie.areali)

#ripuliamo un p? il workspace
	rm(list= c("pippa", "pippo", "siti", "date", "medie.tdr", "medie.res"))
	ls()

#esploriamo il data.frame
	plot(tdr ~ res, data= medie.areali)
	cor.test(~ tdr + res, data= medie.areali)
	cor.test(~ tdr + res, data= medie.areali, method="s")
	lm(tdr ~ res, data= medie.areali)
	summary(lm(tdr ~ res, data= medie.areali))
	nls(tdr ~ a*res^b, data=medie.tutte)
	summary(nls(tdr ~ a*res^b, data=medie.tutte))
################################################################################

#se voglio trasferire le tabelle su un file tipo csv puo essere utile forzare
#gli array a dataframe e poi esportarli:
 tdr.meanSite <- as.data.frame(tdr.meanSite); tdr.meanSite
 tdr.meanData <- as.data.frame(tdr.meanData); tdr.meanData
 res.meanSite <- as.data.frame(res.meanSite); res.meanSite
 res.meanData <- as.data.frame(res.meanData); res.meanData

#anche se funziona meglio write.csv2
#  write.table(tdr.meanSite, filemedie2.csv)
#  write.table(tdr.meanData, filemedie2.csvappendT)
#  write.table(res.meanSite, filemedie2.csvappendT)
#  write.table(res.meanData, filemedie2.csvappendT)
################################################################################

# ora che ho creato un data.frame contenente tutte le medie estraggo i data.frame per ciascun mese di campionamento...
# cosi come ho gi fatto per i dati puntuali (ho volutamente trascurato di creare un dataset con i dati di febbraio)

	january.mean <- medie.areali[medie.areali$date== "jan",]; dim(january.mean)
	march.mean   <- medie.areali[medie.areali$date== "mar",]; dim(march.mean)
	june.mean    <- medie.areali[medie.areali$date== "jun",]; dim(june.mean)
	#july.mean   <- medie.areali[medie.areali$date== "jul",]; dim(july.mean)
	october.mean <- medie.areali[medie.areali$date== "oct",]; dim(october.mean)

# per visualizzarli
 par( mfrow= c(2,2))
 plot(tdr ~ res, data= january.mean, main= "January", 
      xlim= c(0,165), ylim= c(15,55), 
      xlab= "resistivity (Ohm*m)", ylab= "tdr(%H20 cm3/cm3)")
 plot(tdr ~ res, data= march.mean,  main= "March", xlim= c(0,165), ylim= c(15,55),
     xlab= "resistivity (Ohm*m)", ylab= "tdr(%H20 cm3/cm3)")
 plot(tdr ~ res, data= june.mean,  main= "June", xlim= c(0,165), ylim= c(15,55),
       xlab= "resistivity (Ohm*m)", ylab= "tdr(%H20 cm3/cm3)")
 plot(tdr ~ res, data= october.mean, main= "October", 
      xlim= c(0,165), ylim= c(15,55), 
      xlab= "resistivity (Ohm*m)",ylab= "tdr(%H20 cm3/cm3)")

#? molto pi elegante ottenere un data set nel modo seguente: sto costruendo
#un data frame che escluda i valori di Febbraio e Luglio
	 medie.noFebLug <- medie.tutte[medie.tutte$date != "Feb",]

#oppure uso il pacchetto lattice
	xyplot(tdr~res|date, data=medie.noFebLug, xlab= " ?  Ohm*m", ylab= "?v  %cm3/cm3", col="blue")
    