######################################################################
## Date of creation: #18/11/2010
## Author: Giuseppe Calamita
## Carica dati che sono stati salvati con lo script "operazioni_preliminari"
## 
## Contains: Analisi correlazione ER-SM
## PEr Data: Lineare-NonLineare
## Per Sito: Lineare
## Applicazioni di test-diagnostici lmtest packg
## 
## EDU: 
## update()::stats, lmtest pckg, durbin-watson test breush-pagna test,
## nls(); calcolo di R2 e limiti di conf. per parametri non-lineari
## calcolo della sensibilità del metodo: rapporto tra varianza 
#####################################################################
  load("dati.RData")

################################################################
###########   ANALISI PER DATA   ###############################
################################################################

################################
## CORRELATION TESTS PER DATA	###	
################################

#li eseguo tutti e 3 perch? non siamo sicuri della bi-normalit? della distribuzione
#GENNAIO	
	cor.test(~ jan$tdr + jan$res)
	cor.test(~ jan$tdr + jan$res, method="s", exact=F)
	#cor.test(~ jan$tdr + jan$res, method="k", exact=F)

#provo a rimuovere COL che ha un alta dev.std nei valori di res  
  cor.test(~ tdr + res, data= jan, subset= site!="COL")
  cor.test(~ tdr + res, data= jan, subset= site!="COL", method="s")

#MARZO
	cor.test(~ mar$tdr + mar$res)
	cor.test(~ mar$tdr + mar$res, method="s", exact=F)
	#cor.test(~ mar$tdr + mar$res, method="k", exact=F)

#GIUGNO
	cor.test(~ jun$tdr + jun$res)
	cor.test(~ jun$tdr + jun$res, method="s", exact=F)
	#cor.test(~ jun$tdr + jun$res, method="k", exact=F)

#OTTOBRE
	cor.test(~ oct$tdr + oct$res)
	cor.test(~ oct$tdr + oct$res, method="s", exact=F)
	#cor.test(~ oct$tdr + oct$res, method="k", exact=F)


#confrontiamo con una trasf0rmazione di "res"
 cor.test(~ jan$tdr + log10(jan$res), method="s", exact=F)
 cor.test(~ mar$tdr + log10(mar$res), method="s", exact=F)
 cor.test(~ jun$tdr + log10(jun$res), method="s", exact=F)
 cor.test(~ oct$tdr + log10(oct$res), method="s", exact=F)

# SCATTER PLOT CON COEFFICIENTE DI CORRELAZIONE LINEARE
#mi servono dei grafici simili a quelli di sopra leggemete diversi: elimino le linee,
# aggiungo una griglia e il valore della correlazione
par(mfrow=c(2,2))
  assex <- c(0,300)
  assey <- c(10,65)

plot(jan$tdr ~ jan$res, pch= 20, cex=1.4, xlim= assex, ylim=assey, col= "aquamarine3",
      xlab= "resistivity [Ohm m]", ylab= "soil moisture [%vol/vol]", main="January")
  grid()
  text(200,60, paste("cor:", round(cor(jan$tdr, jan$res),2)), cex= 1.7)

plot(mar$tdr ~ mar$res, pch= 20, cex=1.4, xlim= assex, ylim=assey, col= "aquamarine3",  
      xlab= "resistivity [Ohm m]", ylab= "soil moisture [%vol/vol]", main="March")
  grid()
  text(200, 60, paste("cor:", round(cor(mar$tdr, mar$res), 2)), cex= 1.7) 
  
plot(jun$tdr ~ jun$res, pch= 20, cex=1.4,  xlim= assex, ylim=assey, col= "aquamarine3",
      xlab= "resistivity [Ohm m]", ylab= "soil moisture [%vol/vol]", main="June")
  grid()
  text(200, 60, paste("cor:", round(cor(jun$res, jun$tdr), 2)), cex=1.7)
  
plot(oct$tdr ~ oct$res, pch= 20, cex=1.4,  xlim= assex, ylim=assey, col= "aquamarine3",
      xlab= "resistivity [Ohm m]", ylab= "soil moisture [%vol/vol]", main="October")
  grid()
  text(200, 60, paste("cor:", round(cor(oct$res, oct$tdr), 2)), cex= 1.7) 
savePlot("scatter_cor", type="bmp")

##############################################################################

#faccio una cosa simile ma con i ranghi
par(mfrow=c(2,2))

  plot(rank(jan$tdr) ~ rank(jan$res), pch= 20, cex=1.4, col= "blue3",
      xlab= "resistivity [Ohm m]", ylab= "soil moisture [%vol/vol]", main="January")
  grid()
  text(80,110, paste("cor:", round(cor(jan$tdr, jan$res, method="spearman"),2)), cex= 1.7)

 plot(rank(mar$tdr) ~ rank(mar$res), pch= 20, cex=1.4, col= "blue3",  
      xlab= "resistivity [Ohm m]", ylab= "soil moisture [%vol/vol]", main="March")
  grid()
  text(60, 110, paste("cor:", round(cor(mar$tdr, mar$res, method="spearman"), 2)), cex= 1.7) 
  
plot(rank(jun$tdr) ~ rank(jun$res), pch= 20, cex=1.4, col= "blue3",
      xlab= "resistivity [Ohm m]", ylab= "soil moisture [%vol/vol]", main="June")
  grid()
  text(60, 100, paste("cor:", round(cor(jun$res, jun$tdr, method="s"), 2)), cex=1.7)
  
  plot(rank(oct$tdr) ~ rank(oct$res), pch= 20, cex=1.4, col= "blue3",
      xlab= "resistivity [Ohm m]", ylab= "soil moisture [%vol/vol]", main="October")
  grid()
  text(60, 70, paste("cor:", round(cor(oct$res, oct$tdr, method= "sp"), 2)), cex= 1.7) 
savePlot("scatter_rankcor", type="bmp")

##############################################################################
     ############## 	LINEAR LEAST SQUARE REGRESSION	   ############
##############################################################################

#January
	lin.jan <- lm(tdr ~ res, data= dati.tutti, subset= date=="jan")
	summary(lin.jan)
	par(mfrow=c(2,3))
  plot(lin.jan, which=1:6)

#March
 # lin.mar <- lm(tdr ~ res, data = mar)
	lin.mar <- update(lin.jan, subset= date=="mar")                               #? un modo molto pi? elegante
	summary(lin.mar)
  par(mfrow=c(2,3))
  plot(lin.mar, which=1:6)

#June
	#lin.jun <- lm(tdr ~ res, data = jun)
	lin.jun <- update(lin.jan, subset= date=="jun")                               #? un modo molto pi? elegante
  summary(lin.jun)
  par(mfrow=c(2,3))
  plot(lin.jun, which=1:6)

#October
  #lin.oct <- lm(tdr ~ res, data = oct)
	lin.oct <- update(lin.jan, subset= date=="oct")                               #? un modo molto pi? elegante
	summary(lin.oct)
  par(mfrow=c(2,3))
  plot(lin.oct, which=1:6)


############################ 
##TEST di SPECIFICAZIONE 
############################ 
  require(lmtest)                                                                 #carico il pacchetto per la diagnostica
  par(mfrow = c(2,2))                                                             #impostiamo il dispositivo di visualizzazione (cio? "lo schermo!!!)
  
 #gennaio
  plot(lin.jan)                                                                   #per la linearit? guardo il primo grafico residui vs fitted
  bptest(lin.jan)                                                                  #test di BreuschPAgan: H0: residui omoschedastici
 #metodo grafico per l'omoschedasticit?:visualizzo graficamente l'andamento dei rediui vs la variabile predetta
  plot(abs(lin.jan$residuals) ~ lin.jan$fitted.values)	
  dwtest(lin.jan) #test di DurbinWatson: H0:autocorrelzaz.=0
 #metodo grafico per l'autocorrelazione
  n <- length(lin.jan$residuals)
  plot(lin.jan$residuals[-n],lin.jan$residuals[-1])
  
#...cosi per le altre date...
 #marzo
 par(mfrow = c(2,2))
	plot(lin.mar) #per la linearit? guardo il primo grafico residui vs fitted
	bptest(lin.mar)
 #metodo grafico per l'omoschedasticit?
	plot(abs(lin.mar$residuals) ~ lin.mar$fitted.values)
	dwtest(lin.mar) #test di DurbinWatson: H0:autocorrelzaz.=0
#metodo grafico per l'autocorrelazione
      n <- length(lin.mar$residuals)
	plot(lin.mar$residuals[-n],lin.mar$residuals[-1])

#giugno
	plot(lin.jun) #per la linearit? guardo il primo grafico residui vs fitted
	bptest(lin.jun)
#metodo grafico per l'omoschedasticit?
	plot(abs(lin.jun$residuals) ~ lin.jun$fitted.values)
	dwtest(lin.jun) #test di DurbinWatson: H0:autocorrelzaz.=0
#metodo grafico per l'autocorrelazione
      n <- length(lin.jun$residuals)
	plot(lin.jun$residuals[-n], lin.jun$residuals[-1])

#ottobre
	plot(lin.oct) #per la linearit? guardo il primo grafico residui vs fitted
	bptest(lin.oct)
#metodo grafico per l'omoschedasticit?
	plot(abs(lin.oct$residuals) ~ lin.oct$fitted.values)
	dwtest(lin.oct) #test di DurbinWatson: H0:autocorrelzaz.=0
#metodo grafico per l'autocorrelazione
      n <- length(lin.oct$residuals)
	plot(lin.oct$residuals[-n], lin.oct$residuals[-1])
################################################################################

##per ora mi interessa pi? che altro osservare come si distribuiscono i residui 
##rispetto ai valori previsti, quindi ? sufficiente usare:

	 plot(lin.jan) 
	 plot(lin.mar) 
	 plot(lin.jun) 
	 plot(lin.oct) #osservo il primo grafico in alto a destra

## voglio fare un'unica immagine in cui mostro i grafici dei "residui vs fitted"
## per tutti i mesi
	par(mfrow = c(2,2))
	plot(lin.jan, which= c(1), main= "January", cex.caption=0.8)
	plot(lin.mar, which= c(1), main= "March", cex.caption=0.8)
	plot(lin.jun, which= c(1), main= "June", cex.caption=0.8)
	plot(lin.oct, which= c(1), main= "October", cex.caption=0.8)

###############################################################################
        ##################   ANOVA   ####################
################################################################################

#si ? capito che il modello lineare non va proprio bene
#prima di passare alla regressione non lineare , seguimao ancora il Prof. Rossiter 
#nell'ANOVA, proviamo a vedere se introducendo un altra variabile le cose cambiano

  sort(jan$tdr) #mi da i valori ordinati della variabile
  order(jan$tdr) #mi da gli indici dei valori ordinati che posso usare dopo
  jan$site[order(jan$tdr)] # ad esempio qui
  by(jan$tdr, jan$site, range)
  by(jan$tdr, jan$site, function(x) max(x)-min(x)) #FUNZIONE ANONIMA
  by(jan$res, jan$site, function(x) max(x)-min(x))

#si vede meglio graficamente con i boxplot
#january
  boxplot(tdr~site, data=jan, ylab="soil moisture (%)", notch=T, 
    scales= list(cex=1.2))
  boxplot(res~site, data=dati.tutti, ylab="apparent resistivity (Ohm m)", notch=T,
    scales= list(cex=1.2))

#POTREI RIFARLO PER TUTTI LE DATE
#una visone numerica dei boxplot
by(jan$tdr, jan$site, summary)

###ANOVA
#per capire quanto la variabile soil moisture ? spiegata dai vari siti (nelle diverse date)
#si usa l'ANOVA attraverso la funzione lm

(janova <- summary(lm(jan$tdr ~ jan$site)))
(marchova <- summary(lm(mar$tdr ~ mar$site)))
(junova <- summary(lm(jun$tdr ~ jun$site)))
(octova <- summary(lm(oct$tdr ~ oct$site)))
#oppure
summary(aov(janova))

################################################################################
##################   NON LINEAR LEAST SQUARE REGRESSION   ######################
################################################################################
# nlstools ? un pacchetto per regressioni non lineari...intanto lo faccio nel modo classico

# PER DATA....
	pm.jan <- nls(tdr ~ a*res^b, data = jan, start = list(a= 63, b= 0.16))
  pm.jan2 <- nls(tdr ~ a*res^b, data = jan, subset= site!="COL", start = list(a= 63, b= 0.16))
  
  pm.mar <- nls(tdr ~ a*res^b, data = mar, start = list(a= 65, b= 0.19))
	pm.jun <- nls(tdr ~ a*res^b, data = jun, start = list(a= 72, b= 0.3))
	#pm.jul <- nls(tdr ~ a*res^b, data = jul, start = list(a= 31, b= 0.06))
	pm.oct <- nls(tdr ~ a*res^b, data = oct, start = list(a= 54, b= 0.18))

# poich? mi interessa confrontare il RMSE dei due modelli di regressione, quello lineare e quello potenza,
  # uso la funzione summary prima per l'uno e poi per l'altro

	summary(lin.jan); summary(pm.jan)
	summary(lin.mar); summary(pm.mar)
	summary(lin.jun); summary(pm.jun)
	summary(lin.oct); summary(pm.oct)

#per stimare gli intervalli di confidenza dei parametri del modello
  #calcoliamo il coeff.di detrminazione R^2 passando per i residui
  #costruisco una funzione da apllicare a tutti i data set
R2 <- function (model,dataset) {
  RSS.p <- sum(residuals(model)^2)                                              #Residuals sum of squares: la p finale sta per power
  TSS   <- sum((dataset[, "tdr"] - mean(dataset[, "tdr"]))^2)                   #Total Sum of squares
  R2 <- 1 - RSS.p/TSS
  print(paste("RSS =", RSS.p))
  print(paste("TSS =", TSS))
  print(paste("R2 =", R2))
  }                                           
R2(pm.jan, jan)                                                              #esempio di uso
R2(pm.jan, subset(jan, subset= site!="COL"))

  confint(power.jan)
	confint(power.mar)
	confint(power.jun)
	confint(power.oct)


################
# sensibilita metodo  
################
# confrontiamo la varianza della sm-TDR e sm-RES (sm calcolata con la funz. di regressione)
sens <- var(predict(pm.jan))/var(jan$tdr)
sens[2] <- var(predict(pm.mar))/var(mar$tdr)
sens[3] <- var(predict(pm.jun))/var(jun$tdr)
sens[4] <- var(predict(pm.oct))/var(oct$tdr)

median(sens)

###############################################################
###########   ANALISI PER SITO   ###############################
################################################################

################################
# CORRELATION  TESTS PER SITO
################################

  cor.test(~ res + tdr, data=CBE) 
   cor.test(~ res + tdr, data=CBE, method= "s", exact= F) 
  cor.test(~ res + tdr, data=COL) 
   cor.test(~ res + tdr, data=COL, method= "s", exact= F) 
  cor.test(~ res + tdr, data=CON) 
   cor.test(~ res + tdr, data=CON) 
  cor.test(~ res + tdr, data=CRI, method= "s", exact= F) 
   cor.test(~ res + tdr, data=CRI) 
  cor.test(~ res + tdr, data=LEC, method= "s", exact= F) 
   cor.test(~ res + tdr, data=LEC) 
  cor.test(~ res + tdr, data=MOL, method= "s", exact= F) 
   cor.test(~ res + tdr, data=MOL) 
  cor.test(~ res + tdr, data=PRE) 
   cor.test(~ res + tdr, data=PRE, method= "s", exact= F) 
  cor.test(~ res + tdr, data=VRO) 
   cor.test(~ res + tdr, data=VRO, method= "s", exact= F) 
 
#resistività corrette per la Temp dell'aria
 cor.test(~ res_T + tdr, data=CBE) 
   cor.test(~ res_T + tdr, data=CBE, method= "s", exact= F) 
  cor.test(~ res_T + tdr, data=COL) 
   cor.test(~ res_T + tdr, data=COL, method= "s", exact= F) 
  cor.test(~ res_T + tdr, data=CON) 
   cor.test(~ res_T + tdr, data=CON, method= "s", exact= F) 
  cor.test(~ res_T + tdr, data=CRI) 
   cor.test(~ res_T + tdr, data=CRI, method= "s", exact= F) 
   cor.test(~ res_T + tdr, data=LEC) 
cor.test(~ res_T + tdr, data=LEC, method= "s", exact= F) 
   cor.test(~ res_T + tdr, data=MOL) 
  cor.test(~ res_T + tdr, data=MOL, method= "s", exact= F) 
  cor.test(~ res_T + tdr, data=PRE) 
   cor.test(~ res_T + tdr, data=PRE, method= "s", exact= F) 
  cor.test(~ res_T + tdr, data=VRO) 
   cor.test(~ res_T + tdr, data=VRO, method= "s", exact= F) 

#resistività corrette per la Temp del suolo (stimata)
cor.test(~ res_Ts + tdr, data=CBE) 
   cor.test(~ res_Ts + tdr, data=CBE, method= "s", exact= F) 
  cor.test(~ res_Ts + tdr, data=COL) 
   cor.test(~ res_Ts + tdr, data=COL, method= "s", exact= F) 
  cor.test(~ res_Ts + tdr, data=CON) 
   cor.test(~ res_Ts + tdr, data=CON, method= "s", exact= F) 
  cor.test(~ res_Ts + tdr, data=CRI) 
   cor.test(~ res_Ts + tdr, data=CRI, method= "s", exact= F) 
   cor.test(~ res_Ts + tdr, data=LEC) 
cor.test(~ res_Ts + tdr, data=LEC, method= "s", exact= F) 
   cor.test(~ res_Ts + tdr, data=MOL) 
  cor.test(~ res_Ts + tdr, data=MOL, method= "s", exact= F) 
  cor.test(~ res_Ts + tdr, data=PRE) 
   cor.test(~ res_Ts + tdr, data=PRE, method= "s", exact= F) 
  cor.test(~ res_Ts + tdr, data=VRO) 
   cor.test(~ res_T + tdr, data=VRO, method= "s", exact= F) 


  cor.test(~ logres + tdr, data=CBE, method= "s", exact= F) 
  cor.test(~ logres + tdr, data=COL, method= "s", exact= F) 
  cor.test(~ logres + tdr, data=CON, method= "s", exact= F) 
  cor.test(~ logres + tdr, data=CRI, method= "s", exact= F) 
  cor.test(~ logres + tdr, data=LEC, method= "s", exact= F) 
  cor.test(~ logres + tdr, data=MOL, method= "s", exact= F) 
  cor.test(~ logres + tdr, data=PRE, method= "s", exact= F) 
  cor.test(~ logres + tdr, data=VRO, method= "s", exact= F) 

##########################################################################
  ##############
  # grafico mostra estensione dei CI95% dei coefficienti di correlazione
  # voglio mostrare che sono,per larga parte, sovrapposti quindi probabilmente rappresentano 
  # variazioni campionarie di uno stesso valore
  ##############

      plot(2,3, xlim=c(-1,0), ylim=c(1,6), type="n")
      segments(-0.63,1,-0.24,1, lwd=2, col="brown")
      segments(-0.82,2,-0.41,2, lwd=2, col="chocolate")
      segments(-0.75,3,-0.48,3, lwd=2, col=2)
      segments(-0.86,4,-0.66,4, lwd=2, col=3)
      segments(-0.78,5,-0.50,5, lwd=2, col=4)
      segments(-0.47,6,-0.01,6, lwd=2, col=5)	
----------------------------------------------------------------------------------
  
par(mfrow=c(2,4))
  assex <- c(0,300)
  assey <- c(10,65)

plot(CBE$tdr ~ CBE$res, pch= 20, cex=1.4, xlim= assex, ylim=assey, col= "aquamarine3",
      xlab= "resistivity [Ohm m]", ylab= "soil moisture [%vol/vol]", main="CBE")
  grid()
  text(200,55, paste("cor:", round(cor(CBE$tdr, CBE$res),2)), cex= 1.7)

plot(COL$tdr ~ COL$res, pch= 20, cex=1.4, xlim= assex, ylim=assey, col= "aquamarine3",  
      xlab= "resistivity [Ohm m]", ylab= "soil moisture [%vol/vol]", main="COL")
  grid()
  text(200, 55, paste("cor:", round(cor(COL$tdr, COL$res), 2)), cex= 1.7) 
  
plot(CON$tdr ~ CON$res, pch= 20, cex=1.4,  xlim= assex, ylim=assey, col= "aquamarine3",
      xlab= "resistivity [Ohm m]", ylab= "soil moisture [%vol/vol]", main="CON")
  grid()
  text(200, 60, paste("cor:", round(cor(CON$res, CON$tdr), 2)), cex=1.7)
  
plot(CRI$tdr ~ CRI$res, pch= 20, cex=1.4,  xlim= assex, ylim=assey, col= "aquamarine3",
      xlab= "resistivity [Ohm m]", ylab= "soil moisture [%vol/vol]", main="CRI")
  grid()
  text(200, 60, paste("cor:", round(cor(CRI$res, CRI$tdr), 2)), cex= 1.7) 

plot(LEC$tdr ~ LEC$res, pch= 20, cex=1.4, xlim= assex, ylim=assey, col= "aquamarine3",
      xlab= "resistivity [Ohm m]", ylab= "soil moisture [%vol/vol]", main="LEC")
  grid()
  text(200,55, paste("cor:", round(cor(LEC$tdr, LEC$res),2)), cex= 1.7)

plot(MOL$tdr ~ MOL$res, pch= 20, cex=1.4, xlim= assex, ylim=assey, col= "aquamarine3",  
      xlab= "resistivity [Ohm m]", ylab= "soil moisture [%vol/vol]", main="MOL")
  grid()
  text(200, 55, paste("cor:", round(cor(MOL$tdr, MOL$res), 2)), cex= 1.7) 
  
plot(PRE$tdr ~ PRE$res, pch= 20, cex=1.4,  xlim= assex, ylim=assey, col= "aquamarine3",
      xlab= "resistivity [Ohm m]", ylab= "soil moisture [%vol/vol]", main="PRE")
  grid()
  text(200, 60, paste("cor:", round(cor(PRE$res, PRE$tdr), 2)), cex=1.7)
  
plot(VRO$tdr ~ VRO$res, pch= 20, cex=1.4,  xlim= assex, ylim=assey, col= "aquamarine3",
      xlab= "resistivity [Ohm m]", ylab= "soil moisture [%vol/vol]", main="VRO")
  grid()
  text(200, 60, paste("cor:", round(cor(VRO$res, VRO$tdr), 2)), cex= 1.7) 

savePlot("scatter_siti_cor", type="bmp")

#       nb: per Colorno_new e Molino c'? uno script file a parte

################################################################################
#######################		REGRESSIONE LINEARE	##################################
################################################################################
linear_models <- vector("list") #alloco memoria per risultati

#calcolo linear model per ciascun sito
for(i in siti) 
    linear_models[[i]] <- lm(tdr ~ res, data= dati.tutti, subset= site== i)

	ls()

for(i in siti) print(summary(linear_models[[i]]))

for(i in siti) print(confint(linear_models[[i]]))
  
#il sito di CON e MOL sono entrambi non significativi
#per COLN si vede dallo scatter plot che le tre date prese singolarmente sono ben correlate, cosi per verificare faccio questa cosa
	lm(tdr ~ res, data= COLN, subset= date == "jan")
	lm(tdr ~ res, data= COLN, subset= date == "mar")
	lm(tdr ~ res, data= COLN, subset= date == "jun")
	summary(lm(tdr ~ res, data= COLN, subset= date == "jan"))
	summary(lm(tdr ~ res, data= COLN, subset= date == "mar"))
	summary(lm(tdr ~ res, data= COLN, subset= date == "jun"))
#..infatti, cosi sono tutte e 3 significative, significa che in quel sito le condizioni idrogeologiche possono essere cambiate molto.

#per verificare se il modello lineare va bene uso la funzione plot(modello)
	par(mfrow=c(2,2))
	plot(lin.CBE)
	plot(lin.COL)
	plot(lin.CON)
	plot(lin.CRI)
	plot(lin.LEC)
	plot(lin.MOL)
	plot(lin.PRE)
	plot(lin.VRO)

#vale lo stesso discorso fatto per l'analisi per DATA: per ora mi interessa solo
# analizzare i residuals_vs_fitted
## per tanto, voglio fare un'unica immagine in cui mostro i grafici 
## dei "residui vs fitted" per tutti i SITI
	par(mfrow = c(3,2))
	plot(lin.CBE, which= c(1), main= "CBE", cex.caption=0.8)
	plot(lin.COL, which= c(1), main= "COL", cex.caption=0.8)
	# plot(lin.CON, which= c(1), main= "CON", cex.caption=0.8)non lo plotto perch? il modello lineare non ? significativo
	plot(lin.CRI, which= c(1), main= "CRI", cex.caption=0.8)
	plot(lin.LEC, which= c(1), main= "LEC", cex.caption=0.8)
	# plot(lin.MOL, which= c(1), main= "MOL", cex.caption=0.8)non lo plotto perch? il modello lineare non ? significativo
	plot(lin.PRE, which= c(1), main= "PRE", cex.caption=0.8)
	plot(lin.VRO, which= c(1), main= "VRO", cex.caption=0.8)


##################  
#sensibilita
##################  
sens <- var(predict(lin.CBE))/var(CBE$tdr)
sens[2] <- var(predict(lin.COL))/var(COL$tdr)
sens[3] <- var(predict(lin.CRI))/var(CRI$tdr)
sens[4] <- var(predict(lin.LEC))/var(LEC$tdr)
sens[5] <- var(predict(lin.PRE))/var(PRE$tdr)
sens[6] <- var(predict(lin.VRO))/var(VRO$tdr)

##################################
# REGRESSIONE NON LINEARE PER SITO
##################################
  power_models <- vector("list")
  for(i in siti) 
    power_models[[i]] <- nls(tdr ~ a*res^b, data= dati.tutti, 
                             subset= site== i, start= list(a= 1, b= 0.1))
  
  #confrontiamo modello lineare e non lineare
  for(i in siti)
  {print(i)
   print(summary(power_models[[i]]))
   print(summary(linear_models[[i]]))
  }
  
# CBE leggermente meglio la lineare RMSE
# COL va meglio la power..leggeremente
# CON fa cagare in tutti i casi:pendenza positiva!!!!scomporre data per data
# CRI quasi uguali..meglio lin
# LEC vince il lineare
	# MOL nessuno dei 2 ? signif
	# PRE meglio power..leggermente
	# VRO meglio power..leggermente 

	
for(i in siti) {print(confint(power_models[[i]])
                        

                        # potrebbero servirmi i valori di dev std
	tapply(dati.tutti$tdr, dati.tutti$site, sd)
	tapply(dati.tutti$res, dati.tutti$site, sd)

#stavo tentando di rimuovere totalmente la parte di luglio..perch?
# pur non essendo nei dati quando gli chiedo la lunghezza di ciascun
#sito mi restituise anche i vaori di luglio....solo che cosi non funziona
#dati.nolugldate  factor(dati.nolugldate levelsc(January February March JuneOctober))
# bisogna usare il parametro "exclude"

##############################################################################
	#### TUTTI I DATI PUNTUALI ####
##############################################################################

###CORRELATION TESTS
	cor.test(~ dati.tutti$tdr + dati.tutti$res)
	cor.test(~ dati.tutti$tdr + dati.tutti$res, method="s")
	cor.test(~ dati.tutti$tdr + dati.tutti$res, method="s", exact=F)
	cor.test(~ dati.tutti$tdr + dati.tutti$res, method="k")
	cor.test(~ dati.tutti$tdr + dati.tutti$res, method="k", exact=F)

#proviamo una trasformazione
  cor.test(~logres+tdr, dati.tutti)
  cor.test(~logres+tdr, dati.tutti, method="s", exact=F)