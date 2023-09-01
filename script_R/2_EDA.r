######################################################################
## Date of creation: #18/11/2010
## Author: Giuseppe Calamita
## Carica i dati che sono stati salvati con lo script "operazioni_preliminari"
## Confrontare script e integrarlo con "grafica" che contiene
## metodi alternativi più eleganti e sofisticati
## 
## Contains: Exploratory Data Analysis
## uni-variate numerical summaries: normality-tests,
## uni-variate graphical summaries (qqplots, boxplots, histograms)
## bivariate graphical summaries (scatterplots)
##
## EDU: 
## tapply, steam-and-leaf plot, qqplot, qmath, 
## truehist()::MASS, grafici-dentro-grafici, savePlot()
##  istogrammi con funzione densità sovrapposta :: lattice 
## scatterplot di Rossiter: molto carini
#####################################################################

###############################################################################
#############     Exploratory Data Analysis   #################################
###############################################################################
  load("dati/dati_res_TDR.RData")


#####################
# Analisi UNIvariata 
#####################
#NB:!!!! se voglio salvare questi dati su un file da esportare 
  #posso usare la funzione sink("nomefile") con il parametro APPEND =TRUE e 
  #in seguito aprire il file con excel e modificarlo come meglio credo
# ?sink()
  
  summary(dati.tutti)   # numerical summaries, # altre funzioni su quickr -> google

# media e mediana...sono vicine?
# funzione by(): si applica a dataframe che vengono splittati per fattore

  by(dati.tutti, dati.tutti$date, summary)
  by(dati.tutti, dati.tutti$site, summary)
 
# se voglio vedere una variabile per volta
 attach(dati.tutti)
 by(res, date, summary)
 by(tdr, date, summary)
 by(res, site, summary)
 by(tdr, site, summary)

 tapply(tdr, date, mean) #by() e tapply() sono "parenti"
 tapply(res, date, mean)
 tapply(tdr, site, mean) #by() e tapply() sono "parenti"
 tapply(res, site, mean)

 tapply(tdr, date, sd)
 tapply(res, date, sd)
 tapply(tdr, site, sd)
 tapply(res, site, sd)

 by(tdr, date, mad)
 by(res, date, mad) #altro indicatore di variabilit? (pi? robusto) IQR o mad
 by(tdr, site, mad)
 by(res, site, mad)
 
detach()

####################
#STEM-AND-LEAF PLOT
####################

# sono simili a istogrammi ma permettono di vedere subito i numeri
  stem(dati.tutti$tdr)            #tutti i dati puntuali
  stem(dati.tutti$res)

  stem(jan$res)
  stem(jan$tdr)

# uso della funzione split()
 data.splitted <- split(dati.tutti, dati.tutti$date) #creo una lista di data.frame ciascuno contenente i dati suddivisi per data
 stem(data.splitted[[1]]$res)

####################
# QQPLOT & HISTOGRAMS
####################
# procedura presa e modificata da Guastaldi

# qui uso grafici del pacchetto graphs e lattice, conviene dare un occhiata a quelli di qq.plot o scatter.plot
# creo una funzione per plottare un qqnorm e un istogramma e poi la applico a chi voglio

 normalita <- function(x) {
 qqnorm(x, col="cyan2", main= "", xlab= list("Theoretical quantiles", cex= 1.5), ylab= list("Sample quantiles", cex=1.5))
 qqline(x, lwd=2)
 finestrina <- par(fig= c(.02, .5, .5, .98), new= TRUE)                         #imposta una nuova finestra
 hist(x, probability= T, col= "cyan2", xlab= "", ylab= "", main= "", axes= F)
 lines(density(x), col= "red", lwd= 2)
 box()
 par(finestrina)
 }

# la applico alla sm nei vari mesi
 normalita(jan$tdr)
 title("January - TDR")
 savePlot("normalita_jan_tdr", type="bmp")

 normalita(mar$tdr)
 title("March - TDR")
 savePlot("normalita_mar_tdr", type="bmp")

 normalita(jun$tdr)
 title("June - TDR")
 savePlot("normalita_jun_tdr", type="bmp")

 normalita(oct$tdr)
 title("October - TDR")
 savePlot("normalita_oct_tdr", type="bmp")

#e alla resistivit? nei vari mesi...magari cambio il colore nella funzione
 normalita(jan$res)
 title("January - Resistivity")
 savePlot("normalita_jan_res", type="bmp")

 normalita(mar$res)
 title("March - Resistivity")
 savePlot("normalita_mar_res", type="bmp")

 normalita(jun$res)
 title("June - Resistivity")
 savePlot("normalita_jun_res", type="bmp")

 normalita(oct$res)
 title("October - resistivity")
 savePlot("normalita_oct_res", type="bmp")

#e al logaritmo della resistivit?
 normalita(log(jan$res))
 normalita(log(mar$res))
 normalita(log(jun$res))
 normalita(log(oct$res))

#vediamo con LATTICE
  require(lattice)
#per la soil moisture
 qqmath(~ tdr | date, aspect = "xy", data = dati.tutti, subset=(date != "feb"),
            as.table= T, ylab= list("soil moisture [%vol/vol]",cex=1.7),
            prepanel = prepanel.qqmathline,
            panel = function(x, ...) {
               panel.qqmathline(x, ...)
               panel.qqmath(x, ...)
            })

#per la resistivit?
 qqmath(~ res | date, aspect = "fill", data = dati.tutti, subset=(date != "feb"),
            as.table= T, ylab= list("resistivity [Ohm m ]",cex=1.7),
            prepanel = prepanel.qqmathline,
            panel = function(x, ...) {
               panel.qqmathline(x, ...)
               panel.qqmath(x, ...)
            })

#per il log(resistivit?)
 qqmath(~ log(res) | date, aspect = "xy", data = dati.tutti, subset=(date != "feb"),
            as.table= T, ylab= list("resistivity [Ohm m ]",cex=1.7),
            prepanel = prepanel.qqmathline,
            panel = function(x, ...) {
               panel.qqmathline(x, ...)
               panel.qqmath(x, ...)
            })


# dopo l'analisi grafica, si pu? provare l'analisi numerica
# anche se quella grafica ? piu importante perch? permette di capire se
# il discostamento dalla normalit? ? severo e in quali zone avviene
 shapiro.test(jan$tdr)
 shapiro.test(mar$tdr)
 shapiro.test(jun$tdr)
 shapiro.test(oct$tdr)

 shapiro.test(log(jan$res))
 shapiro.test(log(mar$res))
 shapiro.test(log(jun$res))
 shapiro.test(log(oct$res))
# tutti  confermano la non normalit? dei data set
# ho provato anche i test del pacchetto nortest

 require(nortest)
 ad.test(log(jan$tdr))
 lillie.test(log(jan$tdr))
 pearson.test(log(jan$tdr), adjust=F)                                           #test non parametrico (credo)
 pearson.test(log(jan$tdr))                                                     #pi? cautelativa rispetto alla sottostante, usa n-3 g.d.f
#continuano a non risultare normali
#le uniche che sembrano avvicinarsi alla normalit? sono: jan$tdr e log(oct(res))


################################################################################
############	ancora UN P? DI ESPLORAZIONE grafica EDA
################################################################################

	boxplot(tdr ~ date, data= dati.tutti) #per farlo ancora pi figo guarda ?boxplot che  molto utile
	boxplot(tdr ~ date, data= dati.tutti, notch= T, col= "red3")
	title("Boxplot TDR per data")

#inserisco i valori di conducibilit nel data frame dati.tutti:
	dati.tutti$cond <- 1/dati.tutti$res

## inserisco altre due colonne con tdr e resistivit standardizzati nel data frame dati.tutti:
	dati.tutti$tdr.scale <- scale(dati.tutti$tdr)
	dati.tutti$res.scale <- scale(dati.tutti$res)
	dati.tutti$cond.scale <- scale(dati.tutti$cond)
# controllo il data frame
 	str(dati.tutti)

#vediamo come vengono
	par(mfrow=c(1,2))
	boxplot(tdr.scale ~ date, data= dati.tutti, notch= T, col= "lightblue")
	title("TDR")
	boxplot(res.scale ~ date, data= dati.tutti,notch= T, col= "goldenrod")
	title("Electrical Resisitivity")
#se voglio non visualizzare febbraio...
	par(mfrow=c(1,2))
	boxplot(tdr.scale ~ date, subset=(date!="feb"), data= dati.tutti, ylab="%H2O m3/m3", col= "lightblue")
	title("TDR", )
	boxplot(res.scale ~ date, subset=(date!="feb"), data= dati.tutti, ylab="resistivity Ohm*m", col= "goldenrod")
	title("Apparent Electrical Resisitivity")
#anche se febbraio non si vede pi?, l'etichetta resta sull'asse!

# facciamo i boxplot per sito
	boxplot(tdr ~ site, data= dati.tutti)

	par(mfrow=c(2,2))
	 boxplot(tdr ~ date, data= CBE)
	 title("BEL")
	 boxplot(tdr ~ date, data= COL)
 	 title("COL")
	 boxplot(tdr ~ date, data= COLN)
	 title("COLN")
	 boxplot(tdr ~ date, data= CRI)
	 title("CRI")

	boxplot(tdr ~ date, data= LEC)
	 title("LEC")
	 boxplot(tdr ~ date, data= MOL)
 	 title("MOL")
	 boxplot(tdr ~ date, data= PRE)
	 title("PRE")
	 boxplot(tdr ~ date, data= VRO)
	 title("VRO")

#confrontiamo i boxplot di TDR e resistivit?
	par(mfrow=c(1,2))
	boxplot(tdr ~ site, data= dati.tutti, col= "lightblue", ylab="%H2O m3/m3")
	title("TDR")
	boxplot(res ~ site, data= dati.tutti, col= "goldenrod", ylab="resistivity Ohm*m")
	title("Apparent Electrical Resistivity")

#con i dati normalizzati
	boxplot(tdr.scale ~ site, data= dati.tutti, col= "lightblue")
	title("TDR")
	boxplot(res.scale ~ site, data= dati.tutti, col= "goldenrod")
	title("Electrical Resisitivity")

######################
# tutti i dati puntuali
#####################

#istogrammi...andrebbero migliorati con TITOLO, ASSI etc..
require(MASS)
 par(mfrow=c(2,1))
 truehist(dati.tutti$tdr)#prova a giocare con le dimensioni delle barre col parametr h  e con i colori e
 truehist(dati.tutti$res) #sembra che TDR abbia una distribuzione normale
 truehist(dati.tutti$res, h= 10) #il valore centrale  molto spostato a sinistra coda a destra

 require(lattice)
 #i due script seguenti li ho gia copiati nello script di "grafica"
 xyplot(tdr ~ res| date, data= dati.tutti[dati.tutti$date!= "feb",], xlab= "resistivity Ohm*m", ylab= "%H2O m3/m3")
 xyplot(tdr ~ res| site, data= dati.tutti, xlab= "resistivity Ohm*m", ylab= "%H2O m3/m3", layout= c(4,2))

 histogram(~ tdr| date, data= dati.tutti[dati.tutti$date!= "feb",],
 xlab= "soil moisture %", as.table=T)
 histogram(~ res| date, data=  dati.tutti[dati.tutti$date!= "feb",],
 xlab= "apparent resistivity Ohm m", as.table=T, col=2)

 histogram(~ tdr|  site, data=  dati.tutti, xlab= "%H2O m3/m3", layout= c(4,2))
 histogram(~ res|  site, data=  dati.tutti, xlab= "resistivity Ohm*m", layout= c(4,2))


# funziona!! fa istogramma e  plot sovraimpresso
 histogram( ~ tdr | date, data = dati.tutti,
               xlab = "epression(theta)", type = "density",
               panel = function(x, ...) {
                   panel.histogram(x, ...)
                   panel.mathdensity(dmath = dnorm, col = "black",
                                     args = list(mean=mean(x),sd=sd(x)))
               } )

##############################################################################
########################      STATISTICA BIVARIATA     ######################
##############################################################################

#GUARDIAMO PRIMA GRAFICAMENTE I DATI usando il pacchetto graphs                 #vedi rossiter
#creiamo 4 grafici separatamente su un' unica immagine                          #data per data

par(mfrow=c(2,2))
  assex <- c(0,300)
  assey <- c(10,65)

plot(jan$tdr ~ jan$res, pch= 20, cex=1.4, xlim= assex, ylim=assey, col= "aquamarine3",
      xlab= "resistivity [Ohm m]", ylab= "soil moisture [%vol/vol]", main="January")
  abline(v = mean(jan$res), lty = 2, col = "blue")
  abline(h = mean(jan$tdr),  lty = 2, col = "blue")
  abline(v = median(jan$res), lty = 3, col = "cyan4")
  abline(h = median(jan$tdr),  lty = 3, col = "cyan4")
  abline(lm(jan$tdr ~ jan$res), lty=2)
  lines(lowess(jan$res, jan$tdr), lwd=2, col= "red")
  text(200, 45, pos=4, paste("Mean:", round(mean(jan$tdr), 1))) #basta cliccare sul grafico nella posizione in cui voglio mettere il label
  text(80, 60, pos=4, paste("Mean:", round(mean(jan$res), 1))) #basta cliccare sul grafico nella posizione in cui voglio mettere il label

 ## legend(150, 60, levels(jan$site), pch= 20, col = 1:nlevels(jan$site), bty = "n", ncol=2)

plot(mar$tdr ~ mar$res, pch= 20, cex=1.4, xlim= assex, ylim=assey, col= "aquamarine3",
      xlab= "resistivity [Ohm m]", ylab= "soil moisture [%vol/vol]", main="March")
  abline(v = mean(mar$res), lty = 2, col = "blue")
  abline(h = mean(mar$tdr),  lty = 2, col = "blue")
  abline(v = median(mar$res), lty = 3, col = "cyan4")
  abline(h = median(mar$tdr),  lty = 3, col = "cyan4")
  abline(lm(mar$tdr ~ mar$res), lty= 2)
  lines(lowess(mar$res, mar$tdr), lwd=2, col= "red")
  text(200, 45, pos=4, paste("Mean:", round(mean(mar$tdr), 1))) #basta cliccare sul grafico nella posizione in cui voglio mettere il label
  text(80, 60, pos=4, paste("Mean:", round(mean(mar$res), 1))) #basta cliccare sul grafico nella posizione in cui voglio mettere il label
  ## legend(150, 47, levels(mar$site), pch= 20, col = 1:nlevels(mar$site), bty = "n", ncol=2)

plot(jun$tdr ~ jun$res, pch= 20, cex=1.4,  xlim= assex, ylim=assey, col= "aquamarine3",
     xlab= "resistivity [Ohm m]", ylab= "soil moisture [%vol/vol]", main="June")
  abline(v = mean(jun$res), lty = 2, col = "blue")
  abline(h = mean(jun$tdr),  lty = 2, col = "blue")
  abline(v = median(jun$res), lty = 3, col = "cyan4")
  abline(h = median(jun$tdr),  lty = 3, col = "cyan4")
  abline(lm(jun$tdr ~ jun$res), lty= 2)
  lines(lowess(jun$res, jun$tdr), lwd=2, col= "red")
  text(200, 45, pos=4, paste("Mean:", round(mean(jun$tdr), 1))) #basta cliccare sul grafico nella posizione in cui voglio mettere il label
  text(80, 60, pos=4,, paste("Mean:", round(mean(jun$res), 1))) #basta cliccare sul grafico nella posizione in cui voglio mettere il label
  ###legend(150, 47, levels(jun$site), pch= 20, col = 1:nlevels(jun$site), bty = "n", ncol=2)

plot(oct$tdr ~ oct$res, pch= 20, cex=1.4,  xlim= assex, ylim=assey, col= "aquamarine3",
      xlab= "resistivity [Ohm m]", ylab= "soil moisture [%vol/vol]", main="October")
  abline(v = mean(oct$res), lty = 2, col = "blue")
  abline(h = mean(oct$tdr),  lty = 2, col = "blue")
  abline(v = median(oct$res), lty = 3, col = "cyan4")
  abline(h = median(oct$tdr),  lty = 3, col = "cyan4")
  abline(lm(oct$tdr ~ oct$res), lty= 2)
  lines(lowess(oct$res, oct$tdr), lwd=2, col= "red")
  text(200, 45, pos=4, paste("Mean:", round(mean(oct$tdr), 1))) #basta cliccare sul grafico nella posizione in cui voglio mettere il label
  text(80, 60, pos=4, paste("Mean:", round(mean(oct$res), 1))) #basta cliccare sul grafico nella posizione in cui voglio mettere il label
###  legend(150, 47, levels(oct$site), pch= 20, col = 1:nlevels(oct$site), bty = "n", ncol=2)


################################
# unico scatter plot dati tutti puntuali
# EVIDENZIANDO gruppi DATA o SITO
##################################
plot(tdr ~ res, dati.tutti, pch= 20, cex=1.4,
	col= "aquamarine3",
	xlab= "resistivity [Ohm m]", 
	ylab= "soil moisture [%vol/vol]",
	type="n")

#se volessi avere due scatter plot in una immagine modifico gli attributi grafici...
 #par(mfrow= c(1,2))
#...altrimenti passo direttamente sotto...

attach(dati.tutti)
#evidenzio le Date
 points(tdr[date== "jan"] ~ res[date== "jan"],  
 	pch=1, col= 2, cex=1.3) #"red"
 points(tdr[date== "feb"] ~ res[date== "feb"],
 	pch=2, col= 3, cex=1.3)
 points(tdr[date== "mar"] ~ res[date== "mar"],
 	pch=4, col= 4, cex=1.3)
 points(tdr[date== "jun"] ~ res[date== "jun"],
 	pch=5, col= 5, cex=1.3)
 points(tdr[date== "oct"] ~ res[date== "oct"],
 	pch=6, col= 7, cex=1.3)

#inseriamo la legenda
 legend("topright", legend= levels(dati.tutti$date), 
 	col=c(2,3,4,5,7), pch=c(1,2,4,5,6),
 	cex= 1.5)
 	
 text(100, 55, paste("r= ", round(cor(tdr,res),2)), cex=2)
	

 detach(dati.tutti)
 search()
 dev.off() #chiudo il dispositivo

################################################################################
#oppure evidenziando i SITI

attach(dati.tutti)
 	plot(res, tdr, type="n") #fammi lo scatter plot ...ma non plottare
 	points(res[site== "CBE"],  tdr[site== "CBE"],  pch=1, col=2)
 	points(res[site== "COLN"], tdr[site== "COLN"], pch=1, col=3)
 	points(res[site== "COL"],  tdr[site== "COL"],  pch=1, col=4)
 	points(res[site== "CRI"],  tdr[site== "CRI"],  pch=1, col=5)
 	points(res[site== "LEC"],  tdr[site== "LEC"],  pch=1, col=6)
 	points(res[site== "MOL"],  tdr[site== "MOL"],  pch=1, col=7)
 	points(res[site== "VRO"],  tdr[site== "VRO"],  pch=1, col=8)
 	points(res[site== "PRE"],  tdr[site== "PRE"],  pch=1, col=9)
detach()

# interpolazione
scatter.smooth(dati.tutti$res, dati.tutti$tdr)
