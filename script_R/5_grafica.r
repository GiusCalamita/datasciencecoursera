## EDU: 
## split.screen(); dev.print(resolution= 300); prepanel (lattice), curve()
## vari esempi con lattice (xyplot, hist, qqmath): manipolazioni legenda-simplekey
## pckg mvoutlier

#################################################
###############	  grafica   	#################
#################################################
# diverse procedure per produrre grafici elaborati tratti da diverse dispense

load(".RData")
names(dati.tutti)<- tolower(names(dati.tutti))
names(dati.tutti)[3]<- "res"
##############################################################################
#################         STATISTICA uniVARIATA       #######################
##############################################################################

split.screen(c(2,2))
attach(dati.tutti)

screen(1) ### TDR
hist(tdr, col="cyan", border= "red", main="", breaks=20,
    xlab= list("soil moisture (% vol/vol)", cex=1.7), 
	  ylab= list("Abs. freq.", cex=1.7))
	abline(v= mean(tdr), lty=2, lwd=4, col=2)
	abline(v= median(tdr), lty=2, lwd=4, col=3)
	abline(v= mean(tdr) + sd(tdr), lty=3, lwd= 3, col=4)
	abline(v= mean(tdr) - sd(tdr), lty=3, lwd= 3, col=4)
  rug(tdr)

screen(3)	# ER
hist(res, col="chocolate1", border= "chartreuse4", main="",
   xlab= list("apparent resistivity [Ohm m]", cex=1.7), breaks=20,
	 ylab= list("Absolute frequency", cex=1.7))
	abline(v= mean(res), lty=2, lwd=4, col=2)
	abline(v= median(res), lty=2, lwd=4, col=3)
	abline(v= mean(res) + sd(res), lty=3, lwd= 3, col=4)
	abline(v= mean(res) - sd(res), lty=3, lwd= 3, col=4)
  rug(res)

screen(4)	#log of ER
	hist(log(res), col="cadetblue", border= "chocolate1", main=" " ,
   xlab= list("log(apparent resistivity) [Ohm m]", cex=1.7), breaks=20,
	 ylab= list("Absolute frequency", cex=1.7), )
	abline(v= mean(log(res)), lty=2, lwd=4, col=2)
  abline(v= median(log(res)), lty=2, lwd=4, col=3)
	abline(v= mean(log(res)) + sd(log(res)), lty=3, lwd= 3, col=4)
	abline(v= mean(log(res)) - sd(log(res)), lty=3, lwd= 3, col=4)
  rug(log(res))

screen(2)
  legend(0.25,0.7, legend=c("media", "mediana", "dev.std"), 
         col= 2:4, lty= c(2,2,3), lwd= c(4,4,3))
  text(0.5,0.8, "Vallaccia catchment \n all data", cex= 1.7)	

detach()

### Più grafici in una finestra ( grafico dentro grafico) ####
# tratta dalle dispense di Guastaldi
# istogrammi e funzione di densità sovrapposti, mentre in basso disegna l'ecdf
# creiamo una funzione da applicare di volta in volta a tdr, res e log(res) 

	op <- par(mfcol=c(2,3), mar=c(3,2,2,4))
	do.it <- function (x) {
	hist(x, col="light blue", xlab="", ylab="", main="") #crea l'istogramma
	par(new=T)        # nuovi parametri per la stessa figura
	plot(density(x), type="l", col ="red", lwd=2, axes= F, main="", xlab="", ylab="") #disegna la curva di densit?
	axis(4)           #disegna asse secondario verticale
	x <- sort(x)
	q <- ppoints(length(x)) #genera la sequenza di prob. per la cumulativa
	plot(q ~ x, type="l", xlab="", ylab="", main="") #curva cumulativa
	abline(h=c(0.25, 0.5, 0.75), lty=3, lwd=3, col="blue") #linee orizzontali
					}
	
	do.it(dati.tutti$tdr); title("TDR")
	do.it(dati.tutti$res); title("App.res.")
	do.it(log(dati.tutti$res)); title("Log(res)")

#################################################################################
# tratta da Guastaldi
# questa volta facciamo un qqplot e un istogramma 
 
#TDR 
 qqnorm(dati.tutti$tdr, col=3, main= "TDR (all points)")
 qqline(dati.tutti$tdr, col=2, lwd=2)
 finestrina <- par(fig= c(.02, .5, .5, .98), new= TRUE) #imposta una nuova finestra
 hist(dati.tutti$tdr, probability= T, col= "cyan", xlab= "", ylab= "", 
 main= "", axes= F)
 lines(density(dati.tutti$tdr), col= "red", lwd= 2)
 box()
 par(finestrina)
#  savePlot("F:/cnr/perugia/cartelladilavoro/immagini/hist-qqnorn-tdr", type= "jpeg")


# RESISTIVITY
qqnorm(dati.tutti$res, col=3, main= "Resistivity (all points)")
 qqline(dati.tutti$res, col=2, lwd=2)
 finestrina <- par(fig= c(.02, .5, .5, .98), new= TRUE) #imposta una nuova finestra
 hist(dati.tutti$res, probability= T, col= "orange", xlab= "", ylab= "", 
 main= "", axes= F)
 lines(density(dati.tutti$res), col= "red", lwd= 2)
 box()
 par(finestrina)
#  savePlot("F:/cnr/perugia/cartelladilavoro/immagini/hist-qqnorm-res", type= "jpeg")
 
# logaritmo della resistivit?
qqnorm(log(dati.tutti$res), col=3, main= "Log[resistivity] (all points)")
 qqline(log(dati.tutti$res), col=2, lwd=2)
 finestrina <- par(fig= c(.02, .5, .5, .98), new= TRUE) #imposta una nuova finestra
 hist(log(dati.tutti$res), probability= T, col= "goldenrod", xlab= "", ylab= "", 
 main= "", axes= F)
 lines(density(log(dati.tutti$res)), col= "red", lwd= 2)
 box()
 par(finestrina)
#  savePlot("F:/cnr/perugia/cartelladilavoro/immagini/hist-qqnorn-lor-res", type= "jpeg")

###########################
### istogramm
###########################
require(lattice)

histogram(~ tdr |date, dati.tutti, subset= (date!="feb"), 
  xlab= list("soil moisture (%)", cex=1.7), 
  main="TDR - date of sampling set",  scales= list(cex=1.4), col="green")

histogram(~ res |date, dati.tutti, subset= (date!="feb"), 
  xlab= list("apparent resistivity (Ohm m)", cex=1.7),
  main="res - date of sampling set",  scales= list(cex=1.4), col="orange")

histogram(~ log10(res) |date, dati.tutti, subset= (date!="feb"), 
  xlab= list("apparent resistivity (Ohm m)", cex=1.7),
  main="res - date of sampling set",  scales= list(cex=1.4), col="orange")

###########################
### boxplot tdr per sito
###########################
#se voglio mettere una bella griglia sull'imamgine
 bwplot(tdr~site, data=dati.tutti, ylab=list("soil moisture (%)", cex=1.7), 
 notch=T, scales= list(cex=1.4), 
 col="green", 
 panel= function(...) {
 panel.grid(v = 8, h = 8)
 panel.bwplot(...) })
#  savePlot("F:/cnr/perugia/cartelladilavoro/immagini/box-tdr-site", type= "jpeg")

bwplot(res~site, data=dati.tutti, ylab=list("apparent resistivity (Ohm m)", 
 cex=1.7), notch=T, scales= list(cex=1.4), 
 col="green",
 panel= function(...) {
 panel.grid(v = 10, h = 10)
 panel.bwplot(...)
 })
 #savePlot("F:/cnr/perugia/cartelladilavoro/immagini/box-res-site", type= "jpeg")
 dev.print(tiff, file="prova.tiff", width=2000, height=1160, res=300)

#####################################################################################
#####################################################################################

# ANALISI PER DATA
#creo un altro data frame escludendo i dati di febbraio

	dati.nofeb <- dati.tutti[dati.tutti$date!="feb",]
	str(dati.nofeb) # livelli della variabile "date" esiste ancora febbraio 

# lattice esclude automaticamente i livelli non utilizzati 
bwplot(tdr ~ date, data=dati.nofeb, ylab=list("soil moisture (%)", cex=1.7),
 notch=T, scales= list(cex=1.4), 
 col="green",
 panel= function(...) {
 panel.grid(v = 8, h = 8, lty=2)
 panel.bwplot(...)
                     })
  savePlot("F:/cnr/perugia/cartelladilavoro/immagini/box-tdr-date", type= "jpeg")

bwplot(res ~ date, data=dati.nofeb, 
       ylab=list("apparent resistivity (Ohm m)", cex=1.7), 
       notch=T, scales= list(cex=1.4), col="green",
 panel= function(...) {
 panel.grid(v = 8, h = 8)
 panel.bwplot(...)
			   })
#################################################

#istogrammi e density con lattice

 histogram( ~ tdr | date, data = dati.tutti,  subset=(date != "feb"),
               as.table= T, xlab = "soil moisture [%vol/vol]", type = "density",
               panel = function(x, ...) {
                   panel.histogram(x, ...)
                   panel.mathdensity(dmath = dnorm, col = "black",
                                     args = list(mean=mean(x),sd=sd(x)))
               } )
 
 histogram( ~ res | date, data = dati.tutti,  subset=(date != "feb"),
               as.table= T, xlab = "resistivity [Ohm m]", type = "density", col="goldenrod",
               panel = function(x, ...) {
                   panel.histogram(x, ...)
                   panel.mathdensity(dmath = dnorm, col = "black",
                                     args = list(mean=mean(x),sd=sd(x)))
               } )

histogram( ~ logres | date, data = dati.tutti,  subset=(date != "feb"),
               as.table= T, xlab = "log(resistivity) [Ohm]", type = "density", col="cyan4",
               panel = function(x, ...) {
                   panel.histogram(x, ...)
                   panel.mathdensity(dmath = dnorm, col = "black",
                                     args = list(mean=mean(x),sd=sd(x)))
               } )

####	QQPLOT    con lattice

#tutti insieme
#TDR
qqmath(~ tdr, data=dati.tutti, prepanel = prepanel.qqmathline,
      ylab=list("soil moisture [%]", cex=1.5),    
	panel = function(x, ...) {
               panel.qqmathline(x, ...)
               panel.qqmath(x, ...)
            })

#resistivity
qqmath(~ res, data=dati.tutti, prepanel = prepanel.qqmathline,
      ylab=list("apparent resistivity [Ohm m]", cex=1.5),     
	panel = function(x, ...) {
               panel.qqmathline(x, ...)
               panel.qqmath(x, ...)
            })
#LOG of resistivity
qqmath(~ log(res), data=dati.tutti, prepanel = prepanel.qqmathline,
      ylab= list(" Log of apparent resistivity [Ohm m]", cex=1.5),     
	panel = function(x, ...) {
               panel.qqmathline(x, ...)
               panel.qqmath(x, ...)
            })

#TDR by date
qqmath(~ tdr | date, data=dati.tutti, subset= date!="feb",
	 as.table= T, 
	prepanel = prepanel.qqmathline,
            panel = function(x, ...) {
               panel.qqmathline(x, ...)
               panel.qqmath(x, ...)
            })

#REsistivity by date
qqmath(~ log(res) | date, data=dati.tutti, subset= date!="feb",
	as.table= T, 
	prepanel = prepanel.qqmathline,
            panel = function(x, ...) {
               panel.qqmathline(x, ...)
               panel.qqmath(x, ...)
            })

##############################################################################
####################       STATISTICA BIVARIATA        #######################
##############################################################################
plot(dati.tutti$res, dati.tutti$tdr,  xlab="", ylab="", type="n") #fai lo scatter plot ...ma non plottare
	title(main= "Vallaccia area", 
  xlab= list("apparent resistivity  [Ohm m]", cex=1.5), 
  ylab= list("soil moisture [%]", cex=1.5))

attach(dati.tutti)                                                              #evidenzio le Date
 	points(tdr[date== "jan"] ~ res[date== "jan"], pch=21, col= 2, cex=1.1)         #"red"
	points(tdr[date== "feb"] ~ res[date== "feb"], pch=21, col= 7, cex=1.1)
	points(tdr[date== "mar"] ~ res[date== "mar"], pch=21, col= 4, cex=1.1)
	points(tdr[date== "jun"] ~ res[date== "jun"], pch=21, col= 5, cex=1.1)
	points(tdr[date== "oct"] ~ res[date== "oct"], pch=21, col= 3, cex=1.1)
detach(dati.tutti)

#inseriamo la legenda
	legend("topright", legend= levels(dati.tutti$date), 
         col=c(2,7,4,5,3), pch=21, cex= 1.5)
# inserisci un testo  
text(150, 55, paste("r=", round(cor(dati.tutti$tdr, dati.tutti$res),2))) 

# inseriamo tappeto assi 
rug(side=1, dati.tutti$res, col="blue")
rug(side=2, dati.tutti$tdr, col="blue")

# inseriamo linea di tnedenza
abline(lm(tdr~res, dati.tutti), lwd=2, col="red")
lines(lowess(dati.tutti$res,dati.tutti$tdr), lty=2, lwd=2, col="cyan")  #

#se volessi aggiungere la curva dopo aver determianto i parametri della regressione
curve(65.68*x^-0.169, 0.1, max(dati.tutti$res), add=T, lty=3, col=2, lwd=3)
curve(71.50*x^-0.215, 0.1, max(dati.tutti$res), add=T, lty=3, col=4, lwd=3)
curve(67.68*x^-0.268, 0.1, max(dati.tutti$res), add=T, lty=3, col=5, lwd=3)
curve(57.96*x^-0.195, 0.1, max(dati.tutti$res), add=T, lty=3, col=3, lwd=3)

#aggiungiamo anche le medie
 	abline(h= mean(jan$tdr), lty=2, col= 2, lwy=2)         #"red"
  abline(h= mean(mar$tdr), lty=2, col= 4, lwy=2)         
  abline(h= mean(jun$tdr), lty=2, col= 5, lwy=2)       
  abline(h= mean(oct$tdr), lty=2, col= 3, lwy=2)      

##############################################################################
#######################   LATTICE   ##########################################
##############################################################################
require(lattice)

# per i grafici del paper prendi lo script "scatters.R"

#scatter plot "res vs. tdr" suddiviso per sito di campionamento
xyplot(tdr ~ res | site, # suddividi i dati per sito di campionamento 
       data= dati.tutti, 
       ylab= list("soil moisture [%]", cex=1.5), 
       xlab= list("apparent resistivity [Ohm m]", cex=1.5), 
       scales= list(cex=1.3, pch=1:5), #controlla dimensioni etichette assi 
       groups= date, # evidenzia date di campionamento 
       as.table = TRUE, # ordina i quadranti partendo dall'angolo in alto a destra
       layout=c(2,4), # controlla num. righe e colonne 
       auto.key= list(space= "right", border= "blue"),
       panel= function(x, y, ...) { 
                       panel.xyplot(x,y,...)
                       panel.lmline(x,y, col="green")
       })
# savePlot("immagini/scatt_sites_colours_line", type= "jpeg")

#PER DATA
# per? devo eliminare febbraio...faccio cosi
xyplot(tdr ~ res| date, # suddividi i dati per data di campionamento
       data= dati.tutti[dati.tutti$date!= "feb",], 
       ylab= list("soil moisture [%]", cex=1.5), 
       xlab= list("apparent resistivity [Ohm m]", cex=1.5), 
       scales= list(cex=1.3), 
       groups= site, # evidenzia i siti di campionamento 
       as.table = TRUE, 
       aspect= "iso", 
       auto.key= list(space="right", border="blue"))

# savePlot("immagini/scatt_date_colours", type= "jpeg")
                                  


################################################################################
##########   pacchetto mvoutlier  ##############
#richiede che si creino dei vettori con i comandi cbind

require(mvoutlier)
 y<- cbind(dati.tutti$tdr)
 x<- cbind(dati.tutti$res)
 corr.plot(x, y, xlab= list ("apparent resistivity [Ohm m]", cex=1.5), 
 ylab=list("soil moisture [%]", cex=1.5), pch=21, bg="red",
 col="green", cex=1.3)

# data per data
#nello script "nuovo.script" ho creato dei vettori per ciascuna data
 x <- cbind(jan$res)
 y <- cbind(jan$tdr)
 cor.plot(x, y, xlab="apparent resistivity [Ohm m]", ylab="soil moisture [%]",
 pch=21, bg="yellow", col="green", cex=1.3)
 text(200,45, "Jan")
 
 x <- cbind(mar$res)
 y <- cbind(mar$tdr)
 cor.plot(x, y, xlab="apparent resistivity [Ohm m]", ylab="soil moisture [%]", 
 pch=21, bg="yellow", col="green", cex=1.3)
 text(60,55, "March")
 
 x <- cbind(jun$res)
 y <- cbind(jun$tdr)
  cor.plot(x, y, xlab="apparent resistivity [Ohm m]", ylab="soil moisture [%]", 
 pch=21, bg="yellow", col="green", cex=1.3)
 text(60,55, "June")
                                            
 x <- cbind(oct$res)
 y <- cbind(oct$tdr)
 cor.plot(x, y, xlab="apparent resistivity [Ohm m]", ylab="soil moisture [%]", 
 pch=21, bg="yellow", col="green", cex=1.3)
 text(130,55, "October")

################################################################################

# QUA METTO TUTTI INSIEME I DATI DELL'ANNO TDR + FDR 
  
tdr.fdr <- read.csv2(file="dati/tdr-fdr-tutti.csv")                             #leggiamo tutti i dati tdr+fdr raccolti
 	dim(tdr.fdr)                                                                  # diamo un occhiata al data set
 	names(tdr.fdr)
	str(tdr.fdr)
  tdr.fdr$type <- factor(tdr.fdr$type, 
    levels= c("TDR jan", "TDR feb", "TDR mar", "TDR jun", "TDR oct", "FDR"))    # quindi riordino i livelli
	str(tdr.fdr)                                                                  # verifichiamo
	levels(tdr.fdr$type)                                                          # verifichiamo

#scatter plot contenente tutti i dati (TDR + FDR) evidenziando le date di campionamento
	require(lattice)
	xyplot(tdr ~ res, data= tdr.fdr, ylab= list("soil moisture (%)", cex= 1.4), 
          xlab= list("apparent resistivity (Ohm m)", cex=1.4),
          groups= type, scales= list(cex=1.2), 
          auto.key=list(x=0.7, y=0.7, corner=c(0,0)))

#se voglio usare simboli diversi anche per la legenda posso fare anche
	legenda <- simpleKey(levels(tdr.fdr$type), space="right") #creo la legenda
#oppure cosi ? ancora meglio	
	legenda <- simpleKey(levels(tdr.fdr$type), x=0.7, y=0.7, corner=c(0,0), border=T, lwd=2) #creo la legenda
	legenda #la analizzo
	legenda$points$pch <- c(1:6)
	legenda$points$pch #controllo
	
#in scala logaritmica
xyplot(tdr ~ log(res), data= tdr.fdr, 
       ylab= list("soil moisture (%)", cex= 1.4), 
       xlab= list("apparent resistivity (Ohm m)", cex=1.4),
       groups= type, pch= c(1:6), lwd=2, 
       scales= list(cex=1.2))               #attenzione!! l'asse x ha una scala sbagliata!!

########################################################?
#provo a fittare un modello lineare con interazioni (Rositter)

attach(jan)
plot(res, tdr, pch=20, cex=1.5, col=as.numeric(site))
text(200, 47, "Slope of regression")
for (z in as.numeric(site)) {
    m <- lm(tdr~res, subset= (site==z))
    #text(200, 47-(3*z), paste("site", z, ":", round(coefficients(m)[2],3)), col=z)
    m.l <- loess(tdr~res, subset=(site=z), span=100)
    lines(y= c(min(m.l$fitted), max(m.l$fitted)), x= c(min(m.l$x), max(m.l$x)), col=z)
}
detach()
#NON FUNZIONAAAAAAAAAAAAAAAAAAA
