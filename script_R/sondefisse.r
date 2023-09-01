#############################################################################################
## Analizza dati FDR e ER 
## EDU: libreria fBaiscs, per test su normalità dei residui
## test breush-pagan, durbin watson, kolmogorov smirnov
#############################################################################################

#leggiamo i dati FDR (sondefisse)			
	sondefisse <- read.csv2(file= "dati/fdr.csv")
	str(sondefisse)
  summary(sondefisse)
	

attach(sondefisse)
	cor.test(tdr,res)
	cor.test( ~ tdr + res, alternative= "less")
	cor.test(tdr, res, alternative= "l", method= "s") 
	
 lin.fdr <- lm(tdr~res)
	summary(lin.fdr)
	log.fdr <- lm(log(tdr)~log(res))
	summary(log.fdr)
	confint(log.fdr)
	
power.fdr <- nls(tdr ~ a*res ^b, start=list(a=1, b=0.1))
	summary(power.fdr)
	confint(power.fdr)
	lapply(sondefisse, sd, na.rm= T) #manca un valore TDR
detach()

R2 <- function (model,dataset) {
  RSS.p <- sum(residuals(model)^2)                                              #Residuals sum of squares: la p finale sta per power
  TSS   <- sum((dataset[, "tdr"] - mean(dataset[, "tdr"], na.rm=T))^2, na.rm=T)                   #Total Sum of squares
  R2 <- 1 - RSS.p/TSS
  print(paste("RSS =", RSS.p))
  print(paste("TSS =", TSS))
  print(paste("R2 =", R2))
}                                           
R2(power.fdr, fdr)

#jpeg(mediadiskcnrperugiaOKdatiRgraphicsscatterFDR.jpeg)
	plot(tdr ~ res, data= sondefisse, col= 3, pch=16, xlab="", ylab="", cex=2)
	title(xlab= list("apparent resistivity (Ohm m)", cex=1.4),
        ylab= list(" soil moisture (%)", cex=1.4) )
#dev.off()

#e siccom s? ffort assai, mo ti faccio pure la retta di regressione...ohps ..la curva scusa :P

tt<-seq(0,800, by=0.1)
	predict(power.fdr) 
	predict(power.fdr, list(res = tt)) #giusto per capire come funziona il predict
	lines(tt, predict(power.fdr, list(res = tt)), col="red3")
	lines(tt, predict(poppo, list(res = tt)), col="red") #se ti piace unaltro colore YEAH!!!
#inseriamo la formula e qualche altra informazione
	 par(cex=1.3) #aumento le dimensioni del font 
#nella riga che segue devo sceglier la posizione della scritta: legend considera le coordinate x,y come punto di partenza
#attenzione che spesso icaratteri speciali in GRECO no vengono presi in consolle, occorre copiare e incolalrli
	legend(300, 30, legend= list("? = a * ^b", "RMSE=5.456"), bg="lightyellow")
#################################################################################################
  require(lattice)
  xyplot(tdr ~ res, data= sondefisse, main= "FDR vs Resistivity", xlab= "apparen resistivity Ohm m" , ylab= " soil mloisture %" )
#################################################################################################
#################################################################################################
# TEST DI SPECIFICAZIONE: verifica delle ipotesi di base

#  1)la media dei residui  uguale a zero? (ipotesi nulla) 
residuiresiduals(modello)  # vettore dei residui
t.test(residui)
plot(residui)                #visualizza l'andamento dei residui
abline(h0)                  # traccia la retta media0

# 2)ls distribuzione dei residui  normale?
shapiro.test(residui)
qqnorm(scale(residui)); abline(01)  #metodo grafico

library(fBasics) # carica una libreria con diversi test per la normalit
ksnormTest(x)kolmogorovsmirnov
shapiroTest(x)
beraTest(x)
jarqueberaTest(x) 

# 3)omoschedasticit delle varianze dei residui...
library(lmtest) # carica la libreria con i test che ci servono
bptest(FORMULA datasensori.fissi)  #test di BreuschPagan

plot(sensori.fissiTDRresidui) #visualizza la variazione dei residui al variare della var. idipendentejbuie5

# 4)..e correlazione seriale
library(lmtest)  carica la STESSA libreria di prima
testdwdwtest(FORMULA datasensori.fissi)  test di DurbinWatson
testdw