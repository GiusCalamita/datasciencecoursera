##############################
## COSA FA: 
## Script per fare le immagini da pubblicare: alta risoluzione
## ci sono anche altre immagini non usate nel paper
## EDU: plotCI::packg ??
##############################

  load("dati/dati_res_TDR.RData")
  
##############################
## SM vs ER site by site, highlighting dates
##############################
require(lattice)

xyplot(tdr ~ res | site, data= dati.tutti,
       ylab= list("soil moisture (%)", cex=1.5), 
       xlab= list("apparent resistivity (Ohm m)", cex=1.5), 
       scales= list(cex=1.3), groups= date, as.table= TRUE, 
       pch= c(1:5), col= rainbow(5),layout=c(2,4), 
      key= list(space= "right", border= "black", 
                points=list(pch=c(1:5), col= rainbow(5)),
                text= list(levels(dati.tutti$date))
                )
       )
# dev.print(tiff, units="mm", width=200, height=200, res=300,
#           file="immagini/scatter_sites.tiff")

  
# senza COL  
  xyplot(tdr ~ res | site, data= dati.tutti, subset= site!="COL",
       ylab= list("soil moisture (%)", cex=1.5), 
       xlab= list("apparent resistivity (Ohm m)", cex=1.5), 
       scales= list(cex=1.3), groups= date, as.table= TRUE, 
       pch= c(1:5), col= rainbow(5), layout= c(2,4), 
         key= list(space= "right", border= "black", 
                   points=list(pch=c(1:5), col= rainbow(5)), 
                   text= list(levels(dati.tutti$date))
                   )
         )

##############################
## SM vs ER for each date, highlighting sites
##############################

  xyplot(tdr ~ res| date, data= dati.tutti[dati.tutti$date!= "feb",], 
         ylab= list("soil moisture (%)", cex=1.5), 
         xlab= list("apparent resistivity (Ohm m)", cex=1.5), 
         scales= list(cex=1.3), groups= site, as.table = TRUE,
         pch= c(1:8), col= rainbow(8),
         key= list(space= "right", border= "black", 
                   points=list(pch=c(1:8), col= rainbow(8)), 
                   text= list(levels(dati.tutti$site)
                              )
                   )
         )
# dev.print(tiff, units="mm", width=170, height=165, res=300,
#           file="immagini/scatter_dates.tiff")
#          

  
# senza COL
prova= subset(dati.tutti, subset=  date!= "feb"); dim(prova)
attach(prova)
prova2 <- prova[!(site== "COL" & date== "jan"),]
detach()
  
prova2= subset(prova, subset= (site== "COL" & date== "jan"))

xyplot(tdr ~ res| date, data= prova2, 
       ylab= list("soil moisture [%]", cex=1.5), 
       xlab= list("apparent resistivity [Ohm m]", cex=1.5), 
       scales= list(cex=1.3), groups= site, as.table = TRUE,
       pch= c(1:7), col= rainbow(7),
       key= list(space= "right", border= "black", 
                 points=list(pch=c(1:7), col= rainbow(7)), 
                 text= list(levels(prova2$site)))
                 )
       )
         

##############################
## medie areali
##############################
# aggiungiamo gli errori std delle medie 
medie.areali$es_tdr <- medie.areali$sd_tdr/sqrt(16)
medie.areali$es_res <- medie.areali$sd_res/sqrt(16)
medie.areali$date.n <- as.numeric(medie.areali$date)

ndate <- max(medie.areali$date.n)

attach(medie.areali)
plot(res, tdr, 
     xlim= c(0,170), ylim= c(15,55), 
     xlab= "electrical resistivity (Ohm m)", 
     ylab= "soil moisture (%)", 
     type="n",
     cex.axis= 1.5, cex.lab=1.5) 
detach()

colors <- rainbow(ndate)
plotchar <- seq(15, 15+ndate-1,1)

for(i in 1:ndate) {
  medie <- subset(medie.areali, date.n== i)
  plotCI(medie$res, medie$tdr, 
         xlim= c(0,170), ylim= c(15,55), 
         xlab= "", ylab= "", 
         pch= plotchar[i],
         col= colors[i],
         err= 'x',uiw= medie$es_res,
         barcol= "black",
         add=T)
  }

for(i in 1:ndate) {
  medie <- subset(medie.areali, date.n== i)
  plotCI(medie$res, medie$tdr, 
         xlim= c(0,170), ylim= c(15,55), 
         xlab= "", ylab= "", 
         pch= plotchar[i],
         col= colors[i],
         err= 'y',uiw= medie$es_tdr,
         barcol= "black",
         lty=1,add=T)
  }
    
legend("topright", levels(medie.areali$date), 
       col= colors,
       pch= plotchar,
       cex=1.5,
       inset=0.05)
  
dev.print(tiff, units="mm", width=170, height=140, res=300,
          file="immagini/scatter_areal_averages.tiff")

#############################
## FDR
#############################
attach(sondefisse)
plot(res, tdr, 
     #xlim= c(0,170), ylim= c(15,55), 
     xlab= "apparent resistivity (Ohm m)", 
     ylab= "soil moisture (%)", 
     pch= 16, cex=1.2,
     cex.axis= 1.5, cex.lab=1.5)  
detach() 
  
# dev.print(tiff, units="mm", width=170, height=140, res=300,
#           file="immagini/scatter_FDR.tiff")
  
#############################
## letteratura
#############################
#read data
  paper_data <- read.table("dati/fig1_literatureOK.csv", 
                           header=T, sep=";", dec=",")
#elimina colonne inutili
  #for(i in 15:5) paper_data[,i] <- NULL 

table(paper_data$authors,paper_data$soil.type)
#crea colonna con valori numerici invece di classi character  
  paper_data$soil_n <- unclass(paper_data$soil)
  paper_data$author_n <- unclass(paper_data$authors)
 
  nsoil <- max(paper_data$soil_n)
  nauthor <- max(paper_data$author_n)
  
  plot(sm ~ res, paper_data, 
       xlim= c(0,1000), ylim= c(0,50),
       xlab= "electrical resistivity (Ohm m)", 
       ylab= "soil moisture (%)",
       cex.axis= 1.3, cex.lab= 1.3,
       type= "n")
  

colors <- c("green", "red", "black")
char <- c(21:25,21:24)
for(i in 1:nauthor){
  dato <- subset(paper_data, author_n==i)
  points(dato$res, dato$sm, pch=char[i], cex=1.2, 
         bg= ifelse(dato$soil_n==1, "green", ifelse(dato$soil_n==2,"red", "black"))
         )
  }

# letteratura ,scala logaritmica
plot(sm ~ res, paper_data, 
     xlim=c(10,1000),
     xlab= "electrical resistivity (Ohm m)", 
     ylab= "soil moisture (%)",
     cex.axis= 1.3, cex.lab= 1.3,
     log="xy", axes=F, frame.plot=T,
     type= "n")
  at.y <- 10^(0:3)
  axis(1, at = my.at, labels = formatC(my.at, format="fg"))
  axis(2)


colors <- c("green", "red", "black")
char <- c(21:25,21:24)
for(i in 1:nauthor){
  dato <- subset(paper_data, author_n==i)
  points(dato$res, dato$sm, pch=char[i], cex=1.2, 
         bg= ifelse(dato$soil_n==1, "green", ifelse(dato$soil_n==2,"red", "black"))
         )
  }

#legenda da aggiungere
  
###################################
# dati.tutti + curve biliografia
###################################
plot(sm ~ res, paper_data, 
       xlim= c(0,1000), ylim= c(0,50),
       xlab= "electrical resistivity (Ohm m)", 
       ylab= "soil moisture (%)",
       cex.axis= 1.3, cex.lab= 1.3,
       type= "n")

attach(dati.tutti)
points(res, tdr, pch=7, col= "orange", cex= 1.2) 
detach()

attach(sondefisse)
points(res, tdr, pch=7, col= "orange", cex= 1.2) 
detach()

# clay soils
curve(132.25*x^-0.61, 0.1, 750, add=T, col= "darkgreen", lwd=2, lty= 2)
# suoli msiti
curve(126.17*x^-0.45, 0.1, 750, add=T, col= "red", lwd=2, lty= 2)
# sandy soils
curve(654.01*x^-0.71, 0.1, 750, add=T, col= "black", lwd=2, lty= 2)


#log-log scale
plot(sm ~ res, paper_data, 
     xlim=c(10,1000),
     xlab= "electrical resistivity (Ohm m)", 
     ylab= "soil moisture (%)",
     cex.axis= 1.3, cex.lab= 1.3,
     log="xy", axes=F, frame.plot=T,
     type= "n")
  at.y <- 10^(0:3)
  axis(1, at = at.y, labels = formatC(at.y, format="fg"))
  axis(2)

attach(dati.tutti)
points(res, tdr, pch=7, col= "orange", cex= 1.2) 
detach()

attach(sondefisse)
points(res, tdr, pch=7, col= "orange", cex= 1.2) 
detach()
# clay soils
curve(132.25*x^-0.61, 0.1, 750, add=T, col= "darkgreen", lwd=2, lty= 2)
# suoli msiti
curve(126.17*x^-0.45, 0.1, 750, add=T, col= "red", lwd=2, lty= 2)
# sandy soils
curve(654.01*x^-0.71, 0.1, 750, add=T, col= "black", lwd=2, lty= 2)