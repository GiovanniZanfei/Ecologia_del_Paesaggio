# Codice per analisi point patterns

# Installare library "ggplot2"
intall.packages("ggplot2")
library(ggplot2)

# installare library "spatstat"
install.packages("spatstat)
library(spatstat)

# iet working directory
setwd("C:/lab")

# importare tabella dati Covid19; "head=T" per indicare a R che ci sono i titoli delle colonne; dare alla tabella il nome Covid19
Covid19<-read.table("covid_agg.csv",head=T)
head(Covid19)

# creare plot che associa Paesi e casi di Covid19 (anzichè "$" si può fare "attach(Covid19) plot(country,cases)")
plot(Covid19$country,Covid19$cases)

# modificare struttura plot -> posizione etichette rispetto ad asse ("las=0" etichette parallele, 1 orizzontali, 2 perpendicolari, 3 verticali)
plot(Covid19$country,Covid19$cases,las=0)
plot(Covid19$country,Covid19$cases,las=1)
plot(Covid19$country,Covid19$cases,las=2)
plot(Covid19$country,Covid19$cases,las=3)
plot(Covid19$country,Covid19$cases,las=3,cex.axis=0.5) # cex.axis per rimpicciolire dimensione etichette

# ggplot2
data(mpg)
head(mpg)

# esempio di plot con 2 variabili numeriche -> ggplot2 ha bisogno di 3 cose: dati ("mpg"), estetica del grafico ("aes") e geometria ("geom_point")
ggplot(mpg,aes(x=displ,y=hwy))+geom_point()
ggplot(mpg,aes(x=displ,y=hwy))+geom_line()    # usare linee anzichè punti nella visualizzazione
ggplot(mpg,aes(x=displ,y=hwy))+geom_polygon() # usare poligoni 

# ggplot2 per dati Covid19 -> usare longitudine e latitudine per avere i punti nello spazio, "size=cases" -> punti più grandi dove ci sono più casi
ggplot(Covid19,aes(x=lon,y=lat,size=cases))+geom_point() 

# Esercizio: zona con più alta densità casi di Covid19
# creare dataset per spatstat
covids<-ppp(lon,lat,c(-180,180),c(-90,90))
d<-density(covids)
plot(d)
points(covids,pch=19) # Mostare i punti Covid19 sulla mappa di densità

# 1/04/20

setwd("C:/lab")
load("point_pattern.RData")
ls() # per vedere contenuto del file caricato
library(spatstat)
plot(d)

# palette -> modificare colori del plot d; (100) per dire a R quante sfumature deve avere la scala di colori
cl<-colorRampPalette(c('yellow','orange','red')) (100)
plot(d,col=cl)

# esercizio: plot densità dal verde al blu
bluverde<-colorRampPalette(c('blue','grey','green')) (200)
plot(d,col=bluverde)

# mostare punti Covid19 sulla mappa di densità
points(covids)

# inserire nella mappa confini degli stati
coastlines<-readOGR("ne_10m_coastline.shp") # "readOGR" -> leggere file vettoriali
install.packages("rgdal")                   # "rgdal" -> library necessaria per usare il comando "readOGR"
library(rgdal)
coastlines<-readOGR("ne_10m_coastline.shp")
plot(coastlines,add=T)                      # "add=T" per aggiungere confini al vecchio plot senza eliminarlo

# esercizio: plot della mappa di densità con nuova colorazione e aggiunta coastlines
clr<-colorRampPalette(c('light blue','blue','pink','purple')) (400)
plot(d,col=clr)
plot(coastlines,add=T)

setwd("C:/lab")
load("C:/lab/point_ppattern.RData")
library(spatstat)
ls()
library(rgdal)

# esercizio: plot mappa di densità
coastlines<-readOGR("ne_10m_coastline.shp")
clr<-colorRampPalette(c('light blue','blue','pink','purple')) (400)
plot(d,col=clr,main="density")
plot(coastlines,add=T)
points(covids)

# Interpolation
covid
marks(covids)<-covid$cases  # "marks" -> associare dati categoria "cases" al pointpattern "covids"
s<-Smooth(covids)           # "Smooth" -> creare mappa con i dati appena costruiti
plot(s)

# esercizio: plot(s) con coastlines e punti
cls<-colorRampPalette(c('light blue','blue','green'))(100)
plot(s,col=cls,main="Cases")
points(covids)
plot(coastlines,add=T)

### mappa finale (multiframe con entrambi i plot fatti)
par(mfrow=c(2,1))

# - Densità
clr<-colorRampPalette(c('light blue','blue','pink','purple')) (400)
plot(d,col=clr,main="density")
plot(coastlines,add=T)
points(covids)

# - Interpolation
cls<-colorRampPalette(c('light blue','blue','green'))(100)
plot(s,col=cls,main="Cases")
points(covids)
plot(coastlines,add=T)

### San Marino
setwd("C:/lab")
load("C:/lab/Tesi.RData")
ls()
head(Tesi)

library(spatstat)
attach(Tesi)

# - Point pattern: x,y,c(xmin,xmax),c(ymin,ymax)
summary(Tesi)    # sommario del dataset, posso trovare rapidamente le info principali
# Grazie a summary so che 12.42<x<12.46 e 43.91<y<43.94
Tesippp<-ppp(Longitude,Latitude,c(12.41,12.47),c(43.90,43.95))

# - Mappa densità
dT<-density(Tesippp)
dev.off 
plot(dT)
points(Tesippp,col="green")

# 28/04

setwd("C:/lab")
load("C:/lab/Tesi.RData")
library(spatstat)
library(rgdal)

# dt=density map, Tesi=dataset, Tesippp=point pattern (coordinate longitudine e latitudine)

head(Tesi)

# Associare al point pattern il valore d'interesse (ricchezza di specie) e poi procedere con l'interpolazione
marks(Tesippp)<-Tesi$Species_richness
interpol<-Smooth(Tesippp)
plot(interpol)  # mappa
points(Tesippp)

# Carichiamo il file vettoriale "San_Marino" e sovrapponiamo la mappa costruita prima
sanmarino<-readOGR("San_Marino.shp")
plot(sanmarino)
plot(interpol,add=T)
points(Tesippp)
plot(sanmarino,add=T)

# Esercizio: plot multiframe densità e interpolazione
par(mfrow=c(2,1))
plot(dT,main="Density of points")
points(Tesippp)
plot(interpol,main="Estimate of species richness")
points(Tesippp)

# Esercizio: come prima ma 2 colonne e una riga
par(mfrow=c(1,2))
plot(dT,main="Density of points")
points(Tesippp)
plot(interpol,main="Estimate of species richness")
points(Tesippp)



































































