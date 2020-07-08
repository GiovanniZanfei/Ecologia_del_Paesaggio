# codice per analisi point patterns

# installare libraries ("ggplot2", "spatstat")
intall.packages("ggplot2")
library(ggplot2)
install.packages("spatstat)
library(spatstat)

# set working directory
setwd("C:/lab")

# importare tabella dati Covid19; "head=T" per indicare a R che ci sono i titoli delle colonne; dare alla tabella il nome Covid19
Covid19<-read.table("covid_agg.csv",head=T)
head(Covid19)   # comando per vedere tabella

# creare plot che associa Paesi e casi di Covid19 (anzichè "$" si può fare "attach(Covid19) plot(country,cases)")
plot(Covid19$country,Covid19$cases)

# modificare struttura plot -> posizione etichette rispetto ad asse ("las=0" etichette parallele, 1 orizzontali, 2 perpendicolari, 3 verticali)
plot(Covid19$country,Covid19$cases,las=0)
plot(Covid19$country,Covid19$cases,las=1)
plot(Covid19$country,Covid19$cases,las=2)
plot(Covid19$country,Covid19$cases,las=3)
plot(Covid19$country,Covid19$cases,las=3,cex.axis=0.5) # "cex.axis" -> rimpicciolire dimensione etichette

# richiamare "ggplot2" (pacchetto per estetica e cura dei dettagli)
library(ggplot2)

# dataframe "mpg" da library "ggplot2" ("mpg" raccoglie osservazioni US Environmental Protection Agency su 38 modelli di auto)
data(mpg)
head(mpg)

# esempio di plot con 2 variabili numeriche -> ggplot2 ha bisogno di 3 cose: dati ("mpg"), estetica del grafico ("aes", funzione di quotazione) e geometria ("geom_")
ggplot(mpg,aes(x=displ,y=hwy))+geom_point()
ggplot(mpg,aes(x=displ,y=hwy))+geom_line()    # usare linee anzichè punti nella visualizzazione
ggplot(mpg,aes(x=displ,y=hwy))+geom_polygon() # usare poligoni 

# "ggplot2" per dati Covid19 -> usare longitudine e latitudine per avere i punti nello spazio, "size=cases" -> punti più grandi dove ci sono più casi
ggplot(Covid19,aes(x=lon,y=lat,size=cases))+geom_point() 

# richiamare library "spatstat" (mostra analisi dei modelli dei punti spaziali) e fissare dataframe
library(spatstat)
attach(covid)

# esercizio: zona con più alta densità casi di Covid19
# creare dataset per spatstat -> "ppp" crea un oggetto che rappresenta un insieme di dati del pattern puntiforme nel piano bidimensionale
covids<-ppp(lon,lat,c(-180,180),c(-90,90))  # necessario specificare cosa indicano x e y ("lon","lat") e definirne il range
d<-density(covids)                          # comando per calcolare densità dei casi
plot(d)                                     # plot (rappresentazione grafica) densità
points(covids,pch=19)                       # mostare i punti Covid19 sulla mappa di densità

# 1/04/20

# settare wd, caricare file salvato, richiamare "spatstat" e mostrare grafico densità casi Covid
setwd("C:/lab")
load("point_pattern.RData")
ls()   # per vedere contenuto del file caricato
library(spatstat)
plot(d)

# palette -> modificare colori del plot d; (100) per dire a R quante sfumature deve avere la scala di colori
cl<-colorRampPalette(c('yellow','orange','red')) (100)
plot(d,col=cl)  # plot densità con nuovi colori

# esercizio: plot densità dal verde al blu
bluverde<-colorRampPalette(c('blue','grey','green')) (200)
plot(d,col=bluverde)

# mostare punti Covid19 sulla mappa di densità
points(covids)

# inserire nella mappa confini degli stati
install.packages("rgdal")                   # "rgdal" -> library necessaria per usare il comando "readOGR"
library(rgdal)
coastlines<-readOGR("ne_10m_coastline.shp") # "readOGR" -> funzione che legge origine dati OGR e un layer in un oggetto vettoriale spaziale adatto, serve per creare layer dei confini
plot(coastlines,add=T)                      # "add=T" per aggiungere confini al vecchio plot senza eliminarlo -> grafico completo

# esercizio: plot della mappa di densità con nuova colorazione e aggiunta coastlines
clr<-colorRampPalette(c('light blue','blue','pink','purple')) (400)
plot(d,col=clr)
plot(coastlines,add=T)

setwd("C:/lab")
load("C:/lab/point_ppattern.RData")
library(spatstat)
ls()
library(rgdal)

# esercizio: plot mappa di densità con nuova palette
coastlines<-readOGR("ne_10m_coastline.shp")
clr<-colorRampPalette(c('light blue','blue','pink','purple')) (400)
plot(d,col=clr,main="density")
plot(coastlines,add=T)
points(covids)

# Interpolation
covid
marks(covids)<-covid$cases  # "marks" -> associare dati categoria "cases" al pointpattern "covids"
s<-Smooth(covids)           # "Smooth" -> creare mappa con i dati appena costruiti
plot(s)                     # plot mappa appena creata

# esercizio: plot(s) con coastlines e punti
cls<-colorRampPalette(c('light blue','blue','green'))(100)
plot(s,col=cls,main="Cases")
points(covids)
plot(coastlines,add=T)

### mappa finale (multiframe con entrambi i plot fatti)
par(mfrow=c(2,1))

# primo plot per densità
clr<-colorRampPalette(c('light blue','blue','pink','purple')) (400)
plot(d,col=clr,main="density")
plot(coastlines,add=T)
points(covids)

# secondo plot per interpolazione numero di casi
cls<-colorRampPalette(c('light blue','blue','green'))(100)
plot(s,col=cls,main="Cases")
points(covids)
plot(coastlines,add=T)

### San Marino (lavorare con set di dati di una tesi su San Marino scaricati in "lab" )
setwd("C:/lab")
load("C:/lab/Tesi.RData")
ls()
head(Tesi)
library(spatstat)
attach(Tesi)

# - Point pattern: x,y,c(xmin,xmax),c(ymin,ymax)
summary(Tesi)  # sommario del dataset, posso trovare rapidamente le info principali
# "summary" -> longitudine: 12.42<x<12.46 e latitudine: 43.91<y<43.94
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



































































