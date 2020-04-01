# Codice per analisi dei point patterns

# Installare ggplot2
intall.packages("ggplot2")
library(ggplot2)

# Installara spatstat
install.packages("spatstat)
library(spatstat)

# Set working directory
setwd("C:/lab")

# Importare tabella dati su Covid19; head=T per indicare a r che ci sono i titoli delle colonne; dare alla tabella il nome Covid19
Covid19<-read.table("covid_agg.csv",head=T)
head(Covid19)

# Creare plot che associa Paesi e casi di Covid19 (anzichè $ si può fare attach(Covid19) plot(country,cases)
plot(Covid19$country,Covid19$cases)

# Modificare struttura del plot -> posizione etichette rispetto ad asse (las=0 etichette parallele, 1 orizzontali, 2 perpendicolari, 3 verticali)
plot(Covid19$country,Covid19$cases,las=0)
plot(Covid19$country,Covid19$cases,las=1)
plot(Covid19$country,Covid19$cases,las=2)
plot(Covid19$country,Covid19$cases,las=3)
plot(Covid19$country,Covid19$cases,las=3,cex.axis=0.5) #cex.axis per rimpicciolire la dimensione delle etichette

# ggplot2
data(mpg)
head(mpg)

# Esempio di plot con 2 variabili numeriche -> ggplot2 ha bisogno di 3 cose: dati (mpg), estetica del grafico (aes) e geometria (geom_point)
ggplot(mpg,aes(x=displ,y=hwy))+geom_point()
ggplot(mpg,aes(x=displ,y=hwy))+geom_line()    # usare linee anzichè punti nella visualizzazione
ggplot(mpg,aes(x=displ,y=hwy))+geom_polygon() # usare poligoni 

# ggplot2 per dati Covid19 -> usiamo longitudine e latitudine per avere i punti nello spazio e size=cases così punti più grandi dove ci sono più casi
ggplot(Covid19,aes(x=lon,y=lat,size=cases))+geom_point() 

# Esercizio: zona con più alta densità di casi di Covid19
# Creare dataset per spatstat
covids<-ppp(lon,lat,c(-180,180),c(-90,90))
d<-density(covids)
plot(d)
points(covids,pch=19) # Mostare i punti Covid19 sulla mappa di densità

# 1/04/20

setwd("C:/lab")
load("point_pattern.RData")
ls() #per vedere cosa c'è nel file caricato
library(spatstat)
plot(d)

# palette -> modificare colori del plot d; (100) per dire a r che voglio una scala di quei colori con 100 sfumature
cl<-colorRampPalette(c('yellow','orange','red')) (100)
plot(d,col=cl)

# Esercizio: plot densità dal verde al blu
bluverde<-colorRampPalette(c('blue','grey','green')) (200)
plot(d,col=bluverde)

# Mostare i punti Covid19 sulla mappa di densità
points(covids)

# Inserire nella mappa i confini degli stati
coastlines<-readOGR("ne_10m_coastline.shp") #readOGR serve per leggere file vettoriali
install.packages("rgdal")                   #rgdal serve per usare il comando readOGR
library(rgdal)
coastlines<-readOGR("ne_10m_coastline.shp")
plot(coastlines,add=T)                      #add=T per aggiungere i confini al vecchio plot senza eliminarlo

# Esercizio: plot della mappa di densità con una nuova colorazione e aggiunta e coastlines
clr<-colorRampPalette(c('light blue','blue','pink','purple')) (400)
plot(d,col=clr)
plot(coastlines,add=T)








