# R code for analisyng NO2 data from ESA - January to March 2020

setwd("C:/lab")
library(raster)

# importare immagini -> "raster" perchè immagine con una sola banda (con più bande si usa "brick")
EN01<-raster("EN_0001.png")
plot(EN01) 

# eserizio: importare tutte le immagini
EN02<-raster("EN_0002.png")
EN03<-raster("EN_0003.png")
EN04<-raster("EN_0004.png")
EN05<-raster("EN_0005.png")
EN06<-raster("EN_0006.png")
EN07<-raster("EN_0007.png")
EN08<-raster("EN_0008.png")
EN09<-raster("EN_0009.png")
EN10<-raster("EN_0010.png")
EN11<-raster("EN_0011.png")
EN12<-raster("EN_0012.png")
EN13<-raster("EN_0013.png")

# plot multiframe immagine inizale-finale => confronto
cl<-colorRampPalette(c('red','orange',yellow'))(100)
par(mfrow=c(1,2)
plot(EN01,col=cl)
plot(EN13,col=cl)

dev.off()

# differenza "EN13" - "EN01"
difno2<-EN13-EN01
cldif<-colorRampPalette(c('blue','black','yellow'))(100)
plot(difno2,col=cldif)

# esercizio: plot multiframe tutte le immagini
par(mfrow=c(4,4))
plot(EN01,col=cl)
plot(EN02,col=cl)
plot(EN03,col=cl)
plot(EN04,col=cl)
plot(EN05,col=cl)
plot(EN06,col=cl)
plot(EN07,col=cl)
plot(EN08,col=cl)
plot(EN09,col=cl)
plot(EN10,col=cl)
plot(EN11,col=cl)
plot(EN12,col=cl)
plot(EN13,col=cl)
# in alternativa si può fare: plot(EN01,EN02,EN03,EN04,EN05,EN06,EN07,EN08,EN09,EN10,EN11,EN12,EN13,col=cl)

# Day2

setwd("C:/lab")
load("EN.RData")
ls()
library(raster)
# "list.files" (pacchetto "raster") -> vettore comprendente lista di file in una data directory (cartella creata appositamente - "esa_no2")
setwd("C:/lab/esa_no2")
rlist<-list.files(pattern=".png")
rlist

# "lapply" -> applica funzione indicata ad una lista (anzichè ad un solo file)
# in questo caso "raster" -> importare lista di immagini
listafinale<-lapply(rlist,raster)
listafinale

# "stack" -> trasformare lista in una sorta di agglomerato di n bande (13 in questi caso), come fosse un set multitemporale
EN<-stack(listafinale)
cl<-colorRampPalette(c('red','orange',yellow'))(100)
plot(EN,col=cl)                                       # -> visualizzare immagini contenute nello stack "EN"

# differenza marzo ("EN13) - gennaio ("EN01")
difEN<-EN$EN_0013-EN$EN_0001
cld<-colorRampPalette(c('blue','white','red'))(100)
plot(difEN,col=cld)

# boxplot EN -> confronto tra tutte le immagini creando diagramma a riquadri (indicando caratteristiche grafiche)
boxplot(EN,horizontal=T, # barre boxplot orizzontali
        outline=F,       # elimina outliners
        axes=T)          # presenta assi nel plot
# in media cambiamenti non clamorosi, cambiamenti più evidenti sui massimi
