R code patches

# set wd e pacchetti
setwd("C:/lab")
install.packages("igraph")
library(igraph)
library(ggplot2)
library(raster)

# caricare immagini raster -> "raster"
d1c<-raster("d1c.tif")
d2c<-raster("d2c.tif")

# plot per distinguere aree di foresta (palette bicolore)
cl<-colorRampPalette(c('green','black'))(100)
par(mfrow=c(1,2))
plot(d1c,col=cl)
plot(d2c,col=cl)
cl<-colorRampPalette(c('black','green'))(100)  # correzione mappa => inversione colori
par(mfrow=c(1,2))
plot(d1c,col=cl)
plot(d2c,col=cl)

dev.off()

# valori 2 => foresta, 1 => aree agricole

# lasciare solo  pixel aree forestali -> "riclassify" per riclassificare valori, "cbind" per trasformare valori 1 (agricoltura) in valori nulli ("NA" => valore mancante)
d1c.for<-reclassify(d1c,cbind(1,NA))
d2c.for<-reclassify(d2c,cbind(1,NA)) 

# multiframe di confronto (solo foreste, foreste+agricoltura)
par(mfrow=c(1,2))
cl<-colorRampPalette(c('black','green'))(100) 
plot(d1c,col=cl)
plot(d1c.for)

# plot mappe solo foresta
par(mfrow=c(1,2))
plot(d1c)
plot(d2c)


# creare patches ("igraph")
library(igraph)
d1c.for.patches<-clump(d1c.for) # "clump"-> unire e raggruppare pixel vicini (creare patches)
d2c.for.patches<-clump(d2c.for)

# "writerRaster" -> esportare il file in formato ".tif" all'esterno di R (in questo caso cartella "lab))
writeRaster(d1c.for.patches,"d1c.for.patches.tif")
writeRaster(d2c.for.patches,"d2c.for.patches.tif")

# esercizio: plottare mappe una accanto all'altra
par(mfrow=c(1,2))
clp<-colorRampPalette(c('darkblue','blue','green','orange','yellow','red'))(100)   # palette con più colori per visualizzare meglio patch di foresta
plot(d1c.for.patches,col=clp)
plot(d2c.for.patches,col=clp)

# numero patches creati nelle mappe
d1c.for.patches  # => 301 patches
d2c.for.patches  # => 1212 patches

# risultati in nuovo dataframe 
time<-c("Before deforestation","After deforestation")  # "time" -> dati prima e dopo deforestazione
npatches<-c(301,1212)                                  # "npatches" -> numero di patches

# creare dataframe "output"
output<-data.frame(time,npatches)

# plot finale ("ggplot")
library(ggplot2)
ggplot(output,aes(x=time,y=npatches,color="red"))+geom_bar(stat="identity",fill="white")
