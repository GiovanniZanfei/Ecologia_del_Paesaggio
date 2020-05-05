# R code for analisyng NO2 data from ESA - January to March 2020
setwd("C:/lab")
library(raster)

# Importare immagini
EN01<-raster("EN_0001.png")
plot(EN01)

# Eserizio: importare tutte le immagini
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

# plot multiframe immagine inizale-finale
cl<-colorRampPalette(c('red','orange',yellow'))(100)
par(mfrow=c(1,2)
plot(EN01,col=cl)
plot(EN13,col=cl)

dev.off()

# Differenza tra EN13 e EN01
difno2<-EN13-EN01
cldif<-colorRampPalette(c('blue','black','yellow'))(100)

# Esercizio: plot di tutte le mappe
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













