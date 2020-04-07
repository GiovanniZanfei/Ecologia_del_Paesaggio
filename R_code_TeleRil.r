# Codice r per analisi satellitari

# Packages
install.packages("raster")
library(raster)
setwd("C:/lab")
install.packages("RStoolbox")

# Funzione brick per importare un immagine selezionata
p224r63_2011<-brick("p224r63_2011_masked.grd")
plot(p224r63_2011) # Ci dà 7 riquadri che mostrano un immagine basata sulla riflettanza a varie lunghezze d'onda











