# R code land cover

setwd("C:/lab")
library(raster)
library(RStoolbox)

# Importiamo immagine satellitare con il comando brick
p224r63_2011<-brick("p224r63_2011_masked.grd")

# Plot rgb, landsat bands: 1B, 2G, 3R, 4NIR
plotRGB(p224r63_2011,r=4,g=3,b=2,stretch="Lin")
p224r63_2011c<-unsuperClass(p224r63_2011,nClasses=4)   # associa i pixel per classi in 4 classi, generando una mappa

# Plot della mappa appena generata
plot(p224r63_2011c$map)
clclass<-colorRamppalette(c('red','green','blue','black'))(100)
plot(p224r63_2011c$map,col=clclass)

# Generiamo una mappa con 2 sole classi (più facile per l'algoritmo discriminare 2 classi->suolo nudo e foresta/vegetazione in generale)
p224r63_2011c<-unsuperClass(p224r63_2011,nClasses=2)
plot(p224r63_2011c$map)

# Aumentando il numero di classi diventa più difficile per l'algoritmo discernere i pixel nel numero di classi indicato, l'algoritmo è quindi sensibile al numero di classi e con la crescita di quest'ultimo aumenta l'incertezza dell'algoritmo.

#






































