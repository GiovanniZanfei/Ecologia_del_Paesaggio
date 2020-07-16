R code Snow

# set wd e pacchetti
setwd("C:/lab")
install.packages("ncdf4") # pacchetto per fornire interfaccia R per file di dati binari
library(ncdf4)
library(raster)

# importare immagine scaricata da Copernicus (copertura nevosa 18/05/2020)
snowmay<-raster("c_gls_SCE500_202005180000_CEURO_MODIS_V1.0.1.nc")

# plot "snowmay"
cl<-colorRampPalette(c('darkblue','blue','light blue'))(100) 
plot(snowmay,col=cl)

# settare nuova WD (cartella "snow" => immagini copertura nevosa in diversi momenti)
setwd("C:/lab/snow")

# importare file -> "rlist"
library(raster)
rlist<-list.files(pattern=".tif",full.names=T)

# "lapply" lista appena creata (ogni file "rlist" importato con"raster")
list_rast<-lapply(rlist,raster)

# raggruppare raster in unico vettore -> "stack" (consente di plottarle semplicemente tutte assieme)
snow.multitemp<-stack(list_rast)

# plottare (usare palette creata prima) 
plot(snow.multitemp, col=cl)

# multiframe (confronto) 2000 ("snow2000r") - 2020 ("snow2020r")
par(mfrow=c(1,2))
plot(snow.multitemp$snow2000r,col=cl)
plot(snow.multitemp$snow2020r,col=cl)

# limite ordinate uguale per entrambe le mappe => confronto più facile
par(mfrow=c(1,2))
plot(snow.multitemp$snow2000r, col=cl, zlim=c(0,250))
plot(snow.multitemp$snow2020r, col=cl, zlim=c(0,250))

dev.off()

# differenza 2000-2020
difsnow<-snow.multitemp$snow2020r - snow.multitemp$snow2000r
cldif<-colorRampPalette(c('blue','white','red'))(100)         # nuova palette
plot(difsnow,col=cldiff)                                      # pixel blu => diminuzione copertura, bianchi => stato stazionario, rossi => aumento

# "source" -> caricare codice da file esterni
source("prediction.r")
# comando "lento" => caricare direttamente "predicted.snow.2025"
# previsione 2025
predicted.snow.2025.norm<-raster("predicted.snow.2025.norm.tif") 
plot(predicted.snow.2025.norm,col=cl) 
