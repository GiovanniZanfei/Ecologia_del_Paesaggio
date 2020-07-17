### 11_R_code_examproject.r

# setwd e pacchetti
setwd("C:/lab/exam")
library(ncdf4)
library(raster)

# importare dati albedo e raggrupparli
rlist<-list.files(pattern=".nc",full.names=T)
albedo<-lapply(rlist,raster)
albedomultitemp<-stack(albedo)

# plot
cl<-colorRampPalette(c('black','grey','white'))(100) 
plot(albedomultitemp,col=cl)

# confronto anni
par(mfrow=c(2,2))
plot(albedogennaio2000,cl)
plot(albedogennaio2020)
plot(luglio2000)
plot(luglio2020)
zlim per uguagliare

uguale per copertura nevosa


