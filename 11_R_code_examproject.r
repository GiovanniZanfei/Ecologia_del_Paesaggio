### 11_R_code_examproject.r

# setwd e pacchetti
setwd("C:/lab/exam")
library(ncdf4)
library(raster)

# importare dati albedo e raggrupparli
rlist<-list.files(pattern="")
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

# differenze
dif2000<-gennaio 2000 - luglio 2000
dif2020<-gennaio 2020 - luglio 2020
difjanuary<-gennaio 2000- gennaio 2020
difjuly<-luglio - luglio
plot di tutte

uguale per copertura nevosa


