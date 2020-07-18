### 11_R_code_examproject.r

# setwd e pacchetti
setwd("C:/lab/exam")
library(ncdf4)
library(raster)

# importare dati albedo e raggrupparli
examlist<-list.files(pattern=".nc",full.names=T)
list_rast<-lapply(examlistlist,raster)
alb.multitemp<-stack(list_rast)

# plot
cl<-colorRampPalette(c('red','orange','yellow'))(100) 
plot(alb,multitemp,col=cl,zlim=c(0,0.7))

# confronto
2000-2010-2020
2000-2010-2020

# quantificare differenze?

# importare, plot e confronto anche per copertura nevosa

# confronto albedo copertura nevosa

# crop alpi


