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

# confronto
2000-2010-2020
2000-2010-2020

# quantificare differenze?

# importare, plot e confronto anche per copertura nevosa

# confronto albedo copertura nevosa

# crop alpi


