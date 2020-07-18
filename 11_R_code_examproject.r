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

# rinominare i file per praticità
albjan2000<-
albjan2010<-
albjan2020<-
albjul2000<-
albjul2010<-
albjul2020<-

# confronto jan
par(mfrow=c(1,3))
plot(albjan2000)
plot(albjan2010)
plot(albjan2020)

# confronto jul
par(mfrow=c(1,3))
plot2000
plot2010
plot2020

# quantificare differenze notare dove c'è differenza più evidente e fare calcolo della differenza
diff
cldif
plot

# crop arco alpino

# confronto crop arco alpino copertura nevosa




