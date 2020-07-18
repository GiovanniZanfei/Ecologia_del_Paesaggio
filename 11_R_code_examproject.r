### 11_R_code_examproject.r

# setwd e pacchetti
setwd("C:/lab/exam")
library(ncdf4)
library(raster)

# importare dati albedo e raggrupparli
examlist<-list.files(pattern=".nc",full.names=T)
list_rast<-lapply(examlist,raster)
alb.multitemp<-stack(list_rast)

# rinominare i file per praticità
alb.jan2000<-alb.multitemp$Broadband.directional.albedo.over.total.spectrum.1
alb.jan2010<-alb.multitemp$Broadband.directional.albedo.over.total.spectrum.2
alb.jan2020<-alb.multitemp$Broadband.directional.albedo.over.total.spectrum.3
alb.jul2000<-alb.multitemp$Broadband.directional.albedo.over.total.spectrum.4
alb.jul2010<-alb.multitemp$Broadband.directional.albedo.over.total.spectrum.5
alb.jul2020<-alb.multitemp$Broadband.directional.albedo.over.total.spectrum.6

# plot
cl<-colorRampPalette(c('red','orange','yellow'))(100) 
plot(alb.multitemp,col=cl,zlim=c(0,0.9)) # chiedere ad Aaron per titoli

# boxplot -> vedere andamento
boxplot(alb.multitemp,horizontal=T,outline=F)

# confronto jan
par(mfrow=c(1,3))
plot(alb.jan2000,col=cl,zlim=c(0,0.9))
plot(alb.jan2010,col=cl,zlim=c(0,0.9))
plot(alb.jan2020,col=cl,zlim=c(0,0.9))

# confronto jul
par(mfrow=c(1,3))
plot(alb.jul2000,col=cl,zlim=c(0,0.9))
plot(alb.jul2010,col=cl,zlim=c(0,0.9))
plot(alb.jul2020,col=cl,zlim=c(0,0.9))

# quantificare differenze albedo => se varia di più in inverno o estate, in che decennio il cambiamento è massimo
dif1<-alb.jan2000-alb.jan2010
dif2<-alb.jan2010-alb.jan2020
dif3<-alb.jul2000-alb.jul2010
dif4<-alb.jul2010-alb.jul2020
dif1
dif2
dif3
dif4

# differenza 2000-2020 nella stagione con cambio più evidente => confronto con dati snow usati a lezione
difmax(alb.x2000-alb.x2020)
plot(difmax,col=cldif)

# importare raster copertura nevosa
snowlist<-list.files(pattern="snow",full.names=T)
list_snow<-lapply(snowlist,raster)
snow.multitemp<-stack(list_snow)

# confronto con copertura nevosa
clsnow<-colorRampPalette(c('darkblue','blue','light blue'))(100) 
par(mfrow=c(2,2))
plot(snow.multitemp$snow2000r,col=clsnow,zlim=c(0,250))
plot(snow.multitemp$snow2020r,col=clsnow,zlim=c(0,250))
plot(alb.x2000,col=cl,zlim=c(0,0.9))
plot(alb.x2020,col=cl,zlim=c(0,0.9))

# confronto differenze albedo-snow
difsnow<-snow.multitemp$snow2020r-snow.multitemp$snow2000r
par(mfrow=c(1,2))
plot(difmax,col=cldif)
plot(difsnow,col=cldif)

# crop arco alpino (albedo e snow)
extension<-c(6,20,a,b)
dif.alb.alps<-crop(difmax,extension)
dif.snow.alps<-crop(difsnow,extension)
par(mfrow=c(1,2))
plot(dif.alb.alps,col=cldif)
plot(dif.snow.alps,col=cldif)






