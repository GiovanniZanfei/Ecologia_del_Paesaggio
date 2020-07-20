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
plot(alb.multitemp,col=cl,zlim=c(0,1))                # omesso, con sei grafici troppo complicato, la mia palette fa schifo

# confronto jan
par(mfrow=c(1,3))
plot(alb.jan2000,zlim=c(0,1))
plot(alb.jan2010,zlim=c(0,1))
plot(alb.jan2020,zlim=c(0,1))

# confronto jul
par(mfrow=c(1,3))
plot(alb.jul2000,zlim=c(0,1))
plot(alb.jul2010,zlim=c(0,1))
plot(alb.jul2020,zlim=c(0,1))

# quantificare differenze albedo => variazione più evidente in estate o inverno
dif1<-alb.jan2000-alb.jan2020
dif2<-alb.jul2000-alb.jul2020
cldif<-colorRampPalette(c('blue','white','red'))(100)  # blu -> calo, bianco -> stabile, rosso -> aumento    
par(mfrow=c(1,2))
plot(dif1,col=cldif)
plot(dif2,col=cldif)                                   # calo albedo più evidente nella stagione estiva

# confronto con copertura nevosa => atteso pattern simile
# importare raster copertura nevosa
snowlist<-list.files(pattern="snow",full.names=T)
list_snow<-lapply(snowlist,raster)
snow.multitemp<-stack(list_snow)

# confronto
clsnow<-colorRampPalette(c('darkblue','blue','light blue'))(100) 
par(mfrow=c(2,2))
plot(snow.multitemp$snow2000r,col=clsnow,zlim=c(0,250))
plot(snow.multitemp$snow2020r,col=clsnow,zlim=c(0,250))
plot(alb.jul2000,zlim=c(0,1))
plot(alb.jul2020,zlim=c(0,1))

# confronto differenze albedo-snow
difsnow<-snow.multitemp$snow2020r-snow.multitemp$snow2000r
par(mfrow=c(1,2))
plot(dif2,col=cldif)
plot(difsnow,col=cldif)

# crop arco alpino (albedo e snow)
extension<-c(6,20,45,50)
dif.alb.alps<-crop(difmax,extension)
dif.snow.alps<-crop(difsnow,extension)
par(mfrow=c(1,2))
plot(dif.alb.alps,col=cldif)
plot(dif.snow.alps,col=cldif)






