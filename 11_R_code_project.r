### 11_R_code_project.r

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

# crop arco alpino, inverno
extension<-c(0,20,42,50)
alb.alps.2000.jan<-crop(alb.jan2000,extension)
alb.alps.2010.jan<-crop(alb.jan2010,extension)
alb.alps.2020.jan<-crop(alb.jan2020,extension)
par(mfrow=c(1,3))
plot(alb.alps.2000.jan,zlim=c(0,1))
plot(alb.alps.2010.jan,zlim=c(0,1))
plot(alb.alps.2020.jan,zlim=c(0,1))

# crop arco apino estivo
alb.alps.2000.jul<-crop(alb.jul2000,extension)
alb.alps.2010.jul<-crop(alb.jul2010,extension)
alb.alps.2020.jul<-crop(alb.jul2020,extension)
par(mfrow=c(1,3))
plot(alb.alps.2000.jul,zlim=c(0,1))
plot(alb.alps.2010.jul,zlim=c(0,1))
plot(alb.alps.2020.jul,zlim=c(0,1))

# differenze nell'albedo inverno-estate negli anni
dif1<-albjan.2000-albjul.2000
dif2<-albjan.2020-albjul.2020
cldif<-colorRampPalette(c('blue','white','red'))(100)
par(mfrow=c(1,2))
plot(dif1,col=cldif)
plot(dif2,col=cldif)
# mi spettavo differenze più evidenti, forse lettura errata

# confronto con copertura nevosa => attesi pattern in linea
# importare raster copertura nevosa
snowlist<-list.files(pattern="snow",full.names=T)
list_snow<-lapply(snowlist,raster)
snow.multitemp<-stack(list_snow)

# confronto (uso mesi invernali per rendere confronto più visibile)
clsnow<-colorRampPalette(c('darkblue','blue','light blue'))(100) 
par(mfrow=c(2,2))
plot(snow.multitemp$snow2000r,col=clsnow,zlim=c(0,250))
plot(snow.multitemp$snow2020r,col=clsnow,zlim=c(0,250))
plot(alb.jan2000,zlim=c(0,1))
plot(alb.jan2020,zlim=c(0,1))

# crop arco alpino (albedo e snow)
snow.alps.2000<-crop(snow.multitemp$snow2000r,extension)
snow.alps.2020<-crop(snow.multitemp$snow2020r,extension)
par(mfrow=c(2,2))
plot(alb.alps.2000.jan,zlim=c(0,1))
plot(alb.alps.2020.jan,zlim=c(0,1))
plot(snow.alps.2000,col=clsnow,zlim=c(0,250))
plot(snow.alps.2020,col=clsnow,zlim=c(0,250))

# confronto tra differenze 2000-2020 albedo e snow
dif3<-alb.jan2000-alb.jan2020
difsnow<-snow.multitemp$snow2000r-snow.multitemp$snow2020r
par(mfrow=c(2,2))
plot(dif3,col=cldif)
plot(difsnow,col=cldif)

# crop Alpi differenze
alb.dif.alps<-crop(dif3,extension)
snow.dif.alps<-crop(difsnow,extension)
par(mfrow=c(1,2))
plot(alb.dif.alps,col=cldif)
plot(snow.dif.alps,col=cldif)
# differenza molto più evidente a livello di copertura nevosa (ovvio)
