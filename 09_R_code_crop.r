R_code_crop.r
       
# set wd (dati snow già usati => cartella "snow")
setwd("C:/lab/snow")

# esericizio: caricare tutte le immagini della cartella
library(raster)
rlist<-list.files(pattern="snow")  # "pattern=snow" -> permetterer a R riconoscimento dei file 

# lapply
list.rast<-lapply(rlist, raster)
list.rast

# stack
snow.multitemp<-stack(list.rast)

# plot 
clb<-colorRampPalette(c('dark blue','blue','light blue'))(100)
plot(snow.multitemp,col=clb)

# analisi immagini multitemporali
snow.multitemp
plot(snow.multitemp$snow2010r, col=clb)  # plot immagine 2010 (Italia tra 6 e 20 gradi e tra 35 e 50)
# zoom su Italia -> "zoom", prima impostare nuova estensione ("extension")
extension<-c(6,20,35,50)
zoom(snow.multitemp$snow2010r,ext=extension)
zoom(snow.multitemp$snow2010r,ext=extension,col=clb)  # plot 2010, zoom Italia, palette "clb"

# definire estensione tramite disegno ("drawExtent")
plot(snow.multitemp$snow2010r, col=clb)                    # riplottare immagine originale
zoom(snow.multitemp$snow2010r,ext=drawExtent())
extension<-c(6,20,35,50)
snow2010r.italy<-crop(snow.multitemp$snow2010r,extension)  # "crop" -> ottenere immagine zona ritagliata  
plot(snow2010r.italy,col=clb)                              # plot immagine ottenuta

# esercizio: crop Italia con stack completo
extension<-c(6,20,35,50)
snow.multitemp.Italy<-crop(snow.multitemp,extension)
plot(snow.multitemp.Italy,col=clb)

# impostare legenda uniforme
snow.multitemp.Italy
# min->20, MAX->200
# aggiungere limite => "zlim=c(20,200)"
plot(snow.multitemp.italy,col=clb,zlim=c(20,200))

# boxplot => valore MAX copertura nevosa diminuisce nel tempo
boxplot(snow.multitemp.italy, horizontal=T,outline=F)
