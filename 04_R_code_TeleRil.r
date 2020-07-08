# Codice R per analisi satellitari (telerilevamento)

#  set wd e pacchetti ("raster","RStoolbox")  
setwd("C:/lab")
install.packages("raster")       # "raster" per lettura, scrittura, analisi e modellizzazione di dati spaziali
library(raster)
install.packages("RStoolbox")    # "RStoolbox" per analisi dati mediante telerilevamento

# funzione "brick" per importare immagine selezionata e creare ogetto "RasterBrick" (multistrato)
p224r63_2011<-brick("p224r63_2011_masked.grd")

# plot oggetto appena creato
plot(p224r63_2011) # 7 riquadri che mostrano un'immagine basata su riflettanza a varie lunghezze d'onda, come indicato sotto
# B1: blue, B2: green, B3: red, B4: near infrared (nir), B5: medium infrared, B6: thermal infrared, B7: medium infrared

# RampPalette ("cl") per avere immagini con scala di colori da bianco a nero una volta rifatto il comando plot con specifica del colore
cl<-colorRampPalette(c('black','grey','light grey'))(100) 
plot(p224r63_2011,col=cl)

# modifica scala cromatica (da 100 a 5 sfumature)
cllow<-colorRampPalette(c('black','grey','light grey'))(5)
plot(p224r63_2011,col=cllow)

# plot banda blu (B1)
names(p224r63_2011)                                             # "names" -> visionare nomi oggetto
# [1] "B1_sre" "B2_sre" "B3_sre" "B4_sre" "B5_sre" "B6_bt"  "B7_sre"
clb<-colorRampPalette(c('dark blue','blue','light blue'))(100)  # palette blu
plot(p224r63_2011$B1_sre,col=clb)

# esercizio: plottare banda infrarosso vicino palette rosso-arancione-giallo
clnir<-colorRampPalette(c('red','orange','yellow'))(100)
plot(p224r63_2011$B4_sre,col=clnir)

# plot multiframe, quattro bande
par(mfrow=c(2,2))
# blue
clb<-colorRampPalette(c('dark blue','blue','light blue'))(100)  
plot(p224r63_2011$B1_sre,col=clb)
# green
clg<-colorRampPalette(c('dark green','green','light green'))(100) 
plot(p224r63_2011$B2_sre,col=clg)
# red
clr<-colorRampPalette(c('dark red','red','pink'))(100)  
plot(p224r63_2011$B3_sre,col=clr)
# nir
clnir<-colorRampPalette(c('red','orange','yellow'))(100)
plot(p224r63_2011$B4_sre,col=clnir)

# "dev.off()" per chiudere
dev.off()

# natural colours
# 3 componenti: R G B
# 3 bande: R = banda rosso, G = banda verde, B = banda blu
# B1: blue - 1
# B2: green - 2
# B3: red - 3
# B4: near infrared (nir) - 4 

# "plotRGB" -> creare plot rosso-verde-blu su tre livelli (tre strati combinati per rappresentare bande rosso, verde e blu)
plotRGB(p224r63_2011, r=3, g=2, b=1, stretch="Lin") # stretch="Lin" per migliorare visibilità immagine

# nir => aggiunta banda infrarosso per rendere immagine più leggibile (necessario togliere una delle altre tre, in questo caso blu)
plotRGB(p224r63_2011, r=4, g=3, b=2, stretch="Lin")
# permette di visualizzare vegetazione

# salvataggio immagine appena ottenuta
pdf("primografico.pdf") 
plotRGB(p224r63_2011, r=4, g=3, b=2, stretch="Lin")
dev.off()

# multiframe bande diverse
par(mfrow=c(2,1))
plotRGB(p224r63_2011, r=3, g=2, b=1, stretch="Lin")
plotRGB(p224r63_2011, r=4, g=3, b=2, stretch="Lin")
dev.off()

# esercizio: nir nella compnente R(Red)
plotRGB(p224r63_2011, r=4, g=3, b=2, stretch="Lin")

# esercizio: nir nella componente G(Green)
plotRGB(p224r63_2011, r=3, g=4, b=2, stretch="Lin")

# esercizo: nir nella componente B(Blue)
plotRGB(p224r63_2011, r=3, g=2, b=4, stretch="Lin")

# landcover

# setwd e pacchetti
setwd("C:/lab/p224r63")
library(raster)

# "brick" per importare immagine
p224r63_2011<-brick("p224r63_2011_masked.grd")

# richiamare "RStoolbox"
library(RStoolbox)

# plottare immagine in R G B
plotRGB(p224r63_2011,r=4,g=3,b=2,stretch="Lin")

# classificazione dati raster con "unsuperClass", specificando numero di classi
p224r63_2011c<-unsuperClass(p224r63_2011,nClasses = 4)

# visualizzare nuovo modello contenente anche mappa
p224r63_2011c

# plot mappa (quattro colori -> quattro classi specificate) 
plot(p224r63_2011c$map)

# nuova palette (migliore visualizzazione del grafico)
clclass <- colorRampPalette(c('green',"red","blue","black"))(100)
plot(p224r63_2011c$map,col=clclass)

# Day2
# setwd e pacchetti
library(raster)
setwd("C:/lab")
load("TeleRil.RData")
ls()

# importare file 1988 e 2011 ("brick")
p224r63_2011<-brick("p224r63_2011_masked.grd") 
p224r63_1988<-brick("p224r63_1988_masked.grd")

# immagine 1988, come 2011 ha sette bande (colori):
# B1: blue - 1
# B2: green - 2
# B3: red - 3
# B4: near infrared (nir) - 4 
# B4: near infrared (nir)
# B5: medium infrared
# B6: thermal infrared
# B7: medium infrared

# plot oggetto 1988 e visualizzare campi
plot(p224r63_1988)
names(p224r63_1988)

# plot multiframe per banda blu (1), verde (2), rosso (3) e nir (4)
par(mfrow=c(2,2))
clb<-colorRampPalette(c("dark blue","blue","light blue"))(100)    # blue
plot(p224r63_1988$B1_sre,col=clb)
clg<-colorRampPalette(c("dark green","green","light green"))(100) # green
plot(p224r63_1988$B2_sre,col=clg)
clr<-colorRampPalette(c("red","orange","yellow"))(100)            # red
plot(p224r63_1988$B3_sre,col=clr)
clnir<-colorRampPalette(c("purple","pink","light pink"))(100)     # nir
plot(p224r63_1988$B4_sre,col=clnir)
 
dev.off()

# immagine con colori visibili (plotRGB "natural colours")
plotRGB(p224r63_1988, r=3, g=2, b=1, stretch="Lin")

# grafico poco comprensibile => usare infrarosso (plotRGB "false colours"
# esercizio: plotRGB con componenete infrarossa
plotRGB(p224r63_1988,r=4,g=3,b=2,stretch="Lin")

# richiamare immagine 2011
p224r63_2011

# plot per confronto immagini 1988 e 2011
par(mfrow=c(2,1))
plotRGB(p224r63_1988,r=4,g=2,b=1,stretch="Lin")
plotRGB(p224r63_2011,r=4,g=2,b=1,stretch="Lin")  
# => territorio agricolo è molto più sviluppato nel 2011
# nir indica la presenza di vegetazione, zolle di terra sono bianche o celeste

dev.off()

# Spectral indices (DVI) => verificare stato salute vegetazione (foglie sane riflettono infrarosso)
# DVI=nir-red -> es: dvi1988=nir1988-red1988 => risultati diversi in base a salute piante (sane=nir alto)
# DVI 1988
dvi1988<-p224r63_1988$B4_sre-p224r63_1988$B3_sre
plot(dvi1988)

# esercizio: DVI 2011
dvi2011<-p224r63_2011$B4_sre-p224r63_2011$B3_sre
plot(dvi2011)

# cambio "colorRampPalette"
cldvi<-colorRampPalette(c('light blue','light green','green'))(100)
plot(dvi2011,col=cldvi)

# analisi multitemporale (differenza 2011-1988) => differenza tra DVI dei 2 anni mostra cambiamento stato vegetazione
difdvi<-dvi2011-dvi1988
plot(difdvi)
cldifdvi<-colorRampPalette(c('red','white','blue'))(100)
plot(difdvi,col=cldifdvi)
     
dev.off()

# Visualize the output
# multiframe 1988RGB, 2011RGB, difdvi
par(mfrow=c(3,1))
plotRGB(p224r63_1988,r=4,g=3,b=2,stretch="Lin")
plotRGB(p224r63_2011,r=4,g=3,b=2,stretch="Lin")  
plot(difdvi,col=cldifdvi)

dev.off()

# "aggregate" -> modificare risoluzione (grana) immagine creando nuovo RasterLayer con risoluzione più bassa quindi celle più grandi ("fact=n" è un moltiplicatore che ci dà dei pixel n volte più grandi dei precedenti)
p224r63_2011lr<-aggregate(p224r63_2011,fact=10) # lr=lowresolution
# inserire i due oggetti per vedere caratteristiche dei pixel
p224r63_2011
p224r63_2011lr

# plot multiframe confronto tra le due risoluzioni     
par(mfrow=c(2,1))
plotRGB(p224r63_2011, r=4, g=3, b=2, stretch="Lin")
plotRGB(p224r63_2011lr, r=4, g=3, b=2, stretch="Lin")

# lower resolution ("fact=50")
p224r63_2011lr50<-aggregate(p224r63_2011,fact=50)   
p224r63_2011lr50                                  

# plot multiframe comparativo (normale, lr, lr50)     
par(mfrow=c(3,1))
plotRGB(p224r63_2011, r=4, g=3, b=2, stretch="Lin")
plotRGB(p224r63_2011lr, r=4, g=3, b=2, stretch="Lin")
plotRGB(p224r63_2011lr50, r=4, g=3, b=2, stretch="Lin")

# DVI lr50 2011
dvi2011lr50<-p224r63_2011lr50$B4_sre-p224r63_2011lr50$B3_sre
plot(dvi2011lr50)
     
# DVI lr50 1988
p224r63_1988lr50<-aggregate(p224r63_1988,fact=50)              # creare lr50 1988
dvi1988lr50<-p224r63_1988lr50$B4_sre-p224r63_1988lr50$B3_sre
plot(dvi1988lr50)     

# difdvi lr50
difdvilr50<-dvi2011lr50-dvi1988lr50
plot(difdvilr50,col=cldifdvi)        # riprendere palette "cldifdvi" creata in precedenza

# multiframe differenze DVI alle diverse risoluzioni
par(mfrow=c(2,1))
plot(difdvi,col=cldifdvi)
plot(difdvilr50,col=cldifdvi)

