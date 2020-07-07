# Codice r per analisi satellitari

# Packages
install.packages("raster")
library(raster)
setwd("C:/lab")
install.packages("RStoolbox")

# Funzione brick per importare un immagine selezionata
p224r63_2011<-brick("p224r63_2011_masked.grd")
plot(p224r63_2011) # Ci dà 7 riquadri che mostrano un'immagine basata sulla riflettanza a varie lunghezze d'onda, come indicato sotto
# B1: blue, B2: green, B3: red, B4: near infrared (nir), B5: medium infrared, B6: thermal infrared, B7: medium infrared

# Creo una RampPalette che assegno all'oggetto cl per avere le immagini con scala di colori da bianco a nero una volta rifatto il comando plot con specifica del colore
cl<-colorRampPalette(c('black','grey','light grey'))(100) 
plot(p224r63_2011,col=cl)

# Scala del grigio con meno colori
cllow<-colorRampPalette(c('black','grey','light grey'))(5)
plot(p224r63_2011,col=cllow)

# Plot della banda blu (B1)
names(p224r63_2011)
# [1] "B1_sre" "B2_sre" "B3_sre" "B4_sre" "B5_sre" "B6_bt"  "B7_sre"
clb<-colorRampPalette(c('dark blue','blue','light blue'))(100)
plot(p224r63_2011$B1_sre,col=clb)

# Esercizio: plottare la banda dell'infrarosso vicino con RampPalette che varia dal rosso, all'arancione, al giallo
clnir<-colorRampPalette(c('red','orange','yellow'))(100)
plot(p224r63_2011$B4_sre,col=clnir)

# 4 bande, plot multiframe
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

# dev.off per chiudere
dev.off()

# natural colours
# 3 componenti: R G B
# 3 bande: R = banda del rosso, G = banda del verde, B = banda del blu
# B1: blue - 1
# B2: green - 2
# B3: red - 3
# B4: near infrared (nir) - 4 

# plotrgb: no!
plotRGB(p224r63_2011, r=3, g=2, b=1, stretch="Lin") # stretch="Lin" per allargare l'immagine in modo lineare

# nir
# false colours
plotRGB(p224r63_2011, r=4, g=3, b=2, stretch="Lin")

# salvataggio
pdf("primografico.pdf") 
plotRGB(p224r63_2011, r=4, g=3, b=2, stretch="Lin")
dev.off()

# multiframe
par(mfrow=c(2,1))
plotRGB(p224r63_2011, r=3, g=2, b=1, stretch="Lin")
plotRGB(p224r63_2011, r=4, g=3, b=2, stretch="Lin")
dev.off()

# nir nella compnente R(Red)
plotRGB(p224r63_2011, r=4, g=3, b=2, stretch="Lin")
# ESercizio: nir nella componente G(Green)
plotRGB(p224r63_2011, r=3, g=4, b=2, stretch="Lin")
# Esercizo: nir nella componente B(Blue)
plotRGB(p224r63_2011, r=3, g=2, b=4, stretch="Lin")

# Day2
library(raster)
setwd("C:/lab")
load("TeleRil.RData")
ls()

# Funzione brick per importare un immagine selezionata, poi plot
p224r63_1988<-brick("p224r63_1988_masked.grd")
plot(p224r63_1988)

# plot multiframe
par(mfrow=c(2,2))
# blue
clb<-colorRampPalette(c('dark blue','blue','light blue'))(100)  
plot(p224r63_1988$B1_sre,col=clb)
# green
clg<-colorRampPalette(c('dark green','green','light green'))(100) 
plot(p224r63_1988$B2_sre,col=clg)
# red
clr<-colorRampPalette(c('dark red','red','pink'))(100)  
plot(p224r63_1988$B3_sre,col=clr)
# nir
clnir<-colorRampPalette(c('red','orange','yellow'))(100)
plot(p224r63_1988$B4_sre,col=clnir)

dev.off()

# Nuovo plotRGB
# B1: blue - 1
# B2: green - 2
# B3: red - 3
# B4: near infrared (nir) - 4 
plotRGB(p224r63_1988,r=3,g=2,b=1,stretch="Lin")

# Esercizio: plot usando nir nella componenete R di RGB
plotRGB(p224r63_1988,r=4,g=2,b=1,stretch="Lin")

# plot immagini 1988 e 2011
par(mfrow=c(2,1))
plotRGB(p224r63_1988,r=4,g=2,b=1,stretch="Lin")
plotRGB(p224r63_2011,r=4,g=2,b=1,stretch="Lin")  

dev.off()

# Spectral indices (DVI)
# DVI=nir-red -> es: dvi1988=nir1988-red1988
dvi1988<-p224r63_1988$B4_sre-p224r63_1988$B3_sre
plot(dvi1988)

# Esercizio: dvi2011
dvi2011<-p224r63_2011$B4_sre-p224r63_2011$B3_sre
plot(dvi2011)

# cambio colorRampPalette
cldvi<-colorRampPalette(c('light blue','light green','green'))(100)
plot(dvi2011,col=cldvi)

# Analisi multitemporale (differenza 2011-1988)
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

# Modificare risoluzione (grana) [lr=lowresolution]. fact=n è un moltiplicatore che ci dà dei pixel n volte più grandi dei precedenti
p224r63_2011lr<-aggregate(p224r63_2011,fact=10)
p224r63_2011    # inserrisco in r i due oggetti così da poter vedere le caratteristiche dei pixel
p224r63_2011lr

par(mfrow=c(2,1))
plotRGB(p224r63_2011, r=4, g=3, b=2, stretch="Lin")
plotRGB(p224r63_2011lr, r=4, g=3, b=2, stretch="Lin")

# Lower resolution
p224r63_2011lr50<-aggregate(p224r63_2011,fact=50)
p224r63_2011lr50 # originale era 30m, così 1500m

par(mfrow=c(3,1))
plotRGB(p224r63_2011, r=4, g=3, b=2, stretch="Lin")
plotRGB(p224r63_2011lr, r=4, g=3, b=2, stretch="Lin")
plotRGB(p224r63_2011lr50, r=4, g=3, b=2, stretch="Lin")

# DVI lr502011
dvi2011lr50<-p224r63_2011lr50$B4_sre-p224r63_2011lr50$B3_sre

# DVI lr501988
p224r63_1988lr50<-aggregate(p224r63_1988,fact=50)
dvi1988lr50<-p224r63_1988lr50$B4_sre-p224r63_1988lr50$B3_sre

# difdvi lr50
difdvilr50<-dvi2011lr50-dvi1988lr50
plot(difdvilr50,col=cldifdvi)

# multiframe
par(mfrow=c(2,1))
plot(difdvi,col=cldifdvi)
plot(difdvilr50,col=cldifdvi)





























































