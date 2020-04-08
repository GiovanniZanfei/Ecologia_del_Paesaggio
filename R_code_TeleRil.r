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

























