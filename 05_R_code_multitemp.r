# Analisi multitemporale variazione landcover

# setwd e pacchetti
setwd("C:/lab")
library(raster)
library(RStoolbox)
library(ggplot2)

# importare immagini
defor1 <-brick("defor1_.png")
defor2 <-brick("defor2_.png")

# plotRGB "defor1"
defor1                                      # visualizzare i campi dell'oggetto
# "defor1_.1" "defor1_.2" "defor1_.3"
# defor1_.1 = NIR
# defor1_.2 = red
# defor1_.3 = green
plotRGB(defor1,r=1,g=2,b=3,stretch="Lin")   # banda red->nir(r=1), green->red(g=2), blue->green(b=3)

# eserczio: plot seconda data
plotRGB(defor2,r=1,g=2,b=3,stretch="Lin")

# confronto (multiframe) stessa area in momenti differenti (prima e dopo deforestazione)
par(mfrow = c(2,1))
plotRGB(defor1,r=1,g=2,b=3,stretch="Lin")
plotRGB(defor2,r=1,g=2,b=3,stretch="Lin")

dev.off()

# classificazione non supervisionata
d1c<-unsuperClass(defor1,nClasses=2)             # si creano classi di foresta e non foresta (suddivisione pixel in queste due categorie)
plot(d1c$map)
cl<-colorRampPalette(c('green','black'))(100)
plot(d1c$map,col=cl)

# esercizio: come prima per defor2
d2c<-unsuperClass(defor2,nClasses=2)              
plot(d2c$map,col=cl)

dev.off()

# confronto tra i due momenti (multiframe) con pixel classificati
# due righe, una colonna
par(mfrow=c(2,1))
plot(d1c$map,col=cl)
plot(d2c$map,col=cl)
# due colonne, una riga
par(mfrow=c(1,2))
plot(d1c$map,col=cl)
plot(d2c$map,col=cl)

dev.off()

# calcolo frequenza delle due classi di pixel nella prima immagine
freq(d1c$map)
# aree aperte=37039
# foresta=304253

# numero di pixel totali nella prima immagine (necessario per calcolo percentuale)
totd1<-37039+304253       
totd1
# totd1=341292

# calcolo frequenze percentuali
percent1<-freq(d1c$map)*100/totd1
# foreste: 89.1 %
# aree aperte: 10.9 %

# stesso procedimento per la seconda immagine
freq(d2c$map)
# aree aperte=165055
# foreste=177671

totd2<-165055+177671
totd2
# 342726

percent2<-freq(d2c$map)*100/totd2
# aree aperte: 48.2 %
# foreste: 51.8 %

# creare vettori per analisi grafica
cover<-c("Agriculture","Forest")
before<-c(10.9,89.1)
after<-c(48.2,51.8)

# creare nuovo dataset con i dati ottenuti
output<-data.frame(cover,before,after)
output

# Day2

setwd("C:/lab")
load("C:/lab/defor.RData")
library(raster)
library(ggplot2)
install.packages("gridExtra")
library(gridExtra)

# riprendere mappe dell'altra volta
par(mfrow=c(1,2))
cl<-colorRampPalette(c('black','green'))(100)
plot(d1c$map,col=cl)
plot(d2c$map,col=cl)
ls()
output
#        cover before after
# 1 Agriculture   10.9  48.2
# 2      Forest   89.1  51.8

# istogramma delle percentuali di copertura prima della deforestazione
grafico1<-ggplot(output,aes(x=cover,y=before,color=cover)) +
geom_bar(stat="identity",fill="white")
grafico1                                                        # in ascissa "aes"(aree di foresta/aperte), in ordinata percentuale di copertura

# esercizio: istogramma dopo deforestazione
grafico2<-ggplot(output,aes(x=cover,y=after,color=cover)) +
geom_bar(stat="identity",fill="white")
grafico2

# esercizio: usare "grid.arrange" (funzione del pacchetto "gridExtra" che permette confronto tra istogrammi) per creare un plot con grafico1 e grafico2
# grid.arrange=> grid.arrange(plot1,plot2,nrow=1), questa funzione crea un plot con più grafici (equivalente di par)
grid.arrange(grafico1,grafico2,nrow=1)
# evidente cambiamento nelle percentuali di copertura

# per facilitare confronto uniformare sacla dei due istogrammi (imporre al grafico il limite y=100)
grafico1<-ggplot(output,aes(x=cover,y=before,color=cover)) +
geom_bar(stat="identity",fill="white") + 
ylim(0,100)
grafico2 <- ggplot(output, aes(x=cover, y=after, color=cover)) + 
geom_bar(stat="identity", fill="white") +
ylim(0, 100)
grid.arrange(grafico1,grafico2,nrow=1)

