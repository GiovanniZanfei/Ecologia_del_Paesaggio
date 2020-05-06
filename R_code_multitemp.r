# Analisi multitemporale variazione landcover

setwd("C:/lab")
library(raster)
library(RStoolbox)
library(ggplot2)

# Importare immagini
defor1 <-brick("defor1_.png")
defor2 <-brick("defor2_.png")

# defor1, plot
defor1      # per visualizzare i campi dell'oggetto
# "defor1_.1" "defor1_.2" "defor1_.3"
# defor1_.1 = NIR
# defor1_.2 = red
# defor1_.3 = green
plotRGB(defor1,r=1,g=2,b=3,stretch="Lin")

# Eserczio: plot seconda data
plotRGB(defor2,r=1,g=2,b=3,stretch="Lin")

# Confronto della stessa area in momenti differenti (multiframe)
par(mfrow = c(2,1))
plotRGB(defor1,r=1,g=2,b=3,stretch="Lin")
plotRGB(defor2,r=1,g=2,b=3,stretch="Lin")

# Classificazione non supervisionata
d1c <-unsuperClass(defor1,nClasses=2)     # si creano classi di foresta e non foresta
plot(d1c$map)
cl <-colorRampPalette(c('green','black'))(100)
plot(d1c$map,col=cl)
d2c<-unsuperClass(defor2,nClasses=2)      # classificazione dei pixel in due gruppi in modo analogo al precedente
plot(d2c$map)

# Esempio sul significato del $
# mappageologica <-geomap(im_sat,nClasses=....)
# plot(mappageologica$lito)
# plot(mappageologica$lineaments)
# quando un oggetto contiene degli elementi suddivisi in sottocartelle si utilizza il '$' per richiamarli

# Visualizzazione contemporanea dei due periodi con pixel classificati
dev.off()
par(mfrow=c(2,1))
plot(d1c$map,col=cl)
plot(d2c$map,col=cl)

par(mfrow=c(1,2))
plot(d1c$map,col=cl)
plot(d2c$map,col=cl)

# Calcolo frequenza delle due classi di pixel nella prima immagine
freq(d1c$map)
# aree aperte=37039
# foresta=304253

# numero di pixel totali nella prima immagine
totd1<-37039+304253       # questo calcolo poteva anche essere fatto fare automaticamente al software
totd1
# 341292

# Calcolo frequenze percentuali
percent1<-freq(d1c$map)*100/totd1
# foreste: 89.1 %
# aree aperte: 10.9 %

# Calcolo analogo al precendente per le due classi della seconda immagine
freq(d2c$map)
# aree aperte=165055
# foreste=177671

totd2<-165055+177671
totd2
# 342726

percent2<-freq(d2c$map)*100/totd2
# aree aperte: 48.2 %
# foreste: 51.8 %

# Creazione vettori per analisi grafica
cover<-c("Agriculture","Forest")
before<-c(10.9,89.1)
after<-c(48.2,51.8)

# Creazione dataframe con i vettori precedentemente creati
output<-data.frame(cover,before,after)
output

# 5/05
setwd("C:/lab")
load("C:/lab/defor.RData")
library(raster)
library(ggplot2)
install.packages("gridExtra")
library(gridExtra)
ls()

# Riprendere mappe dell'altra volta
par(mfrow=c(1,2))
cl<-colorRampPalette(c('black','green'))(100)
plot(d1c$map,col=cl)
plot(d2c$map,col=cl)

ls()
output
#        cover before after
# 1 Agriculture   10.9  48.2
# 2      Forest   89.1  51.8

# Istogramma delle percentuali di copertura prima della deforestazione
grafico1<-ggplot(output,aes(x=cover,y=before,color=cover)) +
geom_bar(stat="identity",fill="white")

# Esercizio: istogramma dopo deforestazione
grafico2<-ggplot(output,aes(x=cover,y=after,color=cover)) +
geom_bar(stat="identity",fill="white")

# Esercizio: usare grid.arrange per creare un plot con grafico1 e grafico2
#  Es: grid.arrange=> grid.arrange(plot1,plot2,nrow=1), questa funzione crea un plot con più grafici (equivalente di par)
grid.arrange(grafico1,grafico2,nrow=1)

# 6/05

library(ggplot2)
library(gridExtra)

# Riprendere quanto fatto l'ultima volta
grafico1<-ggplot(output,aes(x=cover,y=before,color=cover)) +
geom_bar(stat="identity",fill="white")
grafico2<-ggplot(output,aes(x=cover,y=after,color=cover)) +
geom_bar(stat="identity",fill="white")
grid.arrange(grafico1,grafico2,nrow=1)

# Impongo al grafico il limite a y=100
grafico1<-ggplot(output,aes(x=cover,y=before,color=cover)) +
geom_bar(stat="identity",fill="white") + 
ylim(0,100)
grafico2 <- ggplot(output, aes(x=cover, y=after, color=cover)) + 
geom_bar(stat="identity", fill="white") +
ylim(0, 100)
grid.arrange(grafico1,grafico2,nrow=1)






















































