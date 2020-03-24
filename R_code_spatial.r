# R spaziale: funzioni sapziali in Ecologia del Paesaggio [24/03/2020]

# con install() installiamo un pacchetto in R
install.packages("sp")

# con library() richiamo il pacchetto
library(sp)

# dati
data(meuse)

# head() per visualizzare le prime sei righe di una tabella
head(meuse)

# alleghiamo il dataframe => attach()
attach(meuse)

# plot cadmium e lead segliendo colori[col], caratteri[pch] e dimensioni[cex]
plot(cadmium,lead,col="red",pch=19,cex=1)

# esercizio: plot di copper e zinco con carattere triangolo e colore verde
plot(copper,zinc,col="green",pch=17)

# cambiare etichette => xlab,ylab
plot(copper,zinc,col="green",pch=17,xlab="rame",ylab="zinco")

# multiframe o multipanel => par(mfrow=c(numero righe,numero colonne)) ; a capo i plot che si vogliono mettere in una sola finestra
par(mfrow=c(1,2))
plot(cadmium,lead,col="red",pch=19,cex=1)
plot(copper,zinc,col="green",pch=17,xlab="rame",ylab="zinco")

#invertiamo grafici riga/colonna
par(mfrow=c(2,1))
plot(cadmium,lead,col="red",pch=19,cex=1)
plot(copper,zinc,col="green",pch=17,xlab="rame",ylab="zinco")

# multiframe automatico [pacchetto GGally]
install.packages("GGally")
library(GGally)
ggpairs(meuse[,3:6])

# Spatial ; coordinates devo indicare le coordinate del dataset [in meuse x e y] => facendo ~x+y
head(meuse)
gpairs
coordinates(meuse)=~x+y
plot(meuse)

# spplot () servve per plottare i dati spazialmente
spplot(meuse,"zinc")
