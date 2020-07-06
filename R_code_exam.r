# R_code_exam.r 

# Copernicus data: 


# 1. R_code_first.r	        
# 2. R_code_spatial.r	
# 3. R_code_spatial2.r    #accorpato a spatial
# 4. R_code_point_patterns.r
# 5. R_code_TeleRil.r	
# 6. R_code_Landcover.r	
# 7. R_code_multitemp.r	
# 8. R_code_multitemp_NO2.r	
# 9. R_code_snow.r	            #da far
# 10. R_code_patches.r          # da far

#############################################################################
#############################################################################

# 1. R_code_first.r - Primo codice R Ecologia del Paesaggio

# libraries: "install.packages()" per scaricare libraries che posso poi richiamare con comando "library()" [o "require()]
install.packages("sp")  
library(sp)             

# dataset e funzioni associate
data("meuse")  # richiamo dataset "meuse" (dati su presenza metalli pesanti nel terreno), inserito nella libreria "sp"
meuse          # visualizzare dati  
head(meuse)    # prime 6 righe del dataset 
names(meuse)   # nomi variabili (colonne del dataset)
summary(meuse) # riporta statistiche di base per le variabili del dataset

# grafici: "pairs()" per creare grafici a coppie tra variabili di un dataset
pairs(meuse)                                    # grafici a coppie tra tutte le variabili
pairs(~cadmium + copper + lead, data = meuse)   # grafici a coppie tra le variabili indicate

# esercizio: pairs() quattro variabili [cadmium, copper, lead, zinc]
pairs(~cadmium+copper+lead+zinc,data=meuse)

# [,x:y] per selezionare subset composto da righe selezionate (3, 4, 5, 6 -> cadmium, copper, lead, zinc) 
pairs(meuse[,3:6])

# visualizzazione: scelgo colori["col="], simboli["pch="] e dimensioni["cex="] => per simboli pch=n con 1<n<25 (ad ogni numero un diverso simbolo)
pairs(meuse[,3:6],col="blue",pch=18,cex=3)

# "main=" per dare titolo al grafico
pairs(meuse[,3:6],col="blue",pch=18,cex=3,main="Primo pairs")

# prendere funzioni esterne => "panel.correlations" indica coefficiente di correlazione tra variabili
panel.correlations<-function(x,y,digits=1,prefix="",cex.cor)
{
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  r1=cor(x,y,use="pairwise.complete.obs")
  r <- abs(cor(x, y,use="pairwise.complete.obs"))
  txt <- format(c(r1, 0.123456789), digits=digits)[1]
  txt <- paste(prefix, txt, sep="")
  if(missing(cex.cor)) cex <- 0.9/strwidth(txt)
  text(0.5, 0.5, txt, cex = cex * r)
}

# "panel.smoothing" -> fa una specie di regressione tra variabili
panel.smoothing <- function (x, y, col = par("col"), bg = NA, pch = par("pch"),
                             cex = 1, col.smooth = "red", span = 2/3, iter = 3, ...)
{
  points(x, y, pch = pch, col = col, bg = bg, cex = cex)
  ok <- is.finite(x) & is.finite(y)
  if (any(ok))
    lines(stats::lowess(x[ok], y[ok], f = span, iter = iter),
          col = 1, ...)
}

# "panel.histograms" -> crea istogramma di una variabile
panel.histograms <- function(x, ...)
{
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(usr[1:2], 0, 1.5) )
  h <- hist(x, plot = FALSE)
  breaks <- h$breaks; nB <- length(breaks)
  y <- h$counts; y <- y/max(y)
  rect(breaks[-nB], 0, breaks[-1], y, col="white", ...)
}

# uso funzioni precedentemente create per costruire grafici a coppie fra le quattro variabili selezionate, in cui vengono mostrati anche coefficienti di correlazione tra le variabili 
# lower.panel -> parte sopra la diagonale
# upper.panel -> parte sotto la diagonale
# diag.panel  -> diagonale
pairs(meuse[,3:6],lower.panel=panel.correlations,upper.panel=panel.smoothing,diag.panel=panel.histograms)

# esercizio: invertire posto rispetto alla diagonale di correlazione e interpolazione
pairs(meuse[,3:6],lower.panel=panel.smoothing,upper.panel=panel.correlations,diag.panel=panel.histograms)

#######################################################
#######################################################

# R spaziale: funzioni sapziali in Ecologia del Paesaggio [24/03/2020]

# GZ Caricare library e dati
library(sp)
data(meuse)
head(meuse)

# GZ allegare dataframe -> attach()
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

### Spatial-2 [25/03/2020]
# installare sp e caricare dati meuse
install.packages("sp")
library(sp)
data(meuse)

# coordinate del dataframe => coordinates(dataset)=~(coordinata,coordinata)
coordinates(meuse)=~x+y

# spplot dati zinco
spplot(meuse,"zinc")

# esercizio: spplot dati rame
spplot(meuse,"copper")

# bubble => altro metodo per plottare i dati, per es usiamo zinco
bubble(meuse,"zinc")

# esercizio: bubble del rame, colorato di rosso
bubble(meuse,"copper",col="red")

# esempio: foraminiferi, carbon capture =>per creare due oggetti
# creiamo un vettore che contenga i dati di campionamento dei foraminiferi e lo chiamiamo foram [<- per dare nome al vettore c]
foram<-c(10,20,35,55,67,80)
# idem per carbon stock
carbon<-c(5,15,30,70,85,99)

#plottiamo i dati
plot(foram,carbon,col="green",pch=19)

### prendere dati dall'esterno (dati covid19agg.csv)
# settare la cartella di lavoro [wd("percorso")]
setwd("C:/lab")

# leggere tabella; head=T per indicare a r che ci sono i titoli delle colonne e dare alla tabella il nome covid19
Covid19<-read.table("covid_agg.csv",head=T)

