# R_code_exam.r 

# Copernicus data: 


# 1. R_code_first.r	        #da fare
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

# 1. R_code_first.r - Primo codice R EcoPae

# GZ libraries
install.packages("sp") # GZ  comando per scaricare libraries che posso poi richiamare con comando "library()" [si può anche usare "require()]
library(sp)            # GZ  caricare library precedentemente installata 

data("meuse")  # GZ  richiamo dataset "meuse" (dati su presenza metalli pesanti nel terreno), inserito nella libreria "sp"
meuse          # GZ  per visualizzare dati  
head(meuse)    # GZ  prime 6 righe del database 
names(meuse)   # GZ  nomi variabili (colonne del dataset)
summary(meuse) # GZ  riporta statistiche di base per le variabili del dataset


pairs(meuse)                                   # GZ  grafici a coppie fra tutte le variabili
pairs(~ cadmium + copper + lead, data = meuse) # GZ  grafici a coppie fra le tre variabili selezionate


pairs(meuse[, 3:6])          # GZ  anzichè scrivere varie colonne prendo subset del database meuse
pairs(meuse[, 3:6],
      col = "orange",        # GZ  colore simboli
      pch=19,                # GZ  pch="point character" -> tipo di simbolo
      cex = 2,               # GZ  cex="character exageration" -> grandezza del simbolo (di base cex=1)
      main = "Primo pairs")  # GZ titolo del grafico


panel.correlations <- function(x, y, digits=1, prefix="", cex.cor)
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
# GZ  funzione per calcolare correlazione fra due variabili


panel.smoothing <- function (x, y, col = par("col"), bg = NA, pch = par("pch"),
                             cex = 1, col.smooth = "red", span = 2/3, iter = 3, ...)
{
  points(x, y, pch = pch, col = col, bg = bg, cex = cex)
  ok <- is.finite(x) & is.finite(y)
  if (any(ok))
    lines(stats::lowess(x[ok], y[ok], f = span, iter = iter),
          col = 1, ...)
}
# FS  smoothing fa una sorta di regressione fra due variabili



panel.histograms <- function(x, ...)
{
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(usr[1:2], 0, 1.5) )
  h <- hist(x, plot = FALSE)
  breaks <- h$breaks; nB <- length(breaks)
  y <- h$counts; y <- y/max(y)
  rect(breaks[-nB], 0, breaks[-1], y, col="white", ...)
}
# FS  funzione per creare istogramma di una variabile


# FS  grafici a coppie fra le quattro variabili selezionate, in cui vengono mostrati anche coefficiente di correlazione
# FS  fra le variabili e istogramma delle singole variabili, utilizzando le funzioni precedentemente create
pairs(meuse[, 3:6], lower.panel = panel.correlations, upper.panel = panel.smoothing, diag.panel = panel.histograms)
# FS  lower.panel è la parte sotto la diagonale
# FS  upper.panel è la parte sopra la diagonale
# FS  diag.panel è la diagonale

pairs(meuse[, 3:6], lower.panel = panel.smoothing, upper.panel = panel.correlations, diag.panel = panel.histograms)
# FS  correlazione e interpolazione invertite di posto rispetto alla diagonale

# FS  plot fra due variabili
plot(meuse$cadmium, meuse$copper)
attach(meuse) # FS  permette di richimare i campi dell'oggetto 'meuse' senza dover richiamare l'oggetto stesso (non serve più 'meuse$')
plot(cadmium, copper)
plot(cadmium, copper, pch=17, col = "green", main = "Primo plot", xlab = "Cadmio", ylab = "Rame")


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

