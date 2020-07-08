# R_code_exam.r 

# Copernicus data: 


# 1. 01_R_code_first.r	        
# 2. 02_R_code_spatial.r	
# 3. R_code_spatial2.r    #accorpato a spatial
# 4. 03_R_code_point_patterns.r
# 5. 04_R_code_TeleRil.r	
# 6. 05_R_code_Landcover.r	
# 7. R_code_multitemp.r	
# 8. R_code_multitemp_NO2.r	
# 9. R_code_snow.r	            #da far
# 10. R_code_patches.r          # da far

#############################################################################
#############################################################################

# 1. R_code_first.r - Primo codice R Ecologia del Paesaggio

# GZ libraries: "install.packages()" per scaricare libraries che posso poi richiamare con comando "library()" [o "require()]
install.packages("sp")  
library(sp)             

# GZ dataset e funzioni associate
data("meuse")  # GZ richiamo dataset "meuse" (dati su presenza metalli pesanti nel terreno), inserito nella libreria "sp"
meuse          # GZ visualizzare dati  
head(meuse)    # GZ prime 6 righe del dataset 
names(meuse)   # GZ nomi variabili (colonne del dataset)
summary(meuse) # GZ riporta statistiche di base per le variabili del dataset

# GZ grafici: "pairs()" per creare grafici a coppie tra variabili di un dataset
pairs(meuse)                                    # GZ grafici a coppie tra tutte le variabili
pairs(~cadmium + copper + lead, data = meuse)   # GZ grafici a coppie tra le variabili indicate

# GZ esercizio: pairs() quattro variabili [cadmium, copper, lead, zinc]
pairs(~cadmium+copper+lead+zinc,data=meuse)

# GZ [,x:y] per selezionare subset composto da righe selezionate (3, 4, 5, 6 -> cadmium, copper, lead, zinc) 
pairs(meuse[,3:6])

# GZ visualizzazione: scelgo colori["col="], simboli["pch="] e dimensioni["cex="] => per simboli "pch=n" con 1<n<25 (ad ogni numero un diverso simbolo)
pairs(meuse[,3:6],col="blue",pch=18,cex=3)

# GZ "main=" per dare titolo al grafico
pairs(meuse[,3:6],col="blue",pch=18,cex=3,main="Primo pairs")

# GZ prendere funzioni esterne => "panel.correlations" indica coefficiente di correlazione tra variabili
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

# GZ "panel.smoothing" -> fa una specie di regressione tra variabili
panel.smoothing <- function (x, y, col = par("col"), bg = NA, pch = par("pch"),
                             cex = 1, col.smooth = "red", span = 2/3, iter = 3, ...)
{
  points(x, y, pch = pch, col = col, bg = bg, cex = cex)
  ok <- is.finite(x) & is.finite(y)
  if (any(ok))
    lines(stats::lowess(x[ok], y[ok], f = span, iter = iter),
          col = 1, ...)
}

# GZ "panel.histograms" -> crea istogramma di una variabile
panel.histograms <- function(x, ...)
{
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(usr[1:2], 0, 1.5) )
  h <- hist(x, plot = FALSE)
  breaks <- h$breaks; nB <- length(breaks)
  y <- h$counts; y <- y/max(y)
  rect(breaks[-nB], 0, breaks[-1], y, col="white", ...)
}

# GZ uso funzioni precedentemente create per costruire grafici a coppie fra le quattro variabili selezionate, in cui vengono mostrati anche coefficienti di correlazione tra le variabili 
# GZ lower.panel -> parte sopra la diagonale
# GZ upper.panel -> parte sotto la diagonale
# GZ diag.panel  -> diagonale
pairs(meuse[,3:6],lower.panel=panel.correlations,upper.panel=panel.smoothing,diag.panel=panel.histograms)

# GZ esercizio: invertire posto rispetto alla diagonale di correlazione e interpolazione
pairs(meuse[,3:6],lower.panel=panel.smoothing,upper.panel=panel.correlations,diag.panel=panel.histograms)

#######################################################
#######################################################

# 2. R spaziale: funzioni sapziali in Ecologia del Paesaggio [24/03/2020]

# GZ caricare library e dati
library(sp)
data(meuse)
head(meuse)

# GZ fissare dataframe -> attach()
attach(meuse)

# GZ plot cadmium e lead segliendo colori["col"], caratteri["pch"] e dimensioni["cex"]
plot(cadmium,lead,col="red",pch=19,cex=1)

# GZ esercizio: plot copper e zinc con carattere triangolo(17) e colore verde
plot(copper,zinc,col="green",pch=17)

# GZ cambiare etichette relative ad assi del grafico => "xlab","ylab"
plot(copper,zinc,col="green",pch=17,xlab="rame",ylab="zinco")

# GZ multiframe o multipanel => "par(mfrow=c(numero righe,numero colonne))"; a capo i plot che si vogliono mettere in una sola finestra
par(mfrow=c(1,2))  # GZ "par(mfrow)" -> funzione per gestire aspetto dei grafici (creare diagramma a più riquadri); (1,2) indica una riga e due colonne
plot(cadmium,lead,col="red",pch=19,cex=1)                      
plot(copper,zinc,col="green",pch=17,xlab="rame",ylab="zinco")  

# GZ invertire grafici riga/colonna [(2,1) anzichè (1,2)]
par(mfrow=c(2,1))
plot(cadmium,lead,col="red",pch=19,cex=1)
plot(copper,zinc,col="green",pch=17,xlab="rame",ylab="zinco")

# GZ multiframe automatico -> library "GGally"
install.packages("GGally")
library(GGally)
ggpairs(meuse[,3:6])  # GZ "ggpairs" crea matrice di grafici con un determinato set di dati (in questo caso dalla terza alla sesta colonna del dataset "meuse")

# GZ Spatial; "coordinates()" per indicare che i dati hanno coordinate (in meuse x e y => facendo ~x+y)
head(meuse)
gpairs
coordinates(meuse)=x+y
plot(meuse)

# GZ "spplot()" -> distribuzione spaziale di una variabile (in questo caso "zinc")
spplot(meuse,"zinc")

### Spatial-2 [25/03/2020]

# GZ installare library "sp", caricare dati "meuse" e fissare dataset ["attach()"]
install.packages("sp")
library(sp)
data(meuse)
attach(meuse)

# GZ specificare coordinate del dataset => "coordinates(dataset)=~(coordinata,coordinata)"
coordinates(meuse)=~x+y

# GZ "spplot" dati zinco
spplot(meuse,"zinc")

# GZ esercizio: "spplot" dati rame
spplot(meuse,"copper")

# GZ "bubble(dataset,"variabile")" => rappresentazione spaziale come "spplot", crea un grafico a bolle di grandezza proporzionale a valore variabile
bubble(meuse,"zinc")

# GZ esercizio: bubble rame, colore rosso
bubble(meuse,"copper",col="red")

# GZ esempio: foraminiferi, carbon capture 
# GZ creare vettore che contenga dati di campionamento dei foraminiferi chiamandolo "foram" ["<-" per dare nome al vettore c]
foram<-c(10,20,35,55,67,80)
# GZ "carbon" per carbon stock
carbon<-c(5,15,30,70,85,99)

# GZ plot con questi vettori
plot(foram,carbon,col="green",pch=19)

# GZ prendere dati dall'esterno (dati "covid19agg.csv")
# GZ settare cartella di lavoro -> wd("percorso") [in questo caso dico C, cartella lab]
setwd("C:/lab")

# GZ leggere tabella e usarla per costuire un dataframe; head=T per indicare a R che ci sono titoli delle colonne (prima riga è una stringa di testo)
Covid19<-read.table("covid_agg.csv",head=T)  # GZ intitolare tabella "Covid19"

#######################################################
#######################################################



