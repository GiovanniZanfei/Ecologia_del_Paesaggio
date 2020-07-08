# R_code_exam.r 

# Copernicus data: 


# 1. 01_R_code_first.r	        
# 2. 02_R_code_spatial.r	
# 3. 03_R_code_point_patterns.r
# 4. 04_R_code_TeleRil.r	
# 5. 05_R_code_Landcover.r	
# 6. R_code_multitemp.r	
# 7. R_code_multitemp_NO2.r	
# 8. R_code_snow.r	            #da far
# 9. R_code_patches.r          # da far

#############################################################################
#############################################################################

# 1. 01_R_code_first.r - Primo codice R Ecologia del Paesaggio

# GZ pacchetti: "install.packages()" -> scaricare pacchetti (poi richiamabili con comando "library()" [o "require()])
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

# 2. 02_R_code_spatial.r - Funzioni sapziali in Ecologia del Paesaggio [24/03/2020]

# GZ caricare pacchetti e dati
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

# GZ multiframe automatico -> pacchetto "GGally"
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

# Spatial-2 [25/03/2020]

# GZ installare pacchetto "sp", caricare dati "meuse" e fissare dataset ["attach()"]
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

# 03_R_code_point_patterns.r - Analisi point patterns [25/03/2020]

# GZ installare e richiamare pacchetti ("ggplot2", "spatstat")
intall.packages("ggplot2")
library(ggplot2)
install.packages("spatstat)
library(spatstat)

# GZ set working directory
setwd("C:/lab")

# GZ importare tabella dati Covid19; "head=T" per indicare a R che ci sono i titoli delle colonne; dare alla tabella il nome Covid19
Covid19<-read.table("covid_agg.csv",head=T)
head(Covid19)   # comando per vedere tabella

# GZ creare plot che associa Paesi e casi di Covid19 (anzichè "$" si può fare "attach(Covid19) plot(country,cases)")
plot(Covid19$country,Covid19$cases)

# GZ modificare struttura plot -> posizione etichette rispetto ad asse ("las=0" etichette parallele, 1 orizzontali, 2 perpendicolari, 3 verticali)
plot(Covid19$country,Covid19$cases,las=0)
plot(Covid19$country,Covid19$cases,las=1)
plot(Covid19$country,Covid19$cases,las=2)
plot(Covid19$country,Covid19$cases,las=3)
plot(Covid19$country,Covid19$cases,las=3,cex.axis=0.5)  # GZ "cex.axis" -> rimpicciolire dimensione etichette

# GZ richiamare "ggplot2" (pacchetto per estetica e cura dei dettagli)
library(ggplot2)

# GZ dataframe "mpg" da pacchetto "ggplot2" ("mpg" raccoglie osservazioni US Environmental Protection Agency su 38 modelli di auto)
data(mpg)
head(mpg)

# GZ esempio di plot con 2 variabili numeriche -> ggplot2 ha bisogno di 3 cose: dati ("mpg"), estetica del grafico ("aes", funzione di quotazione) e geometria ("geom_")
ggplot(mpg,aes(x=displ,y=hwy))+geom_point()
ggplot(mpg,aes(x=displ,y=hwy))+geom_line()    # GZ usare linee anzichè punti nella visualizzazione
ggplot(mpg,aes(x=displ,y=hwy))+geom_polygon() # GZ usare poligoni 

# GZ "ggplot2" per dati Covid19 -> usare longitudine e latitudine per avere i punti nello spazio, "size=cases" -> punti più grandi dove ci sono più casi
ggplot(Covid19,aes(x=lon,y=lat,size=cases))+geom_point() 

# GZ richiamare pacchetto "spatstat" (mostra analisi dei modelli dei punti spaziali) e fissare dataframe
library(spatstat)
attach(covid)

# GZ esercizio: zona con più alta densità casi di Covid19
# GZ creare dataset per spatstat -> "ppp" crea un oggetto che rappresenta un insieme di dati del pattern puntiforme nel piano bidimensionale
covids<-ppp(lon,lat,c(-180,180),c(-90,90))  # GZ necessario specificare cosa indicano x e y ("lon","lat") e definirne il range
d<-density(covids)                          # GZ comando per calcolare densità dei casi
plot(d)                                     # GZ plot (rappresentazione grafica) densità
points(covids,pch=19)                       # GZ mostare i punti Covid19 sulla mappa di densità

# point patterns-2 [01/04/20]

# GZ settare wd, caricare file salvato, richiamare "spatstat" e mostrare grafico densità casi Covid
setwd("C:/lab")
load("point_pattern.RData")
ls()   # per vedere contenuto del file caricato
library(spatstat)
plot(d)

# GZ palette -> modificare colori del plot d; (100) per dire a R quante sfumature deve avere la scala di colori
cl<-colorRampPalette(c('yellow','orange','red')) (100)
plot(d,col=cl)                                               # GZ plot densità con nuovi colori
# esercizio: plot densità dal verde al blu
bluverde<-colorRampPalette(c('blue','grey','green')) (200)
plot(d,col=bluverde)

# GZ mostare punti Covid19 sulla mappa di densità
points(covids)

# GZ inserire nella mappa confini degli stati
install.packages("rgdal")                   # GZ "rgdal" -> pacchetto necessario per usare il comando "readOGR"
library(rgdal)
coastlines<-readOGR("ne_10m_coastline.shp") # GZ "readOGR" -> funzione che legge origine dati OGR e un layer in un oggetto vettoriale spaziale adatto, serve per creare layer dei confini
plot(coastlines,add=T)                      # GZ "add=T" per aggiungere confini al vecchio plot senza eliminarlo -> grafico completo

# GZ esercizio: plot della mappa di densità con nuova colorazione e aggiunta coastlines
clr<-colorRampPalette(c('light blue','blue','pink','purple')) (400)
plot(d,col=clr)
plot(coastlines,add=T)

setwd("C:/lab")
load("C:/lab/point_ppattern.RData")
library(spatstat)
ls()
library(rgdal)

# GZ esercizio: plot mappa di densità con nuova palette
coastlines<-readOGR("ne_10m_coastline.shp")
clr<-colorRampPalette(c('light blue','blue','pink','purple')) (400)
plot(d,col=clr,main="density")
plot(coastlines,add=T)
points(covids)

# GZ Interpolation
covid
marks(covids)<-covid$cases  # "marks" -> associare dati categoria "cases" al pointpattern "covids"
s<-Smooth(covids)           # "Smooth" -> creare mappa con i dati appena costruiti
plot(s)                     # plot mappa appena creata

# GZ esercizio: plot(s) con coastlines e punti
cls<-colorRampPalette(c('light blue','blue','green'))(100)
plot(s,col=cls,main="Cases")
points(covids)
plot(coastlines,add=T)

# GZ mappa finale (multiframe con entrambi i plot fatti)
par(mfrow=c(2,1))

# GZ primo plot per densità
clr<-colorRampPalette(c('light blue','blue','pink','purple')) (400)
plot(d,col=clr,main="density")
plot(coastlines,add=T)
points(covids)

# GZ secondo plot per interpolazione numero di casi
cls<-colorRampPalette(c('light blue','blue','green'))(100)
plot(s,col=cls,main="Cases")
points(covids)
plot(coastlines,add=T)

# San Marino (lavorare con set di dati di una tesi su San Marino scaricati in "lab" )
setwd("C:/lab")
load("C:/lab/Tesi.RData")
ls()
head(Tesi)
library(spatstat)
attach(Tesi)

# GZ Point pattern: x,y,c(xmin,xmax),c(ymin,ymax)
summary(Tesi)  # sommario del dataset, posso trovare rapidamente le info principali
# GZ "summary" -> longitudine: 12.42<x<12.46 e latitudine: 43.91<y<43.94
Tesippp<-ppp(Longitude,Latitude,c(12.41,12.47),c(43.90,43.95))

# GZ Mappa densità
dT<-density(Tesippp)
dev.off 
plot(dT)
points(Tesippp,col="green")

# GZ set wd e richiamo pacchetti
setwd("C:/lab")
load("C:/lab/Tesi.RData")
library(spatstat)
library(rgdal)

# GZ dt=density map, Tesi=dataset, Tesippp=point pattern (coordinate longitudine e latitudine)

head(Tesi)

# GZ associare al point pattern il valore d'interesse (ricchezza di specie) e poi procedere con l'interpolazione
marks(Tesippp)<-Tesi$Species_richness
interpol<-Smooth(Tesippp)
plot(interpol)                         # GZ mappa
points(Tesippp)

# GZ caricare il file vettoriale "San_Marino" e sovrapponiamo la mappa costruita prima (così da avere i confini)
sanmarino<-readOGR("San_Marino.shp")
plot(sanmarino)
plot(interpol,add=T)                  # GZ "add=T" per indicare che mappa di interpolazione sovrapposta a mappa di San Marino
points(Tesippp)
plot(sanmarino,add=T)                 # GZ -> vedere nuovamente confini

# GZ esercizio: plot multiframe densità e interpolazione (due righe, una colonna)
par(mfrow=c(2,1))
plot(dT,main="Density of points")
points(Tesippp)
plot(interpol,main="Estimate of species richness")
points(Tesippp)

# GZ esercizio: come prima ma due colonne e una riga
par(mfrow=c(1,2))
plot(dT,main="Density of points")
points(Tesippp)
plot(interpol,main="Estimate of species richness")
points(Tesippp)

#######################################################
#######################################################
