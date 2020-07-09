# R_code_exam.r 

# Copernicus data: 


# 1. 01_R_code_first.r	        
# 2. 02_R_code_spatial.r	
# 3. 03_R_code_point_patterns.r
# 4. 04_R_code_TeleRil.r		
# 5. 05_R_code_multitemp.r	
# 6. 06_R_code_multitemp_NO2.r	
# 7. R_code_snow.r	            #da far
# 8. R_code_patches.r          # da far

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
ggplot(mpg,aes(x=displ,y=hwy))+geom_line()    # GZ linee anzichè punti nella visualizzazione
ggplot(mpg,aes(x=displ,y=hwy))+geom_polygon() # GZ poligoni 

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

### GZ INTERPOLATION

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

# GZ primo plot: densità
clr<-colorRampPalette(c('light blue','blue','pink','purple')) (400)
plot(d,col=clr,main="density")
plot(coastlines,add=T)
points(covids)

# GZ secondo plot: interpolazione numero di casi
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

4. 04_R_code_TeleRil.r - codice R per analisi satellitari (telerilevamento)

#  GZ set wd e pacchetti ("raster","RStoolbox")  
setwd("C:/lab")
install.packages("raster")       # "raster" per lettura, scrittura, analisi e modellizzazione di dati spaziali
library(raster)
install.packages("RStoolbox")    # "RStoolbox" per analisi dati mediante telerilevamento

# GZ funzione "brick" per importare immagine selezionata e creare ogetto "RasterBrick" (multistrato)
p224r63_2011<-brick("p224r63_2011_masked.grd")

# GZ plot oggetto appena creato
plot(p224r63_2011) # 7 riquadri che mostrano un'immagine basata su riflettanza a varie lunghezze d'onda, come indicato sotto
# B1: blue, B2: green, B3: red, B4: near infrared (nir), B5: medium infrared, B6: thermal infrared, B7: medium infrared

# GZ RampPalette ("cl") per avere immagini con scala di colori da bianco a nero una volta rifatto il comando plot con specifica del colore
cl<-colorRampPalette(c('black','grey','light grey'))(100) 
plot(p224r63_2011,col=cl)

# GZ modifica scala cromatica (da 100 a 5 sfumature)
cllow<-colorRampPalette(c('black','grey','light grey'))(5)
plot(p224r63_2011,col=cllow)

# GZ plot banda blu (B1)
names(p224r63_2011)                                                    # GZ "names" -> visionare nomi oggetto
# [1] "B1_sre" "B2_sre" "B3_sre" "B4_sre" "B5_sre" "B6_bt"  "B7_sre"
clb<-colorRampPalette(c('dark blue','blue','light blue'))(100)         # GZ palette blu
plot(p224r63_2011$B1_sre,col=clb)

# GZ esercizio: plottare banda infrarosso vicino palette rosso-arancione-giallo
clnir<-colorRampPalette(c('red','orange','yellow'))(100)
plot(p224r63_2011$B4_sre,col=clnir)

# GZ plot multiframe, quattro bande
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

dev.off()

# GZ natural colours
# 3 componenti: R G B
# 3 bande: R = banda rosso, G = banda verde, B = banda blu
# B1: blue - 1
# B2: green - 2
# B3: red - 3
# B4: near infrared (nir) - 4 

# GZ "plotRGB" -> creare plot rosso-verde-blu su tre livelli (tre strati combinati per rappresentare bande rosso, verde e blu)
plotRGB(p224r63_2011, r=3, g=2, b=1, stretch="Lin") # GZ stretch="Lin" per migliorare visibilità immagine

# GZ nir => aggiunta banda infrarosso per rendere immagine più leggibile (necessario togliere una delle altre tre, in questo caso blu)
plotRGB(p224r63_2011, r=4, g=3, b=2, stretch="Lin")
# GZ permette di visualizzare vegetazione

# GZ salvataggio immagine appena ottenuta
pdf("primografico.pdf") 
plotRGB(p224r63_2011, r=4, g=3, b=2, stretch="Lin")

dev.off()

# GZ multiframe bande diverse
par(mfrow=c(2,1))
plotRGB(p224r63_2011, r=3, g=2, b=1, stretch="Lin")
plotRGB(p224r63_2011, r=4, g=3, b=2, stretch="Lin")

dev.off()

# GZ esercizio: nir nella compnente R(Red)
plotRGB(p224r63_2011, r=4, g=3, b=2, stretch="Lin")

# GZ esercizio: nir nella componente G(Green)
plotRGB(p224r63_2011, r=3, g=4, b=2, stretch="Lin")

# GZ esercizo: nir nella componente B(Blue)
plotRGB(p224r63_2011, r=3, g=2, b=4, stretch="Lin")

### LANDCOVER

# GZ setwd e pacchetti
setwd("C:/lab/p224r63")
library(raster)

# GZ "brick" per importare immagine
p224r63_2011<-brick("p224r63_2011_masked.grd")

# GZ richiamare "RStoolbox"
library(RStoolbox)

# GZ plottare immagine in RGB
plotRGB(p224r63_2011,r=4,g=3,b=2,stretch="Lin")

# GZ classificazione dati raster con "unsuperClass", specificando numero di classi
p224r63_2011c<-unsuperClass(p224r63_2011,nClasses = 4)

# GZ visualizzare nuovo modello contenente anche mappa
p224r63_2011c

# GZ plot mappa (quattro colori -> quattro classi specificate) 
plot(p224r63_2011c$map)

# GZ nuova palette (migliore visualizzazione del grafico)
clclass <- colorRampPalette(c('green',"red","blue","black"))(100)
plot(p224r63_2011c$map,col=clclass)

# Day2

# GZ setwd e pacchetti
library(raster)
setwd("C:/lab")
load("TeleRil.RData")
ls()

# GZ importare file 1988 e 2011 ("brick")
p224r63_2011<-brick("p224r63_2011_masked.grd") 
p224r63_1988<-brick("p224r63_1988_masked.grd")

# GZ immagine 1988, come 2011 ha sette bande (colori):
# B1: blue - 1
# B2: green - 2
# B3: red - 3
# B4: near infrared (nir) - 4 
# B4: near infrared (nir)
# B5: medium infrared
# B6: thermal infrared
# B7: medium infrared

# GZ plot oggetto 1988 e visualizzare campi
plot(p224r63_1988)
names(p224r63_1988)

# GZ plot multiframe per banda blu (1), verde (2), rosso (3) e nir (4)
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

# GZ immagine con colori visibili (plotRGB "natural colours")
plotRGB(p224r63_1988, r=3, g=2, b=1, stretch="Lin")

# GZ grafico poco comprensibile => usare infrarosso (plotRGB "false colours"
# GZ esercizio: plotRGB con componenete infrarossa
plotRGB(p224r63_1988,r=4,g=3,b=2,stretch="Lin")

# GZ richiamare immagine 2011
p224r63_2011

# GZ plot per confronto immagini 1988 e 2011
par(mfrow=c(2,1))
plotRGB(p224r63_1988,r=4,g=2,b=1,stretch="Lin")
plotRGB(p224r63_2011,r=4,g=2,b=1,stretch="Lin")  
# GZ => territorio agricolo è molto più sviluppato nel 2011
# GZ nir indica la presenza di vegetazione, zolle di terra sono bianche o celeste

dev.off()

# GZ spectral indices (DVI) => verificare stato salute vegetazione (foglie sane riflettono infrarosso)
# GZ DVI=nir-red -> es: dvi1988=nir1988-red1988 => risultati diversi in base a salute piante (sane=nir alto)
# GZ DVI 1988
dvi1988<-p224r63_1988$B4_sre-p224r63_1988$B3_sre
plot(dvi1988)

# GZ esercizio: DVI 2011
dvi2011<-p224r63_2011$B4_sre-p224r63_2011$B3_sre
plot(dvi2011)

# GZ cambio palette
cldvi<-colorRampPalette(c('light blue','light green','green'))(100)
plot(dvi2011,col=cldvi)

# GZ analisi multitemporale (differenza 2011-1988) => differenza tra DVI dei 2 anni mostra cambiamento stato vegetazione
difdvi<-dvi2011-dvi1988
plot(difdvi)
cldifdvi<-colorRampPalette(c('red','white','blue'))(100)
plot(difdvi,col=cldifdvi)
     
dev.off()

# GZ visualize the output
# GZ multiframe 1988RGB, 2011RGB, difdvi
par(mfrow=c(3,1))
plotRGB(p224r63_1988,r=4,g=3,b=2,stretch="Lin")
plotRGB(p224r63_2011,r=4,g=3,b=2,stretch="Lin")  
plot(difdvi,col=cldifdvi)

dev.off()

# GZ "aggregate" -> modificare risoluzione (grana) immagine creando nuovo RasterLayer con risoluzione più bassa quindi celle più grandi ("fact=n" è un moltiplicatore che ci dà dei pixel n volte più grandi dei precedenti)
p224r63_2011lr<-aggregate(p224r63_2011,fact=10) # lr=lowresolution
# GZ inserire i due oggetti per vedere caratteristiche dei pixel
p224r63_2011
p224r63_2011lr

# GZ plot multiframe confronto tra le due risoluzioni     
par(mfrow=c(2,1))
plotRGB(p224r63_2011, r=4, g=3, b=2, stretch="Lin")
plotRGB(p224r63_2011lr, r=4, g=3, b=2, stretch="Lin")

# GZ lower resolution ("fact=50")
p224r63_2011lr50<-aggregate(p224r63_2011,fact=50)   
p224r63_2011lr50                                  

# GZ plot multiframe comparativo (normale, lr, lr50)     
par(mfrow=c(3,1))
plotRGB(p224r63_2011, r=4, g=3, b=2, stretch="Lin")
plotRGB(p224r63_2011lr, r=4, g=3, b=2, stretch="Lin")
plotRGB(p224r63_2011lr50, r=4, g=3, b=2, stretch="Lin")

# GZ DVI lr50 2011
dvi2011lr50<-p224r63_2011lr50$B4_sre-p224r63_2011lr50$B3_sre
plot(dvi2011lr50)
     
# GZ DVI lr50 1988
p224r63_1988lr50<-aggregate(p224r63_1988,fact=50)              # GZcreare lr50 1988
dvi1988lr50<-p224r63_1988lr50$B4_sre-p224r63_1988lr50$B3_sre
plot(dvi1988lr50)     

# GZ difdvi lr50
difdvilr50<-dvi2011lr50-dvi1988lr50
plot(difdvilr50,col=cldifdvi)        # GZ riprendere palette "cldifdvi" creata in precedenza

# GZ multiframe differenze DVI alle diverse risoluzioni
par(mfrow=c(2,1))
plot(difdvi,col=cldifdvi)
plot(difdvilr50,col=cldifdvi)

#############################################################################
#############################################################################

5. 05_R_code_multitemp.r - Analisi multitemporale variazione landcover

# GZ setwd e pacchetti
setwd("C:/lab")
library(raster)
library(RStoolbox)
library(ggplot2)

# GZ importare immagini
defor1 <-brick("defor1_.png")
defor2 <-brick("defor2_.png")

# GZ plotRGB "defor1"
defor1                                      # GZ visualizzare i campi dell'oggetto
# "defor1_.1" "defor1_.2" "defor1_.3"
# defor1_.1 = NIR
# defor1_.2 = red
# defor1_.3 = green
plotRGB(defor1,r=1,g=2,b=3,stretch="Lin")   # GZ banda red->nir(r=1), green->red(g=2), blue->green(b=3)

# GZ eserczio: plot seconda data
plotRGB(defor2,r=1,g=2,b=3,stretch="Lin")

# GZ confronto (multiframe) stessa area in momenti differenti (prima e dopo deforestazione)
par(mfrow = c(2,1))
plotRGB(defor1,r=1,g=2,b=3,stretch="Lin")
plotRGB(defor2,r=1,g=2,b=3,stretch="Lin")

dev.off()

# GZ classificazione non supervisionata
d1c<-unsuperClass(defor1,nClasses=2)             # GZ si creano classi di foresta e non foresta (suddivisione pixel in queste due categorie)
plot(d1c$map)
cl<-colorRampPalette(c('green','black'))(100)
plot(d1c$map,col=cl)

# GZ esercizio: come prima per "defor2"
d2c<-unsuperClass(defor2,nClasses=2)              
plot(d2c$map,col=cl)

dev.off()

# GZ confronto tra i due momenti (multiframe) con pixel classificati
# GZ due righe, una colonna
par(mfrow=c(2,1))
plot(d1c$map,col=cl)
plot(d2c$map,col=cl)
# GZ due colonne, una riga
par(mfrow=c(1,2))
plot(d1c$map,col=cl)
plot(d2c$map,col=cl)

dev.off()

# GZ calcolo frequenza delle due classi di pixel nella prima immagine
freq(d1c$map)
# GZ aree aperte=37039
# GZ foresta=304253

# GZ numero di pixel totali nella prima immagine (necessario per calcolo percentuale)
totd1<-37039+304253       
totd1
# totd1=341292

# GZ calcolo frequenze percentuali
percent1<-freq(d1c$map)*100/totd1
# GZ foreste: 89.1 %
# GZ aree aperte: 10.9 %

# GZ stesso procedimento per la seconda immagine
freq(d2c$map)
# GZ aree aperte=165055
# GZ foreste=177671

totd2<-165055+177671
totd2
# GZ totd2=342726

percent2<-freq(d2c$map)*100/totd2
# GZ aree aperte: 48.2 %
# GZ foreste: 51.8 %

# GZ creare vettori per analisi grafica
cover<-c("Agriculture","Forest")
before<-c(10.9,89.1)
after<-c(48.2,51.8)

# GZ creare nuovo dataset con i dati ottenuti
output<-data.frame(cover,before,after)
output

# Day2

setwd("C:/lab")
load("C:/lab/defor.RData")
library(raster)
library(ggplot2)
install.packages("gridExtra")
library(gridExtra)

# GZ riprendere mappe 
par(mfrow=c(1,2))
cl<-colorRampPalette(c('black','green'))(100)
plot(d1c$map,col=cl)
plot(d2c$map,col=cl)
ls()
output
#        cover before after
# 1 Agriculture   10.9  48.2
# 2      Forest   89.1  51.8

# GZ istogramma delle percentuali di copertura prima della deforestazione
grafico1<-ggplot(output,aes(x=cover,y=before,color=cover)) +
geom_bar(stat="identity",fill="white")
grafico1                                                        # GZ in ascissa "aes"(aree di foresta/aperte), in ordinata percentuale di copertura

# GZ esercizio: istogramma dopo deforestazione
grafico2<-ggplot(output,aes(x=cover,y=after,color=cover)) +
geom_bar(stat="identity",fill="white")
grafico2

# GZ esercizio: usare "grid.arrange" (funzione del pacchetto "gridExtra" che permette confronto tra istogrammi) per creare un plot con grafico1 e grafico2
# GZ grid.arrange=> grid.arrange(plot1,plot2,nrow=1), questa funzione crea un plot con più grafici
grid.arrange(grafico1,grafico2,nrow=1)
# GZ evidente cambiamento nelle percentuali di copertura

#GZ  per facilitare confronto uniformare sacla dei due istogrammi (imporre al grafico il limite y=100)
grafico1<-ggplot(output,aes(x=cover,y=before,color=cover)) +
geom_bar(stat="identity",fill="white") + 
ylim(0,100)
grafico2 <- ggplot(output, aes(x=cover, y=after, color=cover)) + 
geom_bar(stat="identity", fill="white") +
ylim(0, 100)
grid.arrange(grafico1,grafico2,nrow=1)

#############################################################################
#############################################################################
