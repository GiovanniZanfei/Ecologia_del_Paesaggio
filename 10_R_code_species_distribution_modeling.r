Species Distribution Modeling

# pacchetti (no set wd perchè dati presenti nel pacchetto "sdm")
install.packages(sdm)
library(sdm)
library(raster)
library(rgdal)

# "system.file" -> caricare file da utilizzare contenuto in "sdm"
file<-system.file("external/species.shp",package="sdm")

# "shapefile" (pacchetto "raster")
species<-shapefile(file)

# caratteristiche dataset
species
species$Occurrence  # valori "Occurrence"  # ogni punto associato a presenza assenza specie => "Occurrence" = 0(assente) o 1(presente)

# plot dataset "species"
plot(species)  # mostrate presenze e assenze
# diversificare assenze (rosso) da presenze (blu)
plot(species[species$Occurrence==1,],col='blue',pch=16)
points(species[species$Occurrence==0,],col='red',pch=16)

# variabili ambientali disponibili (cartella "external", pacchetto "sdm")
path <- system.file("external",package="sdm")

# importare file per prevedere distribuzione spaziale in base a variabili ambientali
lst<-list.files(path=path,pattern='asc$',full.names=T) 
lst                                                    # variabili: elevation, precipitation, temperature, vegetation
preds<-stack(lst)                                      # stack => predittore distribuzione
cl<-colorRampPalette(c('yellow','orange','red'))(100)  # palette
plot(preds,col=cl)                                     # distribuzione probabilmente relazionata a valori variabili

# plot elevation
plot(preds$elevation,col=cl)
points(species[species$Occurrence==1,],pch=16)  # aggiungere punti presenza => specie presente a bassa quota

# temperature
plot(preds$temperature, col=cl)
points(species[species$Occurrence==1,],pch=16)  # => specie non gradisce basse temperature

# precipitation
plot(preds$precipitation, col=cl)
points(species[species$Occurrence==1,],pch=16)  # => condizioni medie sono ottimali

vegetation
plot(preds$vegetation, col=cl)
points(species[species$Occurrence==1,],pch=16)  # => elevata copertura vegetale è favorevole

# sintesi: bassa quota, temperatura medio-alta, piovosità media, buona copertura vegetale

# Generalized Linear Model (glm)
d<-sdmData(train=species,predictors=preds)  # indicare a R dati relativi a specie e variabili da considerare
d
# modello
m1<-sdm(Occurrence~elevation+precipitation+temperature+vegetation,data=d,methods='glm') 

# previsione (creare mappa predittiva distribuzione in base alle quattro variabili) -> "predict"
p1<-predict(m1,newdata=preds)
plot(p1,col=cl)
points(species[species$Occurrence== 1,],pch=16)
