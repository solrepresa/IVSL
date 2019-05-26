# Tesis de Doctorado
# Autora: Represa, Soledad
# Fecha: 03/03/2017
# Modificacion: 08/junio/ 2017
# # Analisis de bases INDEC 

## Construccion de INDICE DE VULNERABILIDAD SOCIAL LOCAL

#########################################################################

# Generacion de variables para indice de vulnerabilidad #################


getwd()
setwd( "/home/usuario/Sol/DOC/SIGA/DOCTORADO/IVSL/INDEC-2010")
setwd("D:/BKP/DiscoE/DOC/SIGA/DOCTORADO/IVSL/INDEC-2010")

# 0- FICHERO de variables #### NO ANDA  ####
# (puede estar el error en los encabezados) 
# NO ES NECESARIO
a <- list.files(pattern=".csv$")
INDEC <- data.frame(0,0)
for (i in 1:length(a)){
  archivo <- read.csv(a[i], header = TRUE, sep=",", na.strings = NA)
  tabla <- data.frame(0,0)
    for (j in 1:length(names(archivo))){
      tabla[j,2] <- names(archivo[j])
      tabla[j,1] <- a[i] }
  INDEC <- rbind(tabla,INDEC)
}
names(INDEC) <- c("Archivo","Variables") 

setwd("E:\\DOC\\INDEC-2010\\RES")  
write.csv(INDEC, file="VARIABLES-INDEC.csv", row.names=FALSE)

##Seleccionar municipios  ####
#LA PLATA
lp <- which(hTOTPER$radio>=64410101 & hTOTPER$radio<=64417101)
#Ensenada
eda <- which(hTOTPER$radio>=62450101 & hTOTPER$radio<=62450507)
#Berisso
bso <- which(hTOTPER$radio>=60980101 & hTOTPER$radio<=60980817)
#Armar tabla con municipios 
hTOTPER <-rbind(hTOTPER[lp,],hTOTPER[eda,], hTOTPER[bso,], make.row.names = FALSE)

for(i in 1:849){ hTOTPER[i,11] = "la plata"}
for(i in 850:911){hTOTPER[i,11] = "ensenada"}
for(i in 912:nrow(hTOTPER)){hTOTPER[i,11] = "berisso"}

###### 1- CONSTRUYENDO BASE PARA EL INDICE ####
# Bases de INDEC #
# Numero de personas por hogar ####

hTOTPER <- read.csv("HOGAR-TOTPERS.csv", header = TRUE, sep=",", na.strings = NA) #Abrir tabla

for (i in 1:nrow(hTOTPER)){
  if (hTOTPER[i,2] > hTOTPER[i,3])
  { hTOTPER[i,11] = 1
  } else {
    if (hTOTPER[i,3] > hTOTPER[i,4])
    {hTOTPER[i,11] = 2
    } else {
      if (hTOTPER[i,4] > hTOTPER[i,5])
      {hTOTPER[i,11] = 3
      } else {
        if (hTOTPER[i,5] > hTOTPER[i,6])
        {hTOTPER[i,11] = 4
        } else{
          if (hTOTPER[i,6] > hTOTPER[i,7])
          {hTOTPER[i,11] = 5
          } else {
            if (hTOTPER[i,7] > hTOTPER[i,8])
            {hTOTPER[i,11] = 6
            } else {
              if (hTOTPER[i,8] > hTOTPER[i,9])
              {hTOTPER[i,11] = 7 
              } else {
                hTOTPER[i,11] = 8}
              }
            }
          }
        }
      }
    }
}

hTOTPER[11] <- as.numeric (unlist(hTOTPER[11]))
names(hTOTPER)[11] <-   "perHOGAR"

INDICE <- data.frame(hTOTPER[1], hTOTPER[11])

# hist(INDICE$perHOGAR)
# hist((hTOTPER$perHOGAR- mean(hTOTPER$perHOGAR )) / sd(hTOTPER$perHOGAR))
# Estandarizacion mediante z-score (media 0!)
INDICE$perHOGAR <- (hTOTPER$perHOGAR- mean(hTOTPER$perHOGAR )) / sd(hTOTPER$perHOGAR) 

# Regimen de propiedad de la vivienda y el terreno ####

hPROP <- read.csv("HOGAR-PROP.csv", header = TRUE, sep=",", na.strings = NA)

for (i in 1:nrow(hPROP)){
  if (hPROP[i,8] == 0){
    hPROP[i,9] <- 0}
  else{
    hPROP[i,9] <- (hPROP[i,5] + hPROP[i,6] + hPROP[i,7])/hPROP[i,8] }
}

names(hPROP)[9] <- "regPROP"     #relativo
hPROP[9] <- (hPROP$regPROP - mean(hPROP$regPROP))/sd(hPROP$regPROP)
INDICE[3] <- hPROP[9]


#hist(INDICE$regPROP)

# Razon de dependencia de jovenes ####

dep <- read.csv("PERSONA-EDADAGRU.csv", header = TRUE, sep=",", na.strings = NA)

for (i in 1:nrow(dep)){
  if (dep[i,3] == 0){
  dep[i,6] <- 0}
  else{
  dep[i,6] <- dep[i,2]/dep[i,3] }
}

names(dep)[6] <- "depJOV"
#hist((dep$depJOV- mean(dep$depJOV)) / sd(dep$depJOV))
dep[6] <- (dep$depJOV- mean(dep$depJOV, na.rm=TRUE)) / sd(dep$depJOV, na.rm=TRUE)
INDICE[4] <- dep[6]

# Razon de dependencia de adultos mayores ####

for (i in 1:nrow(dep)){
  if (dep[i,3] == 0){
    dep[i,7] <- 0}
  else{
    dep[i,7] <- dep[i,4]/dep[i,3] }
}
names(dep)[7] <- "depADU"
dep[7] <- (dep$depADU- mean(dep$depADU, na.rm=TRUE)) / sd(dep$depADU, na.rm=TRUE)
INDICE[5] <- dep[7]

# Tasa de inmigrantes ####

inmi <- read.csv("PERSONA-P05.csv", header = TRUE, sep=",", na.strings = NA)

for (i in 1:nrow(inmi)){
  if (inmi[i,4] == 0){
    inmi[i,5] <- 0}
  else{
    inmi[i,5] <- inmi[i,3]/inmi[i,4] }}

names(inmi)[5] <- "inmi"
inmi[5] <- (inmi$inmi - mean(inmi$inmi))/ sd(inmi$inmi)
INDICE[6] <- inmi[5]

# Tasa de analfabetismo ####

analfa <- read.csv("PERSONA-P07.csv", header = TRUE, sep=",", na.strings = NA)

for (i in 1:nrow(analfa)){
  if ((dep[i,3] + dep[i,4]) == 0){
    analfa[i,5] <- 0}
  else{
    analfa[i,5] <- analfa[i,3]/(dep[i,3] + dep[i,4]) }} #respecto a los mayores de 14

names(analfa)[5] <- "analfa"
analfa[5] <- (analfa$analfa - mean(analfa$analfa))/sd(analfa$analfa)
INDICE[7] <- analfa[5]

# Personas con nivel primario incompleto ####
prim <- read.csv("PERSONA-P08.csv", header = TRUE, sep=",", na.strings = NA)

for (i in 1:nrow(prim)){
  if (prim[i,5] == 0){
    prim[i,6] <- 0}
  else{
    prim[i,6] <- prim[i,4]/prim[i,5] }}

names(prim)[6] <- "escNO"
prim[6] <- (prim$escNO - mean(prim$escNO))/sd(prim$escNO)
INDICE[8] <- prim[6]

# Calidad de los materiales ####

mate <- read.csv("VIVIENDA-INMAT.csv", header = TRUE, sep=",", na.strings = NA)

for (i in 1:nrow(mate)){
  if (mate[i,6] == 0){
    mate[i,7] <- 0}
  else{
    mate[i,7] <- mate[i,5]/mate[i,6] }}

names(mate)[7] <- "matePREC"

mate[7] <- (mate$matePREC - mean(mate$matePREC))/sd(mate$matePREC)
INDICE[9] <- mate[7]

# Viviendas compartidas ####

vivCOM <- read.csv("VIVIENDA-TOTHOG.csv", header = TRUE, sep=",", na.strings = NA)

for (i in 1:nrow(vivCOM)){
  if (vivCOM[i,4] == 0){
    vivCOM[i,5] <- 0}
  else{
    vivCOM[i,5] <- vivCOM[i,3]/vivCOM[i,4] }}

names(vivCOM)[5] <- "viviCOM"
vivCOM[5] <- (vivCOM$viviCOM - mean(vivCOM$viviCOM))/sd(vivCOM$viviCOM)
INDICE[10] <- vivCOM[5]

# Provision de agua ####
aguaNO <- read.csv("HOGAR-H08.csv", header = TRUE, sep=",", na.strings = NA)

for (i in 1:nrow(aguaNO)){
  if (aguaNO[i,5] == 0){
    aguaNO[i,6] <- 0}
  else{
    aguaNO[i,6] <- (aguaNO[i,3]+ aguaNO[i,4])/aguaNO[i,5] }}

names(aguaNO)[6] <- "aguaNO"
aguaNO[6] <- (aguaNO$aguaNO - mean(aguaNO$aguaNO))/sd(aguaNO$aguaNO)
INDICE[11] <- aguaNO[6]

# Heladera ####
helaNO <- read.csv("HOGAR-H19A.csv", header = TRUE, sep=",", na.strings = NA)

for (i in 1:nrow(helaNO)){
  if (helaNO[i,4] == 0){
    helaNO[i,5] <- 0}
  else{
    helaNO[i,5] <- helaNO[i,3]/helaNO[i,4] }}

names(helaNO)[5] <- "helaNO"
helaNO[5] <- (helaNO$helaNO - mean(helaNO$helaNO))/sd(helaNO$helaNO)
INDICE[12] <- helaNO[5]

# Telefono ####
tel <- read.csv("HOGAR-H19D.csv", header = TRUE, sep=",", na.strings = NA)

for (i in 1:nrow(tel)){
  if (tel[i,4] == 0){
    tel[i,5] <- 0}
  else{
    tel[i,5] <- tel[i,3]/tel[i,4] }}

names(tel)[5] <- "telNO"
tel[5] <- (tel$telNO - mean(tel$telNO))/sd(tel$telNO)
INDICE[13] <- tel[5]

# Carencia de bano ####
bano <- read.csv("HOGAR-H10.csv", header = TRUE, sep=",", na.strings = NA)

for (i in 1:nrow(bano)){
  if (bano[i,4] == 0){
    bano[i,5] <- 0}
  else{
    bano[i,5] <- bano[i,3]/bano[i,4] }}

names(bano)[5] <- "banoNO"
bano[5] <- (bano$banoNO - mean(bano$banoNO))/sd(bano$banoNO)
INDICE[14] <- bano[5]

# Desague del bano ####
des <- read.csv("HOGAR-H12.csv", header = TRUE, sep=",", na.strings = NA)

for (i in 1:nrow(des)){
  if (des[i,6] == 0){
    des[i,7] <- 0}
  else{
    des[i,7] <- (des[i,4] + des[i,5])/des[i,6] }}

names(des)[7] <- "desagueNO"
des[7] <- (des$desagueNO - mean(des$desagueNO))/sd(des$desagueNO)
INDICE[15] <- des[7]

# Combustible utilizado para cocinar ####
coc <- read.csv("HOGAR-H14.csv", header = TRUE, sep=",", na.strings = NA)

for (i in 1:nrow(coc)){
  if (coc[i,9] == 0){
    coc[i,10] <- 0}
  else{
    coc[i,10] <- (coc[i,7] + coc[i,8])/coc[i,9] }}

names(coc)[10] <- "combCOC"
coc[10] <- (coc$combCOC - mean(coc$combCOC))/ sd(coc$combCOC)
INDICE[16] <- coc[10]

# NBI ####
nbi <- read.csv("HOGAR-ALGUNBI.csv", header = TRUE, sep=",", na.strings = NA)

for (i in 1:nrow(nbi)){
  if (nbi[i,4] == 0){
    nbi[i,5] <- 0}
  else{
    nbi[i,5] <- nbi[i,3]/nbi[i,4] }}

names(nbi)[5] <- "nbi"
nbi[5] <- (nbi$nbi - mean(nbi$nbi))/sd(nbi$nbi)
INDICE[17] <- nbi[5]

# Proporción de desocupadoson ####
deso <- read.csv("PERSONA-CONDACT.csv", header = TRUE, sep=",", na.strings = NA)

for (i in 1:nrow(deso)){
  if (deso[i,5] == 0){
    deso[i,6] <- 0}
  else{
    deso[i,6] <- (deso[i,3] + deso[i,4])/deso[i,5] }}

names(deso)[6] <- "desocup"
deso[6] <- (deso$desocup - mean(deso$desocup))/ sd(deso$desocup)
INDICE[18] <- deso[6]

# Proporción de personas en situacion de calle ####
calle <- read.csv("VIVIENDA-V01.csv", header = TRUE, sep=",", na.strings = NA)

for (i in 1:nrow(calle)){
  if (calle[i,11] == 0){
    calle[i,12] <- 0}
  else{
    calle[i,12] <- (calle[i,10])/calle[i,11] }}

names(calle)[12] <- "situCALLE"
calle[12] <- (calle$situCALLE - mean(calle$situCALLE))/sd(calle$situCALLE)
INDICE[19] <- calle[12]

# Proporción de mujeres ####
poblacion <- read.csv("datos_basicos_radio.csv", header = TRUE, sep=",", na.strings = NA)

for (i in 1:nrow(poblacion)){
  if (poblacion[i,3] == 0){
   poblacion[i, 9] <- 0}
  else{
    poblacion[i, 9] <- (poblacion[i,3])/poblacion[i,4] }}

names(poblacion)[9] <- "mujeres"
poblacion[9] <- (poblacion$mujeres - mean(poblacion$mujeres))/sd(poblacion$mujeres)
poblacion <- data.frame(poblacion$link, poblacion$mujeres)
names(poblacion) <- c("radio", "mujeres")

for (i in 1:nrow(poblacion)){
  if (nchar(poblacion[i,1]) == 8){
    poblacion[i,1] <- paste(0, poblacion[i,1], sep="")
  }
}

for (i in 1:nrow(INDICE)){
  if (nchar(INDICE[i,1]) == 8){
    INDICE[i,1] <- paste(0, INDICE[i,1], sep="")
  }
}

INDICE <- merge(INDICE, poblacion)


# Guardamos base  ####
setwd( "/home/usuario/Sol/DOC/SIGA/DOCTORADO/IVSL")
write.csv(INDICE, file="indicadores.csv", row.names=FALSE)

########################################################################################
########################################################################################

# Analisis de correlacion entre variables  ####
setwd( "/home/usuario/Sol/DOC/SIGA/DOCTORADO/IVSL")
base <- read.csv("indicadores.csv", header = TRUE, sep=",", na.strings = NA)

#base <- base[complete.cases(base),]

# Grafica de Correlacion TODAS ####

panel.cor <- function(x, y, digits = 2, cex.cor, ...){       #funcion para corPLOT con p-value
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  # correlation coefficient
  r <- cor(x, y)
  txt <- format(c(r, 0.123456789), digits = digits)[1]
  txt <- paste("r= ", txt, sep = "")
  text(0.5, 0.6, txt)
  
  # p-value calculation
  p <- cor.test(x, y)$p.value
  txt2 <- format(c(p, 0.123456789), digits = digits)[1]
  txt2 <- paste("p= ", txt2, sep = "")
  if(p<0.01) txt2 <- paste("p= ", "<0.01", sep = "")
  text(0.5, 0.4, txt2)
}

pairs(base[2:20], lower.panel = panel.cor, upper.panel=NULL)  #corplot con p-value

# Otra opcion:
# Permite ver las correlaciones como tablas

library("psych")
correlacion <- corr.test(base[2:20])

DATOScor <- correlacion$r  #nos da los valores de correlacion
DATOSpvalor <- correlacion$p  #nos da los p-valor


# Analizar las variables que mantienen una correlacioin significativa > 0.3 
# con al menos una variable 

setwd( "/home/usuario/Sol/DOC/SIGA/DOCTORADO/IVSL")
write.csv(DATOScor, file="DATOScor.csv", row.names=FALSE)
write.csv(DATOSpvalor, file="DATOSpvalor.csv", row.names=FALSE)

## Guardar shp con todos los indicadores del índice #####
library(rgdal)

setwd( "/home/usuario/Sol/DOC/SIGA/DOCTORADO/IVSL")
base <- read.csv("indicadores.csv", header = TRUE, sep=",", na.strings = NA)

# SIEMPRE HACER!
# completamos los codigos con 0 donde faltaban
for (i in 1:nrow(base)){
  if (nchar(base[i,1]) == 8){
    base[i,1] <- paste(0, base[i,1], sep="")
  }
}

# Abrimos shp Argentina
sh_arg <- readOGR(dsn = "/home/usuario/Sol/DOC/SIGA/DATOS/shp/INDEC", 
                  layer = "datos_basicos_radio")

# Unimos bases
sh_indicadores <- merge(sh_arg, base, by.x = "link", by.y = "radio")

# guardamos shp
writeOGR(obj=sh_indicadores, dsn="indicadores",  layer="indicadores", driver="ESRI Shapefile")

# Analissi de alpha de cronbach

library(multilevel)

cronbach(base[2:20])

# Analizar correlaciones en la base. 
# Ver variables que correlacionan significativamente.
# Decartar las variables que presentan correlacion menor a 0.3

INDICE <- data.frame(base$radio, base$perHOGAR, base$regPROP, base$depJOV,
                     base$depADU, base$analfa,base$escNO, base$matePREC,
                     base$aguaNO,  base$helaNO, base$telNO, base$banoNO, 
                     base$desagueNO, base$combCOC, base$nbi, base$desocup, base$mujeres)

names(INDICE) <- c("radio", "perHOGAR", "regPROP", "depJOV", 
                   "depADU", "analfa","escNO", "matePREC", 
                   "aguaNO", "helaNO", "telNO", "banoNO", 
                   "desagueNO","combCOC", "nbi", "desocup", "mujeres" )

#write.csv(INDICE, file="variables_IVSL.csv", row.names=FALSE)


# para ver CORRELACION como circulos en colores 
library(corrplot)
corre <- cor(INDICE[2:16],use="pairwise.complete.obs") 
corrplot.mixed(corre, order="hclust", diag="u", tl.col="black", tl.pos="lt", 
               addgrid.col = "grey", upper="number",  lower = "circle", 
               tl.cex=0.7, tl.offset=0.1, number.cex=0.6)

# A armar el indice! ######

INDICE <- read.csv("variables_IVSL.csv", header = TRUE, sep=",", na.strings = NA)
INDICE <- INDICE[complete.cases(INDICE),]
INDICE <- INDICE[3:17] #aca quite más variables más de las q puse en el paper
INDICE <- INDICE[,-c(1,3,9,11, 14)]

# Ver media y desviacion de las VARIABLES
# como se estandarizaron mean= 0, sd=1
descrip <- data.frame() 
for (i in 1:ncol(INDICE)){
  descrip[i,1] <- names(INDICE[i])
  descrip[i,2] <- mean(INDICE[,i])
  descrip[i,3] <- sd(INDICE[,i])}
names(descrip) <- c("Variables", "Media", "Desviacion_standar")

# Matriz de correlación a colores ####
library("corrplot")
cor_INDICE <- cor(INDICE)
det(cor_INDICE) # debe ser cercano a cero, pero no muy bajo 
corrplot.mixed(cor_INDICE, order="hclust", diag="u", tl.col="black", tl.pos="lt", 
               addgrid.col = "grey", upper="number",  lower = "circle", 
               tl.cex=0.7, tl.offset=0.1, number.cex=0.6)

# Pruebas estadisticas ####
# Prueba de normalidad multivariada 
library("MVN"); mvn(INDICE) #tira error :/

# Test de Esfericidad de Barlett 
library("psych")
cortest.bartlett(cor_INDICE, n=nrow(INDICE)) # el p <0,05 solo si Mardia dio NORMAL
bartlett.test(INDICE)


# KMO 
KMO(cor_INDICE) # KMO > 0,7 Aceptable

## PCA #####

INDICE_pca <- prcomp(INDICE) 
#attributes(INDICE_pca)
plot (INDICE_pca, type="l", main="Gráfica de sedimentación")


library("GPArotation")
library("psych")
library("reshape")
library("ggplot2")
library("dplyr")

# el primer PCA es para evaluar el numero de factores
# observar la variancia acumulada
pca12 <- principal(INDICE, nfactors=10, rotate="none", use="varimax" )
pca12

pca <- principal(INDICE, nfactors=3, rotate="none", use="pairwise", scores=TRUE ) # utilizan Kaiser normalization
vari <- principal(INDICE, nfactors=3, rotate="varimax", scores=TRUE  ) # 77 con 4 factores
obli <- principal(INDICE, nfactors=3, rotate="oblimin", scores=TRUE ) #oblimin 81 var acum con 3 fact


cor(pca$loadings[,1:3])
cor(vari$loadings[,1:3])
cor(obli$loadings[,1:3])


#grafica de pesos necesario !!!
pesos <- with(obli, unclass(loadings))
pesos <- as.data.frame(pesos) #convierto para trabajar con ggplot
names(pesos) <- c("RC1", "RC2", "RC3")
pesos <- add_rownames(pesos, "indicadores")   #recupera la row.name como columna
pesos <- as.data.frame(pesos)

pesos_m <- melt(pesos, value.name= "indicadores")

#grafica de pesos 1
ggplot(data = pesos_m, aes(x = indicadores, y = value , fill= variable ) )+
  geom_bar(stat = "identity", position = position_dodge(width = 0.8)) +
  ylim(-0.8,0.8) + ggtitle("Pesos") + 
  geom_hline( yintercept= 0.3, col = "mediumorchid4") +
  geom_hline( yintercept= -0.3, col = "mediumorchid4") +
  scale_fill_brewer(palette="Set2") + theme_bw()

#grafica de factores
ggplot(data = pesos_m, aes(x = variable, y = value , fill= indicadores ) )+
  geom_bar(stat = "identity", position = position_dodge(width = 0.8)) +
  ylim(-0.8,0.8) + ggtitle("Pesos") + 
  geom_hline( yintercept= 0.3, col = "mediumorchid4") +
  geom_hline( yintercept= -0.3, col = "mediumorchid4") +
  scale_fill_brewer(palette="Set2") + theme_bw()



#otra forma de ver los pesos
library(RColorBrewer)
cols <- rev(brewer.pal(11, "RdYlBl"))
pesos_m$value <- round(pesos_m$value,3)
names(pesos_m) <- c("indicadores", "variable", "Carga")

ggplot(data= pesos_m, aes(x=variable, y=indicadores)) + 
  geom_tile(aes(fill=Carga)) +
  scale_fill_gradientn(colours = cols) + 
  xlab("Componentes principales") + ylab("") +
  geom_text(aes(label=Carga))


## Graficas PCA ####
rota <- pca
rota <- vari
rota <- obli

# Grafica de componentes en espacios rotados 2D ####
plot(rota$loadings[,1],rota$loadings[,2],type="n", xlab="CP 1", ylab="CP 2")
text(rota$loadings[,1],rota$loadings[,2],labels=row.names(rota$loadings))
title(main="Gráfico de componentes en espacio rotado")
abline(h=0, v=0, col = "gray60") 


plot(rota$loadings[,1],rota$loadings[,3],type="n", xlab="CP 1", ylab="CP 3")
text(rota$loadings[,1],rota$loadings[,3],labels=row.names(rota$loadings))
title(main="Gráfico de componentes en espacio rotado")
abline(h=0, v=0, col = "gray60") 


#   Grafica de componentes en espacios rotado 3D ####
library("scatterplot3d")

s3d <- scatterplot3d(rota$loadings[,1:3],
              xlab="CP 1", ylab="CP 2", zlab="CP 3",
              angle =200, pch = 16, color = "snow3",
              grid=TRUE, box=FALSE, type="h")
text(s3d$xyz.convert(rota$loadings[,1:3]), labels = rownames(rota$loadings),
     cex= 0.9, col = "violetred4")
title(main="Gráfico de componentes en espacio rotado")

#Matriz de correlacion con los PC ####
INDICE_m <- as.matrix(INDICE) 
cor_INDICE <- cor(INDICE_m, vari$scores)

# Grafica de los componentes 2D ####
plot(obli$scores,
     main="Gráfica de componentes principales", col="mediumseagreen",
     xlab="Componente 1", ylab="Componente 2")
abline(h=0,v=0)

# Grafica de los componentes 3D ####
library("scatterplot3d")

rota <- obli
s3d <- scatterplot3d(obli$scores,
                     xlab="CP 1", ylab="CP 2", zlab="CP 3",
                     angle =210, pch = 16, color = "mediumseagreen",
                     grid=TRUE, box=FALSE)
title(main="Gráfico de componentes en espacio rotado")

# Guardar INDEX sin t ####

INDICE <- read.csv("variables_IVSL.csv", header = TRUE, sep=",", na.strings = NA)
base <- INDICE[complete.cases(INDICE),]

base_PCA <- cbind(base,vari$scores) # se unen base
base_PCA[22] <- base_PCA$RC1 + base_PCA$RC2 + base_PCA$RC3+ base_PCA$RC4
names(base_PCA)[22] <- "IVSL"

#write.csv(base_PCA, file="IVSL_ARG.csv", row.names=FALSE)

# Categorizar el INDICE ####
# boxplot(base_PCA$IVSL)  El IVSL es normal! 
# Eso permite hacer referencia a la desviación estandar

setwd( "/home/usuario/Sol/DOC/SIGA/DOCTORADO/IVSL")
base <- read.csv("IVSL_ARG.csv", header = TRUE, sep=",", na.strings = NA)

base$IVSL_t <- (base$IVSL - min(base$IVSL))/(max(base$IVSL)-min(base$IVSL)) 

cuartil <- boxplot(base$IVSL_t)

# Agregar categorias al ILVS  ################### 

cuartil <- boxplot(base$IVSL_t)

for (i in 1:nrow(base)){
  if (base$IVSL_t[i] < cuartil$stats[1])
    {base$cat_VS[i] <- 1
  } else {
    if (base$IVSL_t[i] > cuartil$stats[1] && base$IVSL_t[i] < cuartil$stats[2])
      {base$cat_VS[i] <- 2
    } else{
      if (base$IVSL_t[i] > cuartil$stats[2] && base$IVSL_t[i] < cuartil$stats[4])
        {base$cat_VS[i] <- 3
      } else{
        if (base$IVSL_t[i] > cuartil$stats[4]  && base$IVSL_t[i] < cuartil$stats[5])
          {base$cat_VS[i] <- 4
        } else {
          base$cat_VS[i] <- 5 
        }
      }
    }
  }
}

##### Contar radios censales en cada fraccion
mb_ILVS <- which(base$IVSL_t<cuartil$stats[1])
b_ILVS <- which(base$IVSL_t > cuartil$stats[1] & base$IVSL_t < cuartil$stats[2])
m_ILVS <- which(base$IVSL_t > cuartil$stats[2] & base$IVSL_t < cuartil$stats[4])
a_ILVS <- which(base$IVSL_t > cuartil$stats[4]  & base$IVSL_t < cuartil$stats[5])
ma_IVLS <- which(base$IVSL_t > cuartil$stats[5])

##### Discriminar por categoría
mb_ILVS <- base[which(base$IVSL_t<cuartil$stats[1]),]
b_ILVS <- base[which(base$IVSL_t > cuartil$stats[1] & base$IVSL_t < cuartil$stats[2]),]
m_ILVS <- base[which(base$IVSL_t > cuartil$stats[2] & base$IVSL_t < cuartil$stats[4]),]
a_ILVS <- base[which(base$IVSL_t > cuartil$stats[4]  & base$IVSL_t < cuartil$stats[5]),]
ma_IVLS <- base[which(base$IVSL_t > cuartil$stats[5]),]


# Guardar base con t y categoria   ######
write.csv(base, file="IVSL_ARG_cat.csv", row.names=FALSE)


#### Visualizar la distribución ######

setwd( "/home/usuario/Sol/DOC/SIGA/DOCTORADO/IVSL")
base <- read.csv("IVSL_ARG_cat.csv", header = TRUE, sep=",", na.strings = NA)

library("ggplot2")

ggplot(data=base, aes(base$IVSL_t)) + 
  geom_histogram(col="white", 
                 fill="green", 
                 alpha = .4, bins=60) + 
  labs(title="Histograma de ILVS") +
  labs(x="Valor para el ILVS", y="") +
  geom_vline(xintercept= cuartil$stats[1], col = "red",  alpha = .4) +
  geom_vline(xintercept= cuartil$stats[2], col = "red",  alpha = .4) +
  geom_vline(xintercept= cuartil$stats[4], col = "red",  alpha = .4) +
  geom_vline(xintercept= cuartil$stats[5], col = "red",  alpha = .4) 

### ARMAR MAPA de VULNERABILIDAD   #####################

library(rgdal)

#abrimos ILVS
setwd( "/home/usuario/Sol/DOC/SIGA/DOCTORADO/IVSL")
base <- read.csv("IVSL_ARG_cat.csv", header = TRUE, sep=",", na.strings = NA)

# SIEMPRE HACER!
# completamos los codigos con 0 donde faltaban
for (i in 1:nrow(base)){
  if (nchar(base[i,1]) == 8){
    base[i,1] <- paste(0, base[i,1], sep="")
  }
}

# Abrimos shp Argentina
sh_arg <- readOGR(dsn = "/home/usuario/Sol/DOC/SIGA/DATOS/shp/INDEC", 
                 layer = "datos_basicos_radio")

# Unimos bases
sh_IVSL <- merge(sh_arg, base, by.x = "link", by.y = "radio")

# guardamos shp
writeOGR(obj=sh_IVSL, dsn="ILVS",  layer="ILVS", driver="ESRI Shapefile")

# Funciones de gdal para JUGAR ####
## NO CORRER! Varias funciones para ver qué onda 
str(sh_arg)     # como esta formado el objeto
sh_arg@bbox     # coordenadas de la extensión de la capa

length(sh_arg)      # numero de poligono
names(sh_arg)
head(sh_arg@data)     #para ver los datos
sapply(sh_arg@data, class)   #para ver las categorias de datos

plot(sh_arg, axes=TRUE)  # Grafica de mapa :)
title(main = "Argentina", sub = "Indice Local de Vulerabilidad Social")
# si queremos agregar un plot( add=TRUE)

##### Separar base por provincias #########

library(rgdal)
setwd( "/home/usuario/Sol/DOC/SIGA/DOCTORADO/IVSL")

# Abrimos shp ILVS
sh_IVSL <- readOGR(dsn = "/home/usuario/Sol/DOC/SIGA/DOCTORADO/IVSL/ILVS", 
                  layer = "ILVS")


LINK <- sh_IVSL@data$link            # Mapear todo es mucho calculo!
buenos_aires <- grep("^06", LINK)
catamarca <- grep("^10", LINK)
chaco <- grep("^22", LINK)
chubut <- grep("^26", LINK)
caba <- grep("^02", LINK)     #ojo! aparece empty!!!
corrientes <- grep("^18", LINK)
cordoba <- grep("^14", LINK)
entre_rios <- grep("^30", LINK)
formosa <- grep("^34", LINK)
jujuy <- grep("^38", LINK)
la_pampa <- grep("^42", LINK)
la_rioja <- grep("^46", LINK) 
mendoza <- grep("^50", LINK)
misiones <- grep("^54", LINK)
neuquen <- grep("^58", LINK)
rio_negro <- grep("^62", LINK)
salta <- grep("^66", LINK)
san_juan <- grep("^70", LINK)
san_luis <- grep("^74", LINK)
santa_cruz <- grep("^78", LINK)
santa_fe <- grep("^82", LINK)
santiago  <- grep("^86", LINK)
tierra <- grep("^94", LINK)
tucuman <- grep("^90", LINK)

plot(sh_IVSL[tierra,])

# Plot colores con colores
spplot(sh_IVSL, "IVSL_t" , col="transparent", 
      main = "Argentina", sub = "ILVS")


############## MAPAS clasificados ##########################
library(RColorBrewer) #display.brewer.all()    #permite ver colores
library(classInt) 

colores <- brewer.pal(n = 5, name = "OrRd")
breaks.qt  <- c(1,2,3,4,5,6)

spplot(sh_IVSL[tierra,], "cat_VS" , col="transparent", 
       col.regions = colores, at = breaks.qt, scales = list(draw = TRUE),
       main = "Tierra del Fuego", sub = "ILVS")
#col da color a las lineas del poligono: "transparent", "grey"


############## MAPAS por PROV ###########################
spplot(sh_IVSL[cordoba,], "cat_VS" , col="transparent", 
       col.regions = colores, at = breaks.qt, scales = list(draw = TRUE),
       main = "Córdoba", sub = "ILVS")

spplot(sh_IVSL[caba,], "cat_VS" , col="transparent", 
       scales = list(draw = TRUE), 
       col.regions = colores, at = breaks.qt,
       main = "CABA", sub = "ILVS")

spplot(sh_IVSL[chubut,], "cat_VS" , col="transparent", 
       scales = list(draw = TRUE),
       col.regions = colores, at = breaks.qt,
       main = "Chubut", sub = "ILVS")

spplot(sh_IVSL[chaco,], "cat_VS" , col="transparent", 
       scales = list(draw = TRUE),
       col.regions = colores, at = breaks.qt,
       main = "Chaco", sub = "ILVS")

spplot(sh_IVSL[catamarca,], "cat_VS" , col="transparent", 
       scales = list(draw = TRUE),
       col.regions = colores, at = breaks.qt,
       main = "Catamarca", sub = "ILVS")

spplot(sh_IVSL[tucuman,], "cat_VS" , col="transparent", 
       scales = list(draw = TRUE),
       col.regions = colores, at = breaks.qt,
       main = "Tucuman", sub = "ILVS")

spplot(sh_IVSL[jujuy,], "cat_VS" , col="transparent", 
       scales = list(draw = TRUE),
       col.regions = colores, at = breaks.qt,
       main = "Jujuy", sub = "ILVS")

spplot(sh_IVSL[corrientes,], "cat_VS" , col="transparent", 
       scales = list(draw = TRUE),
       col.regions = colores, at = breaks.qt,
       main = "Corrientes", sub = "ILVS")

spplot(sh_IVSL[neuquen,], "cat_VS" , col="transparent", 
       scales = list(draw = TRUE),
       col.regions = colores, at = breaks.qt,
       main = "Neuquen", sub = "ILVS")

spplot(sh_IVSL[salta,], "cat_VS" , col="transparent", 
       scales = list(draw = TRUE),
       col.regions = colores, at = breaks.qt,
       main = "Salta", sub = "ILVS")

spplot(sh_IVSL[la_pampa,], "cat_VS" , col="transparent", 
       scales = list(draw = TRUE),
       col.regions = colores, at = breaks.qt,
       main = "La Pampa", sub = "ILVS")

spplot(sh_IVSL[misiones,], "cat_VS" , col="transparent", 
       scales = list(draw = TRUE),
       col.regions = colores, at = breaks.qt,
       main = "Misiones", sub = "ILVS")

spplot(sh_IVSL[santiago,], "cat_VS" , col="transparent", 
       scales = list(draw = TRUE),
       col.regions = colores, at = breaks.qt,
       main = "Santiago", sub = "ILVS")

spplot(sh_IVSL[la_rioja,], "cat_VS" , col="transparent", 
       scales = list(draw = TRUE),
       col.regions = colores, at = breaks.qt,
       main = "La Rioja", sub = "ILVS")

spplot(sh_IVSL[santa_cruz,], "cat_VS" , col="transparent", 
       scales = list(draw = TRUE),
       col.regions = colores, at = breaks.qt,
       main = "Santa Cruz", sub = "ILVS")

spplot(sh_IVSL[formosa,], "cat_VS" , col="transparent", 
       scales = list(draw = TRUE),
       col.regions = colores, at = breaks.qt,
       main = "Formosa", sub = "ILVS")

spplot(sh_IVSL[buenos_aires,], "cat_VS" , col="transparent", 
       scales = list(draw = TRUE),
       col.regions = colores, at = breaks.qt,
       main = "Buenos Aires", sub = "ILVS")

spplot(sh_IVSL[san_luis,], "cat_VS" , col="transparent", 
       scales = list(draw = TRUE),
       col.regions = colores, at = breaks.qt,
       main = "San Luis", sub = "ILVS")

spplot(sh_IVSL[san_juan,], "cat_VS" , col="transparent", 
       scales = list(draw = TRUE),
       col.regions = colores, at = breaks.qt,
       main = "San Juan", sub = "ILVS")

spplot(sh_IVSL[mendoza,], "cat_VS" , col="transparent", 
       scales = list(draw = TRUE),
       col.regions = colores, at = breaks.qt,
       main = "Mendoza", sub = "ILVS")
#####################################################

spplot(sh_IVSL, "cat_VS" , col="transparent", scales = list(draw = TRUE),
       col.regions = colores, at = breaks.qt,
       main = "Argentina", sub = "ILVS")

###################################################

> ricos <- which(sh_IVSL@data$cat_VS <3)
> plot(sh_IVSL[ricos,], col="red")
> View(sh_IVSL[ricos,])

> pobres <- which(sh_IVSL@data$cat_VS >3)
> View(sh_IVSL[pobres,])

#################################################







# ??????????????????? ####
INDICE_m <- as.matrix(INDICE) 
modelo.km <- kmeans(INDICE_m, 12) # PASO 2:  Crear modelo kmeans
data.grf  <- data.frame( pca1  = INDICE_pca$x[,1],
                         pca2  = INDICE_pca$x[,2],
                         color = modelo.km$cluster)
plot(data.grf[,1:2],col=data.grf[,3], pch=as.integer(INDICE[,5]))

# Descomposición en valores singulares ####
pca2=svd(INDICE)

#Primeros dos componentes principales obtenidos de SVD
prc1 <- pca2$u[,1]*pca2$d[1]
prc2 <- pca2$u[,2]*pca2$d[2]

plot(prc1,prc2,main="Gráfica de los dos primeros componentes principales obtenidos de SVD",col="6")
abline(h=0,v=0)

# Analisis de factor comun ####
# Busca un fatcor comun entre las variables

library(stats)
analisis <- factanal(INDICE,factors=2,rotation="none")
print(analisis, digits=2, cutoff=.2, sort=FALSE)

cargas <- analisis$loadings #Matriz de cargas
plot(cargas, type="p",col="red",lwd=1) #Grafica de las cargas

# Analisis con rotacion
analisis <- factanal(INDICE,factors=2,rotation="varimax")
print(analisis, digits=2, cutoff=.2, sort=FALSE)

analisis <- factanal(INDICE,factors=2,rotation="varimax", scores="regression")
nrow(analisis$scores)


INDICE_pp <-cbind(INDICE,analisis$scores)
summary(INDICE_pp)


om.h <- omega(INDICE,n.obs=1010,sl=FALSE)

# Grafica Correlacion con ggplot ####

library("ggplot2")
library("reshape")


#get_upper <- function(tabla){   # Triangulo superior de matrix
# tabla[lower.tri(tabla)]<- NA ; return(tabla) }

#corre <- get_upper(corre)

datos.corre <- melt(corre)
names(datos.corre)=c("Variable_1","Variable_2","Correlacion")
datos.corre$Correlacion <- abs(datos.corre$Correlacion)
corr_plot <- ggplot(datos.corre, aes(Variable_1, Variable_2, fill=Correlacion))+
geom_tile(color = "white")+  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                      midpoint = 0, limit = c(-1,1), space = "Lab", 
                      name="Correlación\nde Pearson") + theme_minimal()

# Ver correlacion ordenada
z <- order(datos.corre$Correlacion)
z <- datos.corre[z,]




geom_histogram(col="white", 
               alpha = .4, bins=60, 
               aes(fill=..count..)) + 
  scale_fill_distiller( palette = "Spectral")

# Prueba de indice por Dimensiones ####
# Dimension Socio

socio <- base[3:7]
socio_pca <- prcomp(socio) 
plot (base_pca, type="l", main="Dimension Sociodemografica")
pairs(socio[,1:5])

# Dimension pobreza
pobre <- base[18:20]
pobre_pca <- prcomp(pobre)
plot (pobre_pca, type="l", main="Dimension Pobreza")
pairs(pobre, upper.panel = panel.cor)

# Dimension Vivienda
vivi<- base[10:17]
vivi_pca <- prcomp(vivi)
plot (vivi_pca, type="l", main="Dimension Vivienda")
pairs(vivi, upper.panel = panel.cor)

plot (prcomp(base[-c(1,2)]), type="l", main="Base")

pairs(base, upper.panel = panel.cor)

# Scatter plot con r y p ########

data(iris)

panel.cor <- function(x, y, digits = 2, cex.cor, ...)
{
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  # correlation coefficient
  r <- cor(x, y)
  txt <- format(c(r, 0.123456789), digits = digits)[1]
  txt <- paste("r= ", txt, sep = "")
  text(0.5, 0.6, txt)
  
  # p-value calculation
  p <- cor.test(x, y)$p.value
  txt2 <- format(c(p, 0.123456789), digits = digits)[1]
  txt2 <- paste("p= ", txt2, sep = "")
  if(p<0.01) txt2 <- paste("p= ", "<0.01", sep = "")
  text(0.5, 0.4, txt2)
}

pairs(iris, upper.panel = panel.cor)
