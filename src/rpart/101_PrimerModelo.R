#Arbol elemental con libreria  rpart
#Debe tener instaladas las librerias  data.table  ,  rpart  y  rpart.plot

#cargo las librerias que necesito
require("data.table")
require("rpart")
require("rpart.plot")
library(ggplot2)

#Aqui se debe poner la carpeta de la materia de SU computadora local
setwd("D:\\Maestria\\DMEyF\\")  #Establezco el Working Directory

#cargo el dataset
dataset  <- fread("./datasets/competencia1_2022.csv")
nrow( dataset )
ncol( dataset )
dataset[ , .N ]
dataset[ , .N, foto_mes ]
colnames( dataset )

# exploracion clase ternaria
dataset[  , .N, list( foto_mes, clase_ternaria) ]

# Defino  el data.table  dfuturo que va a tener solo los datos de  202103
dfuturo <-  dataset[ foto_mes==202103 ]
nrow(dfuturo)
#creo vector de ids
vector_ids  <-   dfuturo[ , numero_de_cliente]
head(vector_ids)
#creo un vector de todos 1, con la cant de registros que tiene dfuturo
vector_enviar <-  rep( 1,  nrow( dfuturo))
head(vector_enviar)
length(vector_enviar)
#genero tabla para enviar a kaggle
tabla_final  <-   as.data.table(  list(  "numero_de_cliente"= vector_ids,
                                         "Predicted"=         vector_enviar))
head(tabla_final)
#genero el archivo para Kaggle
#creo la carpeta donde va el experimento
dir.create( "./exp/",  showWarnings = FALSE ) 
dir.create( "./exp/ZH2016/", showWarnings = FALSE )

#genero el archivo para Kaggle
fwrite(tabla_final, 
       file= "./exp/ZH2016/todos_unos.csv",
       sep=  "," )
# me da una ganancia de -249001.57148, los BAJA+2 para el public leaderboard son 
# 960 (202103), los baja +2 202101 son 850


#contar los baja+2
dataset[ clase_ternaria=="BAJA+2", .N ] 

#la prop de baja+2
dataset[ foto_mes==202101  ,  sum(clase_ternaria=="BAJA+2")/.N]

dataset[ foto_mes==202101 & ctrx_quarter < 20  ,  sum(clase_ternaria=="BAJA+2")/.N]

dataset[ foto_mes==202101 & ctrx_quarter < 20  ,  sum(clase_ternaria=="BAJA+2")/.N]  /dataset[ foto_mes==202101  ,  sum(clase_ternaria=="BAJA+2")/.N]

#ganancia del dataset
dataset[ foto_mes==202101, ganancia := -2000]
dataset[ foto_mes==202101 & clase_ternaria=="BAJA+2", ganancia := 78000]

#si envio estimulo a todos los clientes esta es la ganancia
dataset[ foto_mes==202101 , sum(ganancia)]

# calculo de ganancia para predicados simples
dataset[ foto_mes==202101 & ctrx_quarter < 20,  sum( ganancia )  ]
dataset[ foto_mes==202101 & ctrx_quarter < 4,  sum( ganancia )  ]

campo <- "mcaja_ahorro" 
options(repr.plot.width=15, repr.plot.height=15)
ggplot(dataset[ foto_mes==202101] , aes_string(x = campo)) + geom_density(trim=TRUE, na.rm=TRUE) + facet_grid( "clase_ternaria~ .")

dtrain  <- dataset[ foto_mes==202101 ]  #defino donde voy a entrenar
dapply  <- dataset[ foto_mes==202103 ]  #defino donde voy a aplicar el modelo

modelo <-  rpart(  formula= "clase_ternaria ~ ." ,
                   data= dtrain,
                   xval= 0,
                   cp= -1,
                   maxdepth= 2)

#importancia de features
vector_importance <- names(modelo$variable.importance)
vector_importance
#imprimo el modelo graficamente
options(repr.plot.width=20, repr.plot.height=10) 
prp(modelo, extra=101, digits=5, branch=1, type=4, varlen=0, faclen=0, tweak=1.1, cex = 1.2)

prediccion  <- predict( modelo, dapply, type = "prob")
head( prediccion )
prob_baja2  <- prediccion[, "BAJA+2"]
head( prob_baja2)
length(prob_baja2)
nrow(dapply)
# ahora decido si envio el estimulo o no si prob(baja+2) > 0.025 envio el estimulo
Predicted   <- ifelse( prob_baja2 > 0.025, 1, 0 )
head(Predicted)
hist(Predicted)
# creo tabla para enviar a kaggle
entrega  <-  as.data.table( list( "numero_de_cliente"=dapply$numero_de_cliente, "Predicted"=Predicted)  )
head(entrega)
entrega[ , .N, Predicted]
#genero el archivo para Kaggle
#creo la carpeta donde va el experimento
dir.create( "./exp/",  showWarnings = FALSE ) 
dir.create( "./exp/ZH2017/", showWarnings = FALSE )
# me genera ganancia de 14333.04004

fwrite( entrega, 
        file= "./exp/ZH2017/para_Kaggle_0107.csv",
        sep=  "," )


dataset <- dataset[,clase_binaria:=ifelse(clase_ternaria=="CONTINUA",
                               "neg",
                               "pos")]




modelo  <- rpart(formula=   "foto_mes ~ . -clase_ternaria ",  #quiero predecir clase_ternaria a partir de el resto de las variables
                 data=      dataset,  #los datos donde voy a entrenar
                 xval=      0,
                 cp=       -0.3,   #esto significa no limitar la complejidad de los splits
                 minsplit=  0,     #minima cantidad de registros para que se haga el split
                 minbucket= 1,     #tamaño minimo de una hoja
                 maxdepth=  4 )    #profundidad maxima del arbol


#genero el modelo,  aqui se construye el arbol
modelo  <- rpart(formula=   "clase_binaria ~ . -clase_ternaria",  #quiero predecir clase_ternaria a partir de el resto de las variables
                 data=      dtrain,  #los datos donde voy a entrenar
                 xval=      0,
                 cp=       -0.3,   #esto significa no limitar la complejidad de los splits
                 minsplit=  0,     #minima cantidad de registros para que se haga el split
                 minbucket= 1,     #tamaño minimo de una hoja
                 maxdepth=  4 )    #profundidad maxima del arbol


#grafico el arbol
prp(modelo, extra=101, digits=5, branch=1, type=4, varlen=0, faclen=0, cex=0.8)



#aplico el modelo a los datos nuevos
prediccion  <- predict( object= modelo,
                        newdata= dapply,
                        type = "prob")

#prediccion es una matriz con TRES columnas, llamadas "BAJA+1", "BAJA+2"  y "CONTINUA"
#cada columna es el vector de probabilidades 

#agrego a dapply una columna nueva que es la probabilidad de BAJA+2
dapply[ , prob_baja2 := prediccion[, "BAJA+2"] ]

#solo le envio estimulo a los registros con probabilidad de BAJA+2 mayor  a  1/40
dapply[ , Predicted := as.numeric( prob_baja2 > 1/40 ) ]

#genero el archivo para Kaggle
#primero creo la carpeta donde va el experimento
dir.create( "./exp/" )
dir.create( "./exp/KA2001" )

fwrite( dapply[ , list(numero_de_cliente, Predicted) ], #solo los campos para Kaggle
        file= "./exp/KA2001/K101_001.csv",
        sep=  "," )



