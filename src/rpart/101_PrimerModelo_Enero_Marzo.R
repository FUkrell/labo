#Arbol elemental con libreria  rpart
#Debe tener instaladas las librerias  data.table  ,  rpart  y  rpart.plot
rm( list=ls() )  #remove all objects
gc()             #garbage collection
#cargo las librerias que necesito
require("data.table")
require("rpart")
require("rpart.plot")

#Aqui se debe poner la carpeta de la materia de SU computadora local
setwd("C:\\Users\\PC\\Desktop\\Especializacion UBA\\DMEyF\\")  #Establezco el Working Directory

#cargo el dataset
dataset  <- fread("./datasets/competencia1_2022.csv")





#genero el modelo,  aqui se construye el arbol
modelo  <- rpart(formula=   "foto_mes ~ .-clase_ternaria",  #quiero predecir clase_ternaria a partir de el resto de las variables
                 data=      dataset,  #los datos donde voy a entrenar
                 xval=         0,
                 cp=          -0.54,  
                 minsplit=  1073,    
                 minbucket=  278,    
                 maxdepth=     3 )    #profundidad maxima del arbol


#grafico el arbol
prp(modelo, extra=101, digits=5, branch=1, type=4, varlen=0, faclen=0)


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

