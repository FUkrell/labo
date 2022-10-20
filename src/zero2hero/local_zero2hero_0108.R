rm( list=ls())
gc()

library( "data.table")   #cargo la libreria  data.table
library( "rpart")  #cargo la libreria  rpart
library( "rpart.plot")
library("caret")

options(repr.plot.width=20, repr.plot.height=10) 
setwd("D:\\Maestria\\DMEyF\\")  #Aqui se debe poner la ruta de la PC local

dataset <- fread("./datasets/competencia1_2022.csv")   #cargo el dataset #cargo el dataset
denero <- dataset[ foto_mes==202101 ]

set.seed(13) # seteo semilla para replicabilidad
train_rows <- createDataPartition(denero$clase_ternaria, p= 0.50, list= FALSE)
head(train_rows)

# creo dataset de training y test

dtrain <- denero[ train_rows]
dtest <- denero[ -train_rows]

#compruebo la division
nrow( dtrain)
nrow( dtest )
nrow( dtrain) + nrow(dtest)
nrow( denero)

# compruebo que la particion es estratificada

denero[  , .N, clase_ternaria]
dtrain[  , .N, clase_ternaria]
dtest[  , .N, clase_ternaria]

param  <- list("cp"= -0.5,
               "minsplit"=  900,
               "minbucket"= 440,
               "maxdepth"= 5
)

#genero el modelo
modelo <-  rpart::rpart(formula= "clase_ternaria ~ ." ,
                        data= dtrain,
                        xval= 0,
                        control= param)

prediccion  <- predict( modelo, dtest, type = "prob")

prob_baja2  <- prediccion[, "BAJA+2"]

# hago el calculo de ganancia
ganancia_testing <- dtest[ , sum(  (prob_baja2>0.025) * ifelse( clase_ternaria=="BAJA+2", 78000, -2000) )]
ganancia_testing # esta ganancia esta calculada sobre los datos de testing
# que son el 30%, si quiero extrapolar 
ganancia_testing_normalizada  <-  ganancia_testing/0.3
ganancia_testing_normalizada

Predicted   <- ifelse( prob_baja2 > 0.025, 1, 0 )  # 0.025 es la probabilidad de corte

entrega  <-  as.data.table( list( "numero_de_cliente"=dapply$numero_de_cliente, "Predicted"=Predicted)  )

#creo la carpeta donde va el experimento
dir.create( "./exp/",  showWarnings = FALSE ) 
dir.create( "./exp/ZH2018/", showWarnings = FALSE )

#genero el archivo para Kaggle
fwrite( entrega, 
        file= "./exp/ZH2018/para_Kaggle_0108.csv",
        sep=  "," )

# da una ganancia de 21959.55065