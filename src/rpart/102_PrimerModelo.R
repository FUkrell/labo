#cargo las librerias que necesito
require("data.table")
require("rpart")
require("rpart.plot")
#Aqui se debe poner la carpeta de la materia de SU computadora local
setwd("D:\\Maestria\\DMEyF\\")  
dataset  <- fread("./datasets/competencia1_2022.csv")
dtrain  <- dataset[ foto_mes==202101 ]  #defino donde voy a entrenar
dapply  <- dataset[ foto_mes==202103 ]  #defino donde voy a aplicar el modelo
modelo  <- rpart(formula=    "clase_ternaria ~ .", #quiero predecir clase_ternaria a partir de el resto de las variables
                 data =      dtrain,  #los datos donde voy a entrenar
                 xval=       0,
                 cp=        -0.3, #esto significa no limitar la complejidad de los splits
                 minsplit= 100,   #minima cantidad de registros para que se haga el split
                 minbucket=  1,   #tamaño minimo de una hoja
                 maxdepth=  10 )  #PRUEBO  una profundidad de 10

prp(modelo, extra=101, digits=5, branch=1, type=4, varlen=0, faclen=0)

prediccion  <- predict( modelo, dapply , type = "prob") #aplico el modelo
dapply[ , prob_baja2 := prediccion[, "BAJA+2"] ]
dapply[ , Predicted  := as.numeric(prob_baja2 > 1/40) ]
dir.create( "./exp/KA2002" ) 
fwrite( dapply[ , list(numero_de_cliente, Predicted) ], #solo los campos para Kaggle
        file= "./exp/KA2002/KA2002_001.csv", 
        sep= "," )