#Arbol elemental con libreria  rpart
#Debe tener instaladas las librerias  data.table  ,  rpart  y  rpart.plot
rm( list=ls() )  #Borro todos los objetos
gc()   #Garbage Collection
#cargo las librerias que necesito
require("data.table")
require("rpart")
require("rpart.plot")
#------------------------------------------------------------------------------
#particionar agrega una columna llamada fold a un dataset que consiste en una particion estratificada segun agrupa
# particionar( data=dataset, division=c(70,30), agrupa=clase_ternaria, seed=semilla)   crea una particion 70, 30 
particionar  <- function( data,  division, agrupa="",  campo="fold", start=1, seed=NA )
{
  if( !is.na(100003) )   set.seed( 100003 )

  bloque  <- unlist( mapply(  function(x,y) { rep( y, x )} ,   division,  seq( from=start, length.out=length(division) )  ) )  

  data[ , (campo) :=  sample( rep( bloque, ceiling(.N/length(bloque))) )[1:.N],
          by= agrupa ]
}
#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
#Aqui se debe poner la carpeta de la materia de SU computadora local
setwd("C:\\Users\\PC\\Desktop\\Especializacion UBA\\DMEyF\\")  #Establezco el Working Directory

#cargo el dataset
dataset  <- fread("./datasets/competencia1_2022.csv")






#------------------------------------------------------------------------------
#Feature engineering
dataset[ , campo1 := as.integer( (mcaja_ahorro<141.47) & ( mtarjeta_visa_consumo<1553.0 ) ) ]
dataset[ , campo2 := as.integer( (mcaja_ahorro<141.47 | is.na(mcaja_ahorro)) & ( mtarjeta_visa_consumo<1553.0  ) & ( mtarjeta_master_consumo<547.01 | is.na(mtarjeta_master_consumo) ) ) ]
dataset[ , campo3 := as.integer( (mcaja_ahorro<141.47 | is.na(mcaja_ahorro)) & ( mtarjeta_visa_consumo<1553.0  ) & ( mtarjeta_master_consumo>=547.01  ) ) ]
dataset[ , campo4 := as.integer( (mcaja_ahorro>=141.47 | is.na(mcaja_ahorro)) & ( mtarjeta_visa_consumo<2003.7) & ( mpasivos_margen<231.5  ) ) ]
dataset[ , campo5 := as.integer( (mcaja_ahorro>=141.47 | is.na(mcaja_ahorro)) & ( mtarjeta_visa_consumo<2003.7) & ( mpasivos_margen<231.5 | is.na(mpasivos_margen) ) )  ]
dataset[ , campo6 := as.integer( (mcaja_ahorro>=141.47 | is.na(mcaja_ahorro)) & ( mtarjeta_visa_consumo>=2003.7 | is.na(mtarjeta_visa_consumo) ) & ( cpayroll_trx<1  ) ) ]
dataset[ , campo7 := as.integer( (mcaja_ahorro>=141.47 | is.na(mcaja_ahorro)) & ( mtarjeta_visa_consumo>=2003.7 | is.na(mtarjeta_visa_consumo) ) & ( cpayroll_trx>=1 | is.na(cpayroll_trx) ) ) ]
#------------------------------------------------------------------------------
dtrain  <- dataset[ foto_mes==202101 ]  #defino donde voy a entrenar
dapply  <- dataset[ foto_mes==202103 ]  #defino donde voy a aplicar el modelo
#particiono estratificadamente el dataset
#Cambiar por la primer semilla de cada uno !
particionar( dtrain, division=c(7,3), agrupa="clase_ternaria", seed= 100003 )
#genero el modelo,  aqui se construye el arbol
modelo  <- rpart(formula=   "clase_ternaria ~ .",  #quiero predecir clase_ternaria a partir de el resto de las variables
                 data=      dtrain[fold==1],
                 xval=0,  #los datos donde voy a entrenar
                 cp=       -0.60,   #esto significa no limitar la complejidad de los splits
                 minsplit=  456,     #minima cantidad de registros para que se haga el split
                 minbucket= 9,     #tamaño minimo de una hoja
                 maxdepth=  5 )    #profundidad maxima del arbol


#grafico el arbol
prp(modelo, extra=101, digits=5, branch=1, type=4, varlen=0, faclen=0)
#aplico el modelo a los datos train
prediccion  <- predict( object= modelo,
                        newdata= dtrain[ fold==2],
                        type = "prob")

#prediccion es una matriz con TRES columnas, llamadas "BAJA+1", "BAJA+2"  y "CONTINUA"
#cada columna es el vector de probabilidades 

#agrego una columna que es la de las ganancias
dtrain[  , ganancia :=  ifelse( clase_ternaria=="BAJA+2", 78000, -2000 ) ]

#para testing agrego la probabilidad
dtrain[ fold==2 , prob_baja2 := prediccion[, "BAJA+2"] ]

#calculo la ganancia en testing  qu es fold==2
ganancia_test  <- dtrain[ fold==2 & prob_baja2 >  0.025, sum(ganancia) ]

#escalo la ganancia como si fuera todo el dataset
ganancia_test_normalizada  <-  ganancia_test / 0.3

estimulos  <- dtrain[ fold==2 & prob_baja2 > 0.025 , .N ]
aciertos   <- dtrain[ fold==2 & prob_baja2 > 0.025 & clase_ternaria =="BAJA+2", .N ]


cat( "Testing total: ",  dtrain[ fold==2, .N ], "\n" )
cat( "Testing BAJA+2: ", dtrain[ fold==2 & clase_ternaria =="BAJA+2", .N ], "\n" )

cat( "Estimulos: ", estimulos, "\n" )
cat( "Aciertos (BAJA+2): ",  aciertos,  "\n" )

cat( "Ganancia en testing (normalizada): ", ganancia_test_normalizada, "\n" )

#Elimino la división de train y test.
dtrain  <- dtrain[,fold:=NULL]
dtrain  <- dtrain[,ganancia:=NULL]
dtrain  <- dtrain[,prob_baja2:=NULL]


#genero el modelo,  aqui se construye el arbol con todos los datos
modelo  <- rpart(formula=   "clase_ternaria ~ .",  #quiero predecir clase_ternaria a partir de el resto de las variables
                 data=      dtrain,
                 xval=0,  #los datos donde voy a entrenar
                 cp=       -0.60,   #esto significa no limitar la complejidad de los splits
                 minsplit=  456,     #minima cantidad de registros para que se haga el split
                 minbucket= 9,     #tamaño minimo de una hoja
                 maxdepth=  5 )    #profundidad maxima del arbol



#aplico el modelo a los datos a predecir.
prediccion  <- predict( object= modelo,
                        newdata= dapply,
                        type = "prob")
#agrego a dapply una columna nueva que es la probabilidad de BAJA+2
dapply[ , prob_baja2 := prediccion[, "BAJA+2"] ]

#solo le envio estimulo a los registros con probabilidad de BAJA+2 mayor  a  1/40
dapply[ , Predicted := as.numeric( prob_baja2 > 1/40 ) ]
estimulos  <- dapply[ Predicted==1 , .N ]
noestimulos<- dapply[ Predicted==0 , .N ]
cat( "Estimulos: ", estimulos, "\n" )
cat( "No Estimulos: ", noestimulos, "\n" )
cat( "Proporcion enviada: ", estimulos/(estimulos+noestimulos), "\n" )
#genero el archivo para Kaggle

fwrite( dapply[ , list(numero_de_cliente, Predicted) ], #solo los campos para Kaggle
        file= "./exp/KA2005/K101_005_2.csv",
        sep=  "," )


