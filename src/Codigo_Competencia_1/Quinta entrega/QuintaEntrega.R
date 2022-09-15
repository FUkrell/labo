#Aplicacion de los mejores hiperparametros encontrados en una bayesiana
#Utilizando clase_binaria =  [  SI = { "BAJA+1", "BAJA+2"} ,  NO="CONTINUA ]
#limpio la memoria
rm( list=ls() )  #remove all objects
gc()             #garbage collection
#cargo las librerias que necesito
require("data.table")
require("rpart")
require("rpart.plot")


#Aqui se debe poner la carpeta de la materia de SU computadora local
setwd("C:\\Users\\PC\\Desktop\\Especializacion UBA\\DMEyF")  #Establezco el Working Directory

#cargo el dataset
dataset  <- fread("./datasets/competencia1_2022.csv" )


#creo la clase_binaria SI={ BAJA+1, BAJA+2 }    NO={ CONTINUA }
dataset[ foto_mes==202101, 
         clase_binaria :=  ifelse( clase_ternaria=="CONTINUA", "NO", "SI" ) ]

#Feature engineering
#dataset[ , campo1 := as.integer( (mcaja_ahorro<141.47) & ( mtarjeta_visa_consumo<1553.0 ) ) ]
#dataset[ , campo2 := as.integer( (mcaja_ahorro<141.47 | is.na(mcaja_ahorro)) & ( mtarjeta_visa_consumo<1553.0  ) & ( mtarjeta_master_consumo<547.01 | is.na(mtarjeta_master_consumo) ) ) ]
#dataset[ , campo3 := as.integer( (mcaja_ahorro<141.47 | is.na(mcaja_ahorro)) & ( mtarjeta_visa_consumo<1553.0  ) & ( mtarjeta_master_consumo>=547.01  ) ) ]
#dataset[ , campo4 := as.integer( (mcaja_ahorro>=141.47 | is.na(mcaja_ahorro)) & ( mtarjeta_visa_consumo<2003.7) & ( mpasivos_margen<231.5  ) ) ]
#dataset[ , campo5 := as.integer( (mcaja_ahorro>=141.47 | is.na(mcaja_ahorro)) & ( mtarjeta_visa_consumo<2003.7) & ( mpasivos_margen<231.5 | is.na(mpasivos_margen) ) )  ]
#dataset[ , campo6 := as.integer( (mcaja_ahorro>=141.47 | is.na(mcaja_ahorro)) & ( mtarjeta_visa_consumo>=2003.7 | is.na(mtarjeta_visa_consumo) ) & ( cpayroll_trx<1  ) ) ]
#dataset[ , campo7 := as.integer( (mcaja_ahorro>=141.47 | is.na(mcaja_ahorro)) & ( mtarjeta_visa_consumo>=2003.7 | is.na(mtarjeta_visa_consumo) ) & ( cpayroll_trx>=1 | is.na(cpayroll_trx) ) ) ]
#dataset[, Tienempayroll:=as.integer(mpayroll==0)]
#dataset[, Tienempayroll2:=as.integer(mpayroll2==0)]

#Creo unas variables segun como arranca el feature, no uso la c por que se complica
colnames<-colnames(dataset)
Lista_m <- colnames[colnames %like% "^m"]

Lista_t <- colnames[colnames %like% "^t"]## var_x vector que tiene las variables que empiezan con m

dataset[ , suma_m := rowSums(.SD), .SDcols = Lista_m ]

dataset[ , suma_t := rowSums(.SD), .SDcols = Lista_t ]

#------------------------------------------------------------------------------



dtrain  <- dataset[ foto_mes==202101 ]  #defino donde voy a entrenar
dapply  <- dataset[ foto_mes==202103 ]  #defino donde voy a aplicar el modelo


# Entreno el modelo
# obviamente rpart no puede ve  clase_ternaria para predecir  clase_binaria
#  #no utilizo Visa_mpagado ni  mcomisiones_mantenimiento por drifting

modelo  <- rpart(formula=   "clase_binaria ~ . -clase_ternaria",
                 data=      dtrain,  #los datos donde voy a entrenar
                 xval=         5,
                 cp=          -0.2,#  -0.89
                 minsplit=  1156,   # 621
                 minbucket=  117,   # 309
                 maxdepth=     7 )  #  12


#----------------------------------------------------------------------------
# habilitar esta seccion si el Fiscal General  Alejandro Bolaños  lo autoriza
#----------------------------------------------------------------------------

# corrijo manualmente el drifting de  Visa_fultimo_cierre
# dapply[ Visa_fultimo_cierre== 1, Visa_fultimo_cierre :=  4 ]
# dapply[ Visa_fultimo_cierre== 7, Visa_fultimo_cierre := 11 ]
# dapply[ Visa_fultimo_cierre==21, Visa_fultimo_cierre := 25 ]
# dapply[ Visa_fultimo_cierre==14, Visa_fultimo_cierre := 18 ]
# dapply[ Visa_fultimo_cierre==28, Visa_fultimo_cierre := 32 ]
# dapply[ Visa_fultimo_cierre==35, Visa_fultimo_cierre := 39 ]
# dapply[ Visa_fultimo_cierre> 39, Visa_fultimo_cierre := Visa_fultimo_cierre + 4 ]

# corrijo manualmente el drifting de  Visa_fultimo_cierre
# dapply[ Master_fultimo_cierre== 1, Master_fultimo_cierre :=  4 ]
# dapply[ Master_fultimo_cierre== 7, Master_fultimo_cierre := 11 ]
# dapply[ Master_fultimo_cierre==21, Master_fultimo_cierre := 25 ]
# dapply[ Master_fultimo_cierre==14, Master_fultimo_cierre := 18 ]
# dapply[ Master_fultimo_cierre==28, Master_fultimo_cierre := 32 ]
# dapply[ Master_fultimo_cierre==35, Master_fultimo_cierre := 39 ]
# dapply[ Master_fultimo_cierre> 39, Master_fultimo_cierre := Master_fultimo_cierre + 4 ]


#aplico el modelo a los datos nuevos
prediccion  <- predict( object=  modelo,
                        newdata= dapply,
                        type = "prob")

#prediccion es una matriz con DOS columnas, llamadas "NO", "SI"
#cada columna es el vector de probabilidades 

#agrego a dapply una columna nueva que es la probabilidad de BAJA+2
dfinal  <- copy( dapply[ , list(numero_de_cliente) ] )
dfinal[ , prob_SI := prediccion[ , "SI"] ]


# por favor cambiar por una semilla propia
# que sino el Fiscal General va a impugnar la prediccion
set.seed(100003)  
dfinal[ , azar := runif( nrow(dapply) ) ]

# ordeno en forma descentente, y cuando coincide la probabilidad, al azar
setorder( dfinal, -prob_SI, azar )

dir.create( "./exp/" )
dir.create( "./exp/QuintaEntrega" )


for( corte  in  c( 7500, 8000, 8500, 9000, 9500, 10000, 10500, 11000 ) )
{
  #le envio a los  corte  mejores,  de mayor probabilidad de prob_SI
  dfinal[ , Predicted := 0L ]
  dfinal[ 1:corte , Predicted := 1L ]


  fwrite( dfinal[ , list(numero_de_cliente, Predicted) ], #solo los campos para Kaggle
           file= paste0( "./exp/QuintaEntrega/KA5300_001_",  corte, ".csv"),
           sep=  "," )
}

