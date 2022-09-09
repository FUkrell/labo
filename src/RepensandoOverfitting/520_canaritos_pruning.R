#limpio la memoria
rm( list=ls() )  #remove all objects
gc()             #garbage collection

require("data.table")
require("rpart")
require("rpart.plot")

setwd("C:\\Users\\PC\\Desktop\\Especializacion UBA\\DMEyF" )  #establezco la carpeta donde voy a trabajar
#cargo el dataset
dataset  <- fread( "./datasets/competencia1_2022.csv")

#uso esta semilla para los canaritos
set.seed(100003)
#Creo algunas features
dataset[ , campo1 := as.integer( (mcaja_ahorro<141.47) & ( mtarjeta_visa_consumo<1553.0 ) ) ]
dataset[ , campo2 := as.integer( (mcaja_ahorro<141.47 | is.na(mcaja_ahorro)) & ( mtarjeta_visa_consumo<1553.0  ) & ( mtarjeta_master_consumo<547.01 | is.na(mtarjeta_master_consumo) ) ) ]
dataset[ , campo3 := as.integer( (mcaja_ahorro<141.47 | is.na(mcaja_ahorro)) & ( mtarjeta_visa_consumo<1553.0  ) & ( mtarjeta_master_consumo>=547.01  ) ) ]
dataset[ , campo4 := as.integer( (mcaja_ahorro>=141.47 | is.na(mcaja_ahorro)) & ( mtarjeta_visa_consumo<2003.7) & ( mpasivos_margen<231.5  ) ) ]
dataset[ , campo5 := as.integer( (mcaja_ahorro>=141.47 | is.na(mcaja_ahorro)) & ( mtarjeta_visa_consumo<2003.7) & ( mpasivos_margen<231.5 | is.na(mpasivos_margen) ) )  ]
dataset[ , campo6 := as.integer( (mcaja_ahorro>=141.47 | is.na(mcaja_ahorro)) & ( mtarjeta_visa_consumo>=2003.7 | is.na(mtarjeta_visa_consumo) ) & ( cpayroll_trx<1  ) ) ]
dataset[ , campo7 := as.integer( (mcaja_ahorro>=141.47 | is.na(mcaja_ahorro)) & ( mtarjeta_visa_consumo>=2003.7 | is.na(mtarjeta_visa_consumo) ) & ( cpayroll_trx>=1 | is.na(cpayroll_trx) ) ) ]
dataset[, Tienempayroll:=as.integer(mpayroll==0)]
dataset[, Tienempayroll2:=as.integer(mpayroll2==0)]
colnames<-colnames(dataset)
Lista_m <- colnames[colnames %like% "^m"]

Lista_t <- colnames[colnames %like% "^t"]## var_x vector que tiene las variables que empiezan con m

dataset[ , suma_m := rowSums(.SD), .SDcols = Lista_m ]

dataset[ , suma_t := rowSums(.SD), .SDcols = Lista_t ]
Lista_visa <- colnames[colnames %like% "^Visa"]

Lista_master <- colnames[colnames %like% "^Master"]## var_x vector que tiene las variables que empiezan con m

dataset[ , suma_visa := rowSums(.SD), .SDcols = Lista_visa ]

dataset[ , suma_master := rowSums(.SD), .SDcols = Lista_master ]
#agrego 30 canaritos
for( i in 1:30 ) dataset[ , paste0("canarito", i ) :=  runif( nrow(dataset)) ]

dtrain <- dataset[ foto_mes==202101 ]
dapply <- dataset[ foto_mes==202103 ]

#Primero  veo como quedan mis arboles
modelo_original <- rpart(
    formula= "clase_ternaria ~ . -mcomisiones_mantenimiento -Visa_mpagado",
    data= dtrain,
    model= TRUE,
    xval= 0,
    cp= -1,
    minsplit= 2, # dejo que crezca y corte todo lo que quiera
    minbucket= 1,
    maxdepth= 30 )

#hago el pruning de los canaritos
#haciendo un hackeo a la estructura  modelo_original$frame
# -666 es un valor arbritrariamente negativo que jamas es generado por rpart
modelo_original$frame[ modelo_original$frame$var %like% "canarito", "complexity"] <- -666
modelo_pruned  <- prune(  modelo_original, -666 )

#aplico el modelo a los datos nuevos
prediccion  <- predict( object=  modelo_pruned,
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

#Crear directorio
dir.create( "./exp/PajaritosRandom" )


for( corte  in  c( 7500, 8000, 8500, 9000, 9500, 10000, 10500, 11000 ) )
{
  #le envio a los  corte  mejores,  de mayor probabilidad de prob_SI
  dfinal[ , Predicted := 0L ]
  dfinal[ 1:corte , Predicted := 1L ]
  
  
  fwrite( dfinal[ , list(numero_de_cliente, Predicted) ], #solo los campos para Kaggle
          file= paste0( "./exp/PajaritosRandom/KAPAJ5_001_",  corte, ".csv"),
          sep=  "," )
}

pdf(file = "./work/stopping_at_canaritos3.pdf", width=28, height=4)
prp(modelo_pruned, extra=101, digits=5, branch=1, type=4, varlen=0, faclen=0)
dev.off()

