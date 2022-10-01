# para correr el Google Cloud
#   8 vCPU
#  64 GB memoria RAM
# 256 GB espacio en disco

# son varios archivos, subirlos INTELIGENTEMENTE a Kaggle

#limpio la memoria
rm( list=ls() )  #remove all objects
gc()             #garbage collection

require("data.table")
require("lightgbm")


#defino los parametros de la corrida, en una lista, la variable global  PARAM
#  muy pronto esto se leera desde un archivo formato .yaml
PARAM <- list()
PARAM$experimento  <- "KA7242_2"

PARAM$input$dataset       <- "./datasets/competencia2_2022.csv.gz"
PARAM$input$training      <- c( 202103 )
PARAM$input$future        <- c( 202105 )

PARAM$finalmodel$max_bin           <-     80
PARAM$finalmodel$max_depth         <-     6
PARAM$finalmodel$learning_rate     <-    0.0445824122395491
PARAM$finalmodel$num_iterations    <-    461
PARAM$finalmodel$num_leaves        <-   401
PARAM$finalmodel$min_data_in_leaf  <-   6177
PARAM$finalmodel$feature_fraction  <-     0.796566367624373
PARAM$finalmodel$semilla           <- 1000019

#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
#Aqui empieza el programa
setwd( "C:\\Users\\PC\\Desktop\\Especializacion UBA\\DMEyF" )

#cargo el dataset donde voy a entrenar
dataset  <- fread(PARAM$input$dataset, stringsAsFactors= TRUE)




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

negativos <- c()
otros <- c()

rankear<- c('suma_m','suma_t','mcuenta_corriente','cdescubierto_preacordado','mcuentas_saldo','mcuentas_saldo')
for (campo in rankear) {
#Si algún campo falla en el if lo mando a la lista otros Creditos a Nicolas Nuñez Manzano por el armado del codigo que separa en neg y pos con rankeo
  tryCatch(
    {
     # verifico que el campo sea de valores positivos
      if (min(dataset[, get(campo)]) >= 0) {
        dataset[, paste0("auto_r_", campo, sep = "") := (frankv(dataset, cols = campo) - 1) / (length(dataset[, get(campo)]) - 1)] # rankeo entre 0 y 1
        dataset[, paste0(campo) := NULL] # elimino la variable original
      } else {
        negativos <- c(negativos, campo)
      }
    },
    # por si algun campo no tiene valor minimo o hay un typo en la lista
    error = function(e) {
      otros <- c(otros, campo)
    }
  )
}




#--------------------------------------

#paso la clase a binaria que tome valores {0,1}  enteros
#set trabaja con la clase  POS = { BAJA+1, BAJA+2 } 
#esta estrategia es MUY importante
dataset[ , clase01 := ifelse( clase_ternaria %in%  c("BAJA+2","BAJA+1"), 1L, 0L) ]

#--------------------------------------

#los campos que se van a utilizar
campos_buenos  <- setdiff( colnames(dataset), c("clase_ternaria","clase01") )

#--------------------------------------


#establezco donde entreno
dataset[ , train  := 0L ]
dataset[ foto_mes %in% PARAM$input$training, train  := 1L ]

#--------------------------------------
#creo las carpetas donde van los resultados
#creo la carpeta donde va el experimento
# HT  representa  Hiperparameter Tuning
dir.create( "./exp/",  showWarnings = FALSE ) 
dir.create( paste0("./exp/", PARAM$experimento, "/" ), showWarnings = FALSE )
setwd( paste0("./exp/", PARAM$experimento, "/" ) )   #Establezco el Working Directory DEL EXPERIMENTO



#dejo los datos en el formato que necesita LightGBM
dtrain  <- lgb.Dataset( data= data.matrix(  dataset[ train==1L, campos_buenos, with=FALSE]),
                        label= dataset[ train==1L, clase01] )

#genero el modelo
#estos hiperparametros  salieron de una laaarga Optmizacion Bayesiana
modelo  <- lgb.train( data= dtrain,
                      param= list( objective=          "binary",
                                   max_bin=            PARAM$finalmodel$max_bin,
                                   max_depth =         PARAM$finalmodel$max_depth 
                                   learning_rate=      PARAM$finalmodel$learning_rate,
                                   num_iterations=     PARAM$finalmodel$num_iterations,
                                   num_leaves=         PARAM$finalmodel$num_leaves,
                                   min_data_in_leaf=   PARAM$finalmodel$min_data_in_leaf,
                                   feature_fraction=   PARAM$finalmodel$feature_fraction,
                                   seed=               PARAM$finalmodel$semilla
                                  )
                    )

#--------------------------------------
#ahora imprimo la importancia de variables
tb_importancia  <-  as.data.table( lgb.importance(modelo) ) 
archivo_importancia  <- "impo.txt"

fwrite( tb_importancia, 
        file= archivo_importancia, 
        sep= "\t" )

#--------------------------------------


#aplico el modelo a los datos sin clase
dapply  <- dataset[ foto_mes== PARAM$input$future ]

#aplico el modelo a los datos nuevos
prediccion  <- predict( modelo, 
                        data.matrix( dapply[, campos_buenos, with=FALSE ])                                 )

#genero la tabla de entrega
tb_entrega  <-  dapply[ , list( numero_de_cliente, foto_mes ) ]
tb_entrega[  , prob := prediccion ]

#grabo las probabilidad del modelo
fwrite( tb_entrega,
        file= "prediccion.txt",
        sep= "\t" )

#ordeno por probabilidad descendente
setorder( tb_entrega, -prob )


#genero archivos con los  "envios" mejores
#deben subirse "inteligentemente" a Kaggle para no malgastar submits
cortes <- seq( 5000, 12000, by=500 )
for( envios  in  cortes )
{
  tb_entrega[  , Predicted := 0L ]
  tb_entrega[ 1:envios, Predicted := 1L ]

  fwrite( tb_entrega[ , list(numero_de_cliente, Predicted)], 
          file= paste0(  PARAM$experimento, "_", envios, ".csv" ),
          sep= "," )
}

#--------------------------------------

quit( save= "no" )

