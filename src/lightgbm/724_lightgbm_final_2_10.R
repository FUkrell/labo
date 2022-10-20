<<<<<<< HEAD
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
PARAM$experimento  <- "KA7240_4"

PARAM$input$dataset       <- "./datasets/competencia2_2022.csv.gz"
PARAM$input$training      <- c( 202103 )
PARAM$input$future        <- c( 202105 )
# econtrar los optimos con BO
PARAM$finalmodel$max_bin           <-     77
PARAM$finalmodel$learning_rate     <-     0.052794583546779
PARAM$finalmodel$num_iterations    <-    160
PARAM$finalmodel$num_leaves        <-   711
PARAM$finalmodel$min_data_in_leaf  <-   4111
PARAM$finalmodel$feature_fraction  <-  0.395316213955107
PARAM$finalmodel$semilla           <- 581333 # semilla

#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
#Aqui empieza el programa
setwd( "D:\\Maestria\\DMEyF\\" )

#cargo el dataset donde voy a entrenar
dataset  <- fread(PARAM$input$dataset, stringsAsFactors= TRUE)

#--------------------------------------

#Feature engineering
# dataset[ , campo1 := as.integer( (mcaja_ahorro<141.47) & ( mtarjeta_visa_consumo<1553.0 ) ) ]
# dataset[ , campo2 := as.integer( (mcaja_ahorro<141.47 | is.na(mcaja_ahorro)) & ( mtarjeta_visa_consumo<1553.0  ) & ( mtarjeta_master_consumo<547.01 | is.na(mtarjeta_master_consumo) ) ) ]
#dataset[ , campo3 := as.integer( (mcaja_ahorro<141.47 | is.na(mcaja_ahorro)) & ( mtarjeta_visa_consumo<1553.0  ) & ( mtarjeta_master_consumo>=547.01  ) ) ]
# dataset[ , campo4 := as.integer( (mcaja_ahorro>=141.47 | is.na(mcaja_ahorro)) & ( mtarjeta_visa_consumo<2003.7) & ( mpasivos_margen<231.5  ) ) ]
#dataset[ , campo5 := as.integer( (mcaja_ahorro>=141.47 | is.na(mcaja_ahorro)) & ( mtarjeta_visa_consumo<2003.7) & ( mpasivos_margen<231.5 | is.na(mpasivos_margen) ) )  ]
#dataset[ , campo6 := as.integer( (mcaja_ahorro>=141.47 | is.na(mcaja_ahorro)) & ( mtarjeta_visa_consumo>=2003.7 | is.na(mtarjeta_visa_consumo) ) & ( cpayroll_trx<1  ) ) ]
#dataset[ , campo7 := as.integer( (mcaja_ahorro>=141.47 | is.na(mcaja_ahorro)) & ( mtarjeta_visa_consumo>=2003.7 | is.na(mtarjeta_visa_consumo) ) & ( cpayroll_trx>=1 | is.na(cpayroll_trx) ) ) ]
#dataset[, Tienempayroll:=as.integer(mpayroll==0)]
#dataset[, Tienempayroll2:=as.integer(mpayroll2==0)]



#INICIO de la seccion donde se deben hacer cambios con variables nuevas

# #creo un ctr_quarter que tenga en cuenta cuando los clientes hace 3 menos meses que estan
# dataset[  , ctrx_quarter_normalizado := ctrx_quarter ]
# dataset[ cliente_antiguedad==1 , ctrx_quarter_normalizado := ctrx_quarter * 5 ]
# dataset[ cliente_antiguedad==2 , ctrx_quarter_normalizado := ctrx_quarter * 2 ]
# dataset[ cliente_antiguedad==3 , ctrx_quarter_normalizado := ctrx_quarter * 1.2 ]
# 
# #variable extraida de una tesis de maestria de Irlanda
# dataset[  , mpayroll_sobre_edad  := mpayroll / cliente_edad ]
# 
# # # suma loca
# # dataset[ , suma_loca := sum(("columna1", "columna2")) ]
# # # binning 
# # require("Hmisc")
# # 
# # for( campo in  colnames(dataset) )
# # {
# #   if(  dataset[ , length( unique( get(campo) ) ) > 100 ] )
# #   {
# #     dataset[  , paste0( campo, "_bin" ) := as.integer( cut2(  dataset[ , get(campo) ], m=1, g=31) ) ]
# #     if(  campo !=  "numero_de_cliente" )  dataset[  , paste0( campo ) := NULL ]
# #   }
# #   cat( campo, " " )
# # }
# 
# # para corregir el data drifting entre enero y marzo
# # dapply[ ,mcuentas_saldo := mcuentas_saldo * 0.9523 ]
# 
# 
# 
# #se crean los nuevos campos para MasterCard  y Visa, teniendo en cuenta los NA's
# #varias formas de combinar Visa_status y Master_status
# dataset[ , mv_status01       := pmax( Master_status,  Visa_status, na.rm = TRUE) ]
# dataset[ , mv_status02       := Master_status +  Visa_status ]
# dataset[ , mv_status03       := pmax( ifelse( is.na(Master_status), 10, Master_status) , ifelse( is.na(Visa_status), 10, Visa_status) ) ]
# dataset[ , mv_status04       := ifelse( is.na(Master_status), 10, Master_status)  +  ifelse( is.na(Visa_status), 10, Visa_status)  ]
# dataset[ , mv_status05       := ifelse( is.na(Master_status), 10, Master_status)  +  100*ifelse( is.na(Visa_status), 10, Visa_status)  ]
# 
# dataset[ , mv_status06       := ifelse( is.na(Visa_status), 
#                                         ifelse( is.na(Master_status), 10, Master_status), 
#                                         Visa_status)  ]
# 
# dataset[ , mv_status07       := ifelse( is.na(Master_status), 
#                                         ifelse( is.na(Visa_status), 10, Visa_status), 
#                                         Master_status)  ]
# 
# 
# #combino MasterCard y Visa
# dataset[ , mv_mfinanciacion_limite := rowSums( cbind( Master_mfinanciacion_limite,  Visa_mfinanciacion_limite) , na.rm=TRUE ) ]
# dataset[ , mv_Fvencimiento         := pmin( Master_Fvencimiento, Visa_Fvencimiento, na.rm = TRUE) ]
# dataset[ , mv_Finiciomora          := pmin( Master_Finiciomora, Visa_Finiciomora, na.rm = TRUE) ]
# dataset[ , mv_msaldototal          := rowSums( cbind( Master_msaldototal,  Visa_msaldototal) , na.rm=TRUE ) ]
# dataset[ , mv_msaldopesos          := rowSums( cbind( Master_msaldopesos,  Visa_msaldopesos) , na.rm=TRUE ) ]
# dataset[ , mv_msaldodolares        := rowSums( cbind( Master_msaldodolares,  Visa_msaldodolares) , na.rm=TRUE ) ]
# dataset[ , mv_mconsumospesos       := rowSums( cbind( Master_mconsumospesos,  Visa_mconsumospesos) , na.rm=TRUE ) ]
# dataset[ , mv_mconsumosdolares     := rowSums( cbind( Master_mconsumosdolares,  Visa_mconsumosdolares) , na.rm=TRUE ) ]
# dataset[ , mv_mlimitecompra        := rowSums( cbind( Master_mlimitecompra,  Visa_mlimitecompra) , na.rm=TRUE ) ]
# dataset[ , mv_madelantopesos       := rowSums( cbind( Master_madelantopesos,  Visa_madelantopesos) , na.rm=TRUE ) ]
# dataset[ , mv_madelantodolares     := rowSums( cbind( Master_madelantodolares,  Visa_madelantodolares) , na.rm=TRUE ) ]
# dataset[ , mv_fultimo_cierre       := pmax( Master_fultimo_cierre, Visa_fultimo_cierre, na.rm = TRUE) ]
# dataset[ , mv_mpagado              := rowSums( cbind( Master_mpagado,  Visa_mpagado) , na.rm=TRUE ) ]
# dataset[ , mv_mpagospesos          := rowSums( cbind( Master_mpagospesos,  Visa_mpagospesos) , na.rm=TRUE ) ]
# dataset[ , mv_mpagosdolares        := rowSums( cbind( Master_mpagosdolares,  Visa_mpagosdolares) , na.rm=TRUE ) ]
# dataset[ , mv_fechaalta            := pmax( Master_fechaalta, Visa_fechaalta, na.rm = TRUE) ]
# dataset[ , mv_mconsumototal        := rowSums( cbind( Master_mconsumototal,  Visa_mconsumototal) , na.rm=TRUE ) ]
# dataset[ , mv_cconsumos            := rowSums( cbind( Master_cconsumos,  Visa_cconsumos) , na.rm=TRUE ) ]
# dataset[ , mv_cadelantosefectivo   := rowSums( cbind( Master_cadelantosefectivo,  Visa_cadelantosefectivo) , na.rm=TRUE ) ]
# dataset[ , mv_mpagominimo          := rowSums( cbind( Master_mpagominimo,  Visa_mpagominimo) , na.rm=TRUE ) ]
# 
# #a partir de aqui juego con la suma de Mastercard y Visa
# dataset[ , mvr_Master_mlimitecompra:= Master_mlimitecompra / mv_mlimitecompra ]
# dataset[ , mvr_Visa_mlimitecompra  := Visa_mlimitecompra / mv_mlimitecompra ]
# dataset[ , mvr_msaldototal         := mv_msaldototal / mv_mlimitecompra ]
# dataset[ , mvr_msaldopesos         := mv_msaldopesos / mv_mlimitecompra ]
# dataset[ , mvr_msaldopesos2        := mv_msaldopesos / mv_msaldototal ]
# dataset[ , mvr_msaldodolares       := mv_msaldodolares / mv_mlimitecompra ]
# dataset[ , mvr_msaldodolares2      := mv_msaldodolares / mv_msaldototal ]
# dataset[ , mvr_mconsumospesos      := mv_mconsumospesos / mv_mlimitecompra ]
# dataset[ , mvr_mconsumosdolares    := mv_mconsumosdolares / mv_mlimitecompra ]
# dataset[ , mvr_madelantopesos      := mv_madelantopesos / mv_mlimitecompra ]
# dataset[ , mvr_madelantodolares    := mv_madelantodolares / mv_mlimitecompra ]
# dataset[ , mvr_mpagado             := mv_mpagado / mv_mlimitecompra ]
# dataset[ , mvr_mpagospesos         := mv_mpagospesos / mv_mlimitecompra ]
# dataset[ , mvr_mpagosdolares       := mv_mpagosdolares / mv_mlimitecompra ]
# dataset[ , mvr_mconsumototal       := mv_mconsumototal  / mv_mlimitecompra ]
# dataset[ , mvr_mpagominimo         := mv_mpagominimo  / mv_mlimitecompra ]
# 
# #Aqui debe usted agregar sus propias nuevas variables
# #valvula de seguridad para evitar valores infinitos
# #paso los infinitos a NULOS
# infinitos      <- lapply(names(dataset),function(.name) dataset[ , sum(is.infinite(get(.name)))])
# infinitos_qty  <- sum( unlist( infinitos) )
# if( infinitos_qty > 0 )
# {
#   cat( "ATENCION, hay", infinitos_qty, "valores infinitos en tu dataset. Seran pasados a NA\n" )
#   dataset[mapply(is.infinite, dataset)] <- NA
# }
# 
# 
# #valvula de seguridad para evitar valores NaN  que es 0/0
# #paso los NaN a 0 , decision polemica si las hay
# #se invita a asignar un valor razonable segun la semantica del campo creado
# nans      <- lapply(names(dataset),function(.name) dataset[ , sum(is.nan(get(.name)))])
# nans_qty  <- sum( unlist( nans) )
# if( nans_qty > 0 )
# {
#   cat( "ATENCION, hay", nans_qty, "valores NaN 0/0 en tu dataset. Seran pasados arbitrariamente a 0\n" )
#   cat( "Si no te gusta la decision, modifica a gusto el programa!\n\n")
#   dataset[mapply(is.nan, dataset)] <- 0
# }
# 
# 
# 
# #Creo unas variables segun como arranca el feature, no uso la c por que se complica
# colnames<-colnames(dataset)
#Lista_m <- colnames[colnames %like% "^m"]
# 
# Lista_t <- colnames[colnames %like% "^t"]## var_x vector que tiene las variables que empiezan con m
# 
#dataset[ , suma_m := rowSums(.SD), .SDcols = Lista_m ]
# 
# dataset[ , suma_t := rowSums(.SD), .SDcols = Lista_t ]




rankear<- c('mcuenta_corriente','mcuentas_saldo' ,
 "mcaja_ahorro", "mcomisiones_mantenimiento", 
"mcomisiones", "Master_Fvencimiento", 
"mcomisiones_otras")
# setdiff( colnames(dataset), "clase_ternaria" ) todas las col excepto ternaria



for( campo in rankear )
{
  if(  dataset[ get(campo) < 0, .N ]  > 0 ) {
    dataset[   , paste0( campo, "_neg" ) := ifelse( get(campo)< 0, get(campo), NA ), by =dataset$foto_mes ]
    dataset[   , paste0( campo, "_pos" ) := ifelse( get(campo)> 0, get(campo), NA ), by =dataset$foto_mes ]
    dataset[, paste0(campo) := NULL] # elimino la variable original
  }}
dataset_ene  <- dataset[ foto_mes== 202101 ]
# dataset_feb  <- dataset[ foto_mes== 202102 ]
dataset_mar  <- dataset[ foto_mes== 202103 ]
# dataset_abr  <- dataset[ foto_mes== 202104 ]
dataset_may  <- dataset[ foto_mes== 202105 ]
# 
# hist(dataset$mcuentas_saldo, xlim = c(-110,20000000), breaks = 1000)
# hist(dataset$cprestamos_personales,xlim = c(0,20), breaks=300)
# hist(dataset$mprestamos_personales,xlim = c(-10,2000000), breaks=300)
# hist(dataset$mcaja_ahorro,xlim = c(-100,20000), breaks=300)
hist(dataset$cdescubierto_preacordado)

rankear<- c('mcuenta_corriente_pos','mcuentas_saldo_pos' , "mcaja_ahorro_pos", "mcomisiones_mantenimiento_pos", 
            "mcomisiones_pos",  "Master_Fvencimiento_pos", 
            "mcomisiones_otras_pos", 'mcuenta_corriente_neg','mcuentas_saldo_neg' , 
             "mcaja_ahorro_neg", "mcomisiones_mantenimiento_neg",
            "mcomisiones_neg", "Master_Fvencimiento_neg", 
            "mcomisiones_otras_neg","cliente_edad")
dataset_mar[1:3,]

for (campo in rankear) {
  #Si algún campo falla en el if lo mando a la lista otros Creditos a Nicolas Nuñez Manzano por el armado del codigo que separa en neg y pos con rankeo
    {  dataset_mar[, paste0("auto_r_", campo, sep = "") := (frankv(dataset_mar, cols = campo) - 1) / (length(dataset_mar[, get(campo)]) - 1), by =dataset_mar$foto_mes] # rankeo entre 0 y 1
        dataset_mar[, paste0(campo) := NULL] # elimino la variable original 
    }}

for (campo in rankear) {
  #Si algún campo falla en el if lo mando a la lista otros Creditos a Nicolas Nuñez Manzano por el armado del codigo que separa en neg y pos con rankeo
  {  dataset_may[, paste0("auto_r_", campo, sep = "") := (frankv(dataset_may, cols = campo) - 1) / (length(dataset_may[, get(campo)]) - 1)] # rankeo entre 0 y 1
    dataset_may[, paste0(campo) := NULL] # elimino la variable original
  }}
dataset_mar[1:3,]
dataset <- rbind(dataset_mar, dataset_may)
neg<- c( 'auto_r_mcuenta_corriente_neg','auto_r_mcuentas_saldo_neg' , 
            "auto_r_mcaja_ahorro_neg", "auto_r_mcomisiones_mantenimiento_neg",
            "auto_r_mcomisiones_neg", "auto_r_Master_Fvencimiento_neg", 
            "auto_r_mcomisiones_otras_neg")


dataset$auto_r_mcuenta_corriente_neg <- dataset$auto_r_mcuenta_corriente_neg*(-1)  
dataset$auto_r_mcuentas_saldo_neg <- dataset$auto_r_mcuentas_saldo_neg*(-1)  
dataset$auto_r_mcaja_ahorro_neg <- dataset$auto_r_mcaja_ahorro_neg*(-1)  
dataset$auto_r_mcomisiones_mantenimiento_neg <- dataset$auto_r_mcomisiones_mantenimiento_neg*(-1)  
dataset$auto_r_mcomisiones_neg <- dataset$auto_r_mcomisiones_neg*(-1) 
dataset$auto_r_Master_Fvencimiento_neg <- dataset$auto_r_Master_Fvencimiento_neg*(-1) 
dataset$auto_r_mcomisiones_otras_neg <- dataset$auto_r_mcomisiones_otras_neg*(-1) 


dataset$auto_r_mcuenta_corriente <- rowSums(cbind(dataset$auto_r_mcuenta_corriente_pos,dataset$auto_r_mcuenta_corriente_neg),na.rm=TRUE)
dataset$auto_r_mcuentas_saldo <- rowSums(cbind(dataset$auto_r_mcuentas_saldo_pos,dataset$auto_r_mcuentas_saldo_neg),na.rm=TRUE)
dataset$auto_r_mcaja_ahorro <- rowSums(cbind(dataset$auto_r_mcaja_ahorro_pos,dataset$auto_r_mcaja_ahorro_neg),na.rm=TRUE)
dataset$auto_r_mcomisiones_mantenimiento <- rowSums(cbind(dataset$auto_r_mcomisiones_mantenimiento_pos,dataset$auto_r_mcomisiones_mantenimiento_neg),na.rm=TRUE)
dataset$auto_r_mcomisiones <- rowSums(cbind(dataset$auto_r_mcomisiones_pos,dataset$auto_r_mcomisiones_neg),na.rm=TRUE)
dataset$auto_r_Master_Fvencimiento <- rowSums(cbind(dataset$auto_r_Master_Fvencimiento_pos,dataset$auto_r_Master_Fvencimiento_neg),na.rm=TRUE)
dataset$auto_r_mcomisiones_otras <- rowSums(cbind(dataset$auto_r_mcomisiones_otras_pos,dataset$auto_r_mcomisiones_otras_neg),na.rm=TRUE)


dataset[ , c('auto_r_mcuenta_corriente_pos',"auto_r_mcuenta_corriente_neg","auto_r_mcuentas_saldo_pos",
             "auto_r_mcuentas_saldo_neg", "auto_r_mcaja_ahorro_pos","auto_r_mcaja_ahorro_neg",
             "auto_r_mcomisiones_mantenimiento_pos","auto_r_mcomisiones_mantenimiento_neg",
             "auto_r_mcomisiones_pos","auto_r_mcomisiones_neg","auto_r_Master_Fvencimiento_pos",
             "auto_r_Master_Fvencimiento_neg","auto_r_mcomisiones_otras_pos", "auto_r_mcomisiones_otras_neg")] <- list(NULL)


names(dataset_mar)


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
dapy  <- dataset[ foto_mes== PARAM$input$train ]


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
=======
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
PARAM$experimento  <- "KA7240_4"

PARAM$input$dataset       <- "./datasets/competencia2_2022.csv.gz"
PARAM$input$training      <- c( 202103 )
PARAM$input$future        <- c( 202105 )
# econtrar los optimos con BO
PARAM$finalmodel$max_bin           <-     77
PARAM$finalmodel$learning_rate     <-     0.052794583546779
PARAM$finalmodel$num_iterations    <-    160
PARAM$finalmodel$num_leaves        <-   711
PARAM$finalmodel$min_data_in_leaf  <-   4111
PARAM$finalmodel$feature_fraction  <-  0.395316213955107
PARAM$finalmodel$semilla           <- 581333 # semilla

#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
#Aqui empieza el programa
setwd( "D:\\Maestria\\DMEyF\\" )

#cargo el dataset donde voy a entrenar
dataset  <- fread(PARAM$input$dataset, stringsAsFactors= TRUE)

#--------------------------------------

#Feature engineering
#INICIO de la seccion donde se deben hacer cambios con variables nuevas

# # # suma loca
# # dataset[ , suma_loca := sum(("columna1", "columna2")) ]
# # # binning 
# # require("Hmisc")
# # 
# # for( campo in  colnames(dataset) )
# # {
# #   if(  dataset[ , length( unique( get(campo) ) ) > 100 ] )
# #   {
# #     dataset[  , paste0( campo, "_bin" ) := as.integer( cut2(  dataset[ , get(campo) ], m=1, g=31) ) ]
# #     if(  campo !=  "numero_de_cliente" )  dataset[  , paste0( campo ) := NULL ]
# #   }
# #   cat( campo, " " )
# # }

# 
# 
# 
# #se crean los nuevos campos para MasterCard  y Visa, teniendo en cuenta los NA's
# #varias formas de combinar Visa_status y Master_status
# dataset[ , mv_status01       := pmax( Master_status,  Visa_status, na.rm = TRUE) ]
# dataset[ , mv_status02       := Master_status +  Visa_status ]
# dataset[ , mv_status03       := pmax( ifelse( is.na(Master_status), 10, Master_status) , ifelse( is.na(Visa_status), 10, Visa_status) ) ]
# dataset[ , mv_status04       := ifelse( is.na(Master_status), 10, Master_status)  +  ifelse( is.na(Visa_status), 10, Visa_status)  ]
# dataset[ , mv_status05       := ifelse( is.na(Master_status), 10, Master_status)  +  100*ifelse( is.na(Visa_status), 10, Visa_status)  ]
# 
# dataset[ , mv_status06       := ifelse( is.na(Visa_status), 
#                                         ifelse( is.na(Master_status), 10, Master_status), 
#                                         Visa_status)  ]
# 
# dataset[ , mv_status07       := ifelse( is.na(Master_status), 
#                                         ifelse( is.na(Visa_status), 10, Visa_status), 
#                                         Master_status)  ]
# 
# 
# #combino MasterCard y Visa
# dataset[ , mv_mfinanciacion_limite := rowSums( cbind( Master_mfinanciacion_limite,  Visa_mfinanciacion_limite) , na.rm=TRUE ) ]
# dataset[ , mv_Fvencimiento         := pmin( Master_Fvencimiento, Visa_Fvencimiento, na.rm = TRUE) ]
# dataset[ , mv_Finiciomora          := pmin( Master_Finiciomora, Visa_Finiciomora, na.rm = TRUE) ]
# dataset[ , mv_msaldototal          := rowSums( cbind( Master_msaldototal,  Visa_msaldototal) , na.rm=TRUE ) ]
# dataset[ , mv_msaldopesos          := rowSums( cbind( Master_msaldopesos,  Visa_msaldopesos) , na.rm=TRUE ) ]
# dataset[ , mv_msaldodolares        := rowSums( cbind( Master_msaldodolares,  Visa_msaldodolares) , na.rm=TRUE ) ]
# dataset[ , mv_mconsumospesos       := rowSums( cbind( Master_mconsumospesos,  Visa_mconsumospesos) , na.rm=TRUE ) ]
# dataset[ , mv_mconsumosdolares     := rowSums( cbind( Master_mconsumosdolares,  Visa_mconsumosdolares) , na.rm=TRUE ) ]
# dataset[ , mv_mlimitecompra        := rowSums( cbind( Master_mlimitecompra,  Visa_mlimitecompra) , na.rm=TRUE ) ]
# dataset[ , mv_madelantopesos       := rowSums( cbind( Master_madelantopesos,  Visa_madelantopesos) , na.rm=TRUE ) ]
# dataset[ , mv_madelantodolares     := rowSums( cbind( Master_madelantodolares,  Visa_madelantodolares) , na.rm=TRUE ) ]
# dataset[ , mv_fultimo_cierre       := pmax( Master_fultimo_cierre, Visa_fultimo_cierre, na.rm = TRUE) ]
# dataset[ , mv_mpagado              := rowSums( cbind( Master_mpagado,  Visa_mpagado) , na.rm=TRUE ) ]
# dataset[ , mv_mpagospesos          := rowSums( cbind( Master_mpagospesos,  Visa_mpagospesos) , na.rm=TRUE ) ]
# dataset[ , mv_mpagosdolares        := rowSums( cbind( Master_mpagosdolares,  Visa_mpagosdolares) , na.rm=TRUE ) ]
# dataset[ , mv_fechaalta            := pmax( Master_fechaalta, Visa_fechaalta, na.rm = TRUE) ]
# dataset[ , mv_mconsumototal        := rowSums( cbind( Master_mconsumototal,  Visa_mconsumototal) , na.rm=TRUE ) ]
# dataset[ , mv_cconsumos            := rowSums( cbind( Master_cconsumos,  Visa_cconsumos) , na.rm=TRUE ) ]
# dataset[ , mv_cadelantosefectivo   := rowSums( cbind( Master_cadelantosefectivo,  Visa_cadelantosefectivo) , na.rm=TRUE ) ]
# dataset[ , mv_mpagominimo          := rowSums( cbind( Master_mpagominimo,  Visa_mpagominimo) , na.rm=TRUE ) ]
# 
# #a partir de aqui juego con la suma de Mastercard y Visa
# dataset[ , mvr_Master_mlimitecompra:= Master_mlimitecompra / mv_mlimitecompra ]
# dataset[ , mvr_Visa_mlimitecompra  := Visa_mlimitecompra / mv_mlimitecompra ]
# dataset[ , mvr_msaldototal         := mv_msaldototal / mv_mlimitecompra ]
# dataset[ , mvr_msaldopesos         := mv_msaldopesos / mv_mlimitecompra ]
# dataset[ , mvr_msaldopesos2        := mv_msaldopesos / mv_msaldototal ]
# dataset[ , mvr_msaldodolares       := mv_msaldodolares / mv_mlimitecompra ]
# dataset[ , mvr_msaldodolares2      := mv_msaldodolares / mv_msaldototal ]
# dataset[ , mvr_mconsumospesos      := mv_mconsumospesos / mv_mlimitecompra ]
# dataset[ , mvr_mconsumosdolares    := mv_mconsumosdolares / mv_mlimitecompra ]
# dataset[ , mvr_madelantopesos      := mv_madelantopesos / mv_mlimitecompra ]
# dataset[ , mvr_madelantodolares    := mv_madelantodolares / mv_mlimitecompra ]
# dataset[ , mvr_mpagado             := mv_mpagado / mv_mlimitecompra ]
# dataset[ , mvr_mpagospesos         := mv_mpagospesos / mv_mlimitecompra ]
# dataset[ , mvr_mpagosdolares       := mv_mpagosdolares / mv_mlimitecompra ]
# dataset[ , mvr_mconsumototal       := mv_mconsumototal  / mv_mlimitecompra ]
# dataset[ , mvr_mpagominimo         := mv_mpagominimo  / mv_mlimitecompra ]
# 
# #Aqui debe usted agregar sus propias nuevas variables
# #valvula de seguridad para evitar valores infinitos
# #paso los infinitos a NULOS
# infinitos      <- lapply(names(dataset),function(.name) dataset[ , sum(is.infinite(get(.name)))])
# infinitos_qty  <- sum( unlist( infinitos) )
# if( infinitos_qty > 0 )
# {
#   cat( "ATENCION, hay", infinitos_qty, "valores infinitos en tu dataset. Seran pasados a NA\n" )
#   dataset[mapply(is.infinite, dataset)] <- NA
# }
# 
# 
# #valvula de seguridad para evitar valores NaN  que es 0/0
# #paso los NaN a 0 , decision polemica si las hay
# #se invita a asignar un valor razonable segun la semantica del campo creado
# nans      <- lapply(names(dataset),function(.name) dataset[ , sum(is.nan(get(.name)))])
# nans_qty  <- sum( unlist( nans) )
# if( nans_qty > 0 )
# {
#   cat( "ATENCION, hay", nans_qty, "valores NaN 0/0 en tu dataset. Seran pasados arbitrariamente a 0\n" )
#   cat( "Si no te gusta la decision, modifica a gusto el programa!\n\n")
#   dataset[mapply(is.nan, dataset)] <- 0
# }
# 
# 


rankear<- c('mcuenta_corriente','mcuentas_saldo' ,
 "mcaja_ahorro", "mcomisiones_mantenimiento", 
"mcomisiones", "Master_Fvencimiento", 
"mcomisiones_otras")
# setdiff( colnames(dataset), "clase_ternaria" ) todas las col excepto ternaria



for( campo in rankear )
{
  if(  dataset[ get(campo) < 0, .N ]  > 0 ) {
    dataset[   , paste0( campo, "_neg" ) := ifelse( get(campo)< 0, get(campo), NA ), by =dataset$foto_mes ]
    dataset[   , paste0( campo, "_pos" ) := ifelse( get(campo)> 0, get(campo), NA ), by =dataset$foto_mes ]
    dataset[, paste0(campo) := NULL] # elimino la variable original
  }}
dataset_ene  <- dataset[ foto_mes== 202101 ]
# dataset_feb  <- dataset[ foto_mes== 202102 ]
dataset_mar  <- dataset[ foto_mes== 202103 ]
# dataset_abr  <- dataset[ foto_mes== 202104 ]
dataset_may  <- dataset[ foto_mes== 202105 ]
# 
# hist(dataset$mcuentas_saldo, xlim = c(-110,20000000), breaks = 1000)
# hist(dataset$cprestamos_personales,xlim = c(0,20), breaks=300)
# hist(dataset$mprestamos_personales,xlim = c(-10,2000000), breaks=300)
# hist(dataset$mcaja_ahorro,xlim = c(-100,20000), breaks=300)
hist(dataset$cdescubierto_preacordado)

rankear<- c('mcuenta_corriente_pos','mcuentas_saldo_pos' , "mcaja_ahorro_pos", "mcomisiones_mantenimiento_pos", 
            "mcomisiones_pos",  "Master_Fvencimiento_pos", 
            "mcomisiones_otras_pos", 'mcuenta_corriente_neg','mcuentas_saldo_neg' , 
             "mcaja_ahorro_neg", "mcomisiones_mantenimiento_neg",
            "mcomisiones_neg", "Master_Fvencimiento_neg", 
            "mcomisiones_otras_neg","cliente_edad")
dataset_mar[1:3,]

for (campo in rankear) {
  #Si algún campo falla en el if lo mando a la lista otros Creditos a Nicolas Nuñez Manzano por el armado del codigo que separa en neg y pos con rankeo
    {  dataset_mar[, paste0("auto_r_", campo, sep = "") := (frankv(dataset_mar, cols = campo) - 1) / (length(dataset_mar[, get(campo)]) - 1), by =dataset_mar$foto_mes] # rankeo entre 0 y 1
        dataset_mar[, paste0(campo) := NULL] # elimino la variable original 
    }}

for (campo in rankear) {
  #Si algún campo falla en el if lo mando a la lista otros Creditos a Nicolas Nuñez Manzano por el armado del codigo que separa en neg y pos con rankeo
  {  dataset_may[, paste0("auto_r_", campo, sep = "") := (frankv(dataset_may, cols = campo) - 1) / (length(dataset_may[, get(campo)]) - 1)] # rankeo entre 0 y 1
    dataset_may[, paste0(campo) := NULL] # elimino la variable original
  }}
dataset_mar[1:3,]
dataset <- rbind(dataset_mar, dataset_may)
neg<- c( 'auto_r_mcuenta_corriente_neg','auto_r_mcuentas_saldo_neg' , 
            "auto_r_mcaja_ahorro_neg", "auto_r_mcomisiones_mantenimiento_neg",
            "auto_r_mcomisiones_neg", "auto_r_Master_Fvencimiento_neg", 
            "auto_r_mcomisiones_otras_neg")


dataset$auto_r_mcuenta_corriente_neg <- dataset$auto_r_mcuenta_corriente_neg*(-1)  
dataset$auto_r_mcuentas_saldo_neg <- dataset$auto_r_mcuentas_saldo_neg*(-1)  
dataset$auto_r_mcaja_ahorro_neg <- dataset$auto_r_mcaja_ahorro_neg*(-1)  
dataset$auto_r_mcomisiones_mantenimiento_neg <- dataset$auto_r_mcomisiones_mantenimiento_neg*(-1)  
dataset$auto_r_mcomisiones_neg <- dataset$auto_r_mcomisiones_neg*(-1) 
dataset$auto_r_Master_Fvencimiento_neg <- dataset$auto_r_Master_Fvencimiento_neg*(-1) 
dataset$auto_r_mcomisiones_otras_neg <- dataset$auto_r_mcomisiones_otras_neg*(-1) 


dataset$auto_r_mcuenta_corriente <- rowSums(cbind(dataset$auto_r_mcuenta_corriente_pos,dataset$auto_r_mcuenta_corriente_neg),na.rm=TRUE)
dataset$auto_r_mcuentas_saldo <- rowSums(cbind(dataset$auto_r_mcuentas_saldo_pos,dataset$auto_r_mcuentas_saldo_neg),na.rm=TRUE)
dataset$auto_r_mcaja_ahorro <- rowSums(cbind(dataset$auto_r_mcaja_ahorro_pos,dataset$auto_r_mcaja_ahorro_neg),na.rm=TRUE)
dataset$auto_r_mcomisiones_mantenimiento <- rowSums(cbind(dataset$auto_r_mcomisiones_mantenimiento_pos,dataset$auto_r_mcomisiones_mantenimiento_neg),na.rm=TRUE)
dataset$auto_r_mcomisiones <- rowSums(cbind(dataset$auto_r_mcomisiones_pos,dataset$auto_r_mcomisiones_neg),na.rm=TRUE)
dataset$auto_r_Master_Fvencimiento <- rowSums(cbind(dataset$auto_r_Master_Fvencimiento_pos,dataset$auto_r_Master_Fvencimiento_neg),na.rm=TRUE)
dataset$auto_r_mcomisiones_otras <- rowSums(cbind(dataset$auto_r_mcomisiones_otras_pos,dataset$auto_r_mcomisiones_otras_neg),na.rm=TRUE)


dataset[ , c('auto_r_mcuenta_corriente_pos',"auto_r_mcuenta_corriente_neg","auto_r_mcuentas_saldo_pos",
             "auto_r_mcuentas_saldo_neg", "auto_r_mcaja_ahorro_pos","auto_r_mcaja_ahorro_neg",
             "auto_r_mcomisiones_mantenimiento_pos","auto_r_mcomisiones_mantenimiento_neg",
             "auto_r_mcomisiones_pos","auto_r_mcomisiones_neg","auto_r_Master_Fvencimiento_pos",
             "auto_r_Master_Fvencimiento_neg","auto_r_mcomisiones_otras_pos", "auto_r_mcomisiones_otras_neg")] <- list(NULL)


names(dataset_mar)


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
dapy  <- dataset[ foto_mes== PARAM$input$train ]


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
>>>>>>> 986a806e8f47b369135510569a1d20d07a2ae9fa
