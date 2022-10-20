#limpio la memoria
rm( list=ls() )  #remove all objects
gc()             #garbage collection

require("data.table")
require("rpart")
require("rpart.plot")
library(skimr)
library(devtools)
library(visdat)
library(DataExplorer)
library(ggplot2)

# crear reporte visualizacion generico
# DataExplorer::create_report(dataset)



setwd( "D:\\Maestria\\DMEyF\\" )   #establezco la carpeta donde voy a trabajar
#cargo el dataset
dataset  <- fread( "./datasets/competencia1_2022.csv")

dataset[ foto_mes==202101, clase_binaria :=  ifelse( clase_ternaria=="CONTINUA", "NO", "SI" ) ]

dataset$ctarjeta_transacciones <- dataset$ctarjeta_debito_transacciones + dataset$ctarjeta_master_transacciones +dataset$ctarjeta_visa_transacciones
dataset$consumospesos <- dataset$Visa_mconsumospesos + dataset$Master_mconsumospesos
dataset$consumosdolares <- dataset$Visa_mconsumosdolares + dataset$Master_mconsumosdolares
dataset$saldopesos <- dataset$Visa_msaldopesos + dataset$Master_msaldopesos
dataset$saldodolares <- dataset$Visa_msaldodolares + dataset$Master_msaldodolares
dataset$saldototal <- dataset$Visa_msaldototal + dataset$Master_msaldototal

# analisis exploratorio
# skim(dtrain)

#uso esta semilla para los canaritos
set.seed(102191)

#agrego una variable canarito, random distribucion uniforme en el intervalo [0,1]
dataset[ ,  canarito1 :=  runif( nrow(dataset) ) ]

#agrego los siguientes canaritos
for( i in 13:100 ) dataset[ , paste0("canarito", i ) :=  runif( nrow(dataset)) ]

campo <- "Visa_status" 
options(repr.plot.width=15, repr.plot.height=15)
ggplot(dataset[ foto_mes==202101] , aes_string(x = campo)) + geom_density(trim=TRUE, na.rm=TRUE) + facet_grid( "clase_binaria~ .")




#Primero  veo como quedan mis arboles
  modelo  <- rpart(formula= "clase_binaria ~ . -mcomisiones_mantenimiento -Visa_mpagado -clase_ternaria",
                 data= dataset[ foto_mes==202101, ],
                 model= TRUE,
                 xval= 0,
                 cp= 0,
                 minsplit= 10,
                 maxdepth= 10)


pdf(file = "arbol_canaritos_binario.pdf", width=28, height=4)
prp(modelo, extra=101, digits=5, branch=1, type=4, varlen=0, faclen=0)
dev.off()

