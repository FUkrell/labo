#Necesita para correr en Google Cloud
# 128 GB de memoria RAM
# 256 GB de espacio en el disco local
#   8 vCPU

#limpio la memoria
rm( list=ls() )  #remove all objects
gc()             #garbage collection

require("data.table")


#Parametros del script
PARAM <- list()
PARAM$dataset  <- "./datasets/competenciaFINAL_2022.csv.gz"
PARAM$experimento <- "ER10011"
PARAM$exp_input <- "ZZ10011"


#------------------------------------------------------------------------------
#Aqui empieza el programa

setwd( "~/buckets/b1/" )

#leo el dataset
dataset  <- fread( PARAM$dataset )


#leo las semillas usadas en la prediccion 
arch_semillas  <- paste0( "./exp/", PARAM$exp_input, "/ksemillas.csv" )
semillas_data  <- c(fread( arch_semillas )[,1])

#creo lista de semillas usadas
semillas <- c()
for (semilla in semillas_data){
  semillas <- paste0("_", semilla)
}

#selecciono lista de clientes a predecir
data_eval  <- dataset[ foto_mes== 202109,c("numero_de_cliente")]

#itero en los resultados por semillas y hago join by numero de cliente
for (semilla in semillas){
  data_semilla <- fread(paste0('./exp/',PARAM$exp_input,'/',PARAM$exp_input,semilla,'_resultados.csv'))
  data_semilla <- data_semilla[ ,c("numero_de_cliente", "rank")] #"rank" ó "prob"
  colnames(data_semilla) <- c("numero_de_cliente", semilla)
  data_eval <- data_eval[data_semilla, on = c("numero_de_cliente")]  
}

#calculo la media del rank por numero de cliente
semillerio <- data_eval[, .(mean = rowMeans(.SD)), by = numero_de_cliente]
#ordeno de manera creciente el rank
setorder(semillerio, mean)
#ordeno de manera decreciente la prob (si usé prob)
#setorder(semillerio, -mean)

#creo la carpeta donde va el experimento
dir.create( paste0( "./exp/", PARAM$experimento, "/"), showWarnings = FALSE )
setwd(paste0( "./exp/", PARAM$experimento, "/"))   #Establezco el Working Directory DEL EXPERIMENTO


#genero los archivos para Kaggle
cortes  <- seq( from=  8000,
                to=    16000,
                by=     500 )

for( corte in cortes )
{
  semillerio[  , Predicted := 0L ]
  semillerio[ 1:corte, Predicted := 1L ]
  
  nom_submit  <- paste0( PARAM$experimento, 
                         "_",
                         sprintf( "%05d", corte ),
                         ".csv" )
  
  # Guardo el submit 
  fwrite(  semillerio[ , list( numero_de_cliente, Predicted ) ],
           file= nom_submit,
           sep= "," )
  
}
