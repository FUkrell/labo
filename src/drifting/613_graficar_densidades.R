#limpio la memoria
rm( list=ls() )  #remove all objects
gc()             #garbage collection

require("data.table")
require("rpart")


#------------------------------------------------------------------------------

graficar_campo  <- function( campo, campo_clase, valores_clase )
{

  #quito de grafico las colas del 5% de las densidades
  qA  <- quantile(  dataset[ foto_mes==202101 , get(campo) ] , prob= c(0.05, 0.95), na.rm=TRUE )
  qB  <- quantile(  dataset[ foto_mes==202103 , get(campo) ] , prob= c(0.05, 0.95), na.rm=TRUE )

  xxmin  <- pmin( qA[[1]], qB[[1]] )
  xxmax  <- pmax( qA[[2]], qB[[2]] )

  densidad_A  <- density( dataset[ foto_mes==202101 & get(campo_clase) %in% valores_clase, get(campo) ],
                          kernel="gaussian", na.rm=TRUE )

  densidad_B  <- density( dataset[ foto_mes==202103 & get(campo_clase) %in% valores_clase, get(campo) ],
                          kernel="gaussian", na.rm=TRUE )

  plot( densidad_A,
        col="blue",
        xlim= c( xxmin, xxmax ),
        ylim= c( 0, pmax( max(densidad_A$y), max(densidad_B$y) ) ),
        main= paste0( campo, ",   ", campo_clase, " in ",  paste( valores_clase,collapse=",")) 
      )

  lines(densidad_B, col="red", lty=2)
  
  legend(  "topright",  
           legend=c("202001", "202003"),
           col=c("blue", "red"), lty=c(1,2))

}
#------------------------------------------------------------------------------
#Aqui comienza el programa

setwd( "D:\\Maestria\\DMEyF\\" )

#cargo el dataset donde voy a entrenar
dataset  <- fread("./datasets/competencia2_2022.csv.gz")

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


rankear<- c('mcuenta_corriente_pos','mcuentas_saldo_pos' , "mcaja_ahorro_pos", "mcomisiones_mantenimiento_pos", 
            "mcomisiones_pos",  "Master_Fvencimiento_pos", 
            "mcomisiones_otras_pos", 'mcuenta_corriente_neg','mcuentas_saldo_neg' , 
            "mcaja_ahorro_neg", "mcomisiones_mantenimiento_neg",
            "mcomisiones_neg", "Master_Fvencimiento_neg", "cliente_edad",
            "mcomisiones_otras_neg")

for (campo in rankear) {
  #Si algún campo falla en el if lo mando a la lista otros Creditos a Nicolas Nuñez Manzano por el armado del codigo que separa en neg y pos con rankeo
  {  dataset_ene[, paste0("auto_r_", campo, sep = "") := (frankv(dataset_ene, cols = campo) - 1) / (length(dataset_ene[, get(campo)]) - 1), by =dataset_ene$foto_mes] # rankeo entre 0 y 1
    dataset_ene[, paste0(campo) := NULL] # elimino la variable original 
  }}

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

names(dataset_mar)
dataset <- rbind(dataset_mar, dataset_may, dataset_ene)

dataset  <- dataset[  foto_mes %in% c( 202101, 202103 ) ]

#creo la clase_binaria SI={ BAJA+1, BAJA+2 }    NO={ CONTINUA }
dataset[ foto_mes==202101, 
         clase_binaria :=  ifelse( clase_ternaria=="CONTINUA", "NO", "SI" ) ]

# Entreno el modelo
# utilizo los mejores hiperparametros encontrados en una Bayesian Optimizationcon 5-fold Cross Validation
modelo  <- rpart(formula=   "clase_binaria ~ . -clase_ternaria",
                 data=      dataset[ foto_mes==202101 ],  #los datos donde voy a entrenar
                 xval=         0,
                 cp=           -0.69,
                 minsplit=    870,
                 minbucket=     9,
                 maxdepth=      9)


campos_modelo  <- names( modelo$variable.importance )
campos_buenos  <- c( campos_modelo,  setdiff( colnames(dataset), campos_modelo ) )
campos_buenos  <-  setdiff(  campos_buenos,  c( "foto_mes","clase_ternaria","clase_binaria" ) )


dir.create( "./exp/",  showWarnings = FALSE ) 
dir.create( "./exp/DR6130/", showWarnings = FALSE )
setwd("./exp/DR6130/")



pdf("densidades_01_03.pdf")

for( campo in  campos_buenos )
{
  cat( campo, "  " )
  
  graficar_campo( campo, "clase_ternaria", c( "BAJA+1", "BAJA+2", "CONTINUA" ) )
  graficar_campo( campo, "clase_ternaria", c( "BAJA+1", "BAJA+2" ) )
  graficar_campo( campo, "clase_ternaria", c( "BAJA+2" ) )
  graficar_campo( campo, "clase_ternaria", c( "BAJA+1" ) )
  graficar_campo( campo, "clase_ternaria", c( "CONTINUA" ) )
}

dev.off()

