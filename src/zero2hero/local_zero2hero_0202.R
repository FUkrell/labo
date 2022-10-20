#Ejemplo de optimizacion bayesiana, univariada

#limpio la memoria
rm( list=ls() )  #remove all objects
gc()             #garbage collection

require("data.table")
#paquetes necesarios para la Bayesian Optimization
require("DiceKriging")
require("mlrMBO")
require("plotly")

options(repr.plot.width=20, repr.plot.height=10)
func_univariada01  <- function( x )
{
  y  <- -2 * (x+13) * (x-3) * (x-7) * (x-19)
  
  return( y )
}

intervalo  <- seq(-15,21,0.1)
plot(intervalo, func_univariada01(intervalo))
# en este caso quiero maximizar la funcion
obj.fun  <- makeSingleObjectiveFunction(
  fn=       func_univariada01,
  minimize= FALSE,   #estoy Maximizando la ganancia
  par.set=  makeParamSet(  makeNumericParam( "x", lower= -100, upper=  100) ),
)

fproxy  <- makeLearner( cl= "regr.km",
                        predict.type= "se", 
                        covtype= "matern3_2" )

ctrl  <- makeMBOControl()
ctrl  <- setMBOControlInfill( ctrl, crit= makeMBOInfillCritEI())
ctrl  <- setMBOControlTermination( ctrl, iters= 25 )

run  <- mbo( fun=      obj.fun, 
             learner= fproxy, 
             control= ctrl )
tb_resultados  <- as.data.table( run$opt.path )
tb_resultados
tb_resultados[ which.max( tb_resultados$y ) ]

configureMlr( show.learner.output = FALSE)

func_volcano  <- function( x )
{
  z  <- volcano[ x$b, x$a ]
  
  return( z )
}
p <- plot_ly(z = volcano, type = "surface")
p 

obj.fun  <- makeSingleObjectiveFunction(
  fn=       func_volcano,
  minimize= FALSE,   #estoy Maximizando la ganancia
  has.simple.signature = FALSE,  #porque tengo DOS dimensiones
  par.set=  makeParamSet(  makeIntegerParam( "a", lower= 1, upper=  61),
                           makeIntegerParam( "b", lower= 1, upper=  87)
  ),
)
