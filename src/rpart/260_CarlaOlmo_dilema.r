#limpio la memoria
rm(list=ls())   #remove all objects
gc()            #garbage collection

require("data.table")
require("parallel")
require("rpart")

#setwd( "C:\\ECD\\DataMining\\DM_EF" )
setwd( "C:\\ECD\\DataMining\\DM_EF" )

#Aqui van VEINTE semillas
ksemillas  <- c(13131317, 191000093, 191537, 241313, 277183, 101641 , 104231 , 104053 , 100109 , 102329,
                104549 , 104579 , 104149 , 102763 , 103291 , 102139 , 104729, 101723 , 101929, 104309  )

#------------------------------------------------------------------------------

particionar  <- function( data,  division, agrupa="",  campo="fold", start=1, seed=NA )
{
  if( !is.na(seed) )   set.seed( seed )

  bloque  <- unlist( mapply(  function(x,y) { rep( y, x )} ,   division,  seq( from=start, length.out=length(division) )  ) )  

  data[ ,  (campo) :=  sample( rep( bloque, ceiling(.N/length(bloque))) )[1:.N],
            by= agrupa ]
}
#------------------------------------------------------------------------------

ArbolSimple  <- function( fold_test, data, param )
{
  #genero el modelo
  modelo  <- rpart("clase_ternaria ~ .", 
                   data= data[ fold != fold_test, ], #training  fold==1
                   xval= 0,
                   control= param )

  #aplico el modelo a los datos de testing, fold==2
  prediccion  <- predict( modelo, data[ fold==fold_test, ], type = "prob")

  prob_baja2  <- prediccion[, "BAJA+2"]

  ganancia_testing  <- sum(  data[ fold==fold_test ][ prob_baja2 >0.025,  ifelse( clase_ternaria=="BAJA+2", 48750, -1250 ) ] )

  return( ganancia_testing )
}
#------------------------------------------------------------------------------

ArbolEstimarGanancia  <- function( semilla, data, param )
{
  pct_test  <- 30/(30+70)
  particionar( data, division=c(70,30), agrupa="clase_ternaria", seed=semilla )

  ganancia_testing  <- ArbolSimple( 2, data, param )

  ganancia_testing_normalizada  <- ganancia_testing / pct_test   #normalizo la ganancia

  return( ganancia_testing_normalizada )
}
#------------------------------------------------------------------------------

ArbolesMontecarlo  <- function( data, param, semillas )
{
  ganancias  <- mcmapply( ArbolEstimarGanancia, 
                          semillas, 
                          MoreArgs= list( data, param), 
                          SIMPLIFY= FALSE,
                          mc.cores= 1 )  #debe ser 1 si se tiene Windows

  #devuelvo la primer ganancia y el promedio
  return( mean( unlist( ganancias ))  ) 
}
#------------------------------------------------------------------------------

#cargo los datos donde voy a ENTRENAR el modelo
dataset  <- fread("./datasetsOri/paquete_premium_202011.csv")

#ganancia en Kaggle de param1  14.90453
param1 <- list( "cp"= -1,
                "minsplit"=  200,
                "minbucket"= 100,
                "maxdepth"=    6 )

#ganancia en Kaggle de param2  17.87127
param2 <- list( "cp"= -1,
                "minsplit"=   50,
                "minbucket"=  10,
                "maxdepth"=    6 )


#ganancias en  20 - Montecarlo
gan1  <- ArbolesMontecarlo( dataset, param1, ksemillas )
gan2  <- ArbolesMontecarlo( dataset, param2, ksemillas )

gan1
gan2

