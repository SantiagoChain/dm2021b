rm( list=ls() )  #Borro todos los objetos
gc()   #Garbage Collection

require("data.table")
require("rpart")

#------------------------------------------------------------------------------

particionar  <- function( data,  division, agrupa="",  campo="fold", start=1, seed=NA )
{
  if( !is.na(seed) )   set.seed( seed )

  bloque  <- unlist( mapply(  function(x,y) { rep( y, x )} ,   division,  seq( from=start, length.out=length(division) )  ) )  

  data[ ,  (campo) :=  sample( rep( bloque, ceiling(.N/length(bloque))) )[1:.N],
            by= agrupa ]
}
#------------------------------------------------------------------------------

#Aqui se debe poner la carpeta de la computadora local
setwd("C:\\Users\\SCHAIN\\OneDrive - Pampa Energia\\Desktop\\ECD\\DataMining\\DM_EF\\datasetsOri\\")  #Establezco el Working Directory

#cargo los datos
dataset  <- fread("./paquete_premium_202011.csv")

particionar( dataset, division=c(80,20), agrupa="clase_ternaria", seed= 101641 )  #Cambiar por la primer semilla de cada uno !

#genero el modelo
modelo  <- rpart("clase_ternaria ~ .",
                 data= dataset[ fold==1],
                 xval= 0,
                 cp= -1,
                 maxdepth= 6 )


prediccion  <- predict( modelo, dataset[ fold==2] , type= "prob") #aplico el modelo

#prediccion es una matriz con TRES columnas, llamadas "BAJA+1", "BAJA+2"  y "CONTINUA"
#cada columna es el vector de probabilidades 

dataset[  , ganancia :=  ifelse( clase_ternaria=="BAJA+2", 48750, -1250 ) ]

dataset[ fold==2 , prob_baja2 := prediccion[, "BAJA+2"] ]
ganancia_test  <- dataset[ fold==2 & prob_baja2 > 0.025, sum(ganancia) ]
ganancia_test_normalizada  <-  ganancia_test / 0.2

ganancia_test_normalizada
