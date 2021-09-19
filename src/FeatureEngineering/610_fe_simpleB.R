#Feature Engineering
#creo nuevas variables dentro del mismo mes
#Condimentar a gusto con nuevas variables
#Solo Variables creadas por mi

#limpio la memoria
rm( list=ls() )
gc()

require("data.table")



#Establezco el Working Directory
setwd( "~/buckets/b1" )


EnriquecerDataset <- function( dataset , arch_destino )
{
  columnas_originales <-  copy(colnames( dataset ))

  #INICIO de la seccion donde se deben hacer cambios con variables nuevas

  #Variables creadas por mi
  dataset[ , com_prod:= mcomisiones / cproductos ]
  dataset[ , ant_edad:= (cliente_antiguedad / 12) / (cliente_edad - 18) ]
  dataset[ , ctarjetas          := rowSums( cbind( ctarjeta_visa,  ctarjeta_master) , na.rm=TRUE ) ]
  dataset[ , mtarjetas          := rowSums( cbind( mtarjeta_visa_consumo,  mtarjeta_master_consumo) , na.rm=TRUE ) ]
  dataset[ , cdebitos_tarjetas          := rowSums( cbind( ctarjeta_visa_debitos_automaticos,  ctarjeta_master_debitos_automaticos) , na.rm=TRUE ) ]
  #no encuentra mtarjeta_visa_debitos_automaticos en el dataset, si esta en el diccionario
  #dataset[ , mdebitos_tarjetas          := rowSums( cbind( mtarjeta_visa_debitos_automaticos,  mttarjeta_master_debitos_automaticos) , na.rm=TRUE ) ]
  #dataset[ , mtotal_tarjetas          := rowSums( cbind( mtarjeta_visa_consumo,  mtarjeta_master_consumo, mtarjeta_visa_debitos_automaticos, mttarjeta_master_debitos_automaticos) , na.rm=TRUE ) ]
  dataset[ , cprestamos          := rowSums( cbind( cprestamos_personales,  cprestamos_prendarios, cprestamos_hipotecarios) , na.rm=TRUE ) ]
  dataset[ , mprestamos          := rowSums( cbind( mprestamos_personales,  mprestamos_prendarios, mprestamos_hipotecarios) , na.rm=TRUE ) ]
  dataset[ , cseguros          := rowSums( cbind( cseguro_vida,  cseguro_auto, cseguro_vivienda, cseguro_accidentes_personales) , na.rm=TRUE ) ]
  dataset[ , cservicios          := rowSums( cbind( cpagodeservicios,  cpagomiscuentas) , na.rm=TRUE ) ]
  dataset[ , mservicios          := rowSums( cbind( mpagodeservicios,  mpagomiscuentas) , na.rm=TRUE ) ]
  dataset[ , mpagominimo_tarjetas          := rowSums( cbind( Master_mpagominimo,  Visa_mpagominimo) , na.rm=TRUE ) ]
  dataset[ , mpagopesos_tarjetas          := rowSums( cbind( Master_mpagospesos,  Visa_mpagospesos) , na.rm=TRUE ) ]
  dataset[ , cpagomin_visa          := ifelse( Visa_mpagominimo == Visa_mpagospesos, 1, 0)  ]
  dataset[ , cpagomin_master          := ifelse( Master_mpagominimo == Master_mpagospesos, 1, 0)  ]
  dataset[ , cdescuentos          := rowSums( cbind( ccajeros_propios_descuentos,  ctarjeta_visa_descuentos, ctarjeta_master_descuentos) , na.rm=TRUE ) ]
  dataset[ , mdescuentos          := rowSums( cbind( mcajeros_propios_descuentos,  mtarjeta_visa_descuentos, mtarjeta_master_descuentos) , na.rm=TRUE ) ]
  dataset[ , ctransferencias          := rowSums( cbind( ctransferencias_recibidas,  ctransferencias_emitidas) , na.rm=TRUE ) ]
  dataset[ , mtransferencias          := rowSums( cbind( mtransferencias_recibidas,  mtransferencias_emitidas) , na.rm=TRUE ) ]
  dataset[ , mlimitecompra          := rowSums( cbind( Visa_mlimitecompra,  Master_mlimitecompra) , na.rm=TRUE ) ]
  dataset[ , ctrx_ant_edad:= ctrx_quarter / (cliente_antiguedad / 12)/(cliente_edad - 18) ]
  dataset[ , trx_mes          := rowSums( cbind( cpayroll2_trx,  catm_trx, catm_trx_other, cmobile_app_trx) , na.rm=TRUE ) ]
  dataset[ , sueldo_trx_mes          := cpayroll_trx / rowSums( cbind( cpayroll2_trx,  catm_trx, catm_trx_other, cmobile_app_trx) , na.rm=TRUE ) ]
  

  #valvula de seguridad para evitar valores infinitos
  #paso los infinitos a NULOS
  infinitos      <- lapply(names(dataset),function(.name) dataset[ , sum(is.infinite(get(.name)))])
  infinitos_qty  <- sum( unlist( infinitos) )
  if( infinitos_qty > 0 )
  {
    cat( "ATENCION, hay", infinitos_qty, "valores infinitos en tu dataset. Seran pasados a NA\n" )
    dataset[mapply(is.infinite, dataset)] <- NA
  }


  #valvula de seguridad para evitar valores NaN  que es 0/0
  #paso los NaN a 0 , decision polemica si las hay
  #se invita a asignar un valor razonable segun la semantica del campo creado
  nans      <- lapply(names(dataset),function(.name) dataset[ , sum(is.nan(get(.name)))])
  nans_qty  <- sum( unlist( nans) )
  if( nans_qty > 0 )
  {
    cat( "ATENCION, hay", nans_qty, "valores NaN 0/0 en tu dataset. Seran pasados arbitrariamente a 0\n" )
    cat( "Si no te gusta la decision, modifica a gusto el programa!\n\n")
    dataset[mapply(is.nan, dataset)] <- 0
  }

  #FIN de la seccion donde se deben hacer cambios con variables nuevas

  columnas_extendidas <-  copy( setdiff(  colnames(dataset), columnas_originales ) )

  #grabo con nombre extendido
  fwrite( dataset,
          file=arch_destino,
          sep= "," )
}
#------------------------------------------------------------------------------

dir.create( "./datasets/" )


#lectura rapida del dataset  usando fread  de la libreria  data.table
dataset1  <- fread("./datasetsOri/paquete_premium_202011.csv")
dataset2  <- fread("./datasetsOri/paquete_premium_202101.csv")

EnriquecerDataset( dataset1, "./datasets/paquete_premium_202011_extB.csv" )
EnriquecerDataset( dataset2, "./datasets/paquete_premium_202101_extB.csv" )

quit( save="no")
