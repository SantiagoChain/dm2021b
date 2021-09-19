#Feature Engineering
#creo nuevas variables dentro del mismo mes
#Condimentar a gusto con nuevas variables

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
  #se crean los nuevos campos para MasterCard  y Visa, teniendo en cuenta los NA's
  #varias formas de combinar Visa_status y Master_status
#  dataset[ , mv_status01       := pmax( Master_status,  Visa_status, na.rm = TRUE) ]
#  dataset[ , mv_status02       := Master_status +  Visa_status ]
#  dataset[ , mv_status03       := pmax( ifelse( is.na(Master_status), 10, Master_status) , ifelse( is.na(Visa_status), 10, Visa_status) ) ]
#  dataset[ , mv_status04       := ifelse( is.na(Master_status), 10, Master_status)  +  ifelse( is.na(Visa_status), 10, Visa_status)  ]
#  dataset[ , mv_status05       := ifelse( is.na(Master_status), 10, Master_status)  +  100*ifelse( is.na(Visa_status), 10, Visa_status)  ]

#  dataset[ , mv_status06       := ifelse( is.na(Visa_status), 
#                                          ifelse( is.na(Master_status), 10, Master_status), 
#                                          Visa_status)  ]

#  dataset[ , mv_status07       := ifelse( is.na(Master_status), 
#                                          ifelse( is.na(Visa_status), 10, Visa_status), 
#                                          Master_status)  ]


  #combino MasterCard y Visa
 # dataset[ , mv_mfinanciacion_limite := rowSums( cbind( Master_mfinanciacion_limite,  Visa_mfinanciacion_limite) , na.rm=TRUE ) ]
#
  dataset[ , mv_Fvencimiento         := pmin( Master_Fvencimiento, Visa_Fvencimiento, na.rm = TRUE) ]
#  dataset[ , mv_Finiciomora          := pmin( Master_Finiciomora, Visa_Finiciomora, na.rm = TRUE) ]
  dataset[ , mv_msaldototal          := rowSums( cbind( Master_msaldototal,  Visa_msaldototal) , na.rm=TRUE ) ]
#  dataset[ , mv_msaldopesos          := rowSums( cbind( Master_msaldopesos,  Visa_msaldopesos) , na.rm=TRUE ) ]
#  dataset[ , mv_msaldodolares        := rowSums( cbind( Master_msaldodolares,  Visa_msaldodolares) , na.rm=TRUE ) ]
#  dataset[ , mv_mconsumospesos       := rowSums( cbind( Master_mconsumospesos,  Visa_mconsumospesos) , na.rm=TRUE ) ]
#  dataset[ , mv_mconsumosdolares     := rowSums( cbind( Master_mconsumosdolares,  Visa_mconsumosdolares) , na.rm=TRUE ) ]
#  dataset[ , mv_mlimitecompra        := rowSums( cbind( Master_mlimitecompra,  Visa_mlimitecompra) , na.rm=TRUE ) ]
#  dataset[ , mv_madelantopesos       := rowSums( cbind( Master_madelantopesos,  Visa_madelantopesos) , na.rm=TRUE ) ]
#  dataset[ , mv_madelantodolares     := rowSums( cbind( Master_madelantodolares,  Visa_madelantodolares) , na.rm=TRUE ) ]
#  dataset[ , mv_fultimo_cierre       := pmax( Master_fultimo_cierre, Visa_fultimo_cierre, na.rm = TRUE) ]
#  dataset[ , mv_mpagado              := rowSums( cbind( Master_mpagado,  Visa_mpagado) , na.rm=TRUE ) ]
#  dataset[ , mv_mpagospesos          := rowSums( cbind( Master_mpagospesos,  Visa_mpagospesos) , na.rm=TRUE ) ]
#  dataset[ , mv_mpagosdolares        := rowSums( cbind( Master_mpagosdolares,  Visa_mpagosdolares) , na.rm=TRUE ) ]
#  dataset[ , mv_fechaalta            := pmax( Master_fechaalta, Visa_fechaalta, na.rm = TRUE) ]
#  dataset[ , mv_mconsumototal        := rowSums( cbind( Master_mconsumototal,  Visa_mconsumototal) , na.rm=TRUE ) ]
#  dataset[ , mv_cconsumos            := rowSums( cbind( Master_cconsumos,  Visa_cconsumos) , na.rm=TRUE ) ]
 # dataset[ , mv_cadelantosefectivo   := rowSums( cbind( Master_cadelantosefectivo,  Visa_cadelantosefectivo) , na.rm=TRUE ) ]
  dataset[ , mv_mpagominimo          := rowSums( cbind( Master_mpagominimo,  Visa_mpagominimo) , na.rm=TRUE ) ]

  #a partir de aqui juego con la suma de Mastercard y Visa
#  dataset[ , mvr_Master_mlimitecompra:= Master_mlimitecompra / mv_mlimitecompra ]
#  dataset[ , mvr_Visa_mlimitecompra  := Visa_mlimitecompra / mv_mlimitecompra ]
#  dataset[ , mvr_msaldototal         := mv_msaldototal / mv_mlimitecompra ]
#  dataset[ , mvr_msaldopesos         := mv_msaldopesos / mv_mlimitecompra ]
#  dataset[ , mvr_msaldopesos2        := mv_msaldopesos / mv_msaldototal ]
#  dataset[ , mvr_msaldodolares       := mv_msaldodolares / mv_mlimitecompra ]
#  dataset[ , mvr_msaldodolares2      := mv_msaldodolares / mv_msaldototal ]
#  dataset[ , mvr_mconsumospesos      := mv_mconsumospesos / mv_mlimitecompra ]
#  dataset[ , mvr_mconsumosdolares    := mv_mconsumosdolares / mv_mlimitecompra ]
#  dataset[ , mvr_madelantopesos      := mv_madelantopesos / mv_mlimitecompra ]
#  dataset[ , mvr_madelantodolares    := mv_madelantodolares / mv_mlimitecompra ]
#  dataset[ , mvr_mpagado             := mv_mpagado / mv_mlimitecompra ]
#  dataset[ , mvr_mpagospesos         := mv_mpagospesos / mv_mlimitecompra ]
#  dataset[ , mvr_mpagosdolares       := mv_mpagosdolares / mv_mlimitecompra ]
#  dataset[ , mvr_mconsumototal       := mv_mconsumototal  / mv_mlimitecompra ]
#  dataset[ , mvr_mpagominimo         := mv_mpagominimo  / mv_mlimitecompra ]
  
  #Variables creadas por mi
  dataset[ , com_prod:= mcomisiones / cproductos ]
  dataset[ , ant_edad:= (cliente_antiguedad / 12) / (cliente_edad - 18) ]
  dataset[ , ctarjetas          := rowSums( cbind( ctarjeta_visa,  ctarjeta_master) , na.rm=TRUE ) ]
  dataset[ , mtarjetas          := rowSums( cbind( mtarjeta_visa_consumo,  mtarjeta_master_consumo) , na.rm=TRUE ) ]
#  dataset[ , cdebitos_tarjetas          := rowSums( cbind( ctarjeta_visa_debitos_automaticos,  ctarjeta_master_debitos_automaticos) , na.rm=TRUE ) ]
  #no encuentra mtarjeta_visa_debitos_automaticos en el dataset, si esta en el diccionario
  #dataset[ , mdebitos_tarjetas          := rowSums( cbind( mtarjeta_visa_debitos_automaticos,  mttarjeta_master_debitos_automaticos) , na.rm=TRUE ) ]
  #dataset[ , mtotal_tarjetas          := rowSums( cbind( mtarjeta_visa_consumo,  mtarjeta_master_consumo, mtarjeta_visa_debitos_automaticos, mttarjeta_master_debitos_automaticos) , na.rm=TRUE ) ]
#  dataset[ , cprestamos          := rowSums( cbind( cprestamos_personales,  cprestamos_prendarios, cprestamos_hipotecarios) , na.rm=TRUE ) ]
#  dataset[ , mprestamos          := rowSums( cbind( mprestamos_personales,  mprestamos_prendarios, mprestamos_hipotecarios) , na.rm=TRUE ) ]
#  dataset[ , cseguros          := rowSums( cbind( cseguro_vida,  cseguro_auto, cseguro_vivienda, cseguro_accidentes_personales) , na.rm=TRUE ) ]
  dataset[ , cservicios          := rowSums( cbind( cpagodeservicios,  cpagomiscuentas) , na.rm=TRUE ) ]
  dataset[ , mservicios          := rowSums( cbind( mpagodeservicios,  mpagomiscuentas) , na.rm=TRUE ) ]
  dataset[ , mpagominimo_tarjetas          := rowSums( cbind( Master_mpagominimo,  Visa_mpagominimo) , na.rm=TRUE ) ]
#  dataset[ , mpagopesos_tarjetas          := rowSums( cbind( Master_mpagospesos,  Visa_mpagospesos) , na.rm=TRUE ) ]
#  dataset[ , cpagomin_visa          := ifelse( Visa_mpagominimo == Visa_mpagospesos, 1, 0)  ]
#  dataset[ , cpagomin_master          := ifelse( Master_mpagominimo == Master_mpagospesos, 1, 0)  ]
  dataset[ , cdescuentos          := rowSums( cbind( ccajeros_propios_descuentos,  ctarjeta_visa_descuentos, ctarjeta_master_descuentos) , na.rm=TRUE ) ]
#  dataset[ , mdescuentos          := rowSums( cbind( mcajeros_propios_descuentos,  mtarjeta_visa_descuentos, mtarjeta_master_descuentos) , na.rm=TRUE ) ]
  dataset[ , ctransferencias          := rowSums( cbind( ctransferencias_recibidas,  ctransferencias_emitidas) , na.rm=TRUE ) ]
#  dataset[ , mtransferencias          := rowSums( cbind( mtransferencias_recibidas,  mtransferencias_emitidas) , na.rm=TRUE ) ]
  dataset[ , mlimitecompra          := rowSums( cbind( Visa_mlimitecompra,  Master_mlimitecompra) , na.rm=TRUE ) ]
  dataset[ , ctrx_ant_edad     := ctrx_quarter / (cliente_antiguedad / 12)/(cliente_edad - 18) ]
  dataset[ , trx_mes          := rowSums( cbind( cpayroll2_trx,  catm_trx, catm_trx_other, cmobile_app_trx) , na.rm=TRUE ) ]
  dataset[ , sueldo_trx_mes          := cpayroll_trx / rowSums( cbind( cpayroll2_trx,  catm_trx, catm_trx_other, cmobile_app_trx) , na.rm=TRUE ) ]
  dataset[ , sservicios          := mservicios/cservicios]
  dataset[ , edadant_sueldotrx          := sueldo_trx_mes/ant_edad]
  dataset[ , haypagomin_tarjeta          := ifelse( cpagomin_visa == 0, ifelse( cpagomin_master == 0, 0, 1))  ]
  dataset[ , servicios_sueldo          := cpayroll_trx/sservicios]
  dataset[ , servicios_sueldo_edad_trx          := edadant_sueldotrx/sservicios]
  dataset[ , mlimitecompra_ant_edad          := mlimitecompra/ant_edad]
  dataset[ , mlimitecompra_sueldo          := mlimitecompra/cpayroll_trx]
  dataset[ , mlimitecompra_edadant_sueldotrx          := mlimitecompra/edadant_sueldotrx]
  dataset[ , sueldo_ctrx2          := sueldo_trx_mes/(ctrx_quarter	/3)]
  dataset[ , sueldo_ctrx3          := saldo_ctrx2/ant_edad]
  dataset[ , sueldo_ctrx4          := mprestamos/sueldo_trx_mes]
  dataset[ , ctrx_tarjetas          := mtarjetas	/ctrx_quarter	]
  dataset[ , montosvarios_edad          := rowSums( cbind( mtarjetas	,  mcaja_ahorro	, mcuentas_saldo	, mpayroll	) , na.rm=TRUE )/cliente_edad	 ]
  dataset[ , trx_edad_ant          := (ctrx_quarter	/3)	/((cliente_edad	-18)*12)	]
  dataset[ , limite_saldo          := mlimitecompra/mv_msaldototal]
  dataset[ , limite_saldo2          := limite_saldo/((cliente_edad	-18)*12)]
  dataset[ , ant_edad2 := ((cliente_edad - 18)*12)/cliente_antiguedad ]
  dataset[ , trx_edad_ant2          := (ctrx_quarter	/3)	/ant_edad2	]
  dataset[ , trx_prod          := com_prod	/ (ctrx_quarter	/3)	]
  dataset[ , montos2          := rowSums( cbind( mcaja_ahorro	,  mcuentas_saldo	) , na.rm=TRUE ) ]
  dataset[ , montos_sueldo_trx_mes          := montos2	/ sueldo_trx_mes		]
  dataset[ , trx_montos2          := montos2	/ (ctrx_quarter	/3)	]
  dataset[ , montos_sueldo_trx_mes          := (mrentabilidad_annual	/ 12) /mrentabilidad		]
  dataset[ , discreta1          := ifelse( ctrx_quarter > mean(ctrx_quarter) , 1, 0)	]
  dataset[ , discreta2          := ifelse( mtarjetas > mean(mtarjetas) , 1, 0)	]
  dataset[ , discreta3          := ifelse( mcaja_ahorro > mode(mcaja_ahorro) , 1, 0)	]

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

EnriquecerDataset( dataset1, "./datasets/paquete_premium_202011_extF.csv" )
EnriquecerDataset( dataset2, "./datasets/paquete_premium_202101_extF.csv" )

quit( save="no")
