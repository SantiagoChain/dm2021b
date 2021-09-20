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
  
  #LAS VARIABLES QUE NO USO LAS SACO EN "CAMPOS MALOS"
  
  #se crean los nuevos campos para MasterCard  y Visa, teniendo en cuenta los NA's
  #varias formas de combinar Visa_status y Master_status
  dataset[ , mv_status01       := pmax( Master_status,  Visa_status, na.rm = TRUE) ]
  dataset[ , mv_status02       := Master_status +  Visa_status ]
  dataset[ , mv_status03       := pmax( ifelse( is.na(Master_status), 10, Master_status) , ifelse( is.na(Visa_status), 10, Visa_status) ) ]
  dataset[ , mv_status04       := ifelse( is.na(Master_status), 10, Master_status)  +  ifelse( is.na(Visa_status), 10, Visa_status)  ]
  dataset[ , mv_status05       := ifelse( is.na(Master_status), 10, Master_status)  +  100*ifelse( is.na(Visa_status), 10, Visa_status)  ]

  dataset[ , mv_status06       := ifelse( is.na(Visa_status), 
                                          ifelse( is.na(Master_status), 10, Master_status), 
                                          Visa_status)  ]

  dataset[ , mv_status07       := ifelse( is.na(Master_status), 
                                          ifelse( is.na(Visa_status), 10, Visa_status), 
                                          Master_status)  ]


  #combino MasterCard y Visa
 dataset[ , mv_mfinanciacion_limite := rowSums( cbind( Master_mfinanciacion_limite,  Visa_mfinanciacion_limite) , na.rm=TRUE ) ]

  dataset[ , mv_Fvencimiento         := pmin( Master_Fvencimiento, Visa_Fvencimiento, na.rm = TRUE) ]
  dataset[ , mv_Finiciomora          := pmin( Master_Finiciomora, Visa_Finiciomora, na.rm = TRUE) ]
  dataset[ , mv_msaldototal          := rowSums( cbind( Master_msaldototal,  Visa_msaldototal) , na.rm=TRUE ) ]
  dataset[ , mv_msaldopesos          := rowSums( cbind( Master_msaldopesos,  Visa_msaldopesos) , na.rm=TRUE ) ]
  dataset[ , mv_msaldodolares        := rowSums( cbind( Master_msaldodolares,  Visa_msaldodolares) , na.rm=TRUE ) ]
  dataset[ , mv_mconsumospesos       := rowSums( cbind( Master_mconsumospesos,  Visa_mconsumospesos) , na.rm=TRUE ) ]
  dataset[ , mv_mconsumosdolares     := rowSums( cbind( Master_mconsumosdolares,  Visa_mconsumosdolares) , na.rm=TRUE ) ]
  dataset[ , mv_mlimitecompra        := rowSums( cbind( Master_mlimitecompra,  Visa_mlimitecompra) , na.rm=TRUE ) ]
  dataset[ , mv_madelantopesos       := rowSums( cbind( Master_madelantopesos,  Visa_madelantopesos) , na.rm=TRUE ) ]
  dataset[ , mv_madelantodolares     := rowSums( cbind( Master_madelantodolares,  Visa_madelantodolares) , na.rm=TRUE ) ]
  dataset[ , mv_fultimo_cierre       := pmax( Master_fultimo_cierre, Visa_fultimo_cierre, na.rm = TRUE) ]
  dataset[ , mv_mpagado              := rowSums( cbind( Master_mpagado,  Visa_mpagado) , na.rm=TRUE ) ]
  dataset[ , mv_mpagospesos          := rowSums( cbind( Master_mpagospesos,  Visa_mpagospesos) , na.rm=TRUE ) ]
  dataset[ , mv_mpagosdolares        := rowSums( cbind( Master_mpagosdolares,  Visa_mpagosdolares) , na.rm=TRUE ) ]
  dataset[ , mv_fechaalta            := pmax( Master_fechaalta, Visa_fechaalta, na.rm = TRUE) ]
  dataset[ , mv_mconsumototal        := rowSums( cbind( Master_mconsumototal,  Visa_mconsumototal) , na.rm=TRUE ) ]
  dataset[ , mv_cconsumos            := rowSums( cbind( Master_cconsumos,  Visa_cconsumos) , na.rm=TRUE ) ]
  dataset[ , mv_cadelantosefectivo   := rowSums( cbind( Master_cadelantosefectivo,  Visa_cadelantosefectivo) , na.rm=TRUE ) ]
  dataset[ , mv_mpagominimo          := rowSums( cbind( Master_mpagominimo,  Visa_mpagominimo) , na.rm=TRUE ) ]

  #a partir de aqui juego con la suma de Mastercard y Visa
  dataset[ , mvr_Master_mlimitecompra:= Master_mlimitecompra / mv_mlimitecompra ]
  dataset[ , mvr_Visa_mlimitecompra  := Visa_mlimitecompra / mv_mlimitecompra ]
  dataset[ , mvr_msaldototal         := mv_msaldototal / mv_mlimitecompra ]
  dataset[ , mvr_msaldopesos         := mv_msaldopesos / mv_mlimitecompra ]
  dataset[ , mvr_msaldopesos2        := mv_msaldopesos / mv_msaldototal ]
  dataset[ , mvr_msaldodolares       := mv_msaldodolares / mv_mlimitecompra ]
  dataset[ , mvr_msaldodolares2      := mv_msaldodolares / mv_msaldototal ]
  dataset[ , mvr_mconsumospesos      := mv_mconsumospesos / mv_mlimitecompra ]
  dataset[ , mvr_mconsumosdolares    := mv_mconsumosdolares / mv_mlimitecompra ]
  dataset[ , mvr_madelantopesos      := mv_madelantopesos / mv_mlimitecompra ]
  dataset[ , mvr_madelantodolares    := mv_madelantodolares / mv_mlimitecompra ]
  dataset[ , mvr_mpagado             := mv_mpagado / mv_mlimitecompra ]
  dataset[ , mvr_mpagospesos         := mv_mpagospesos / mv_mlimitecompra ]
  dataset[ , mvr_mpagosdolares       := mv_mpagosdolares / mv_mlimitecompra ]
  dataset[ , mvr_mconsumototal       := mv_mconsumototal  / mv_mlimitecompra ]
  dataset[ , mvr_mpagominimo         := mv_mpagominimo  / mv_mlimitecompra ]
  
  #Variables creadas por mi
  dataset[ , Scom_prod:= mcomisiones / cproductos ]
  dataset[ , Sant_edad:= (cliente_antiguedad / 12) / (cliente_edad - 18) ]
  dataset[ , Sctarjetas          := rowSums( cbind( ctarjeta_visa,  ctarjeta_master) , na.rm=TRUE ) ]
  dataset[ , Sctarjetas_trx       := rowSums( cbind( ctarjeta_visa_transacciones,  ctarjeta_master_transacciones) , na.rm=TRUE ) ]
  dataset[ , Smtarjetas     := rowSums( cbind( mtarjeta_visa_consumo,  mtarjeta_master_consumo) , na.rm=TRUE ) ]
  dataset[ , Smprestamos          := rowSums( cbind( mprestamos_personales,  mprestamos_prendarios, mprestamos_hipotecarios) , na.rm=TRUE ) ]
  dataset[ , Scservicios          := rowSums( cbind( cpagodeservicios,  cpagomiscuentas) , na.rm=TRUE ) ]
  dataset[ , Smservicios          := rowSums( cbind( mpagodeservicios,  mpagomiscuentas) , na.rm=TRUE ) ]
  dataset[ , Smpagominimo_tarjetas          := rowSums( cbind( Master_mpagominimo,  Visa_mpagominimo) , na.rm=TRUE ) ]
  dataset[ , Scdescuentos          := rowSums( cbind( ccajeros_propios_descuentos,  ctarjeta_visa_descuentos, ctarjeta_master_descuentos) , na.rm=TRUE ) ]
  dataset[ , Sctransferencias          := rowSums( cbind( ctransferencias_recibidas,  ctransferencias_emitidas) , na.rm=TRUE ) ]
  dataset[ , Smlimitecompra          := rowSums( cbind( Visa_mlimitecompra,  Master_mlimitecompra) , na.rm=TRUE ) ]
  dataset[ , Sctrx_ant_edad     := ctrx_quarter / Sant_edad ]
  dataset[ , Strx_mes          := rowSums( cbind( cpayroll2_trx,  catm_trx, catm_trx_other, cmobile_app_trx) , na.rm=TRUE ) ]
  dataset[ , Ssueldo_trx_mes          := cpayroll_trx / rowSums( cbind( cpayroll2_trx,  catm_trx, catm_trx_other, cmobile_app_trx) , na.rm=TRUE ) ]
  dataset[ , Sservicios          := Smservicios/Scservicios]
  dataset[ , Sedadant_sueldotrx          := Ssueldo_trx_mes/Sant_edad]
  dataset[ , Sservicios_sueldo          := cpayroll_trx/Sservicios]
  dataset[ , Sservicios_sueldo_edad_trx          := Sedadant_sueldotrx/Sservicios]
  dataset[ , Smlimitecompra_ant_edad          := Smlimitecompra/Sant_edad]
  dataset[ , Smlimitecompra_sueldo          := Smlimitecompra/cpayroll_trx]
  dataset[ , Smlimitecompra_edadant_sueldotrx          := Smlimitecompra/Sedadant_sueldotrx]
  dataset[ , Ssueldo_ctrx2          := Ssueldo_trx_mes/(ctrx_quarter	/3)]
  dataset[ , Ssueldo_ctrx3          := Ssueldo_ctrx2/Sant_edad]
  dataset[ , Ssueldo_ctrx4          := Smprestamos/Ssueldo_trx_mes]
  dataset[ , Sctrx_tarjetas          := Smtarjetas	/ctrx_quarter	]
  dataset[ , Smontosvarios_edad          := rowSums( cbind( Smtarjetas	,  mcaja_ahorro	, mcuentas_saldo	, mpayroll	) , na.rm=TRUE )/cliente_edad	 ]
  dataset[ , Strx_edad_ant          := (ctrx_quarter	/3)	/((cliente_edad	-18)*12)	]
  dataset[ , Slimite_saldo          := Smlimitecompra/mv_msaldototal]
  dataset[ , Slimite_saldo2          := Slimite_saldo/((cliente_edad	-18)*12)]
  dataset[ , Sant_edad2 := ((cliente_edad - 18)*12)/cliente_antiguedad ]
  dataset[ , Strx_edad_ant2          := (ctrx_quarter	/3)	/Sant_edad2	]
  dataset[ , Strx_prod          := Scom_prod	/ (ctrx_quarter	/3)	]
  dataset[ , Smontos2          := rowSums( cbind( mcaja_ahorro	,  mcuentas_saldo	) , na.rm=TRUE ) ]
  dataset[ , Smontos_sueldo_trx_mes          := Smontos2	/ Ssueldo_trx_mes		]
  dataset[ , Strx_montos2          := Smontos2	/ (ctrx_quarter	/3)	]
  dataset[ , Starjetas_trx          := Smtarjetas	/ (ctrx_quarter	/3)	]
  dataset[ , Svar_rent          := mrentabilidad/(mrentabilidad_annual	/ 12) ]
  dataset[ , Smontosvarios_edad_ctrx      := (rowSums( cbind( Smtarjetas	,  mcaja_ahorro	, mcuentas_saldo	, mpayroll	) , na.rm=TRUE )/cliente_edad)/(ctrx_quarter	/3)	 ]
  dataset[ , Sdiscreta1          := ifelse( ctrx_quarter < (median(ctrx_quarter)*0.25) , 1, 0)	]
  dataset[ , Sdiscreta2_pagomin  := ifelse (Visa_mpagominimo == Visa_mpagospesos, 1, 
                                            ifelse (Master_mpagominimo == Master_mpagospesos, 1,
                                            0)) ]
  dataset[ , Sdiscreta3          := ifelse( mcaja_ahorro > median(mcaja_ahorro) , 1, 0)	]
  dataset[ , Sdiscreta4          := ifelse( Smlimitecompra_sueldo < (median(Smlimitecompra_sueldo)*0.25) , 1, 0)	]
  dataset[ , Sdiscreta5_edad          := ifelse( cliente_edad < 30 , 1,
                                                 ifelse( cliente_edad < 40 , 2,
                                                         ifelse( cliente_edad < 50 , 3,
                                                                 ifelse( cliente_edad < 65 , 4,
                                                                         5))))	]
  dataset[ , Sdiscreta6_antig          := ifelse ( cliente_antiguedad < 50 , 1,
                                                 ifelse ( cliente_antiguedad < 100 , 2,
                                                         ifelse ( cliente_antiguedad < 150 , 3,
                                                                 ifelse ( cliente_antiguedad < 200 , 4,
                                                                         5))))	]
  dataset[ , Sdiscreta7rent          := ifelse ( mrentabilidad_annual == 0, 0,
                                                ifelse ( Svar_rent == 0, 1,
                                                        ifelse ( Svar_rent < 0.6, 2, 
                                                                ifelse ( Svar_rent > 1, 3,
                                                                        5))))	]
  dataset[ , Sdiscreta8          := ifelse (Sctarjetas_trx == 0 , 0,
                                            ifelse (Sctarjetas_trx < 3 , 1,
                                                    2)) ]
  dataset[ , Sdiscreta9          := ifelse (Smontos2 < (median(Smontos2)*0.8), 0, 1)]
  
  
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

EnriquecerDataset( dataset1, "./datasets/paquete_premium_202011_extG9.csv" )
EnriquecerDataset( dataset2, "./datasets/paquete_premium_202101_extG9.csv" )

quit( save="no")
