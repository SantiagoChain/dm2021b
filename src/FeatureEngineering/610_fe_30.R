#Feature Engineering
#creo nuevas variables dentro del mismo mes
#Condimentar a gusto con nuevas variables

#limpio la memoria
rm( list=ls() )
gc()

require("data.table")



#Establezco el Working Directory
setwd( "~/buckets/b11" )


EnriquecerDataset <- function( dataset , arch_destino )
{
  columnas_originales <-  copy(colnames( dataset ))
  
  #INICIO de la seccion donde se deben hacer cambios con variables nuevas
  
  #se crean los nuevos campos para MasterCard  y Visa, teniendo en cuenta los NA's
  
  #Variables GUSTAVO = 43
  #varias formas de combinar Visa_status y Master_status (son 7)
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
  
  
  #combino MasterCard y Visa (son 20)
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
  
  #a partir de aqui juego con la suma de Mastercard y Visa (16)
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
  
  #Variables creadas por mi (separo de a 10)
  
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
  
  dataset[ , Smontosvarios_edad_cpayroll      := (rowSums( cbind( Smtarjetas	,  mcaja_ahorro	, mcuentas_saldo	, mpayroll	) , na.rm=TRUE )/cliente_edad)/cpayroll_trx	 ]
  dataset[ , Sant_edad2 := ((cliente_edad - 18)*12)/cliente_antiguedad ]
  dataset[ , Strx_edad_ant2          := (ctrx_quarter	/3)	/Sant_edad2	]
  dataset[ , Strx_prod          := Scom_prod	/ (ctrx_quarter	/3)	]
  dataset[ , Smontos2          := rowSums( cbind( mcaja_ahorro	,  mcuentas_saldo	) , na.rm=TRUE ) ]
  dataset[ , Smontos_sueldo_trx_mes          := Smontos2	/ Ssueldo_trx_mes		]
  dataset[ , Strx_montos2          := Smontos2	/ (ctrx_quarter	/3)	]
  dataset[ , Starjetas_trx          := Smtarjetas	/ (ctrx_quarter	/3)	]
  dataset[ , Smontosvarios_edad_ctrx      := (rowSums( cbind( Smtarjetas	,  mcaja_ahorro	, mcuentas_saldo	, mpayroll	) , na.rm=TRUE )/cliente_edad)/(ctrx_quarter	/3)	 ]
  dataset[ , Sdiscreta2_pagomin  := ifelse (Visa_mpagominimo == Visa_mpagospesos, 1, 
                                            ifelse (Master_mpagominimo == Master_mpagospesos, 1,
                                                    0)) ]
  
  dataset[ , Smean_caja_mcuen          := (mcuentas_saldo/mean(mcaja_ahorro))*(mcaja_ahorro/mean(mcuentas_saldo))]
  dataset[ , Smcuentas_ctrx          := mcuentas_saldo/(ctrx_quarter/3)]
  dataset[ , Starj_mvaredad          := Smtarjetas	/Smontosvarios_edad]
  dataset[ , Starj_mvaredad          := Smtarjetas	/Smontosvarios_edad]
  dataset[ , mean_median1  := ifelse (Smontosvarios_edad < mean(Smontosvarios_edad),  
                                      ifelse (ctrx_quarter < (median(ctrx_quarter))*0.75, 1, 0),0)]
  dataset[ , Ssueldo_ctrx4          := Ssueldo_ctrx3/(median(cpayroll_trx))]
  dataset[ , Ssueldo_ctrx5          := (Ssueldo_ctrx2*Ssueldo_ctrx3*Ssueldo_ctrx4)/Smontosvarios_edad]
  dataset[ , Smontosvarios_edad2          := Smontosvarios_edad/(median(ctrx_quarter))]
  dataset[ , Smontosvarios_edad3          := median(Smontosvarios_edad)/(var(Sctrx_ant_edad))]
  dataset[ , Smontosvarios_edad4          := var(Smontosvarios_edad)/(median(ctrx_quarter))]
  
  dataset[ , Smontosvarios_edad5          := var(ctrx_quarter)/Smontosvarios_edad]
  dataset[ , Smontosvarios_edad6          := var(Sctrx_ant_edad)/Smontosvarios_edad]
  dataset[ , Smontosvarios_edad7          := (Sctrx_ant_edad/Smontosvarios_edad)*Sctrx_ant_edad]
  dataset[ , Smontosvarios_edad8          := (Smontosvarios_edad/Smtarjetas)/(1/(Sctrx_ant_edad))]
  dataset[ , Smontosvarios_edad9          := (ctrx_quarter*((Smontosvarios_edad/Smtarjetas)/
                                                              (1/(Sctrx_ant_edad))))/(var(ctrx_quarter))]
  dataset[ , Smontosvarios_edad10       := (Smontos2/Smontosvarios_edad)*((ctrx_quarter*((Smontosvarios_edad/Smtarjetas)/
                                                                                           (1/(Sctrx_ant_edad))))/(var(ctrx_quarter)))]
  dataset[ , Smrentabilidad1          := ifelse (mrentabilidad < (0.75*(mean(mrentabilidad))),1,0)]
  dataset[ , Smrentabilidad2          := ifelse (mrentabilidad < (0.75*(median(mrentabilidad))),1,0)]
  dataset[ , Smrentabilidad3          := ifelse (Smrentabilidad2 == 1 ,
                                                 ifelse (mrentabilidad_annual < (0.75*(median(mrentabilidad_annual))),1,0),0)]
  dataset[ , Smrentabilidad4          := ifelse (Smrentabilidad1 == 1 ,
                                                 ifelse (mrentabilidad_annual < (0.75*(mean(mrentabilidad_annual))),1,0),0)]
  
  dataset[ , Smrentabilidad_other1      := Smontosvarios_edad*Smrentabilidad1]
  dataset[ , Smrentabilidad_other2      := Smontosvarios_edad*Smrentabilidad2]
  dataset[ , Smrentabilidad_other3      := Smontosvarios_edad*Smrentabilidad3]
  dataset[ , Smrentabilidad_other4      := Smontosvarios_edad*Smrentabilidad4]
  dataset[ , Smpasivos1      := ifelse (mpasivos_margen < (0.75*(mean(mpasivos_margen))),1,0)]
  dataset[ , Smpasivos2     := ifelse (mpasivos_margen < (0.75*(median(mpasivos_margen))),1,0)]
  dataset[ , Smactivos1      := ifelse (mactivos_margen > (0.75*(mean(mactivos_margen))),1,0)]
  dataset[ , Smactivos2     := ifelse (mactivos_margen > (0.75*(median(mactivos_margen))),1,0)]
  dataset[ , Sactpas1      := Smpasivos1 + Smactivos1]
  dataset[ , Sactpas2      := Smpasivos2 + Smactivos2]
  
  dataset[ , Sdatadriffters1     := Smrentabilidad1+Sactpas1]
  dataset[ , Sdatadriffters2      := Sdatadriffters1*Smontosvarios_edad]
  dataset[ , Sfechalta      := max(Visa_fechaalta, Master_fechaalta, na.rm=TRUE)]
  dataset[ , Sfechalta2      := Sfechalta/Sant_edad]
  dataset[ , Sfechalta3      := Sfechalta2 * Smontosvarios_edad]
  dataset[ , Smontosvarios      := rowSums( cbind( Smtarjetas,  mcaja_ahorro, mcuentas_saldo, mpayroll), na.rm=TRUE )]
  dataset[ , Smontosvarios_trx      := ifelse(ctrx_quarter==0,0,
                                              (Smontosvarios/(ctrx_quarter-mean(ctrx_quarter))))]
  dataset[ , Smpresta_montosvariosedad := ifelse(Smprestamos > 0.75 * median(Smprestamos), (Smprestamos/Smontosvarios_edad),(Smprestamos*Smontosvarios_edad))]
  dataset[ , Sctrxrara     := ifelse(ctrx_quarter > mean(ctrx_quarter),0,
                                     (Sdatadriffters1/Sant_edad))]
  dataset[ , Smforex          := rowSums( cbind( mforex_buy,  mforex_sell) , na.rm=TRUE ) ]
  
  dataset[ , Sforex1      := Smforex / cforex]
  dataset[ , Sforex2      := (Sforex1 / (cforex-mean(cforex)))/(1/(Smontosvarios_edad))]
  dataset[ , Sforex3      := (Sforex1 / (1/(ctrx_quarter/3)))]
  dataset[ , Sforex4      := Sforex3*Smprestamos]
  dataset[ , Sforex5      := Sforex3*Sfechalta]
  dataset[ , Sforex6      := Sforex3*Smtarjetas]
  dataset[ , Sforex7      := Sforex3*Ssueldo_trx_mes]
  
  dataset[ , Sservicios2      := Sservicios / (1/(ctrx_quarter/3))]
  dataset[ , Smctacte     := ifelse(mcuenta_corriente < (mean(mcuenta_corriente)*0.5),0,1)]
  dataset[ , Smcjahrro     := ifelse(mcaja_ahorro < (mean(mcaja_ahorro)*0.5),0,1)]
  dataset[ , Sctacja     := rowSums( cbind( Smctacte,  Smcjahrro) , na.rm=TRUE ) ]
  dataset[ , Smtarj     := rowSums( cbind( mtarjeta_visa_consumo,  mtarjeta_master_consumo) , na.rm=TRUE ) ]
  
  dataset[ , Sctarj     := rowSums( cbind( ctarjeta_master_transacciones,  ctarjeta_visa_transacciones) , na.rm=TRUE ) ]
  dataset[ , Smtarjdisc     := ifelse(Smtarj < (mean(Smtarj)*0.5),0,1)]
  dataset[ , Sctarjdisc     := ifelse(Sctarj < (mean(Sctarj)*0.5),0,1)]
  dataset[ , Sm_c_tarj      := ifelse((Smtarj/Sctarj) < (mean(Smtarj/Sctarj)*0.5),0,1)]
  
  dataset[ , Scpayrolltrx     := ifelse(cpayroll_trx > ((mean(cpayroll_trx))*1.5),2,1)]
  dataset[ , Smpayrolltrx     := ifelse(mpayroll > ((mean(mpayroll))*1.5),2,1)]
  dataset[ , Scplazofijo     := ifelse(cplazo_fijo > ((mean(cplazo_fijo))*1.5),2,1)] 
  dataset[ , Smplazofijo     := ifelse(mplazo_fijo_pesos> ((mean(mplazo_fijo_pesos))*1.5),2,1)] 
  
  dataset[ , Smtarjdeb     := rowSums( cbind( mttarjeta_visa_debitos_automaticos,  mttarjeta_master_debitos_automaticos) , na.rm=TRUE ) ]
  dataset[ , Smtarjdeb_disc     := ifelse(Smtarjdeb > ((mean(Smtarjdeb))*1.5),2,1)] 
  dataset[ , Ssumastarj := rowSums( cbind( Master_msaldototal,  Master_mconsumospesos, Master_mconsumototal,
                                           Visa_msaldototal, Visa_mconsumospesos, Visa_msaldopesos) , na.rm=TRUE ) ]
  dataset[ , Ssumastarj_disc     := ifelse(Ssumastarj > ((mean(Ssumastarj))*1.5),2,1)] 
  dataset[ , Sdesccajeros     := ifelse(ccajeros_propios_descuentos > ((mean(ccajeros_propios_descuentos))*1.5),2,1)] 
  dataset[ , Scheques_disc     := ifelse(ccheques_emitidos > ((mean(ccheques_emitidos))*1.5),2,1)] 
  
  
  
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

EnriquecerDataset( dataset1, "./datasets/paquete_premium_202011_ext30.csv" )
EnriquecerDataset( dataset2, "./datasets/paquete_premium_202101_ext30.csv" )

quit( save="no")