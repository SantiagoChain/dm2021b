#Necesita para correr en Google Cloud
#64 GB de memoria RAM
#256 GB de espacio en el disco local
#8 vCPU


#limpio la memoria
rm( list=ls() )
gc()

library( "data.table" )


setwd( "~/buckets/b1/" )
#lectura rapida del dataset  usando fread  de la libreria  data.table
dataset <- fread( "./datasetsOri/paquete_premium.csv.gz")


#acomodo los errores del dataset
dataset[ foto_mes==201801,  internet   := NA ]
dataset[ foto_mes==201801,  thomebanking   := NA ]
dataset[ foto_mes==201801,  chomebanking_transacciones   := NA ]
dataset[ foto_mes==201801,  tcallcenter   := NA ]
dataset[ foto_mes==201801,  ccallcenter_transacciones   := NA ]
dataset[ foto_mes==201801,  cprestamos_personales   := NA ]
dataset[ foto_mes==201801,  mprestamos_personales   := NA ]
dataset[ foto_mes==201801,  mprestamos_hipotecarios  := NA ]
dataset[ foto_mes==201801,  ccajas_transacciones   := NA ]
dataset[ foto_mes==201801,  ccajas_consultas   := NA ]
dataset[ foto_mes==201801,  ccajas_depositos   := NA ]
dataset[ foto_mes==201801,  ccajas_extracciones   := NA ]
dataset[ foto_mes==201801,  ccajas_otras   := NA ]
dataset[ foto_mes==201801,  tcuentas   := NA ]
dataset[ foto_mes==201801,  mcomisiones   := NA ]
dataset[ foto_mes==201801,  cpagodeservicios     := NA ]
dataset[ foto_mes==201801,  mpagodeservicios     := NA ]

dataset[ foto_mes==201802,  tcuentas   := NA ]
dataset[ foto_mes==201802,  mcomisiones   := NA ]
dataset[ foto_mes==201802,  cpagodeservicios     := NA ]
dataset[ foto_mes==201802,  mpagodeservicios     := NA ]

dataset[ foto_mes==201803,  tcuentas   := NA ]
dataset[ foto_mes==201803,  mcomisiones   := NA ]
dataset[ foto_mes==201803,  cpagodeservicios     := NA ]
dataset[ foto_mes==201803,  mpagodeservicios     := NA ]

dataset[ foto_mes==201804,  tcuentas   := NA ]
dataset[ foto_mes==201804,  mcomisiones   := NA ]
dataset[ foto_mes==201804,  cpagodeservicios     := NA ]
dataset[ foto_mes==201804,  mpagodeservicios     := NA ]

dataset[ foto_mes==201805,  tcuentas   := NA ]
dataset[ foto_mes==201805,  mcomisiones   := NA ]
dataset[ foto_mes==201805,  cpagodeservicios     := NA ]
dataset[ foto_mes==201805,  mpagodeservicios     := NA ]

dataset[ foto_mes==201806,  tcuentas   := NA ]
dataset[ foto_mes==201806,  tcallcenter   :=  NA ]
dataset[ foto_mes==201806,  ccallcenter_transacciones   :=  NA ]
dataset[ foto_mes==201806,  mcomisiones   := NA ]
dataset[ foto_mes==201806,  cpagodeservicios     := NA ]
dataset[ foto_mes==201806,  mpagodeservicios     := NA ]

dataset[ foto_mes==201807,  mcomisiones   := NA ]
dataset[ foto_mes==201807,  cpagodeservicios     := NA ]
dataset[ foto_mes==201807,  mpagodeservicios     := NA ]

dataset[ foto_mes==201808,  mcomisiones   := NA ]
dataset[ foto_mes==201808,  cpagodeservicios     := NA ]
dataset[ foto_mes==201808,  mpagodeservicios     := NA ]

dataset[ foto_mes==201809,  mcomisiones   := NA ]
dataset[ foto_mes==201809,  cpagodeservicios     := NA ]
dataset[ foto_mes==201809,  mpagodeservicios     := NA ]

dataset[ foto_mes==201810,  mcomisiones   := NA ]
dataset[ foto_mes==201810,  cpagodeservicios     := NA ]
dataset[ foto_mes==201810,  mpagodeservicios     := NA ]

dataset[ foto_mes==201811,  mcomisiones   := NA ]
dataset[ foto_mes==201811,  cpagodeservicios     := NA ]
dataset[ foto_mes==201811,  mpagodeservicios     := NA ]

dataset[ foto_mes==201812,  mcomisiones   := NA ]
dataset[ foto_mes==201812,  cpagodeservicios     := NA ]
dataset[ foto_mes==201812,  mpagodeservicios     := NA ]

dataset[ foto_mes==201901,  mrentabilidad   := NA ]
dataset[ foto_mes==201901,  mcomisiones_mantenimiento   := NA ]
dataset[ foto_mes==201901,  mcomisiones_otras   := NA ]
dataset[ foto_mes==201901,  mplazo_fijo_dolares   := NA ]
dataset[ foto_mes==201901,  mplazo_fijo_pesos   := NA ]
dataset[ foto_mes==201901,  mcomisiones   := NA ]
dataset[ foto_mes==201901,  cpagodeservicios     := NA ]
dataset[ foto_mes==201901,  mpagodeservicios     := NA ]

dataset[ foto_mes==201902,  mcomisiones   := NA ]
dataset[ foto_mes==201902,  cpagodeservicios     := NA ]
dataset[ foto_mes==201902,  mpagodeservicios     := NA ]

dataset[ foto_mes==201903,  cpagodeservicios     := NA ]
dataset[ foto_mes==201903,  mpagodeservicios     := NA ]

dataset[ foto_mes==201904,  ctarjeta_visa_debitos_automaticos  :=  NA ]
dataset[ foto_mes==201904,  Visa_mfinanciacion_limite := NA ]
dataset[ foto_mes==201904,  chomebanking_transacciones := NA ]
dataset[ foto_mes==201904,  cpagodeservicios     := NA ]
dataset[ foto_mes==201904,  mpagodeservicios     := NA ]

dataset[ foto_mes==201905,  mrentabilidad     := NA ]
dataset[ foto_mes==201905,  mrentabilidad_annual     := NA ]
dataset[ foto_mes==201905,  mcomisiones      := NA ]
dataset[ foto_mes==201905,  mpasivos_margen  := NA ]
dataset[ foto_mes==201905,  mactivos_margen  := NA ]
dataset[ foto_mes==201905,  ctarjeta_visa_debitos_automaticos  := NA ]
dataset[ foto_mes==201905,  ccomisiones_otras := NA ]
dataset[ foto_mes==201905,  mcomisiones_otras := NA ]
dataset[ foto_mes==201905,  mrentabilidad     := NA ]
dataset[ foto_mes==201905,  mrentabilidad_annual     := NA ]
dataset[ foto_mes==201905,  cpagodeservicios     := NA ]
dataset[ foto_mes==201905,  mpagodeservicios     := NA ]

ataset[ foto_mes==201906,  mrentabilidad     := NA ]
dataset[ foto_mes==201906,  mrentabilidad_annual     := NA ]

dataset[ foto_mes==201910,  mpasivos_margen   := NA ]
dataset[ foto_mes==201910,  mactivos_margen   := NA ]
dataset[ foto_mes==201910,  ccomisiones_otras := NA ]
dataset[ foto_mes==201910,  mcomisiones_otras := NA ]
dataset[ foto_mes==201910,  mcomisiones       := NA ]
dataset[ foto_mes==201910,  mrentabilidad     := NA ]
dataset[ foto_mes==201910,  mrentabilidad_annual        := NA ]
dataset[ foto_mes==201910,  chomebanking_transacciones  := NA ]
dataset[ foto_mes==201910,  ctarjeta_visa_descuentos    := NA ]
dataset[ foto_mes==201910,  ctarjeta_master_descuentos  := NA ]
dataset[ foto_mes==201910,  mtarjeta_visa_descuentos    := NA ]
dataset[ foto_mes==201910,  mtarjeta_master_descuentos  := NA ]
dataset[ foto_mes==201910,  ccajeros_propios_descuentos := NA ]
dataset[ foto_mes==201910,  mcajeros_propios_descuentos := NA ]

dataset[ foto_mes==201911,  mrentabilidad := NA ]
dataset[ foto_mes==201911,  mrentabilidad_annual := NA ]

dataset[ foto_mes==202001,  cliente_vip   := NA ]
dataset[ foto_mes==202001,  mrentabilidad   := NA ]
dataset[ foto_mes==202001, mrentabilidad_annual := NA ]
         
dataset[ foto_mes==202002, ccajeros_propios_descuentos := NA ]
dataset[ foto_mes==202002, mcajeros_propios_descuentos := NA ]
dataset[ foto_mes==202002, ctarjeta_master_descuentos := NA ]
dataset[ foto_mes==202002, ctarjeta_visa_descuentos := NA ]
dataset[ foto_mes==202002, mtarjeta_master_descuentos := NA ]
dataset[ foto_mes==202002, mtarjeta_visa_descuentos := NA ]
dataset[ foto_mes==202002, ccomisiones_otras := NA ]
dataset[ foto_mes==202002, mcomisiones_otras := NA ]

dataset[ foto_mes==202006,  active_quarter   := NA ]
dataset[ foto_mes==202006,  internet   := NA ]
dataset[ foto_mes==202006,  mrentabilidad   := NA ]
dataset[ foto_mes==202006,  mrentabilidad_annual   := NA ]
dataset[ foto_mes==202006,  mcomisiones   := NA ]
dataset[ foto_mes==202006,  mactivos_margen   := NA ]
dataset[ foto_mes==202006,  mpasivos_margen   := NA ]
dataset[ foto_mes==202006,  mcuentas_saldo   := NA ]
dataset[ foto_mes==202006,  ctarjeta_debito_transacciones   := NA ]
dataset[ foto_mes==202006,  mautoservicio   := NA ]
dataset[ foto_mes==202006,  ctarjeta_visa_transacciones   := NA ]
dataset[ foto_mes==202006,  mtarjeta_visa_consumo   := NA ]
dataset[ foto_mes==202006,  ctarjeta_master_transacciones   := NA ]
dataset[ foto_mes==202006,  mtarjeta_master_consumo   := NA ]
dataset[ foto_mes==202006,  ccomisiones_otras   := NA ]
dataset[ foto_mes==202006,  mcomisiones_otras   := NA ]
dataset[ foto_mes==202006,  cextraccion_autoservicio   := NA ]
dataset[ foto_mes==202006,  mextraccion_autoservicio   := NA ]
dataset[ foto_mes==202006,  ccheques_depositados   := NA ]
dataset[ foto_mes==202006,  mcheques_depositados   := NA ]
dataset[ foto_mes==202006,  ccheques_emitidos   := NA ]
dataset[ foto_mes==202006,  mcheques_emitidos   := NA ]
dataset[ foto_mes==202006,  ccheques_depositados_rechazados   := NA ]
dataset[ foto_mes==202006,  mcheques_depositados_rechazados   := NA ]
dataset[ foto_mes==202006,  ccheques_emitidos_rechazados   := NA ]
dataset[ foto_mes==202006,  mcheques_emitidos_rechazados   := NA ]
dataset[ foto_mes==202006,  tcallcenter   := NA ]
dataset[ foto_mes==202006,  ccallcenter_transacciones   := NA ]
dataset[ foto_mes==202006,  thomebanking   := NA ]
dataset[ foto_mes==202006,  chomebanking_transacciones   := NA ]
dataset[ foto_mes==202006,  ccajas_transacciones   := NA ]
dataset[ foto_mes==202006,  ccajas_consultas   := NA ]
dataset[ foto_mes==202006,  ccajas_depositos   := NA ]
dataset[ foto_mes==202006,  ccajas_extracciones   := NA ]
dataset[ foto_mes==202006,  ccajas_otras   := NA ]
dataset[ foto_mes==202006,  catm_trx   := NA ]
dataset[ foto_mes==202006,  matm   := NA ]
dataset[ foto_mes==202006,  catm_trx_other   := NA ]
dataset[ foto_mes==202006,  matm_other   := NA ]
dataset[ foto_mes==202006,  ctrx_quarter   := NA ]
dataset[ foto_mes==202006,  tmobile_app   := NA ]
dataset[ foto_mes==202006,  cmobile_app_trx   := NA ]

dataset[ foto_mes==20209,  ccajeros_propios_descuentos  := NA ]
dataset[ foto_mes==20209,  mcajeros_propios_descuentos  := NA ]
dataset[ foto_mes==20209,  ctarjeta_master_descuentos  := NA ]
dataset[ foto_mes==20209,  ctarjeta_visa_descuentos  := NA ]
dataset[ foto_mes==20209,  mtarjeta_master_descuentos  := NA ]
dataset[ foto_mes==20209,  mtarjeta_visa_descuentos  := NA ]

dataset[ foto_mes==20210,  ccajeros_propios_descuentos  := NA ]
dataset[ foto_mes==20210,  mcajeros_propios_descuentos  := NA ]
dataset[ foto_mes==20210,  ctarjeta_master_descuentos  := NA ]
dataset[ foto_mes==20210,  ctarjeta_visa_descuentos  := NA ]
dataset[ foto_mes==20210,  mtarjeta_master_descuentos  := NA ]
dataset[ foto_mes==20210,  mtarjeta_visa_descuentos  := NA ]
dataset[ foto_mes==202010,  internet  := NA ]

dataset[ foto_mes==202011,  internet  := NA ]
dataset[ foto_mes==202012,  internet  := NA ]
dataset[ foto_mes==202101,  internet  := NA ]

dataset[ foto_mes==202009,  tmobile_app  := NA ]
dataset[ foto_mes==202010,  tmobile_app  := NA ]
dataset[ foto_mes==202011,  tmobile_app  := NA ]
dataset[ foto_mes==202012,  tmobile_app  := NA ]
dataset[ foto_mes==202101,  tmobile_app  := NA ]

#grabo con nombre extendido
fwrite( dataset,
        file="./datasets/paquete_premium_corregido.csv.gz",
        sep= "\t" )


quit( save="no")
