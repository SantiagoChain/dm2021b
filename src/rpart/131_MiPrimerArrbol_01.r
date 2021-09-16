#Arbol elemental con libreria  rpart
require("data.table")
require("rpart")
require("rpart.plot")

#cargo los datos
dataset  <- fread("C:\\Users\\SCHAIN\\OneDrive - Pampa Energia\\Desktop\\ECD\\DataMining\\DM_EF\\datasetsOri\\paquete_premium_202011.csv")

# generacion del modelo
modelo  <- rpart("clase_ternaria ~ .",  
                  data = dataset, 
                  cp= -1,
                  maxdepth= 3
                  )

#impresion elaborada del arbol
jpeg(file ="C:\\Users\\SCHAIN\\OneDrive - Pampa Energia\\Desktop\\ECD\\DataMining\\DM_EF\\work\\MiPrimerArbol_03.jpg",  width = 6, height = 4, units = 'in', res = 300)
prp(modelo, extra=101, digits=5, branch=1, type=4, varlen=0, faclen=0)
dev.off()


#hipotesis Dolores
dataset[ , clase_Dolores := "MALO"]
dataset[ clase_ternaria=="BAJA+2" , clase_Dolores := "BUENO"]

#generacion del modelo
modelo <- rpart("clase_Dolores ~ . -clase_ternaria",
                data = dataset,
                cp= -1,
                maxdepth= 3
)

#impresion elaborada del arbol
jpeg(file ="C:\\Users\\SCHAIN\\OneDrive - Pampa Energia\\Desktop\\ECD\\DataMining\\DM_EF\\work\\ArbolDolores.jpg", width = 6, height = 4, units = 'in', res = 300)
prp(modelo, extra=101, digits=5, branch=1, type=4, varlen=0, faclen=0)
dev.off()
# generacion del modelo
modelo  <- rpart("clase_ternaria ~ .",  
                 data = dataset, 
                 cp= -1,
                 maxdepth= 2
)

#impresion elaborada del arbol
jpeg(file ="C:\\Users\\SCHAIN\\OneDrive - Pampa Energia\\Desktop\\ECD\\DataMining\\DM_EF\\work\\MiPrimerArbol_03.jpg",  width = 6, height = 4, units = 'in', res = 300)
prp(modelo, extra=101, digits=5, branch=1, type=4, varlen=0, faclen=0)
dev.off()


# Es util el arbol que salió ?
# Que ganancia tiene cada hoja de ese arbol ?

