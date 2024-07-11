#!/usr/bin/env Rscript

# Experimentos Colaborativos Default
# Workflow  Data Drifting repair

# limpio la memoria
rm(list = ls(all.names = TRUE)) # remove all objects
gc(full = TRUE) # garbage collection

require("data.table")
require("yaml")

#cargo la libreria
# args <- c( "~/dm2024a" )
args <- commandArgs(trailingOnly=TRUE)
source( paste0( args[1] , "/src/lib/action_lib.r" ) )

#------------------------------------------------------------------------------
# Drift Dólar BCRA (Oficial)
# El valor del dólar oficial del último día de cada mes
# API: https://www.bcra.gob.ar/BCRAyVos/catalogo-de-APIs-banco-central.asp
#   Tipo de Cambio Minorista ($ por USD) Comunicación B 9791 - Promedio vendedor
#   Ejemplo: GET http://api.bcra.gob.ar/estadisticas/v2.0/datosvariable/4/2019-01-31/2019-01-31

drift_bcra <- function(campos_monetarios) {
  cat( "inicio drift_bcra()\n")
  
  vfoto_mes <- c(
    201901, 201902, 201903, 201904, 201905, 201906,
    201907, 201908, 201909, 201910, 201911, 201912,
    202001, 202002, 202003, 202004, 202005, 202006,
    202007, 202008, 202009, 202010, 202011, 202012,
    202101, 202102, 202103, 202104, 202105, 202106,
    202107, 202108, 202109
  )
  
  vBCRA <- c(
    38.280000, 40.140000, 44.400000, #2019
    45.360000, 46.100000, 43.700000,
    45.020000, 62.040000, 59.820000,
    63.230000, 62.930000, 62.990000, 
    63.030000, 64.260000, 66.580000, #2020
    69.160000, 70.760000, 74.070000,
    76.390000, 78.360000, 80.630000,
    83.890000, 86.720000, 89.870000,
    92.700000, 95.120000, 97.690000, #2021
    98.900000, 100.090000, 101.170000,
    102.060000, 103.140000, 104.300000
  )
  
  tb_BCRA <- as.data.table( list( vfoto_mes, vBCRA) )
  
  colnames( tb_BCRA ) <- c( envg$PARAM$dataset_metadata$periodo, "BCRA" )
  
  dataset[tb_BCRA,
          on = c(envg$PARAM$dataset_metadata$periodo),
          (campos_monetarios) := .SD / i.BCRA, # Divido cada monto por el precio del dolar
          .SDcols = campos_monetarios
  ]
  
  cat( "fin drift_bcra()\n")
}

#------------------------------------------------------------------------------
# Drift Dólar Ámbito Financiero (Blue)
# El valor del dólar blue del último día de cada mes
# Web: https://www.ambito.com/contenidos/dolar-informal-historico.html
#   Dólar blue histórico: Venta

drift_ambito <- function(campos_monetarios) {
  cat( "inicio drift_ambito()\n")
  
  vfoto_mes <- c(
    201901, 201902, 201903, 201904, 201905, 201906,
    201907, 201908, 201909, 201910, 201911, 201912,
    202001, 202002, 202003, 202004, 202005, 202006,
    202007, 202008, 202009, 202010, 202011, 202012,
    202101, 202102, 202103, 202104, 202105, 202106,
    202107, 202108, 202109
  )
  
  vAMBITO <- c(
    37.50, 39.00, 43.65, #2019
    46.00, 46.00, 43.80,
    45.20, 63.00, 61.25,
    69.00, 69.25, 78.50,
    78.00, 78.50, 83.50, #2020
    118.00, 125.00, 126.00,
    136.00, 135.00, 146.00,
    169.00, 155.00, 166.00,
    153.00, 146.00, 141.00, #2021
    150.00, 157.00, 168.00,
    180.50, 181.50, 186.00
  )
  
  tb_AMBITO <- as.data.table( list( vfoto_mes, vAMBITO) )
  
  colnames( tb_AMBITO ) <- c( envg$PARAM$dataset_metadata$periodo, "AMBITO" )
  
  dataset[tb_AMBITO,
          on = c(envg$PARAM$dataset_metadata$periodo),
          (campos_monetarios) := .SD / i.AMBITO, # Divido cada monto por el precio del dolar
          .SDcols = campos_monetarios
  ]
  
  cat( "fin drift_ambito()\n")
}

#------------------------------------------------------------------------------
# Drift UVA
# El valor del UVA del último día de cada mes
# API: https://www.bcra.gob.ar/BCRAyVos/catalogo-de-APIs-banco-central.asp
#   Unidad de Valor Adquisitivo (UVA) (en pesos -con dos decimales-, base 31.3.2016=14.05)
#   Ejemplo: GET http://api.bcra.gob.ar/estadisticas/v2.0/datosvariable/31/2019-01-01/2019-01-01

drift_uva <- function(campos_monetarios) {
  cat( "inicio drift_uva()\n")
  
  vfoto_mes <- c(
    201901, 201902, 201903, 201904, 201905, 201906,
    201907, 201908, 201909, 201910, 201911, 201912,
    202001, 202002, 202003, 202004, 202005, 202006,
    202007, 202008, 202009, 202010, 202011, 202012,
    202101, 202102, 202103, 202104, 202105, 202106,
    202107, 202108, 202109
  )
  
  vUVA <- c(
    32.030000, 32.860000, 33.970000, #2019
    35.420000, 36.890000, 38.030000,
    39.200000, 40.160000, 41.260000,
    43.430000, 45.420000, 47.160000, 
    49.050000, 50.490000, 51.620000, #2020
    52.950000, 54.240000, 55.060000,
    56.090000, 57.170000, 58.520000,
    60.160000, 61.940000, 64.320000,
    66.540000, 69.040000, 71.920000, #2021
    74.870000, 78.070000, 81.130000,
    83.820000, 86.170000, 88.760000
  )
  
  tb_UVA <- as.data.table( list( vfoto_mes, vUVA) )
  
  colnames( tb_UVA ) <- c( envg$PARAM$dataset_metadata$periodo, "UVA" )
  
  dataset[tb_UVA,
          on = c(envg$PARAM$dataset_metadata$periodo),
          (campos_monetarios) := .SD / i.UVA, # Divido cada monto por el precio del UVA
          .SDcols = campos_monetarios
  ]
  
  cat( "fin drift_uva()\n")
}

#------------------------------------------------------------------------------
# Drift Dólar BCRA (Oficial) + Deflaciona por el IPC de USA
#
# https://data.bls.gov/timeseries/CUUR0000SA0
# Base Period:	1982-84=100
# momento 1.0  2019-01

drift_dolar_deflacion <- function(campos_monetarios) {
  cat( "inicio drift_bcra()\n")
  
  vfoto_mes <- c(
    201901, 201902, 201903, 201904, 201905, 201906,
    201907, 201908, 201909, 201910, 201911, 201912,
    202001, 202002, 202003, 202004, 202005, 202006,
    202007, 202008, 202009, 202010, 202011, 202012,
    202101, 202102, 202103, 202104, 202105, 202106,
    202107, 202108, 202109
  )
  
  vBCRA <- c(
    38.280000, 40.140000, 44.400000, #2019
    45.360000, 46.100000, 43.700000,
    45.020000, 62.040000, 59.820000,
    63.230000, 62.930000, 62.990000, 
    63.030000, 64.260000, 66.580000, #2020
    69.160000, 70.760000, 74.070000,
    76.390000, 78.360000, 80.630000,
    83.890000, 86.720000, 89.870000,
    92.700000, 95.120000, 97.690000, #2021
    98.900000, 100.090000, 101.170000,
    102.060000, 103.140000, 104.300000
  )
  
  tb_BCRA <- as.data.table( list( vfoto_mes, vBCRA) )
  
  colnames( tb_BCRA ) <- c( envg$PARAM$dataset_metadata$periodo, "BCRA" )
  
  dataset[tb_BCRA,
          on = c(envg$PARAM$dataset_metadata$periodo),
          (campos_monetarios) := .SD / i.BCRA, # Divido cada monto por el precio del dolar
          .SDcols = campos_monetarios
  ]
  
  cat( "fin drift_bcra()\n")
  
  cat( "inicio drift_deflacion_usd()\n")
  
  vfoto_mes <- c(
    201901, 201902, 201903, 201904, 201905, 201906,
    201907, 201908, 201909, 201910, 201911, 201912,
    202001, 202002, 202003, 202004, 202005, 202006,
    202007, 202008, 202009, 202010, 202011, 202012,
    202101, 202102, 202103, 202104, 202105, 202106,
    202107, 202108, 202109
  )
  
  base_period <- 251.712
  
  vCPI <- c(
    base_period, 252.776, 254.202, # 2019
    255.548, 256.092, 256.143,
    256.571, 256.558, 256.759,
    257.346, 257.208, 256.974,
    257.971, 258.678, 258.115, # 2020
    256.389, 256.394, 257.797,
    259.101, 259.918, 260.280,
    260.388, 260.229, 260.474,
    261.582, 263.014, 264.877, # 2021
    267.054, 269.195, 271.696,
    273.003, 273.567, 274.310
  )
  
  tb_CPI <- as.data.table( list( vfoto_mes, vCPI) )
  
  colnames( tb_CPI ) <- c( envg$PARAM$dataset_metadata$periodo, "CPI" )
  
  dataset[tb_CPI,
          on = c(envg$PARAM$dataset_metadata$periodo),
          (campos_monetarios) := .SD * (i.CPI / base_period),
          .SDcols = campos_monetarios
  ]
  
  cat( "fin drift_deflacion_usd()\n")
}

#------------------------------------------------------------------------------
# Drift UVI
# El valor del UVI del último día de cada mes
# API: https://www.bcra.gob.ar/BCRAyVos/catalogo-de-APIs-banco-central.asp
#   Unidad de Vivienda (UVI) (en pesos -con dos decimales-, base 31.3.2016=14.05)
#   Ejemplo: GET http://api.bcra.gob.ar/estadisticas/v2.0/datosvariable/32/2019-01-01/2019-01-01

drift_uvi <- function(campos_monetarios) {
  cat( "inicio drift_uvi()\n")
  
  vfoto_mes <- c(
    201901, 201902, 201903, 201904, 201905, 201906,
    201907, 201908, 201909, 201910, 201911, 201912,
    202001, 202002, 202003, 202004, 202005, 202006,
    202007, 202008, 202009, 202010, 202011, 202012,
    202101, 202102, 202103, 202104, 202105, 202106,
    202107, 202108, 202109
  )
  
  vUVI <- c(
    31.500000, 32.210000, 32.640000, #2019
    33.250000, 34.170000, 35.020000,
    35.930000, 36.320000, 37.170000,
    39.360000, 40.710000, 42.680000, 
    44.080000, 45.130000, 47.410000, #2020
    48.970000, 49.440000, 49.640000,
    50.060000, 51.030000, 52.340000,
    54.010000, 56.000000, 61.040000,
    67.930000, 70.690000, 74.410000, #2021
    77.670000, 80.350000, 84.460000,
    86.450000, 89.100000, 92.040000
  )
  
  tb_UVI <- as.data.table( list( vfoto_mes, vUVI) )
  
  colnames( tb_UVI ) <- c( envg$PARAM$dataset_metadata$periodo, "UVI" )
  
  dataset[tb_UVI,
          on = c(envg$PARAM$dataset_metadata$periodo),
          (campos_monetarios) := .SD / i.UVI, # Divido cada monto por el precio del UVI
          .SDcols = campos_monetarios
  ]
  
  cat( "fin drift_uvi()\n")
}

#------------------------------------------------------------------------------
# deflaciona por IPC
# momento 1.0  31-dic-2020 a las 23:59

drift_deflacion <- function(campos_monetarios) {
  cat( "inicio drift_deflacion()\n")
  vfoto_mes <- c(
    201901, 201902, 201903, 201904, 201905, 201906,
    201907, 201908, 201909, 201910, 201911, 201912,
    202001, 202002, 202003, 202004, 202005, 202006,
    202007, 202008, 202009, 202010, 202011, 202012,
    202101, 202102, 202103, 202104, 202105, 202106,
    202107, 202108, 202109
  )
  
  vIPC <- c(
    1.9903030878, 1.9174403544, 1.8296186587,
    1.7728862972, 1.7212488323, 1.6776304408,
    1.6431248196, 1.5814483345, 1.4947526791,
    1.4484037589, 1.3913580777, 1.3404220402,
    1.3154288912, 1.2921698342, 1.2472681797,
    1.2300475145, 1.2118694724, 1.1881073259,
    1.1693969743, 1.1375456949, 1.1065619600,
    1.0681100000, 1.0370000000, 1.0000000000,
    0.9680542110, 0.9344152616, 0.8882274350,
    0.8532444140, 0.8251880213, 0.8003763543,
    0.7763107219, 0.7566381305, 0.7289384687
  )
  
  tb_IPC <- as.data.table( list( vfoto_mes, vIPC) )
  
  colnames( tb_IPC ) <- c( envg$PARAM$dataset_metadata$periodo, "IPC" )
  
  dataset[tb_IPC,
          on = c(envg$PARAM$dataset_metadata$periodo),
          (campos_monetarios) := .SD * i.IPC,
          .SDcols = campos_monetarios
  ]
  
  cat( "fin drift_deflacion()\n")
}

#------------------------------------------------------------------------------

drift_rank_simple <- function(campos_drift) {
  
  cat( "inicio drift_rank_simple()\n")
  for (campo in campos_drift)
  {
    cat(campo, " ")
    dataset[, paste0(campo, "_rank") :=
              (frank(get(campo), ties.method = "random") - 1) / (.N - 1), by = eval(envg$PARAM$dataset_metadata$periodo)]
    dataset[, (campo) := NULL]
  }
  cat( "fin drift_rank_simple()\n")
}
#------------------------------------------------------------------------------
# El cero se transforma en cero
# los positivos se rankean por su lado
# los negativos se rankean por su lado

drift_rank_cero_fijo <- function(campos_drift) {
  
  cat( "inicio drift_rank_cero_fijo()\n")
  for (campo in campos_drift)
  {
    cat(campo, " ")
    dataset[get(campo) == 0, paste0(campo, "_rank") := 0]
    dataset[get(campo) > 0, paste0(campo, "_rank") :=
              frank(get(campo), ties.method = "random") / .N, by = eval(envg$PARAM$dataset_metadata$periodo)]
    
    dataset[get(campo) < 0, paste0(campo, "_rank") :=
              -frank(-get(campo), ties.method = "random") / .N, by = eval(envg$PARAM$dataset_metadata$periodo)]
    dataset[, (campo) := NULL]
  }
  cat("\n")
  cat( "fin drift_rank_cero_fijo()\n")
}
#------------------------------------------------------------------------------

drift_estandarizar <- function(campos_drift) {
  
  cat( "inicio drift_estandarizar()\n")
  for (campo in campos_drift)
  {
    cat(campo, " ")
    dataset[, paste0(campo, "_normal") := 
              (get(campo) -mean(campo, na.rm=TRUE)) / sd(get(campo), na.rm=TRUE),
            by = eval(envg$PARAM$dataset_metadata$periodo)]
    
    dataset[, (campo) := NULL]
  }
  cat( "fin drift_estandarizar()\n")
}
#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
# Aqui comienza el programa
cat( "z541_DR_corregir_drifting.r  START\n")
action_inicializar() 

# cargo el dataset donde voy a entrenar
# esta en la carpeta del exp_input y siempre se llama  dataset.csv.gz
# cargo el dataset
envg$PARAM$dataset <- paste0( "./", envg$PARAM$input, "/dataset.csv.gz" )
envg$PARAM$dataset_metadata <- read_yaml( paste0( "./", envg$PARAM$input, "/dataset_metadata.yml" ) )

cat( "lectura del dataset\n")
action_verificar_archivo( envg$PARAM$dataset )
cat( "Iniciando lectura del dataset\n" )
dataset <- fread(envg$PARAM$dataset)
cat( "Finalizada lectura del dataset\n" )

GrabarOutput()

# ordeno dataset
setorderv(dataset, envg$PARAM$dataset_metadata$primarykey)

# por como armé los nombres de campos,
#  estos son los campos que expresan variables monetarias
campos_monetarios <- colnames(dataset)
campos_monetarios <- campos_monetarios[campos_monetarios %like%
                                         "^(m|Visa_m|Master_m|vm_m)"]

# aqui aplico un metodo para atacar el data drifting
# hay que probar experimentalmente cual funciona mejor
switch(envg$PARAM$metodo,
       "ninguno"        = cat("No hay correccion del data drifting"),
       "rank_simple"    = drift_rank_simple(campos_monetarios),
       "rank_cero_fijo" = drift_rank_cero_fijo(campos_monetarios),
       "deflacion"      = drift_deflacion(campos_monetarios),
       "estandarizar"   = drift_estandarizar(campos_monetarios),
       "bcra"           = drift_bcra(campos_monetarios),
       "ambito"         = drift_ambito(campos_monetarios),
       "uva"            = drift_uva(campos_monetarios),
       "dolar_deflacion"= drift_dolar_deflacion(campos_monetarios),
       "uvi"            = drift_uvi(campos_monetarios)
)


#------------------------------------------------------------------------------
# grabo el dataset
cat( "escritura del dataset\n")
cat( "Iniciando grabado del dataset\n" )
fwrite(dataset,
       file = "dataset.csv.gz",
       logical01 = TRUE,
       sep = ","
)
cat( "Finalizado grabado del dataset\n" )

# copia la metadata sin modificar
cat( "escritura de metadata\n")
write_yaml( envg$PARAM$dataset_metadata, 
            file="dataset_metadata.yml" )

#------------------------------------------------------------------------------

# guardo los campos que tiene el dataset
tb_campos <- as.data.table(list(
  "pos" = 1:ncol(dataset),
  "campo" = names(sapply(dataset, class)),
  "tipo" = sapply(dataset, class),
  "nulos" = sapply(dataset, function(x) {
    sum(is.na(x))
  }),
  "ceros" = sapply(dataset, function(x) {
    sum(x == 0, na.rm = TRUE)
  })
))

fwrite(tb_campos,
       file = "dataset.campos.txt",
       sep = "\t"
)

#------------------------------------------------------------------------------
cat( "Fin del programa\n")

envg$OUTPUT$dataset$ncol <- ncol(dataset)
envg$OUTPUT$dataset$nrow <- nrow(dataset)
envg$OUTPUT$time$end <- format(Sys.time(), "%Y%m%d %H%M%S")
GrabarOutput()

#------------------------------------------------------------------------------
# finalizo la corrida
#  archivos tiene a los files que debo verificar existen para no abortar

action_finalizar( archivos = c("dataset.csv.gz","dataset_metadata.yml")) 
cat( "z541_DR_corregir_drifting.r  END\n")