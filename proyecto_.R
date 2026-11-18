### PROYECTO SOBRE MACROECONOMÍA ###

# ==============================================================
## 1. CARGAR DATOS
# ==============================================================

library(readxl)# leer archivos Excel
library(dplyr)   # seleccionar, filtrar y transformar
library(tidyr)   # cambiar la estructura de las tablas
library(stringr) # trabajar con texto
library(lubridate)
library(ggplot2)

# Ruta del archivo del BCU
ruta_excel <- "BCU_datos.xlsx"

# Cargar datos del BCU
datos_bcu <- read_excel(
  ruta_excel,
  sheet = "Valores_K",  # hoja que queremos leer
  skip = 7              # ignoramos las primeras 7 filas
)

# Ver datos originales
View(datos_bcu)

# ==============================================================
## 2. ORDENAR Y LIMPIAR
# ==============================================================

# Renombramos la segunda columna porque contiene los nombres de las variables
names(datos_bcu)[2] <- "variable"

# Pasamos los trimestres de columnas a filas
datos_largos <- datos_bcu %>%
  pivot_longer(
    cols = starts_with(c("I ", "II ", "III ", "IV ")),
    names_to = "trimestre",
    values_to = "valor"
  )

View(datos_largos)

# Ver nombres disponibles
unique(datos_largos$variable)

# Nos quedamos con las variables que nos interesan
datos_macro <- datos_largos %>%
  filter(
    variable %in% c(
      "PRODUCTO INTERNO BRUTO",
      "Formación Bruta de Capital",
      "Exportaciones de bienes y servicios",
      "(-) Importaciones de bienes y servicios",
      "Hogares",
      "Gobierno e ISFLSH"
    )
  )

# Ponemos cada variable como columna
datos_macro <- datos_macro %>%
  select(
    variable,
    trimestre,
    valor
  ) %>%
  pivot_wider(
    names_from = variable,
    values_from = valor
  )

# Renombramos variables para trabajar más fácilmente
datos_macro <- datos_macro %>%
  rename(
    pbi = `PRODUCTO INTERNO BRUTO`,
    inversion = `Formación Bruta de Capital`,
    exportaciones = `Exportaciones de bienes y servicios`,
    importaciones = `(-) Importaciones de bienes y servicios`,
    consumo = Hogares,
    gasto_publico = `Gobierno e ISFLSH`
  )

# Eliminamos el * de algunos trimestres
datos_macro <- datos_macro %>%
  mutate(
    trimestre = gsub("\\*", "", trimestre),
    # El BCU escribe 2021 como "202I" (con I mayúscula): lo corregimos
    trimestre = gsub("202I", "2021", trimestre)
  )

# Separamos trimestre y año
datos_macro <- datos_macro %>%
  separate(
    trimestre,
    into = c("trim", "año"),
    sep = " ",
    remove = FALSE
  )

# Convertimos año a número y trimestre romano a número
datos_macro <- datos_macro %>%
  mutate(
    
    año = as.integer(año),
    
    trimestre_num = case_when(
      trim == "I" ~ 1,
      trim == "II" ~ 2,
      trim == "III" ~ 3,
      trim == "IV" ~ 4
    )
  )

# Nos quedamos con las columnas necesarias
datos_macro <- datos_macro %>%
  select(
    trimestre,
    año,
    trimestre_num,
    pbi,
    consumo,
    gasto_publico,
    inversion,
    exportaciones,
    importaciones
  )

# Ordenamos cronológicamente
datos_macro <- datos_macro %>%
  arrange(
    año,
    trimestre_num
  )

View(datos_macro)

# ==============================================================
## 3.i AGREGAR INFORMACIÓN --> VARIABLE INFLACIÓN
# ==============================================================

# Ruta del archivo del INE
ruta_inflacion <- "INE_inflacion.csv"

# Cargar archivo
inflacion_raw <- read.csv2(
  ruta_inflacion,
  skip = 7,
  header = TRUE,
  stringsAsFactors = FALSE,
  fileEncoding = "latin1"
)

View(inflacion_raw)
names(inflacion_raw)

# Crear tabla trimestral de inflación
inflacion_trimestral <- inflacion_raw %>%
  mutate(
    mes = substr(Mes.y.año, 1, 3),
    
    año = as.integer(
      paste0("20", substr(Mes.y.año, 5, 6))
    )
  ) %>%
  
  filter(
    mes %in% c("mar", "jun", "sep", "dic")
  ) %>%
  
  mutate(
    trimestre_num = case_when(
      mes == "mar" ~ 1,
      mes == "jun" ~ 2,
      mes == "sep" ~ 3,
      mes == "dic" ~ 4
    ),
    
    inflacion = Trimestre
  ) %>%
  
  select(
    año,
    trimestre_num,
    inflacion
  ) %>%
  
  distinct(
    año,
    trimestre_num,
    .keep_all = TRUE
  ) %>%
  
  arrange(
    año,
    trimestre_num
  )

View(inflacion_trimestral)

# Eliminar inflación anterior si ya existe
datos_macro <- datos_macro %>%
  select(-matches("^inflacion"))

# Unir inflación a datos_macro
datos_macro <- datos_macro %>%
  left_join(
    inflacion_trimestral,
    by = c("año", "trimestre_num")
  )

View(datos_macro)

# ==============================================================
## 3.ii AGREGAR INFORMACIÓN --> VARIABLE TIPO DE CAMBIO
# ==============================================================

# 1. Leer y unir todos los archivos CSV
carpeta_tc <- "datos_tc"

archivos_tc <- list.files(
  carpeta_tc,
  pattern = "\\.csv$",
  full.names = TRUE
)

tipo_cambio_raw <- archivos_tc %>%
  lapply(function(x) {
    read.csv2(
      x,
      stringsAsFactors = FALSE,
      fileEncoding = "latin1"
    )
  }) %>%
  bind_rows()

# Ver resultado
View(tipo_cambio_raw)

# Ver nombres de columnas
names(tipo_cambio_raw)

# Ver cantidad total de filas
nrow(tipo_cambio_raw)

# 2. Convertir Fecha a formato fecha
tipo_cambio_raw <- tipo_cambio_raw %>%
  mutate(
    Fecha = dmy(Fecha)
  )

# 3. Crear año y trimestre
tipo_cambio_raw <- tipo_cambio_raw %>%
  mutate(
    año = year(Fecha),
    trimestre_num = quarter(Fecha)
  )

View(tipo_cambio_raw)

# 4. Convertir Venta a número
tipo_cambio_raw <- tipo_cambio_raw %>%
  mutate(
    Venta = as.numeric(
      gsub(",", ".", Venta, fixed = TRUE)
    )
  )

# 5. Crear promedio trimestral del tipo de cambio
tipo_cambio_trimestral <- tipo_cambio_raw %>%
  group_by(
    año,
    trimestre_num
  ) %>%
  summarise(
    tipo_cambio = mean(
      Venta,
      na.rm = TRUE
    ),
    .groups = "drop"
  ) %>%
  arrange(
    año,
    trimestre_num
  )


View(tipo_cambio_trimestral)

# 6. Revisar que no haya trimestres repetidos
tipo_cambio_trimestral %>%
  count(
    año,
    trimestre_num
  ) %>%
  filter(n > 1)

# 7. Eliminar tipo de cambio previo si ya existe
datos_macro <- datos_macro %>%
  select(
    -matches("^tipo_cambio")
  )

# 8. Unir tipo de cambio a la base principal
datos_macro <- datos_macro %>%
  left_join(
    tipo_cambio_trimestral,
    by = c(
      "año",
      "trimestre_num"
    )
  )

# Ver resultado final
View(datos_macro)

# 9. Comprobaciones
# La cantidad de filas debería seguir siendo 41
nrow(datos_macro)

# Ver cuántos faltantes hay en tipo de cambio
sum(is.na(datos_macro$tipo_cambio))

# Ver qué trimestres no encontraron tipo de cambio
datos_macro %>%
  filter(
    is.na(tipo_cambio)
  ) %>%
  select(
    trimestre,
    año,
    trimestre_num
  )

# ==============================================================
## 4. CREAR VARIABLES PARA EL ANÁLISIS
# ==============================================================

datos_macro <- datos_macro %>%
  arrange(año, trimestre_num) %>%
  mutate(
    # Crecimiento interanual del PBI
    crecimiento_pbi = 100 * (pbi / lag(pbi, 4) - 1),
    
    # Crecimiento interanual de la inversión
    crecimiento_inversion = 100 * (inversion / lag(inversion, 4) - 1),
    
    # Crecimiento interanual de las exportaciones
    crecimiento_exportaciones = 100 * (exportaciones / lag(exportaciones, 4) - 1),
    
    # Variación interanual del tipo de cambio
    variacion_tipo_cambio = 100 * (tipo_cambio / lag(tipo_cambio, 4) - 1)
  )

# Ver la tabla con las nuevas columnas
View(datos_macro)

# ==============================================================
## 5. CREAR TABLA FINAL PARA LA REGRESIÓN
# ==============================================================

datos_modelo <- datos_macro %>%
  select(
    trimestre,
    año,
    trimestre_num,
    crecimiento_pbi,
    inflacion,
    crecimiento_inversion,
    crecimiento_exportaciones,
    variacion_tipo_cambio
  )

# Ver tabla final
View(datos_modelo)

# eliminar las filas sin crecimientos
datos_modelo <- datos_modelo %>%
  drop_na(
    crecimiento_pbi,
    inflacion,
    crecimiento_inversion,
    crecimiento_exportaciones,
    variacion_tipo_cambio
  )

# Comprobar cantidad de observaciones
nrow(datos_modelo)

# Ver resultado
View(datos_modelo)

# ==============================================================
## 5. AGREGAR VARIABLES RECOMENDADAS
# ==============================================================

# Guardar una copia de seguridad
datos_macro_original <- datos_macro

# --------------------------------------------------------------
# 5.i PBI REAL DE BRASIL
# --------------------------------------------------------------

brasil_raw <- read.csv("PBI real Brasil FRED.csv")

brasil_trimestral <- brasil_raw %>%
  mutate(
    fecha = as.Date(observation_date),
    año = year(fecha),
    trimestre_num = quarter(fecha)
  ) %>%
  arrange(fecha) %>%
  mutate(
    crecimiento_brasil =
      100 * (NGDPRNSAXDCBRQ /
               lag(NGDPRNSAXDCBRQ, 4) - 1)
  ) %>%
  select(año, trimestre_num, crecimiento_brasil)

# --------------------------------------------------------------
# 5.ii PBI REAL DE ARGENTINA
# --------------------------------------------------------------

argentina_raw <- read.csv("PBI real Argentina FRED.csv")

argentina_trimestral <- argentina_raw %>%
  mutate(
    fecha = as.Date(observation_date),
    año = year(fecha),
    trimestre_num = quarter(fecha)
  ) %>%
  arrange(fecha) %>%
  mutate(
    crecimiento_argentina =
      100 * (NGDPRNSAXDCARQ /
               lag(NGDPRNSAXDCARQ, 4) - 1)
  ) %>%
  select(año, trimestre_num, crecimiento_argentina)

# --------------------------------------------------------------
# 5.iii PBI REAL DE CHINA
# --------------------------------------------------------------

china_raw <- read.csv("PBI real China ChinaData.csv")

# El archivo tiene Year (ejemplo: 2017-Q1) y Value
china_trimestral <- china_raw %>%
  mutate(
    año = as.integer(substr(Year, 1, 4)),
    trimestre_num = as.integer(substr(Year, 7, 7)),
    pbi_china = as.numeric(Value)
  ) %>%
  arrange(año, trimestre_num) %>%
  mutate(
    crecimiento_china =
      100 * (pbi_china / lag(pbi_china, 4) - 1)
  ) %>%
  select(año, trimestre_num, crecimiento_china)

# --------------------------------------------------------------
# 5.iv TASA DE INTERES DE LA FED
# --------------------------------------------------------------

fed_raw <- read.csv("Tasa de la FED FRED.csv")

fed_trimestral <- fed_raw %>%
  mutate(
    fecha = as.Date(observation_date),
    año = year(fecha),
    trimestre_num = quarter(fecha)
  ) %>%
  group_by(año, trimestre_num) %>%
  summarise(
    tasa_fed = if (sum(!is.na(FEDFUNDS)) == 3) {
      mean(FEDFUNDS)
    } else {
      NA_real_
    },
    .groups = "drop"
  )

# --------------------------------------------------------------
# 5.v INDICE DE PRECIOS DE EXPORTACION (IPX)
# --------------------------------------------------------------

ipx_raw <- read.csv(
  "IPX Uruguay BCU econuy.csv",
  check.names = FALSE,
  stringsAsFactors = FALSE
)

# El archivo contiene filas iniciales de metadatos.
# Nos quedamos solamente con las filas de fechas.

ipx_mensual <- ipx_raw %>%
  filter(grepl("^\\d{4}-\\d{2}-\\d{2}$", Name)) %>%
  mutate(
    fecha = as.Date(Name),
    año = year(fecha),
    trimestre_num = quarter(fecha),
    ipx = as.numeric(`Total exportaciones`)
  )

# Convertir de mensual a trimestral.
# Exigimos los tres meses para cada trimestre.

ipx_trimestral <- ipx_mensual %>%
  group_by(año, trimestre_num) %>%
  summarise(
    ipx = if (sum(!is.na(ipx)) == 3) {
      mean(ipx)
    } else {
      NA_real_
    },
    .groups = "drop"
  ) %>%
  arrange(año, trimestre_num) %>%
  mutate(
    crecimiento_ipx =
      100 * (ipx / lag(ipx, 4) - 1)
  ) %>%
  select(año, trimestre_num, crecimiento_ipx)

# --------------------------------------------------------------
# 5.vi UNIR LAS CINCO VARIABLES A LA BASE ORIGINAL
# --------------------------------------------------------------

# Si ejecutamos este bloque nuevamente,
# evitamos duplicar las columnas.

datos_macro <- datos_macro %>%
  select(
    -any_of(c(
      "crecimiento_brasil",
      "crecimiento_argentina",
      "crecimiento_china",
      "tasa_fed",
      "crecimiento_ipx"
    ))
  ) %>%
  left_join(
    brasil_trimestral,
    by = c("año", "trimestre_num")
  ) %>%
  left_join(
    argentina_trimestral,
    by = c("año", "trimestre_num")
  ) %>%
  left_join(
    china_trimestral,
    by = c("año", "trimestre_num")
  ) %>%
  left_join(
    fed_trimestral,
    by = c("año", "trimestre_num")
  ) %>%
  left_join(
    ipx_trimestral,
    by = c("año", "trimestre_num")
  ) %>%
  arrange(año, trimestre_num)

# Ver tabla con las nuevas variables
View(datos_macro)

# Comprobar cantidad de filas
nrow(datos_macro)

# ==============================================================
## 6. CREAR BASE FINAL DEL MODELO
# ==============================================================

datos_modelo_nuevo <- datos_macro %>%
  select(
    trimestre,
    año,
    trimestre_num,
    crecimiento_pbi,
    crecimiento_brasil,
    crecimiento_argentina,
    crecimiento_china,
    tasa_fed,
    crecimiento_ipx,
    inflacion,
    variacion_tipo_cambio
  )

# Verificar los datos faltantes
colSums(is.na(datos_modelo_nuevo))

# Crear tabla con observaciones completas
datos_modelo_nuevo <- datos_modelo_nuevo %>%
  tidyr::drop_na()

# Ver tabla final
View(datos_modelo_nuevo)

## Comprobamos:

# Cantidad de observaciones
nrow(datos_modelo_nuevo)

# Comprobar si faltan datos
colSums(is.na(datos_modelo_nuevo))

# Revisar el período que cubre la muestra
range(datos_modelo_nuevo$año)

# Ver estadísticas descriptivas
summary(datos_modelo_nuevo)

# ==============================================================
## 7. ESTIMAR EL MODELO ECONOMETRICO
# ==============================================================

modelo_1 <- lm(
  crecimiento_pbi ~
    crecimiento_brasil +
    crecimiento_argentina +
    crecimiento_china +
    tasa_fed +
    crecimiento_ipx,
  data = datos_modelo_nuevo
)
summary(modelo_1)


# ==============================================================
## 8. ANALISIS DESCRIPTIVO Y EXPLORATORIO
# ==============================================================

# 8.i ESTADISTICAS DESCRIPTIVAS

# Seleccionamos las variables del modelo
variables_modelo <- datos_modelo_nuevo %>%
  select(
    crecimiento_pbi,
    crecimiento_brasil,
    crecimiento_argentina,
    crecimiento_china,
    tasa_fed,
    crecimiento_ipx,
    inflacion,
    variacion_tipo_cambio
  )

# Calculamos las estadisticas descriptivas
estadisticas <- variables_modelo %>%
  summarise(
    across(
      everything(),
      list(
        media = ~mean(.x),
        mediana = ~median(.x),
        minimo = ~min(.x),
        maximo = ~max(.x),
        desvio = ~sd(.x)
      )
    )
  ) %>%
  pivot_longer(
    cols = everything(),
    names_to = c("variable", ".value"),
    names_pattern =
      "^(.*)_(media|mediana|minimo|maximo|desvio)$"
  ) %>%
  mutate(
    across(where(is.numeric), ~round(.x, 2))
  )

View(estadisticas)

# --------------------------------------------------------------
## 8.ii GRAFICO DEL CRECIMIENTO DEL PBI DE URUGUAY
# --------------------------------------------------------------

# Creamos una fecha para cada trimestre
datos_modelo_nuevo <- datos_modelo_nuevo %>%
  mutate(
    fecha = as.Date(
      paste0(
        año, "-",
        (trimestre_num - 1) * 3 + 1,
        "-01"
      )
    )
  ) %>%
  arrange(fecha)

# Grafico del crecimiento del PBI uruguayo
ggplot(
  datos_modelo_nuevo,
  aes(x = fecha, y = crecimiento_pbi)
) +
  geom_line(
    color = "steelblue",
    linewidth = 1
  ) +
  geom_point(
    color = "steelblue",
    size = 2
  ) +
  geom_hline(
    yintercept = 0,
    linetype = "dashed",
    color = "gray50"
  ) +
  labs(
    title = "Evolucion del crecimiento del PBI de Uruguay",
    subtitle = "Variacion interanual - datos trimestrales",
    x = "Año",
    y = "Crecimiento del PBI (%)"
  ) +
  theme_minimal()

# --------------------------------------------------------------
## 8.iii TRIMESTRES DE MAYOR Y MENOR CRECIMIENTO
# --------------------------------------------------------------

# Trimestre con mayor crecimiento del PBI
datos_modelo_nuevo %>%
  slice_max(
    order_by = crecimiento_pbi,
    n = 1
  ) %>%
  select(trimestre, crecimiento_pbi)

# Trimestre con menor crecimiento del PBI
datos_modelo_nuevo %>%
  slice_min(
    order_by = crecimiento_pbi,
    n = 1
  ) %>%
  select(trimestre, crecimiento_pbi)

# Comprobar qué trimestres quedaron fuera de la muestra

datos_macro %>%
  filter(
    año >= 2017,
    año <= 2025
  ) %>%
  anti_join(
    datos_modelo_nuevo,
    by = c("año", "trimestre_num")
  ) %>%
  select(trimestre, año, trimestre_num)

# --------------------------------------------------------------
## 8.iv COMPARACION DEL CRECIMIENTO ENTRE PAISES
# --------------------------------------------------------------

# Seleccionamos los datos de los cuatro paises
datos_paises <- datos_modelo_nuevo %>%
  select(
    fecha,
    crecimiento_pbi,
    crecimiento_brasil,
    crecimiento_argentina,
    crecimiento_china
  ) %>%
  pivot_longer(
    cols = -fecha,
    names_to = "pais",
    values_to = "crecimiento"
  ) %>%
  mutate(
    pais = recode(
      pais,
      crecimiento_pbi = "Uruguay",
      crecimiento_brasil = "Brasil",
      crecimiento_argentina = "Argentina",
      crecimiento_china = "China"
    )
  )

# Grafico comparativo
ggplot(
  datos_paises,
  aes(
    x = fecha,
    y = crecimiento,
    color = pais
  )
) +
  geom_line(linewidth = 1) +
  geom_point(size = 1) +
  geom_hline(
    yintercept = 0,
    linetype = "dashed"
  ) +
  labs(
    title = "Crecimiento del PBI: Uruguay, Brasil, Argentina y China",
    subtitle = "Variaciones interanuales - datos trimestrales",
    x = "Año",
    y = "Crecimiento interanual (%)",
    color = "Pais"
  ) +
  theme_minimal()

# --------------------------------------------------------------
## 8.v EVOLUCION DE LA TASA DE INTERES DE LA FED
# --------------------------------------------------------------

ggplot(
  datos_modelo_nuevo,
  aes(x = fecha, y = tasa_fed)
) +
  geom_line(
    color = "darkred",
    linewidth = 1
  ) +
  geom_point(
    color = "darkred",
    size = 1.5
  ) +
  labs(
    title = "Evolucion de la tasa de interes de la Fed",
    subtitle = "Promedio trimestral",
    x = "Año",
    y = "Tasa de interes (%)"
  ) +
  theme_minimal()

# --------------------------------------------------------------
## 8.vi EVOLUCION DEL IPX DE URUGUAY
# --------------------------------------------------------------

ggplot(
  datos_modelo_nuevo,
  aes(x = fecha, y = crecimiento_ipx)
) +
  geom_line(
    color = "darkgreen",
    linewidth = 1
  ) +
  geom_point(
    color = "darkgreen",
    size = 1.5
  ) +
  geom_hline(
    yintercept = 0,
    linetype = "dashed"
  ) +
  labs(
    title = "Evolucion de los precios de exportacion de Uruguay",
    subtitle = "Variacion interanual del IPX",
    x = "Año",
    y = "Crecimiento del IPX (%)"
  ) +
  theme_minimal()

# --------------------------------------------------------------
## 8.vii EVOLUCION DE LA INFLACION
# --------------------------------------------------------------

ggplot(
  datos_modelo_nuevo,
  aes(x = fecha, y = inflacion)
) +
  geom_line(
    color = "purple",
    linewidth = 1
  ) +
  geom_point(
    color = "purple",
    size = 1.5
  ) +
  labs(
    title = "Evolucion de la inflacion en Uruguay",
    subtitle = "Inflacion trimestral",
    x = "Año",
    y = "Inflacion (%)"
  ) +
  theme_minimal()

# --------------------------------------------------------------
## 8.viii EVOLUCION DEL TIPO DE CAMBIO
# --------------------------------------------------------------

ggplot(
  datos_modelo_nuevo,
  aes(x = fecha, y = variacion_tipo_cambio)
) +
  geom_line(
    color = "orange",
    linewidth = 1
  ) +
  geom_point(
    color = "orange",
    size = 1.5
  ) +
  geom_hline(
    yintercept = 0,
    linetype = "dashed"
  ) +
  labs(
    title = "Evolucion del tipo de cambio en Uruguay",
    subtitle = "Variacion interanual",
    x = "Año",
    y = "Variacion del tipo de cambio (%)"
  ) +
  theme_minimal()

# --------------------------------------------------------------
## 8.ix GRAFICOS DE DISPERSION
# --------------------------------------------------------------

# Seleccionar variables
datos_dispersion <- datos_modelo_nuevo %>%
  select(
    crecimiento_pbi,
    crecimiento_brasil,
    crecimiento_argentina,
    crecimiento_china,
    tasa_fed,
    crecimiento_ipx,
    inflacion,
    variacion_tipo_cambio
  ) %>%
  pivot_longer(
    cols = -crecimiento_pbi,
    names_to = "variable",
    values_to = "valor"
  )

# Crear graficos
ggplot(
  datos_dispersion,
  aes(x = valor, y = crecimiento_pbi)
) +
  geom_point(
    color = "steelblue",
    alpha = 0.7
  ) +
  geom_smooth(
    method = "lm",
    se = FALSE,
    color = "darkred"
  ) +
  facet_wrap(
    ~variable,
    scales = "free_x",
    ncol = 2
  ) +
  labs(
    title = "Relacion entre el crecimiento del PBI y sus explicativas",
    x = "Variable explicativa",
    y = "Crecimiento del PBI de Uruguay (%)"
  ) +
  theme_minimal()

# --------------------------------------------------------------
## 8.x MATRIZ DE CORRELACIONES
# --------------------------------------------------------------

matriz_correlaciones <- cor(
  variables_modelo,
  use = "complete.obs"
)

# Mostrar matriz
round(matriz_correlaciones, 2)

# Visualizar la matriz 
# install.packages("corrplot")  
library(corrplot)

corrplot(
  matriz_correlaciones,
  method = "color",
  type = "upper",
  addCoef.col = "black",
  number.cex = 0.7,
  tl.cex = 0.8,
  tl.col = "black"
)

# ==============================================================
## 9. INFERENCIA: PRUEBAS DE HIPOTESIS
# ==============================================================

# install.packages("car")
# install.packages("lmtest")
# install.packages("broom")
# install.packages("strucchange")
# install.packages("orcutt")
# install.packages("stargazer")

library(car)       
library(lmtest)    
library(broom)    
library(strucchange) 
library(orcutt)    
library(stargazer)
options(scipen = 999)

# --------------------------------------------------------------
# 9.i Pruebas t de significancia individual
# --------------------------------------------------------------

# Grados de libertad: n - k = 36 - 6 = 30
n <- nobs(modelo_1)
k <- length(coef(modelo_1))
n
k
modelo_1$df.residual

summary(modelo_1)$coefficients

# t critico al 5% (prueba bilateral)
t_critico <- qt(0.975, df = modelo_1$df.residual)
t_critico

# --------------------------------------------------------------
# 9.ii Intervalos de confianza al 95%
# --------------------------------------------------------------

confint(modelo_1, level = 0.95)

# --------------------------------------------------------------
# 9.iii Significancia conjunta del modelo (prueba F)
# --------------------------------------------------------------

# Ho: todos los coeficientes (salvo la constante) son 0
glance(modelo_1)

f_critico_global <- qf(0.95, df1 = 5, df2 = modelo_1$df.residual)
f_critico_global

# F observado = 19.2 > F critico = 2.53 (p-valor < 0.001).
# Rechazamos Ho: el modelo en su conjunto explica el crecimiento del PBI.

# --------------------------------------------------------------
# 9.iv Pruebas F de restricciones lineales 
# --------------------------------------------------------------

# a) Bloque REGIONAL --> Ho: beta_brasil = beta_argentina = 0
linearHypothesis(
  modelo_1,
  c("crecimiento_brasil = 0", "crecimiento_argentina = 0")
)

qf(0.95, df1 = 2, df2 = modelo_1$df.residual)

# F = 22.67 > F critico = 3.32 (p-valor < 0.001).
# Rechazamos Ho: la region (Brasil + Argentina) es conjuntamente muy
# significativa, aun mas que cada variable por separado. Esto es tipico
# cuando dos regresores estan correlacionados.

# b) Bloque GLOBAL --> Ho: beta_china = beta_fed = beta_ipx = 0
linearHypothesis(
  modelo_1,
  c("crecimiento_china = 0", "tasa_fed = 0", "crecimiento_ipx = 0")
)

qf(0.95, df1 = 3, df2 = modelo_1$df.residual)

# F = 0.98 < F critico = 2.92 (p-valor = 0.42).
# No rechazamos Ho: una vez que controlamos por la region, los factores
# globales (China, FED, precios de exportacion) no aportan en conjunto.

# c) ¿Brasil y Argentina tienen el mismo efecto?
# Ho: beta_brasil = beta_argentina
linearHypothesis(
  modelo_1,
  "crecimiento_brasil = crecimiento_argentina"
)

# F = 0.27, p-valor = 0.60. No rechazamos Ho: no hay evidencia de que el
# efecto de Brasil sea distinto al de Argentina.

# --------------------------------------------------------------
# 9.v Ajuste del modelo: crecimiento observado vs. estimado
# --------------------------------------------------------------

datos_modelo_nuevo <- datos_modelo_nuevo %>%
  mutate(
    pbi_estimado = fitted(modelo_1),
    residuo = residuals(modelo_1)
  )

datos_grafico <- datos_modelo_nuevo %>%
  select(fecha, crecimiento_pbi, pbi_estimado) %>%
  pivot_longer(
    cols = c(crecimiento_pbi, pbi_estimado),
    names_to = "serie",
    values_to = "crecimiento"
  ) %>%
  mutate(
    serie = dplyr::recode(
      serie,
      crecimiento_pbi = "Observado",
      pbi_estimado = "Estimado"
    )
  )

ggplot(datos_grafico, aes(x = fecha, y = crecimiento, color = serie)) +
  geom_line(linewidth = 1) +
  geom_point(size = 1.5) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  labs(
    title = "Crecimiento del PBI: observado vs. estimado",
    subtitle = "Modelo 1 - MCO",
    x = "Año",
    y = "Crecimiento interanual (%)",
    color = "Serie"
  ) +
  theme_minimal()

# ==============================================================
## 10. MULTICOLINEALIDAD
# ==============================================================

# Correlaciones entre los regresores
cor(
  datos_modelo_nuevo %>%
    select(crecimiento_brasil, crecimiento_argentina,
           crecimiento_china, tasa_fed, crecimiento_ipx)
)

# ==============================================================
## 11. SESGO DE VARIABLE OMITIDA Y VARIABLES IRRELEVANTES
# ==============================================================

# --------------------------------------------------------------
# 11.i ¿Que pasa si omitimos a Argentina?
# --------------------------------------------------------------

# Modelo corto: solo Brasil
modelo_corto <- lm(
  crecimiento_pbi ~ crecimiento_brasil,
  data = datos_modelo_nuevo
)
summary(modelo_corto)

# Para que haya sesgo deben cumplirse dos condiciones:
# 1. La variable omitida (Argentina) esta correlacionada con Brasil
cor(datos_modelo_nuevo$crecimiento_brasil,
    datos_modelo_nuevo$crecimiento_argentina)

# 2. La variable omitida es un determinante de Y (beta_argentina distinto a 0)
coef(modelo_1)["crecimiento_argentina"]

# Comparamos el coeficiente de Brasil en ambos modelos
coef(modelo_corto)["crecimiento_brasil"]
coef(modelo_1)["crecimiento_brasil"]

# --------------------------------------------------------------
# 11.ii ¿Faltan variables? Comparamos tres especificaciones
# --------------------------------------------------------------

# Modelo 1: solo variables externas (modelo_1, ya estimado)

# Modelo 2: agregamos inflacion
modelo_2 <- lm(
  crecimiento_pbi ~ crecimiento_brasil + crecimiento_argentina +
    crecimiento_china + tasa_fed + crecimiento_ipx +
    inflacion,
  data = datos_modelo_nuevo
)

# Modelo 3: agregamos tambien la variacion del tipo de cambio
modelo_3 <- lm(
  crecimiento_pbi ~ crecimiento_brasil + crecimiento_argentina +
    crecimiento_china + tasa_fed + crecimiento_ipx +
    inflacion + variacion_tipo_cambio,
  data = datos_modelo_nuevo
)

summary(modelo_2)
summary(modelo_3)

# Prueba F parcial: ¿las variables nuevas aportan?
# F = [(SCR_R - SCR_NR) / q] / [SCR_NR / (n - k)]
prueba_f_parcial <- function(restringido, no_restringido) {
  SCR_R  <- sum(residuals(restringido)^2)
  SCR_NR <- sum(residuals(no_restringido)^2)
  q  <- length(coef(no_restringido)) - length(coef(restringido))
  gl <- df.residual(no_restringido)
  F  <- ((SCR_R - SCR_NR) / q) / (SCR_NR / gl)
  data.frame(
    Estadistico_F = F,
    F_critico = qf(0.95, q, gl),
    P_value = pf(F, q, gl, lower.tail = FALSE)
  )
}

prueba_f_parcial(modelo_1, modelo_2)  # ¿aporta la inflacion?
prueba_f_parcial(modelo_2, modelo_3)  # ¿aporta el tipo de cambio?
prueba_f_parcial(modelo_1, modelo_3)  # ¿aportan las dos juntas?

# Para comparar modelos con distinta cantidad de variables
# miramos el R2 ajustado
tabla_especificacion <- data.frame(
  Modelo = c("Modelo 1", "Modelo 2", "Modelo 3"),
  Variables = c(5, 6, 7),
  R2 = c(summary(modelo_1)$r.squared,
         summary(modelo_2)$r.squared,
         summary(modelo_3)$r.squared),
  R2_ajustado = c(summary(modelo_1)$adj.r.squared,
                  summary(modelo_2)$adj.r.squared,
                  summary(modelo_3)$adj.r.squared)
)
tabla_especificacion

# Comparamos los coeficientes de los tres modelos
stargazer(
  modelo_1, modelo_2, modelo_3,
  type = "text",
  title = "Comparacion de especificaciones (MCO)",
  column.labels = c("Modelo 1", "Modelo 2", "Modelo 3"),
  dep.var.labels = "Crecimiento interanual del PBI",
  digits = 3
)

# ==============================================================
## 12. HETEROCEDASTICIDAD 
# ==============================================================

# Residuos del modelo
datos_modelo_nuevo$residuos <- residuals(modelo_1)
datos_modelo_nuevo$ajustados <- fitted(modelo_1)

# --------------------------------------------------------------
# 12.i Analisis grafico: residuos contra valores ajustados
# --------------------------------------------------------------

ggplot(datos_modelo_nuevo, aes(x = ajustados, y = residuos)) +
  geom_point(color = "navy") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(
    title = "Residuos vs. valores ajustados",
    x = "Crecimiento del PBI ajustado (%)",
    y = "Residuos"
  ) +
  theme_minimal()

# --------------------------------------------------------------
# 12.ii Test de White
# --------------------------------------------------------------

# Ho: errores homocedasticos
# Ha: errores heterocedasticos

# Paso 1: residuos al cuadrado
residuos_cuadrado <- residuals(modelo_1)^2

# Paso 2: regresion auxiliar contra los ajustados y su cuadrado
# (con 5 regresores, el White completo tendria 20 terminos para 36 obs.)
ajustados <- fitted(modelo_1)
ajustados2 <- ajustados^2

W <- lm(residuos_cuadrado ~ ajustados + ajustados2)
summary(W)

# Paso 3a: prueba F de significancia conjunta
linearHypothesis(W, c("ajustados = 0", "ajustados2 = 0"))
qf(0.95, df1 = 2, df2 = W$df.residual)

# Paso 3b: version LM --> N * R2 ~ Chi cuadrado (2 gl)
R2_W <- glance(W)$r.squared
chi_W <- nobs(W) * R2_W
chi_W
1 - pchisq(chi_W, df = 2)

# --------------------------------------------------------------
# 12.iii Test de Breusch-Pagan
# --------------------------------------------------------------

bptest(modelo_1)

# --------------------------------------------------------------
# 12.iv Errores robustos a heterocedasticidad 
# --------------------------------------------------------------

cov_hc1 <- hccm(modelo_1, type = "hc1")
modelo_hc1 <- coeftest(modelo_1, vcov. = cov_hc1)
modelo_hc1

# ==============================================================
## 13. AUTOCORRELACION
# ==============================================================

# --------------------------------------------------------------
# 13.i Grafico de los residuos en el tiempo
# --------------------------------------------------------------

ggplot(datos_modelo_nuevo, aes(x = fecha, y = residuos)) +
  geom_line(color = "darkred") +
  geom_point(color = "darkred") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(
    title = "Residuos del modelo en el tiempo",
    x = "Trimestre",
    y = "Residuos"
  ) +
  theme_minimal()

# --------------------------------------------------------------
# 13.ii Antes del DW: ¿los trimestres son consecutivos?
# --------------------------------------------------------------

# El DW y Cochrane-Orcutt comparan cada trimestre con el anterior,
# asi que no puede faltar ningun trimestre en la serie.
datos_modelo_nuevo <- datos_modelo_nuevo %>%
  arrange(año, trimestre_num)

indice <- with(datos_modelo_nuevo, año * 4 + trimestre_num)
all(diff(indice) == 1)   # TRUE: no falta ningun trimestre
sum(datos_modelo_nuevo$año == 2021)   # los 4 trimestres de 2021

# --------------------------------------------------------------
# 13.iv Test de Durbin-Watson
# --------------------------------------------------------------

# Ho: no hay autocorrelacion de primer orden (rho = 0)

# Primera forma: 
dwtest(modelo_1)

# Segunda forma: durbinWatsonTest() del paquete car
durbinWatsonTest(modelo_1)

# --------------------------------------------------------------
# 13.v Solucion: transformacion de Cochrane-Orcutt
# --------------------------------------------------------------

# Paso 1: estimamos el modelo por MCO y obtenemos los residuos
residuos_mco <- residuals(modelo_1)
n <- length(residuos_mco)

# Paso 2: rho = Corr(e_t, e_(t-1))
rho_estimado <- cor(residuos_mco[2:n], residuos_mco[1:(n - 1)])
rho_estimado

# Paso 3: transformamos y*t = y_t - rho*y(t-1) ; X*t = X_t - rho*X(t-1)
y <- model.response(model.frame(modelo_1))
X <- model.matrix(modelo_1)   # incluye la columna de la constante

y_transformado <- y[2:n] - rho_estimado * y[1:(n - 1)]
X_transformado <- X[2:n, , drop = FALSE] -
  rho_estimado * X[1:(n - 1), , drop = FALSE]

# Paso 4: estimamos por MCO el modelo transformado
modelo_co_manual <- lm(y_transformado ~ X_transformado - 1)
summary(modelo_co_manual)

# Durbin-Watson del modelo transformado
dwtest(modelo_co_manual)

# rho = 0.60. Con la transformacion, Brasil (0.60, p = 0.010) y
# Argentina (0.32, p = 0.037) son significativos al 5%.
# El DW sube de 0.76 a 1.38.

# --------------------------------------------------------------
# 13.vi Comparamos MCO y Cochrane-Orcutt
# --------------------------------------------------------------

resumen_mco <- summary(modelo_1)$coefficients
resumen_co  <- summary(modelo_co_manual)$coefficients

comparacion <- data.frame(
  coef_mco = resumen_mco[, "Estimate"],
  ee_mco   = resumen_mco[, "Std. Error"],
  p_mco    = resumen_mco[, "Pr(>|t|)"],
  coef_co  = resumen_co[, "Estimate"],
  ee_co    = resumen_co[, "Std. Error"],
  p_co     = resumen_co[, "Pr(>|t|)"],
  row.names = rownames(resumen_mco)
)
round(comparacion, 3)

# ==============================================================
## 14. NORMALIDAD DE LOS ERRORES Y BOOTSTRAP
# ==============================================================

# --------------------------------------------------------------
# 14.i Normalidad: histograma de los residuos con la curva normal
# --------------------------------------------------------------

residuos_co <- residuals(modelo_co_manual)

hist(
  residuos_co, breaks = 10, freq = FALSE,
  col = "lightblue",
  main = "Distribucion de los residuos (Cochrane-Orcutt)",
  xlab = "Residuos"
)
curve(
  dnorm(x, mean = 0, sd = sd(residuos_co)),
  add = TRUE, col = "blue", lwd = 2
)

# --------------------------------------------------------------
# 14.ii Bootstrap
# --------------------------------------------------------------

rho <- rho_estimado   # rho calculado a mano en la seccion 13.v
n_obs <- nrow(datos_modelo_nuevo)
t1 <- 2:n_obs        # periodo t
t0 <- 1:(n_obs - 1)  # periodo t-1

# Variables transformadas: z*t = z_t - rho * z(t-1)
transformar <- function(z) z[t1] - rho * z[t0]

datos_co <- data.frame(
  y_t       = transformar(datos_modelo_nuevo$crecimiento_pbi),
  constante = 1 - rho,
  brasil    = transformar(datos_modelo_nuevo$crecimiento_brasil),
  argentina = transformar(datos_modelo_nuevo$crecimiento_argentina),
  china     = transformar(datos_modelo_nuevo$crecimiento_china),
  fed       = transformar(datos_modelo_nuevo$tasa_fed),
  ipx       = transformar(datos_modelo_nuevo$crecimiento_ipx)
)

# Modelo transformado (sin constante: ya esta la columna 1 - rho).
modelo_transformado <- lm(
  y_t ~ constante + brasil + argentina + china + fed + ipx - 1,
  data = datos_co
)
coef(modelo_transformado)

set.seed(123)

coef_boot_brasil <- NULL
coef_boot_argentina <- NULL
coef_boot_china <- NULL
for (i in 1:10000) {
  muestra_boot <- datos_co[sample(1:nrow(datos_co), nrow(datos_co), replace = TRUE), ]
  
  modelo_boot <- lm(
    y_t ~ constante + brasil + argentina + china + fed + ipx - 1,
    data = muestra_boot
  )
coef_boot_brasil    <- c(coef_boot_brasil,    coef(modelo_boot)["brasil"])
coef_boot_argentina <- c(coef_boot_argentina, coef(modelo_boot)["argentina"])
coef_boot_china     <- c(coef_boot_china,     coef(modelo_boot)["china"])
}

# Intervalos de confianza empiricos al 95%
ic_boot <- rbind(
  brasil    = quantile(coef_boot_brasil,    c(0.025, 0.975)),
  argentina = quantile(coef_boot_argentina, c(0.025, 0.975)),
  china     = quantile(coef_boot_china,     c(0.025, 0.975))
)
ic_boot

# Comparamos con los intervalos t del modelo transformado
confint(modelo_transformado)[c("brasil", "argentina", "china"), ]

# Distribucion bootstrap del coeficiente de Brasil
hist(
  coef_boot_brasil, breaks = 40, col = "lightyellow",
  main = "Distribucion bootstrap del beta de Brasil",
  xlab = "Beta Brasil"
)
abline(v = quantile(coef_boot_brasil, c(0.025, 0.975)), col = "darkred", lwd = 2)
abline(v = mean(coef_boot_brasil), col = "navy", lwd = 2)
abline(v = 0, lty = 2)

# ==============================================================
## 15. ESPECIFICACION Y CAMBIO DE REGIMEN
## (Levantamiento de supuestos: RESET, Chow, CUSUM)
# ==============================================================

# --------------------------------------------------------------
# 15.i Test RESET
# --------------------------------------------------------------

# Ho: la forma funcional lineal es correcta
resettest(modelo_1, power = 2, type = "fitted")

# RESET = 0.98 (p-valor = 0.33): no rechazamos Ho. No hay evidencia de
# que falten terminos no lineales; la forma lineal es adecuada.

# --------------------------------------------------------------
# 15.ii Prueba de Chow: ¿cambian los betas con la pandemia?
# --------------------------------------------------------------

# Quiebre en II 2020. Periodo I: I 2017 a I 2020 ; periodo II: II 2020 en adelante
# Ho: beta_I = beta_II (los parametros son constantes)
# F = [(SCR* - SCR) / K] / [SCR / (N - 2K)], con SCR = SCR_I + SCR_II

formula_modelo <- crecimiento_pbi ~ crecimiento_brasil + crecimiento_argentina +
  crecimiento_china + tasa_fed + crecimiento_ipx

periodo_I  <- datos_modelo_nuevo %>% filter(fecha <  as.Date("2020-04-01"))
periodo_II <- datos_modelo_nuevo %>% filter(fecha >= as.Date("2020-04-01"))
nrow(periodo_I)
nrow(periodo_II)

K <- length(coef(modelo_1))
N <- nrow(datos_modelo_nuevo)

# ¿Alcanzan las observaciones?
# Cada periodo necesita mas observaciones que parametros
data.frame(
  Parametros = K,
  Obs_periodo_I = nrow(periodo_I),
  Obs_periodo_II = nrow(periodo_II),
  Grados_libertad = N - 2 * K
)

SCR_restringido <- sum(residuals(modelo_1)^2)                       # SCR*
SCR_I  <- sum(residuals(lm(formula_modelo, data = periodo_I))^2)
SCR_II <- sum(residuals(lm(formula_modelo, data = periodo_II))^2)
SCR_no_restringido <- SCR_I + SCR_II                                # SCR

F_chow <- ((SCR_restringido - SCR_no_restringido) / K) /
  (SCR_no_restringido / (N - 2 * K))
F_chow

# F critico y p-valor
qf(0.95, df1 = K, df2 = N - 2 * K)
1 - pf(F_chow, df1 = K, df2 = N - 2 * K)

# F de Chow = 1.82 < F critico = 2.51 (p-valor = 0.14).
# No rechazamos Ho: no hay evidencia de que los betas cambien a partir de
# la pandemia.
# Limitacion: el periodo I tiene solo 13 observaciones para
# estimar 6 parametros.

# --------------------------------------------------------------
# 15.iii Prueba CUSUM de estabilidad de parametros
# --------------------------------------------------------------

# Suma acumulada de residuos recursivos: si sale de las bandas al 5%
# hay evidencia de cambio de regimen.
cusum <- efp(formula_modelo, data = datos_modelo_nuevo, type = "Rec-CUSUM")
plot(cusum, main = "Prueba CUSUM")
sctest(cusum)

# El estadistico CUSUM no sale de las bandas (p-valor = 0.68):
# los parametros son estables en todo el periodo.
