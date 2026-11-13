### PROYECTO ECONOMETRÍA ###

# ==============================================================
## 1. CARGAR DATOS
# ==============================================================

library(readxl) 
library(dplyr)  
library(tidyr)   
library(stringr) 
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

# Renombramos la segunda columna porque contiene
# los nombres de las variables
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

# ==============================================================
## RECONSTRUIR DATOS_MACRO CON 2021
# ==============================================================

# Seleccionar las variables
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
  ) %>%
  select(variable, trimestre, valor) %>%
  pivot_wider(
    names_from = variable,
    values_from = valor
  ) %>%
  rename(
    pbi = `PRODUCTO INTERNO BRUTO`,
    inversion = `Formación Bruta de Capital`,
    exportaciones = `Exportaciones de bienes y servicios`,
    importaciones = `(-) Importaciones de bienes y servicios`,
    consumo = `Hogares`,
    gasto_publico = `Gobierno e ISFLSH`
  ) %>%
  
  mutate(
    trimestre = trimws(gsub("\\*", "", trimestre)),
    trimestre = gsub("202I", "2021", trimestre)
  ) %>%
  separate(
    trimestre,
    into = c("trim", "año"),
    sep = " ",
    remove = FALSE
  ) %>%
  mutate(
    año = as.integer(año),
    trimestre_num = case_when(
      trim == "I" ~ 1L,
      trim == "II" ~ 2L,
      trim == "III" ~ 3L,
      trim == "IV" ~ 4L
    )
  ) %>%
  select(
    trimestre, año, trimestre_num,
    pbi, consumo, gasto_publico,
    inversion, exportaciones, importaciones
  ) %>%
  arrange(año, trimestre_num)

# Comprobar si recuperamos 2021
datos_macro %>%
  filter(año == 2021) %>%
  select(trimestre, año, trimestre_num, pbi) %>%
  print()

View(datos_macro)

# ==============================================================
## 3. AGREGAR INFORMACIÓN --> VARIABLE INFLACIÓN
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
    trimestre, año, trimestre_num,
    crecimiento_pbi, inflacion,
    crecimiento_inversion,
    crecimiento_exportaciones,
    variacion_tipo_cambio
  ) %>%
  drop_na(
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

# Visualizar la matriz con colores
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
## 9. ESTIMACION DE LOS MODELOS POR MCO
# ==============================================================

# --------------------------------------------------------------
## 9.i ESTIMAR LOS TRES MODELOS
# --------------------------------------------------------------

# Modelo 1: variables externas
modelo_1 <- lm(
  crecimiento_pbi ~
    crecimiento_brasil +
    crecimiento_argentina +
    crecimiento_china +
    tasa_fed +
    crecimiento_ipx,
  data = datos_modelo_nuevo
)

# Modelo 2: agregamos inflacion
modelo_2 <- lm(
  crecimiento_pbi ~
    crecimiento_brasil +
    crecimiento_argentina +
    crecimiento_china +
    tasa_fed +
    crecimiento_ipx +
    inflacion,
  data = datos_modelo_nuevo
)

# Modelo 3: agregamos variacion del tipo de cambio
modelo_3 <- lm(
  crecimiento_pbi ~
    crecimiento_brasil +
    crecimiento_argentina +
    crecimiento_china +
    tasa_fed +
    crecimiento_ipx +
    inflacion +
    variacion_tipo_cambio,
  data = datos_modelo_nuevo
)

# Ver resultados
summary(modelo_1)
summary(modelo_2)
summary(modelo_3)

# --------------------------------------------------------------
## 9.ii COMPARACION DE LOS TRES MODELOS
# --------------------------------------------------------------

library(stargazer)

stargazer(
  modelo_1,
  modelo_2,
  modelo_3,
  type = "text",
  title = "Estimacion por MCO - Crecimiento del PBI de Uruguay",
  column.labels = c(
    "Modelo 1",
    "Modelo 2",
    "Modelo 3"
  ),
  dep.var.labels = "Crecimiento interanual del PBI",
  digits = 3
)

# --------------------------------------------------------------
## 9.iii VALORES ESTIMADOS Y RESIDUOS
# --------------------------------------------------------------

# Incorporar valores estimados y residuos
datos_modelo_nuevo <- datos_modelo_nuevo %>%
  mutate(
    pbi_estimado = fitted(modelo_3),
    residuo = residuals(modelo_3)
  )

# Ver resultados
datos_modelo_nuevo %>%
  select(
    trimestre,
    crecimiento_pbi,
    pbi_estimado,
    residuo
  ) %>%
  View()

# --------------------------------------------------------------
## 9.iv COMPARACION: PBI OBSERVADO Y ESTIMADO
# --------------------------------------------------------------

datos_grafico <- datos_modelo_nuevo %>%
  select(
    fecha,
    crecimiento_pbi,
    pbi_estimado
  ) %>%
  pivot_longer(
    cols = c(crecimiento_pbi, pbi_estimado),
    names_to = "serie",
    values_to = "crecimiento"
  ) %>%
  mutate(
    serie = recode(
      serie,
      crecimiento_pbi = "Observado",
      pbi_estimado = "Estimado"
    )
  )

ggplot(
  datos_grafico,
  aes(
    x = fecha,
    y = crecimiento,
    color = serie
  )
) +
  geom_line(linewidth = 1) +
  geom_point(size = 1.5) +
  geom_hline(
    yintercept = 0,
    linetype = "dashed",
    color = "gray50"
  ) +
  labs(
    title = "Crecimiento del PBI: observado vs estimado",
    subtitle = "Modelo 3 - MCO",
    x = "Año",
    y = "Crecimiento interanual (%)",
    color = "Serie"
  ) +
  theme_minimal()

# --------------------------------------------------------------
## 9.v GRAFICO DE RESIDUOS
# --------------------------------------------------------------

ggplot(
  datos_modelo_nuevo,
  aes(x = fecha, y = residuo)
) +
  geom_line(color = "steelblue", linewidth = 1) +
  geom_point(color = "steelblue", size = 1.5) +
  geom_hline(
    yintercept = 0,
    linetype = "dashed",
    color = "darkred"
  ) +
  labs(
    title = "Residuos del Modelo 3",
    x = "Año",
    y = "Residuo"
  ) +
  theme_minimal()

# ==============================================================
## 10. INFERENCIA ESTADISTICA DEL MODELO
# ==============================================================

# --------------------------------------------------------------
## 10.i PRUEBAS t INDIVIDUALES
# --------------------------------------------------------------

# Resultados de las pruebas t para cada coeficiente
summary(modelo_3)$coefficients

# Tabla de coeficientes y significancia
tabla_t <- as.data.frame(
  summary(modelo_3)$coefficients
)

tabla_t$Variable <- rownames(tabla_t)

View(tabla_t)

# --------------------------------------------------------------
## 10.ii PRUEBA F DE SIGNIFICANCIA CONJUNTA
# --------------------------------------------------------------

# Resumen del modelo
summary(modelo_3)

# Estadistico F
summary(modelo_3)$fstatistic

# Valor-p de la prueba F
f <- summary(modelo_3)$fstatistic

p_value_f <- pf(
  f[1],
  f[2],
  f[3],
  lower.tail = FALSE
)

p_value_f

# --------------------------------------------------------------
## 10.iii R2 Y R2 AJUSTADO
# --------------------------------------------------------------

# R2
summary(modelo_3)$r.squared

# R2 ajustado
summary(modelo_3)$adj.r.squared

# --------------------------------------------------------------
## 10.iv INTERVALOS DE CONFIANZA AL 95%
# --------------------------------------------------------------

# Intervalos de confianza
intervalos_95 <- confint(
  modelo_3,
  level = 0.95
)

View(as.data.frame(intervalos_95))

# --------------------------------------------------------------
## 10.v TABLA RESUMEN DEL MODELO 3
# --------------------------------------------------------------

# Extraer resultados
resumen <- summary(modelo_3)$coefficients

# Intervalos de confianza
ic <- confint(modelo_3, level = 0.95)

# Construir tabla
tabla_inferencia <- data.frame(
  Variable = rownames(resumen),
  Coeficiente = resumen[, 1],
  Error_estandar = resumen[, 2],
  Estadistico_t = resumen[, 3],
  P_value = resumen[, 4],
  IC_inferior = ic[, 1],
  IC_superior = ic[, 2],
  row.names = NULL
)

# Redondear resultados
tabla_inferencia <- tabla_inferencia %>%
  mutate(
    across(
      where(is.numeric),
      ~round(.x, 4)
    )
  )

View(tabla_inferencia)

# ==============================================================
## 11. MULTICOLINEALIDAD
# ==============================================================

# --------------------------------------------------------------
## 11.i CORRELACIONES ENTRE VARIABLES EXPLICATIVAS
# --------------------------------------------------------------

# Seleccionar las variables explicativas del Modelo 3
variables_explicativas <- datos_modelo_nuevo %>%
  select(
    crecimiento_brasil,
    crecimiento_argentina,
    crecimiento_china,
    tasa_fed,
    crecimiento_ipx,
    inflacion,
    variacion_tipo_cambio
  )

# Calcular matriz de correlaciones
matriz_cor_x <- cor(variables_explicativas)

# Mostrar matriz
round(matriz_cor_x, 3)

corrplot(
  matriz_cor_x,
  method = "color",
  type = "upper",
  addCoef.col = "black",
  number.cex = 0.7,
  tl.cex = 0.8,
  tl.col = "black"
)

# --------------------------------------------------------------
## 11.ii IDENTIFICAR CORRELACIONES ELEVADAS
# --------------------------------------------------------------

# Convertir matriz en tabla
tabla_correlaciones <- as.data.frame(
  as.table(matriz_cor_x)
) %>%
  rename(
    Variable_1 = Var1,
    Variable_2 = Var2,
    Correlacion = Freq
  ) %>%
  filter(
    as.integer(Variable_1) <
      as.integer(Variable_2)
  ) %>%
  mutate(
    Correlacion_absoluta = abs(Correlacion)
  ) %>%
  arrange(desc(Correlacion_absoluta))

# Mostrar correlaciones ordenadas
View(tabla_correlaciones)

# --------------------------------------------------------------
## 11.iii COMPARAR ERRORES ESTANDAR
# --------------------------------------------------------------

# Extraer errores estandar de cada modelo
ee_modelo_1 <- coef(summary(modelo_1))[, 2]
ee_modelo_2 <- coef(summary(modelo_2))[, 2]
ee_modelo_3 <- coef(summary(modelo_3))[, 2]

# Comparar las variables presentes en los tres modelos
variables_comunes <- intersect(
  names(ee_modelo_1),
  intersect(
    names(ee_modelo_2),
    names(ee_modelo_3)
  )
)

tabla_errores <- data.frame(
  Variable = variables_comunes,
  EE_Modelo_1 = ee_modelo_1[variables_comunes],
  EE_Modelo_2 = ee_modelo_2[variables_comunes],
  EE_Modelo_3 = ee_modelo_3[variables_comunes],
  row.names = NULL
)

View(tabla_errores)

# --------------------------------------------------------------
## 11.iv DIAGNOSTICO DE MULTICOLINEALIDAD
# --------------------------------------------------------------

# Correlaciones superiores a 0.6 en valor absoluto
correlaciones_altas <- tabla_correlaciones %>%
  filter(Correlacion_absoluta > 0.6)

View(correlaciones_altas)

# Comprobar significancia conjunta del Modelo 3
summary(modelo_3)$fstatistic

# Comprobar significancia individual y errores estandar
summary(modelo_3)$coefficients

# Comparar estabilidad de los coeficientes
coeficientes_comparados <- data.frame(
  Variable = names(coef(modelo_1)),
  Modelo_1 = coef(modelo_1),
  Modelo_2 = coef(modelo_2)[names(coef(modelo_1))],
  Modelo_3 = coef(modelo_3)[names(coef(modelo_1))]
)

View(coeficientes_comparados)

# ==============================================================
## 12. ESPECIFICACION DEL MODELO
# ==============================================================

library(lmtest)

# --------------------------------------------------------------
## 12.i PRUEBA RESET DE RAMSEY
# --------------------------------------------------------------

# Aplicar RESET al Modelo 3
reset_modelo_3 <- resettest(
  modelo_3,
  power = 2:3,
  type = "fitted"
)

# Mostrar resultados
reset_modelo_3

# --------------------------------------------------------------
## 12.ii COMPARACION DE ESPECIFICACIONES - PRUEBA F
# --------------------------------------------------------------

# Funcion para calcular la F parcial
prueba_f_parcial <- function(restringido, no_restringido) {
  
  SCR_R <- sum(residuals(restringido)^2)
  SCR_NR <- sum(residuals(no_restringido)^2)
  
  q <- length(coef(no_restringido)) -
    length(coef(restringido))
  
  gl <- df.residual(no_restringido)
  
  F <- ((SCR_R - SCR_NR) / q) / (SCR_NR / gl)
  
  p_value <- pf(F, q, gl, lower.tail = FALSE)
  
  data.frame(
    Estadistico_F = F,
    P_value = p_value
  )
}

# Evaluar si inflacion aporta al Modelo 1
prueba_f_parcial(modelo_1, modelo_2)

# Evaluar si tipo de cambio aporta al Modelo 2
prueba_f_parcial(modelo_2, modelo_3)

# Evaluar si ambas variables aportan conjuntamente
prueba_f_parcial(modelo_1, modelo_3)

# --------------------------------------------------------------
## 12.iii COMPARACION DEL R2 AJUSTADO
# --------------------------------------------------------------

tabla_especificacion <- data.frame(
  Modelo = c("Modelo 1", "Modelo 2", "Modelo 3"),
  R2_ajustado = c(
    summary(modelo_1)$adj.r.squared,
    summary(modelo_2)$adj.r.squared,
    summary(modelo_3)$adj.r.squared
  )
)

View(tabla_especificacion)

# --------------------------------------------------------------
## 12.iv RESIDUOS VS VALORES ESTIMADOS
# --------------------------------------------------------------

ggplot(
  datos_modelo_nuevo,
  aes(x = pbi_estimado, y = residuo)
) +
  geom_point(
    color = "steelblue",
    size = 2
  ) +
  geom_hline(
    yintercept = 0,
    linetype = "dashed",
    color = "darkred"
  ) +
  labs(
    title = "Residuos vs valores estimados",
    subtitle = "Modelo 3 - MCO",
    x = "Crecimiento estimado del PBI",
    y = "Residuos"
  ) +
  theme_minimal()

# ==============================================================
## 13. HETEROCEDASTICIDAD
# ==============================================================

library(sandwich)

# --------------------------------------------------------------
## 13.i GRAFICO DE RESIDUOS AL CUADRADO
# --------------------------------------------------------------

datos_hetero <- data.frame(
  estimado = fitted(modelo_3),
  residuo_cuadrado = residuals(modelo_3)^2
)

ggplot(
  datos_hetero,
  aes(x = estimado, y = residuo_cuadrado)
) +
  geom_point(color = "steelblue", size = 2) +
  labs(
    title = "Residuos al cuadrado vs valores estimados",
    subtitle = "Modelo 3 - MCO",
    x = "Valor estimado",
    y = "Residuo al cuadrado"
  ) +
  theme_minimal()

# --------------------------------------------------------------
## 13.ii PRUEBA DE BREUSCH-PAGAN
# --------------------------------------------------------------

# Prueba BP clasica
bp_modelo_3 <- bptest(
  modelo_3,
  studentize = FALSE
)

bp_modelo_3

# --------------------------------------------------------------
## 13.iii PRUEBA DE WHITE REDUCIDA
# --------------------------------------------------------------

# Regresion auxiliar
modelo_white <- lm(
  I(residuals(modelo_3)^2) ~
    fitted(modelo_3) +
    I(fitted(modelo_3)^2)
)

# Resultados de la regresion auxiliar
summary(modelo_white)

# Estadistico LM = N * R2
LM_white <- nobs(modelo_white) *
  summary(modelo_white)$r.squared

# Valor-p con distribucion chi-cuadrado (2 gl)
p_white <- pchisq(
  LM_white,
  df = 2,
  lower.tail = FALSE
)

# Mostrar resultados
data.frame(
  Estadistico_LM = LM_white,
  P_value = p_white
)

# --------------------------------------------------------------
## 13.iv ERRORES ESTANDAR ROBUSTOS DE WHITE
# --------------------------------------------------------------

# Matriz de varianzas robusta
varianza_robusta <- vcovHC(
  modelo_3,
  type = "HC1"
)

# Pruebas t con errores estandar robustos
resultados_robustos <- coeftest(
  modelo_3,
  vcov. = varianza_robusta
)

resultados_robustos

# --------------------------------------------------------------
## 13.v COMPARACION DE ERRORES ESTANDAR
# --------------------------------------------------------------

# Resultados convencionales
resultados_mco <- summary(modelo_3)$coefficients

# Crear tabla comparativa
tabla_hetero <- data.frame(
  Variable = rownames(resultados_mco),
  Coeficiente = resultados_mco[, 1],
  EE_MCO = resultados_mco[, 2],
  EE_White = resultados_robustos[, 2],
  P_value_MCO = resultados_mco[, 4],
  P_value_White = resultados_robustos[, 4],
  row.names = NULL
)

# Redondear resultados
tabla_hetero <- tabla_hetero %>%
  mutate(
    across(where(is.numeric), ~round(.x, 4))
  )

View(tabla_hetero)

# ==============================================================
## 14. AUTOCORRELACION - DURBIN WATSON
# ==============================================================

# ==============================================================
## 14. AUTOCORRELACION - DURBIN WATSON
# ==============================================================

# --------------------------------------------------------------
## 14.i VERIFICAR CONTINUIDAD TRIMESTRAL
# --------------------------------------------------------------

# Ordenar los datos cronologicamente
datos_modelo_nuevo <- datos_modelo_nuevo %>%
  arrange(año, trimestre_num)

# Comprobar si hay trimestres faltantes
indice <- with(
  datos_modelo_nuevo,
  año * 4 + trimestre_num
)

which(diff(indice) != 1)

# Comprobar cuantos trimestres de 2021 tenemos
sum(datos_modelo_nuevo$año == 2021)

# --------------------------------------------------------------
## 14.ii PRUEBA DURBIN-WATSON
# --------------------------------------------------------------

# Ejecutar solo si los trimestres son consecutivos
if (all(diff(indice) == 1)) {
  
  # Volver a estimar el modelo con la base actual
  modelo_3 <- update(
    modelo_3,
    data = datos_modelo_nuevo
  )
  
  # Prueba DW
  dwtest(
    modelo_3,
    alternative = "two.sided"
  )
  
} else {
  
  message("Todavia faltan trimestres en la muestra.")
  
}

# --------------------------------------------------------------
## 14.iii CORRECCION POR COCHRANE-ORCUTT
# --------------------------------------------------------------

# Paso 1: obtener los residuos del Modelo 3
residuos_mco <- residuals(modelo_3)

# Paso 2: estimar rho
n <- length(residuos_mco)

rho_estimado <- cor(
  residuos_mco[2:n],
  residuos_mco[1:(n-1)]
)

# Mostrar rho estimado
rho_estimado

# Paso 3: transformar las variables

# Variable dependiente
y <- model.response(model.frame(modelo_3))

# Matriz de variables explicativas (incluye constante)
X <- model.matrix(modelo_3)

# Transformacion de Y
y_transformado <- y[2:n] -
  rho_estimado * y[1:(n-1)]

# Transformacion de X
X_transformado <- X[2:n, , drop = FALSE] -
  rho_estimado * X[1:(n-1), , drop = FALSE]

# Paso 4: estimar por MCO el modelo transformado
modelo_cochrane <- lm(
  y_transformado ~ X_transformado - 1
)

summary(modelo_cochrane)

# --------------------------------------------------------------
## 14.iv DURBIN-WATSON DESPUES DE LA CORRECCION
# --------------------------------------------------------------

dw_cochrane <- dwtest(
  modelo_cochrane,
  alternative = "two.sided"
)

dw_cochrane

# --------------------------------------------------------------
## 14.v COMPARACION MCO Y COCHRANE-ORCUTT
# --------------------------------------------------------------

tabla_cochrane <- data.frame(
  Variable = names(coef(modelo_3)),
  Coeficiente_MCO = as.numeric(coef(modelo_3)),
  Coeficiente_Cochrane = as.numeric(coef(modelo_cochrane))
)

# Redondear resultados
tabla_cochrane <- tabla_cochrane %>%
  mutate(
    across(where(is.numeric), ~round(.x, 4))
  )

View(tabla_cochrane)

# --------------------------------------------------------------
## IDENTIFICAR DONDE SE PIERDE 2021
# --------------------------------------------------------------

# 1. Comprobar 2021 en datos_macro
datos_macro %>%
  filter(año == 2021) %>%
  select(trimestre, año, trimestre_num, pbi) %>%
  print()

# 2. Ver si hay años que R no pudo convertir
datos_macro %>%
  filter(is.na(año)) %>%
  select(trimestre, año, trimestre_num, pbi) %>%
  print()

# 3. Comprobar si los registros de 2021
# aparecen con un año incorrecto
datos_macro %>%
  filter(grepl("2021", trimestre)) %>%
  select(trimestre, año, trimestre_num, pbi) %>%
  print()

# ==============================================================
## 15. QUIEBRES ESTRUCTURALES
# ==============================================================

# --------------------------------------------------------------
## 15.i IDENTIFICAR EL QUIEBRE DE 2020
# --------------------------------------------------------------

datos_quiebre <- datos_modelo_nuevo %>%
  arrange(año, trimestre_num) %>%
  mutate(
    post_covid = ifelse(
      año > 2020 |
        (año == 2020 & trimestre_num >= 2),
      1, 0
    )
  )

# Cantidad de observaciones antes y despues
table(datos_quiebre$post_covid)

# --------------------------------------------------------------
## 15.ii MODELO CON DUMMY DE QUIEBRE
# --------------------------------------------------------------

modelo_dummy <- lm(
  crecimiento_pbi ~
    crecimiento_brasil +
    crecimiento_argentina +
    crecimiento_china +
    tasa_fed +
    crecimiento_ipx +
    inflacion +
    variacion_tipo_cambio +
    post_covid,
  data = datos_quiebre
)

summary(modelo_dummy)

# Inferencia con errores estandar robustos de White

coeftest(
  modelo_dummy,
  vcov. = vcovHC(modelo_dummy, type = "HC1")
)

# --------------------------------------------------------------
## 15.iii VERIFICAR SI PODEMOS APLICAR CHOW
# --------------------------------------------------------------

# Cantidad de parametros del Modelo 3
K <- length(coef(modelo_3))

# Observaciones por periodo
N_antes <- sum(datos_quiebre$post_covid == 0)
N_despues <- sum(datos_quiebre$post_covid == 1)

data.frame(
  Parametros = K,
  Observaciones_antes = N_antes,
  Observaciones_despues = N_despues,
  Grados_libertad_total = N_antes + N_despues - 2*K
)

# --------------------------------------------------------------
## 15.iv PRUEBA DE CHOW
# --------------------------------------------------------------

# Separar periodos
datos_antes <- datos_quiebre %>%
  filter(post_covid == 0)

datos_despues <- datos_quiebre %>%
  filter(post_covid == 1)

# Comprobar que ambas muestras permiten estimar el modelo
formula_chow <- formula(modelo_3)

rango_antes <- qr(
  model.matrix(formula_chow, data = datos_antes)
)$rank

rango_despues <- qr(
  model.matrix(formula_chow, data = datos_despues)
)$rank

if (
  N_antes <= K ||
  N_despues <= K ||
  rango_antes < K ||
  rango_despues < K
) {
  
  message("No hay observaciones suficientes para Chow completo.")
  
} else {
  
  # Estimar cada periodo por separado
  modelo_antes <- lm(
    formula_chow,
    data = datos_antes
  )
  
  modelo_despues <- lm(
    formula_chow,
    data = datos_despues
  )
  
  # Sumas de cuadrados residuales
  SCR_R <- sum(residuals(modelo_3)^2)
  
  SCR_NR <- sum(residuals(modelo_antes)^2) +
    sum(residuals(modelo_despues)^2)
  
  N <- N_antes + N_despues
  
  # Estadistico F de Chow
  F_chow <- ((SCR_R - SCR_NR) / K) /
    (SCR_NR / (N - 2*K))
  
  p_chow <- pf(
    F_chow,
    K,
    N - 2*K,
    lower.tail = FALSE
  )
  
  print(data.frame(
    F_Chow = F_chow,
    P_value = p_chow
  ))
}

# --------------------------------------------------------------
## 15.v PRUEBA CUSUM
# --------------------------------------------------------------

library(strucchange)
library(dplyr)

# Ordenar observaciones cronologicamente
datos_cusum <- datos_modelo_nuevo %>%
  arrange(año, trimestre_num)

# Estimar proceso CUSUM con residuos recursivos
cusum_modelo_3 <- efp(
  formula(modelo_3),
  data = datos_cusum,
  type = "Rec-CUSUM"
)

# Grafico CUSUM
plot(
  cusum_modelo_3,
  main = "Prueba CUSUM - Modelo 3"
)

# Prueba estadistica de estabilidad
resultado_cusum <- sctest(cusum_modelo_3)

# Mostrar resultado
resultado_cusum

# ==============================================================
## 16. CHEQUEO DE ROBUSTEZ
# ==============================================================

# --------------------------------------------------------------
## 16.i ESTIMACION EXCLUYENDO 2020
# --------------------------------------------------------------

# Crear base excluyendo 2020
datos_sin_2020 <- datos_modelo_nuevo %>%
  filter(año != 2020)

# Estimar el Modelo 3 con la nueva muestra
modelo_sin_2020 <- update(
  modelo_3,
  data = datos_sin_2020
)

# Resultados
summary(modelo_sin_2020)

# Comparar cantidad de observaciones
nobs(modelo_3)
nobs(modelo_sin_2020)

# --------------------------------------------------------------
## 16.ii COMPARACION DE COEFICIENTES
# --------------------------------------------------------------

tabla_robustez <- data.frame(
  Variable = names(coef(modelo_3)),
  Modelo_original = as.numeric(coef(modelo_3)),
  Modelo_sin_2020 = as.numeric(coef(modelo_sin_2020))
) %>%
  mutate(
    Diferencia = Modelo_sin_2020 - Modelo_original,
    Cambio_signo = sign(Modelo_original) !=
      sign(Modelo_sin_2020)
  )

View(tabla_robustez)

# --------------------------------------------------------------
## 16.iii COMPARACION CON ERRORES ESTANDAR DE WHITE
# --------------------------------------------------------------

# Modelo 3 original
robusto_original <- coeftest(
  modelo_3,
  vcov. = vcovHC(modelo_3, type = "HC1")
)

# Modelo sin 2020
robusto_sin_2020 <- coeftest(
  modelo_sin_2020,
  vcov. = vcovHC(modelo_sin_2020, type = "HC1")
)

# Tabla comparativa
tabla_significancia <- data.frame(
  Variable = rownames(robusto_original),
  P_value_original = robusto_original[, 4],
  P_value_sin_2020 = robusto_sin_2020[, 4]
)

View(tabla_significancia)

# --------------------------------------------------------------
## 16.iv COMPARACION DE LOS TRES MODELOS
# --------------------------------------------------------------

# Comparar R2 ajustado
tabla_modelos <- data.frame(
  Modelo = c("Modelo 1", "Modelo 2", "Modelo 3"),
  R2_ajustado = c(
    summary(modelo_1)$adj.r.squared,
    summary(modelo_2)$adj.r.squared,
    summary(modelo_3)$adj.r.squared
  ),
  Observaciones = c(
    nobs(modelo_1),
    nobs(modelo_2),
    nobs(modelo_3)
  )
)

View(tabla_modelos)

# Comparar las estimaciones
stargazer(
  modelo_1,
  modelo_2,
  modelo_3,
  type = "text",
  title = "Comparacion de especificaciones",
  digits = 3
)

# --------------------------------------------------------------
## 16.v ESTABILIDAD DE LOS COEFICIENTES
# --------------------------------------------------------------

# Variables presentes en los tres modelos
variables_comunes <- names(coef(modelo_1))

tabla_estabilidad <- data.frame(
  Variable = variables_comunes,
  Modelo_1 = as.numeric(
    coef(modelo_1)[variables_comunes]
  ),
  Modelo_2 = as.numeric(
    coef(modelo_2)[variables_comunes]
  ),
  Modelo_3 = as.numeric(
    coef(modelo_3)[variables_comunes]
  )
)

View(tabla_estabilidad)

# ==============================================================
## 17. RESULTADOS FINALES DEL MODELO 3
# ==============================================================

# --------------------------------------------------------------
## 17.i RESULTADOS GENERALES
# --------------------------------------------------------------

summary(modelo_3)

# Coeficientes estimados
coef(modelo_3)

# Cantidad de observaciones
nobs(modelo_3)

# R2 y R2 ajustado
summary(modelo_3)$r.squared
summary(modelo_3)$adj.r.squared

# --------------------------------------------------------------
## 17.ii SIGNIFICANCIA INDIVIDUAL
# --------------------------------------------------------------

# Pruebas t convencionales
summary(modelo_3)$coefficients

# Errores estandar robustos a heterocedasticidad
resultados_finales <- coeftest(
  modelo_3,
  vcov. = vcovHC(modelo_3, type = "HC1")
)

resultados_finales

# --------------------------------------------------------------
## 17.iii SIGNIFICANCIA CONJUNTA
# --------------------------------------------------------------

# Prueba F convencional
summary(modelo_3)$fstatistic

# --------------------------------------------------------------
## 17.iv TABLA FINAL DE COEFICIENTES
# --------------------------------------------------------------

tabla_final_modelo_3 <- data.frame(
  Variable = rownames(resultados_finales),
  Coeficiente = resultados_finales[, 1],
  Error_estandar_White = resultados_finales[, 2],
  Estadistico_t = resultados_finales[, 3],
  P_value = resultados_finales[, 4],
  row.names = NULL
) %>%
  mutate(
    across(where(is.numeric), ~round(.x, 4))
  )

View(tabla_final_modelo_3)

# --------------------------------------------------------------
## 17.v RESUMEN DE DIAGNOSTICOS
# --------------------------------------------------------------

# Multicolinealidad
View(correlaciones_altas)

# Especificacion
reset_modelo_3

# Heterocedasticidad
bp_modelo_3
data.frame(
  LM_White = LM_white,
  P_value_White = p_white
)

# Autocorrelacion
dwtest(modelo_3, alternative = "two.sided")

# Autocorrelacion tras Cochrane-Orcutt
dw_cochrane

# Estabilidad temporal
resultado_cusum

