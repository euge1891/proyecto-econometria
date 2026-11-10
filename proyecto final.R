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
    consumo = `Hogares`,
    gasto_publico = `Gobierno e ISFLSH`
  )


# Eliminamos el * de algunos trimestres
datos_macro <- datos_macro %>%
  mutate(
    trimestre = gsub("\\*", "", trimestre)
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
## 3i. AGREGAR INFORMACIÓN --> DESEMPLEO
# ==============================================================

# Ruta del archivo del INE
ruta_desempleo <- "INE_desempleo.csv"


# Leer archivo del INE
lineas <- readLines(
  ruta_desempleo,
  encoding = "latin1",
  warn = FALSE
)


# Separar filas por ;
filas <- strsplit(
  lineas[-c(1:5)],
  ";",
  fixed = TRUE
)


# Nos quedamos con las primeras 10 columnas útiles
matriz_desempleo <- t(
  sapply(
    filas,
    function(x) x[1:10]
  )
)


# Convertimos a data frame
desempleo_raw <- as.data.frame(
  matriz_desempleo,
  stringsAsFactors = FALSE
)


# Ponemos nombres claros
names(desempleo_raw) <- c(
  "periodo",
  "actividad_total",
  "actividad_hombres",
  "actividad_mujeres",
  "empleo_total",
  "empleo_hombres",
  "empleo_mujeres",
  "desempleo",
  "desempleo_hombres",
  "desempleo_mujeres"
)


View(desempleo_raw)


# Creamos una base trimestral de desempleo
desempleo_trimestral <- desempleo_raw %>%
  mutate(
    
    periodo = trimws(periodo),
    
    trimestre_num = case_when(
      grepl("Enero.*Marzo", periodo) ~ 1,
      grepl("Abril.*Junio", periodo) ~ 2,
      grepl("Julio.*Setiembre|Julio.*Septiembre", periodo) ~ 3,
      grepl("Octubre.*Diciembre", periodo) ~ 4,
      TRUE ~ NA_real_
    ),
    
    año = 2000 + as.integer(
      sub(
        ".*\\/([0-9]{2}).*",
        "\\1",
        periodo
      )
    ),
    
    desempleo = as.numeric(
      gsub(
        ",",
        ".",
        desempleo,
        fixed = TRUE
      )
    )
  ) %>%
  
  filter(
    !is.na(año),
    !is.na(trimestre_num)
  ) %>%
  
  select(
    año,
    trimestre_num,
    desempleo
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


View(desempleo_trimestral)


# Comprobamos que no haya trimestres repetidos
desempleo_trimestral %>%
  count(
    año,
    trimestre_num
  ) %>%
  filter(n > 1)


# Eliminamos cualquier columna de desempleo anterior
# por si volvemos a correr esta parte del script
datos_macro <- datos_macro %>%
  select(
    -matches("^desempleo")
  )


# Unimos desempleo a datos_macro
datos_macro <- datos_macro %>%
  left_join(
    desempleo_trimestral,
    by = c(
      "año",
      "trimestre_num"
    )
  )


View(datos_macro)


# Comprobamos que no se hayan agregado filas
nrow(datos_macro)


# Ver cuántos datos de desempleo faltan
sum(is.na(datos_macro$desempleo))


# Ver exactamente qué trimestres no tienen desempleo
datos_macro %>%
  filter(
    is.na(desempleo)
  ) %>%
  select(
    trimestre,
    año,
    trimestre_num
  )

# ==============================================================
## 3.ii AGREGAR INFORMACIÓN --> VARIABLE INFLACIÓN
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
## 3.iii AGREGAR INFORMACIÓN --> VARIABLE TIPO DE CAMBIO
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
    desempleo,
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
    desempleo,
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

# Visualizar la matriz con colores
install.packages("corrplot")  # Solo la primera vez
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
