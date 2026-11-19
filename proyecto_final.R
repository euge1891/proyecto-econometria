                ### PROYECTO ECONOMETRÍA ###

# ==============================================================
## 1. CARGAR DATOS
# ==============================================================

library(readxl)    # leer archivos Excel
library(dplyr)     # seleccionar, filtrar y transformar
library(tidyr)     # cambiar la estructura de las tablas
library(stringr)   # trabajar con texto
library(lubridate) # fechas
library(ggplot2)   # gráficos

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
  select(variable, trimestre, valor) %>%
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

# Nos quedamos con las columnas necesarias y ordenamos en el tiempo
datos_macro <- datos_macro %>%
  select(trimestre, año, trimestre_num, pbi, consumo, gasto_publico,
         inversion, exportaciones, importaciones) %>%
  arrange(año, trimestre_num)

View(datos_macro)

# ==============================================================
## 3.i AGREGAR INFORMACIÓN --> VARIABLE INFLACIÓN
# ==============================================================

ruta_inflacion <- "INE_inflacion.csv"

inflacion_raw <- read.csv2(
  ruta_inflacion,
  skip = 7,
  header = TRUE,
  stringsAsFactors = FALSE,
  fileEncoding = "latin1"
)

View(inflacion_raw)
names(inflacion_raw)

# Inflación trimestral: tomamos el último mes de cada trimestre
inflacion_trimestral <- inflacion_raw %>%
  mutate(
    mes = substr(Mes.y.año, 1, 3),
    año = as.integer(paste0("20", substr(Mes.y.año, 5, 6)))
  ) %>%
  filter(mes %in% c("mar", "jun", "sep", "dic")) %>%
  mutate(
    trimestre_num = case_when(
      mes == "mar" ~ 1,
      mes == "jun" ~ 2,
      mes == "sep" ~ 3,
      mes == "dic" ~ 4
    ),
    inflacion = Trimestre
  ) %>%
  select(año, trimestre_num, inflacion) %>%
  distinct(año, trimestre_num, .keep_all = TRUE) %>%
  arrange(año, trimestre_num)

View(inflacion_trimestral)

# Eliminar inflación anterior si ya existe y unir
datos_macro <- datos_macro %>%
  select(-matches("^inflacion")) %>%
  left_join(inflacion_trimestral, by = c("año", "trimestre_num"))

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
    read.csv2(x, stringsAsFactors = FALSE, fileEncoding = "latin1")
  }) %>%
  bind_rows()

View(tipo_cambio_raw)
names(tipo_cambio_raw)
nrow(tipo_cambio_raw)

# 2. Fecha, año y trimestre
tipo_cambio_raw <- tipo_cambio_raw %>%
  mutate(
    Fecha = dmy(Fecha),
    año = year(Fecha),
    trimestre_num = quarter(Fecha),
    Venta = as.numeric(gsub(",", ".", Venta, fixed = TRUE))
  )

# 3. Promedio trimestral del tipo de cambio
tipo_cambio_trimestral <- tipo_cambio_raw %>%
  group_by(año, trimestre_num) %>%
  summarise(tipo_cambio = mean(Venta, na.rm = TRUE), .groups = "drop") %>%
  arrange(año, trimestre_num)

View(tipo_cambio_trimestral)

# 4. Revisar que no haya trimestres repetidos
tipo_cambio_trimestral %>%
  count(año, trimestre_num) %>%
  filter(n > 1)

# 5. Unir a la base principal (sacando el tipo de cambio previo si existe)
datos_macro <- datos_macro %>%
  select(-matches("^tipo_cambio")) %>%
  left_join(tipo_cambio_trimestral, by = c("año", "trimestre_num"))

View(datos_macro)

# 6. Comprobaciones: las filas no cambian con el join
nrow(datos_macro)
sum(is.na(datos_macro$tipo_cambio))

# ==============================================================
## 4. VARIABLES EXTERNAS (EN NIVELES)
# ==============================================================

# --------------------------------------------------------------
# 4.i PBI REAL DE BRASIL
# --------------------------------------------------------------

brasil_raw <- read.csv("PBI real Brasil FRED.csv")

brasil_trimestral <- brasil_raw %>%
  mutate(
    fecha = as.Date(observation_date),
    año = year(fecha),
    trimestre_num = quarter(fecha),
    pbi_brasil = NGDPRNSAXDCBRQ
  ) %>%
  select(año, trimestre_num, pbi_brasil)

# --------------------------------------------------------------
# 4.ii PBI REAL DE ARGENTINA
# --------------------------------------------------------------

argentina_raw <- read.csv("PBI real Argentina FRED.csv")

argentina_trimestral <- argentina_raw %>%
  mutate(
    fecha = as.Date(observation_date),
    año = year(fecha),
    trimestre_num = quarter(fecha),
    pbi_argentina = NGDPRNSAXDCARQ
  ) %>%
  select(año, trimestre_num, pbi_argentina)

# --------------------------------------------------------------
# 4.iii PBI REAL DE CHINA
# --------------------------------------------------------------

china_raw <- read.csv("PBI real China ChinaData.csv")

# El archivo tiene Year (ejemplo: 2017-Q1) y Value
china_trimestral <- china_raw %>%
  mutate(
    año = as.integer(substr(Year, 1, 4)),
    trimestre_num = as.integer(substr(Year, 7, 7)),
    pbi_china = as.numeric(Value)
  ) %>%
  select(año, trimestre_num, pbi_china)

# --------------------------------------------------------------
# 4.iv TASA DE INTERÉS DE LA FED (promedio trimestral, en nivel)
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
    tasa_fed = if (sum(!is.na(FEDFUNDS)) == 3) mean(FEDFUNDS) else NA_real_,
    .groups = "drop"
  )

# --------------------------------------------------------------------------
# 4.v ÍNDICE DE PRECIOS DE EXPORTACIÓN (IPX, promedio trimestral, en nivel)
# --------------------------------------------------------------------------

ipx_raw <- read.csv(
  "IPX Uruguay BCU econuy.csv",
  check.names = FALSE,
  stringsAsFactors = FALSE
)

# El archivo tiene filas iniciales de metadatos: nos quedamos con las fechas
ipx_mensual <- ipx_raw %>%
  filter(grepl("^\\d{4}-\\d{2}-\\d{2}$", Name)) %>%
  mutate(
    fecha = as.Date(Name),
    año = year(fecha),
    trimestre_num = quarter(fecha),
    ipx = as.numeric(`Total exportaciones`)
  )

# De mensual a trimestral, exigiendo los tres meses
ipx_trimestral <- ipx_mensual %>%
  group_by(año, trimestre_num) %>%
  summarise(
    ipx = if (sum(!is.na(ipx)) == 3) mean(ipx) else NA_real_,
    .groups = "drop"
  )

# --------------------------------------------------------------
# 4.vi UNIR LAS CINCO VARIABLES A LA BASE
# --------------------------------------------------------------

datos_macro <- datos_macro %>%
  select(-any_of(c("pbi_brasil", "pbi_argentina", "pbi_china",
                   "tasa_fed", "ipx"))) %>%
  left_join(brasil_trimestral,    by = c("año", "trimestre_num")) %>%
  left_join(argentina_trimestral, by = c("año", "trimestre_num")) %>%
  left_join(china_trimestral,     by = c("año", "trimestre_num")) %>%
  left_join(fed_trimestral,       by = c("año", "trimestre_num")) %>%
  left_join(ipx_trimestral,       by = c("año", "trimestre_num")) %>%
  arrange(año, trimestre_num)

View(datos_macro)
nrow(datos_macro)

# ==============================================================
## 5. CREAR LAS VARIABLES DEL MODELO: LOGARITMOS Y DUMMIES
# ==============================================================

datos_macro <- datos_macro %>%
  mutate(
    # PBI en logaritmos (lo que pidió el profesor)
    ln_pbi       = log(pbi),
    ln_brasil    = log(pbi_brasil),
    ln_argentina = log(pbi_argentina),
    ln_china     = log(pbi_china),

    # Dummies de trimestre (el trimestre I es la base)
    d_trim2 = as.integer(trimestre_num == 2),
    d_trim3 = as.integer(trimestre_num == 3),
    d_trim4 = as.integer(trimestre_num == 4),

    # Dummy COVID: vale 1 en los cuatro trimestres de 2020
    d_covid = as.integer(año == 2020)
  )

View(datos_macro)

# ==============================================================
## 6. CREAR BASE FINAL DEL MODELO
# ==============================================================

datos_modelo_nuevo <- datos_macro %>%
  select(
    trimestre, año, trimestre_num,
    pbi, pbi_brasil, pbi_argentina, pbi_china,
    ln_pbi, ln_brasil, ln_argentina, ln_china,
    tasa_fed, ipx,
    d_trim2, d_trim3, d_trim4, d_covid,
    inflacion, tipo_cambio
  )

# Verificar los datos faltantes
colSums(is.na(datos_modelo_nuevo))

# Nos quedamos con los trimestres completos
datos_modelo_nuevo <- datos_modelo_nuevo %>%
  tidyr::drop_na()

View(datos_modelo_nuevo)

# Comprobamos: observaciones, faltantes y período
nrow(datos_modelo_nuevo)
colSums(is.na(datos_modelo_nuevo))
range(datos_modelo_nuevo$año)
summary(datos_modelo_nuevo)

# Creamos una fecha para cada trimestre (para los gráficos)
datos_modelo_nuevo <- datos_modelo_nuevo %>%
  mutate(fecha = as.Date(paste0(año, "-", (trimestre_num - 1) * 3 + 1, "-01"))) %>%
  arrange(fecha)

# ==============================================================
## 7. ESTIMAR EL MODELO ECONOMÉTRICO
# ==============================================================

modelo_1 <- lm(
  ln_pbi ~
    ln_brasil +
    ln_argentina +
    ln_china +
    tasa_fed +
    ipx +
    d_trim2 + d_trim3 + d_trim4,
  data = datos_modelo_nuevo
)
summary(modelo_1)

# ==============================================================
## 8. ANÁLISIS DESCRIPTIVO Y EXPLORATORIO
# ==============================================================

# --------------------------------------------------------------
## 8.i ESTADÍSTICAS DESCRIPTIVAS
# --------------------------------------------------------------

variables_modelo <- datos_modelo_nuevo %>%
  select(ln_pbi, ln_brasil, ln_argentina, ln_china, tasa_fed, ipx,
         inflacion, tipo_cambio)

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
    names_pattern = "^(.*)_(media|mediana|minimo|maximo|desvio)$"
  ) %>%
  mutate(across(where(is.numeric), ~round(.x, 3)))

View(estadisticas)

# Variación acumulada de cada PBI en todo el período (en %, con logs):
# 100 * [ln(PBI final) - ln(PBI inicial)], comparando IV-2025 con IV-2016
# (mismo trimestre, para no mezclar la estacionalidad)
datos_modelo_nuevo %>%
  filter(trimestre_num == 4, año %in% c(2016, 2025)) %>%
  summarise(across(c(ln_pbi, ln_brasil, ln_argentina, ln_china),
                   ~ round(100 * (last(.x) - first(.x)), 1)))

# --------------------------------------------------------------
## 8.ii GRÁFICO DEL PBI DE URUGUAY (EN LOGARITMOS)
# --------------------------------------------------------------

ggplot(datos_modelo_nuevo, aes(x = fecha, y = ln_pbi)) +
  geom_line(color = "steelblue", linewidth = 1) +
  geom_point(aes(shape = factor(trimestre_num)), color = "steelblue", size = 2) +
  labs(
    title = "Logaritmo del PBI real de Uruguay",
    subtitle = "Datos trimestrales sin desestacionalizar",
    x = "Año",
    y = "ln(PBI)",
    shape = "Trimestre"
  ) +
  theme_minimal()

# Trimestres de mayor y menor PBI
datos_modelo_nuevo %>% slice_max(order_by = ln_pbi, n = 1) %>% select(trimestre, pbi, ln_pbi)
datos_modelo_nuevo %>% slice_min(order_by = ln_pbi, n = 1) %>% select(trimestre, pbi, ln_pbi)

# --------------------------------------------------------------
## 8.iii COMPARACIÓN ENTRE PAÍSES
# --------------------------------------------------------------

# Para comparar países con escalas distintas, restamos el logaritmo del
# primer trimestre: 100 * [ln(PBI_t) - ln(PBI_I-2016)] es aproximadamente
# cuánto % creció cada país desde el inicio de la muestra.

datos_paises <- datos_modelo_nuevo %>%
  select(fecha, ln_pbi, ln_brasil, ln_argentina, ln_china) %>%
  mutate(across(-fecha, ~ 100 * (.x - first(.x)))) %>%
  pivot_longer(
    cols = -fecha,
    names_to = "pais",
    values_to = "valor"
  ) %>%
  mutate(pais = dplyr::recode(pais,
                              "ln_pbi" = "Uruguay",
                              "ln_brasil" = "Brasil",
                              "ln_argentina" = "Argentina",
                              "ln_china" = "China"
  ))

ggplot(datos_paises, aes(x = fecha, y = valor, color = pais)) +
  geom_line(linewidth = 1) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(
    title = "PBI real en logaritmos: Uruguay, Brasil, Argentina y China",
    subtitle = "100 x [ln(PBI) - ln(PBI de I-2016)]",
    x = "Año",
    y = "Variación acumulada aprox. (%)",
    color = "País"
  ) +
  theme_minimal()

# --------------------------------------------------------------
## 8.iv EVOLUCIÓN DE LA TASA DE LA FED
# --------------------------------------------------------------

ggplot(datos_modelo_nuevo, aes(x = fecha, y = tasa_fed)) +
  geom_line(color = "darkred", linewidth = 1) +
  geom_point(color = "darkred", size = 1.5) +
  labs(title = "Tasa de interés de la Fed", subtitle = "Promedio trimestral",
       x = "Año", y = "Tasa de interés (%)") +
  theme_minimal()

# --------------------------------------------------------------
## 8.v EVOLUCIÓN DEL IPX
# --------------------------------------------------------------

ggplot(datos_modelo_nuevo, aes(x = fecha, y = ipx)) +
  geom_line(color = "darkgreen", linewidth = 1) +
  geom_point(color = "darkgreen", size = 1.5) +
  labs(title = "Índice de precios de exportación de Uruguay (IPX)",
       subtitle = "Promedio trimestral", x = "Año", y = "IPX") +
  theme_minimal()

# --------------------------------------------------------------
## 8.vi INFLACIÓN Y TIPO DE CAMBIO (variables internas)
# --------------------------------------------------------------

ggplot(datos_modelo_nuevo, aes(x = fecha, y = inflacion)) +
  geom_line(color = "purple", linewidth = 1) +
  labs(title = "Inflación trimestral en Uruguay", x = "Año", y = "Inflación (%)") +
  theme_minimal()

ggplot(datos_modelo_nuevo, aes(x = fecha, y = tipo_cambio)) +
  geom_line(color = "orange", linewidth = 1) +
  labs(title = "Tipo de cambio (promedio trimestral)", x = "Año", y = "Pesos por dólar") +
  theme_minimal()

# --------------------------------------------------------------
## 8.vii GRÁFICOS DE DISPERSIÓN (marcamos 2020)
# --------------------------------------------------------------

datos_dispersion <- datos_modelo_nuevo %>%
  mutate(periodo = ifelse(año == 2020, "2020", "Resto")) %>%
  select(trimestre, periodo, ln_pbi, ln_brasil, ln_argentina, ln_china, tasa_fed, ipx) %>%
  pivot_longer(cols = c(ln_brasil, ln_argentina, ln_china, tasa_fed, ipx),
               names_to = "variable", values_to = "valor")

ggplot(datos_dispersion, aes(x = valor, y = ln_pbi)) +
  geom_point(aes(color = periodo), alpha = 0.8) +
  geom_smooth(method = "lm", se = FALSE, color = "darkred") +
  facet_wrap(~variable, scales = "free_x", ncol = 3) +
  scale_color_manual(values = c("2020" = "darkorange", "Resto" = "steelblue")) +
  labs(title = "ln(PBI de Uruguay) contra cada variable explicativa",
       x = "Variable explicativa", y = "ln(PBI Uruguay)", color = "") +
  theme_minimal()

# --------------------------------------------------------------
## 8.viii MATRIZ DE CORRELACIONES
# --------------------------------------------------------------

matriz_correlaciones <- cor(variables_modelo, use = "complete.obs")
round(matriz_correlaciones, 2)

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
## 9. INFERENCIA: PRUEBAS DE HIPÓTESIS
# ==============================================================

# install.packages("car")
# install.packages("lmtest")
# install.packages("broom")
# install.packages("strucchange")

library(car)
library(lmtest)
library(broom)
library(strucchange)
options(scipen = 999)

# --------------------------------------------------------------
# 9.i Pruebas t de significancia individual
# --------------------------------------------------------------

n <- nobs(modelo_1)
k <- length(coef(modelo_1))
n
k
modelo_1$df.residual   # 40 - 9 = 31

summary(modelo_1)$coefficients

# t crítico al 5% (prueba bilateral)
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
qf(0.95, df1 = k - 1, df2 = modelo_1$df.residual)

# --------------------------------------------------------------
# 9.iv Pruebas F de restricciones lineales
# --------------------------------------------------------------

# a) Bloque REGIONAL --> Ho: beta_brasil = beta_argentina = 0
F_region <- linearHypothesis(modelo_1, c("ln_brasil = 0", "ln_argentina = 0"))
F_region
qf(0.95, df1 = 2, df2 = modelo_1$df.residual)

# b) Bloque GLOBAL --> Ho: beta_china = beta_fed = beta_ipx = 0
F_global <- linearHypothesis(modelo_1, c("ln_china = 0", "tasa_fed = 0", "ipx = 0"))
F_global
qf(0.95, df1 = 3, df2 = modelo_1$df.residual)

# c) ¿Brasil y Argentina tienen la misma elasticidad?
F_igual <- linearHypothesis(modelo_1, "ln_brasil = ln_argentina")
F_igual

# d) ¿Hace falta controlar la estacionalidad? Ho: las 3 dummies = 0
F_estac <- linearHypothesis(modelo_1, c("d_trim2 = 0", "d_trim3 = 0", "d_trim4 = 0"))
F_estac
qf(0.95, df1 = 3, df2 = modelo_1$df.residual)

# --------------------------------------------------------------
# 9.v Ajuste del modelo: observado vs. estimado
# --------------------------------------------------------------

datos_modelo_nuevo <- datos_modelo_nuevo %>%
  mutate(
    ln_pbi_estimado = fitted(modelo_1),
    residuo = residuals(modelo_1)
  )

datos_grafico <- datos_modelo_nuevo %>%
  select(fecha, ln_pbi, ln_pbi_estimado) %>%
  pivot_longer(cols = c(ln_pbi, ln_pbi_estimado),
               names_to = "serie", values_to = "valor") %>%
  mutate(serie = dplyr::recode(serie, ln_pbi = "Observado",
                               ln_pbi_estimado = "Estimado"))

ggplot(datos_grafico, aes(x = fecha, y = valor, color = serie)) +
  geom_line(linewidth = 1) +
  geom_point(size = 1.5) +
  labs(title = "ln(PBI de Uruguay): observado vs. estimado",
       subtitle = "Modelo 1 - MCO", x = "Año", y = "ln(PBI)", color = "Serie") +
  theme_minimal()

# Error típico del modelo en %: 100 * sigma (con logs, el residuo es un %)
100 * summary(modelo_1)$sigma
datos_modelo_nuevo %>% slice_max(order_by = abs(residuo), n = 3) %>%
  select(trimestre, residuo)

# ==============================================================
## 10. MULTICOLINEALIDAD
# ==============================================================

X_vars <- c("ln_brasil", "ln_argentina", "ln_china", "tasa_fed", "ipx")

# --------------------------------------------------------------
# 10.i Matriz de correlaciones entre los regresores
# --------------------------------------------------------------

matriz_X <- cor(datos_modelo_nuevo[, X_vars])
round(matriz_X, 2)

# --------------------------------------------------------------
# 10.ii MAPA DE CALOR de las correlaciones
# --------------------------------------------------------------

nombres_X <- c(ln_brasil = "ln Brasil", ln_argentina = "ln Argentina",
               ln_china = "ln China", tasa_fed = "Tasa FED", ipx = "IPX")

mapa_calor <- as.data.frame(as.table(matriz_X)) %>%
  rename(var1 = Var1, var2 = Var2, correlacion = Freq) %>%
  mutate(var1 = factor(nombres_X[as.character(var1)], levels = nombres_X),
         var2 = factor(nombres_X[as.character(var2)], levels = rev(nombres_X)))

ggplot(mapa_calor, aes(x = var1, y = var2, fill = correlacion)) +
  geom_tile(color = "white") +
  geom_text(aes(label = round(correlacion, 2)), size = 4) +
  scale_fill_gradient2(low = "steelblue", mid = "white", high = "darkred",
                       midpoint = 0, limits = c(-1, 1)) +
  labs(title = "Mapa de calor: correlaciones entre las variables explicativas",
       subtitle = "Rojo: correlación positiva alta. Azul: negativa.",
       x = "", y = "", fill = "Correlación") +
  theme_minimal()

# --------------------------------------------------------------
# 10.iii Pares con correlación mayor a 0,6
# --------------------------------------------------------------

pares_altos <- which(abs(matriz_X) > 0.6 & upper.tri(matriz_X), arr.ind = TRUE)
data.frame(
  variable_1  = rownames(matriz_X)[pares_altos[, 1]],
  variable_2  = colnames(matriz_X)[pares_altos[, 2]],
  correlacion = round(matriz_X[pares_altos], 2)
)

# --------------------------------------------------------------
# 10.iv det(X'X) con las X estandarizadas (0 = colinealidad exacta, 1 = nada)
# --------------------------------------------------------------

det_XX <- det(matriz_X)
det_XX

# --------------------------------------------------------------
# 10.v R2 auxiliares: cada X contra las demás (y las dummies de trimestre)
# Var(beta_i) = sigma2 / [SCT_i * (1 - R2_i)]: si R2_i -> 1, explota.
# --------------------------------------------------------------

r2_auxiliar <- sapply(X_vars, function(v) {
  otras <- c(setdiff(X_vars, v), "d_trim2", "d_trim3", "d_trim4")
  summary(lm(reformulate(otras, response = v), data = datos_modelo_nuevo))$r.squared
})
round(r2_auxiliar, 2)

# --------------------------------------------------------------
# 10.vi Síntoma clásico: no significativas individualmente pero F global alta
# --------------------------------------------------------------

p_individuales <- summary(modelo_1)$coefficients[X_vars, "Pr(>|t|)"]
f_global <- summary(modelo_1)$fstatistic
p_global <- pf(f_global[1], f_global[2], f_global[3], lower.tail = FALSE)
data.frame(no_significativas = sum(p_individuales > 0.05),
           p_F_global = signif(p_global, 3))

# ==============================================================
## 11. SESGO DE VARIABLE OMITIDA Y VARIABLES IRRELEVANTES
# ==============================================================

# --------------------------------------------------------------
# 11.i ¿Qué pasa si omitimos a Argentina?
# --------------------------------------------------------------

modelo_corto <- lm(
  ln_pbi ~ ln_brasil + ln_china + tasa_fed + ipx + d_trim2 + d_trim3 + d_trim4,
  data = datos_modelo_nuevo
)
summary(modelo_corto)

# Condición 1: la omitida (Argentina) está correlacionada con Brasil
cor(datos_modelo_nuevo$ln_brasil, datos_modelo_nuevo$ln_argentina)

# Más preciso con varios regresores: delta = pendiente de regresar
# Argentina contra los demás regresores del modelo corto
reg_delta <- lm(ln_argentina ~ ln_brasil + ln_china + tasa_fed + ipx +
                  d_trim2 + d_trim3 + d_trim4, data = datos_modelo_nuevo)
coef(reg_delta)["ln_brasil"]

# Condición 2: la omitida afecta a Y
coef(modelo_1)["ln_argentina"]

# Comparamos el beta de Brasil en los dos modelos
coef(modelo_corto)["ln_brasil"]
coef(modelo_1)["ln_brasil"]

# Fórmula del sesgo: beta_corto = beta_largo + beta_argentina * delta
coef(modelo_1)["ln_brasil"] + coef(modelo_1)["ln_argentina"] * coef(reg_delta)["ln_brasil"]

# --------------------------------------------------------------
# 11.ii ¿Faltan variables? Comparamos tres especificaciones
# --------------------------------------------------------------

modelo_2 <- update(modelo_1, . ~ . + inflacion)
modelo_3 <- update(modelo_1, . ~ . + inflacion + tipo_cambio)

summary(modelo_2)
summary(modelo_3)

# Prueba F parcial: F = [(SCR_R - SCR_NR) / q] / [SCR_NR / (n - k)]
prueba_f_parcial <- function(restringido, no_restringido) {
  SCR_R  <- sum(residuals(restringido)^2)
  SCR_NR <- sum(residuals(no_restringido)^2)
  q  <- length(coef(no_restringido)) - length(coef(restringido))
  gl <- df.residual(no_restringido)
  F  <- ((SCR_R - SCR_NR) / q) / (SCR_NR / gl)
  data.frame(Estadistico_F = F, F_critico = qf(0.95, q, gl),
             P_value = pf(F, q, gl, lower.tail = FALSE))
}

prueba_f_parcial(modelo_1, modelo_2)  # ¿aporta la inflación?
prueba_f_parcial(modelo_2, modelo_3)  # ¿aporta el tipo de cambio?
prueba_f_parcial(modelo_1, modelo_3)  # ¿aportan las dos juntas?

tabla_especificacion <- data.frame(
  Modelo = c("Modelo 1", "Modelo 2", "Modelo 3"),
  Variables = c(8, 9, 10),
  R2 = c(summary(modelo_1)$r.squared, summary(modelo_2)$r.squared,
         summary(modelo_3)$r.squared),
  R2_ajustado = c(summary(modelo_1)$adj.r.squared, summary(modelo_2)$adj.r.squared,
                  summary(modelo_3)$adj.r.squared)
)
tabla_especificacion

# ==============================================================
## 12. HETEROCEDASTICIDAD
# ==============================================================

datos_modelo_nuevo$residuos  <- residuals(modelo_1)
datos_modelo_nuevo$ajustados <- fitted(modelo_1)

# --------------------------------------------------------------
# 12.i Gráfico: residuos contra valores ajustados
# --------------------------------------------------------------

ggplot(datos_modelo_nuevo, aes(x = ajustados, y = residuos)) +
  geom_point(color = "navy") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(title = "Residuos vs. valores ajustados",
       x = "ln(PBI) ajustado", y = "Residuos") +
  theme_minimal()

# --------------------------------------------------------------
# 12.ii Test de White: NO SE USA (el profesor indicó que necesita N > 100;
# tenemos N = 40). Usamos Breusch-Pagan y Goldfeld-Quandt (sección 16.iv).
# --------------------------------------------------------------

# --------------------------------------------------------------
# 12.iii Breusch-Pagan (versión de R; a mano en 16.iv)
# --------------------------------------------------------------

bptest(modelo_1)

# ==============================================================
## 13. AUTOCORRELACIÓN
# ==============================================================

# --------------------------------------------------------------
# 13.i Gráfico de los residuos en el tiempo y correlograma
# --------------------------------------------------------------

ggplot(datos_modelo_nuevo, aes(x = fecha, y = residuos)) +
  geom_line(color = "darkred") +
  geom_point(color = "darkred") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(title = "Residuos del modelo en el tiempo", x = "Trimestre", y = "Residuos") +
  theme_minimal()

acf(residuals(modelo_1), main = "Correlograma de los residuos")

# --------------------------------------------------------------
# 13.ii Antes del DW: ¿los trimestres son consecutivos?
# --------------------------------------------------------------

datos_modelo_nuevo <- datos_modelo_nuevo %>% arrange(año, trimestre_num)
indice <- with(datos_modelo_nuevo, año * 4 + trimestre_num)
all(diff(indice) == 1)   # TRUE: no falta ningún trimestre

# --------------------------------------------------------------
# 13.iii Test de Durbin-Watson
# --------------------------------------------------------------

# Ho: no hay autocorrelación de primer orden (rho = 0)
dwtest(modelo_1)
durbinWatsonTest(modelo_1)

# --------------------------------------------------------------
# 13.iv Solución: transformación de Cochrane-Orcutt (a mano)
# --------------------------------------------------------------

# Paso 1: MCO y residuos
residuos_mco <- residuals(modelo_1)
n <- length(residuos_mco)

# Paso 2: rho = Corr(e_t, e_(t-1))
rho_estimado <- cor(residuos_mco[2:n], residuos_mco[1:(n - 1)])
rho_estimado

# Paso 3: y*_t = y_t - rho*y_(t-1) ; X*_t = X_t - rho*X_(t-1)
y <- model.response(model.frame(modelo_1))
X <- model.matrix(modelo_1)   # incluye la constante y las dummies

y_transformado <- y[2:n] - rho_estimado * y[1:(n - 1)]
X_transformado <- X[2:n, , drop = FALSE] - rho_estimado * X[1:(n - 1), , drop = FALSE]

# Paso 4: MCO sobre el modelo transformado
modelo_co_manual <- lm(y_transformado ~ X_transformado - 1)
summary(modelo_co_manual)

dwtest(modelo_co_manual)

# --------------------------------------------------------------
# 13.v Comparamos MCO y Cochrane-Orcutt
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

# Prueba F de la región en el modelo corregido
F_region_co <- linearHypothesis(modelo_co_manual,
                                c("X_transformadoln_brasil = 0",
                                  "X_transformadoln_argentina = 0"))
F_region_co

# ==============================================================
## 14. NORMALIDAD DE LOS ERRORES Y BOOTSTRAP
# ==============================================================

# --------------------------------------------------------------
# 14.i Histograma de los residuos con la curva normal
# --------------------------------------------------------------

residuos_co <- residuals(modelo_co_manual)

hist(residuos_co, breaks = 10, freq = FALSE, col = "lightblue",
     main = "Distribución de los residuos (Cochrane-Orcutt)", xlab = "Residuos")
curve(dnorm(x, mean = 0, sd = sd(residuos_co)), add = TRUE, col = "blue", lwd = 2)

# ¿Cuántos residuos estandarizados caen dentro de +-2 desvíos? (normal: 95%)
sum(abs(residuos_co / sd(residuos_co)) <= 2)
length(residuos_co)

# --------------------------------------------------------------
# 14.ii Bootstrap sobre el modelo transformado
# --------------------------------------------------------------

rho <- rho_estimado
n_obs <- nrow(datos_modelo_nuevo)
t1 <- 2:n_obs        # período t
t0 <- 1:(n_obs - 1)  # período t-1

transformar <- function(z) z[t1] - rho * z[t0]

datos_co <- data.frame(
  y_t       = transformar(datos_modelo_nuevo$ln_pbi),
  constante = 1 - rho,
  brasil    = transformar(datos_modelo_nuevo$ln_brasil),
  argentina = transformar(datos_modelo_nuevo$ln_argentina),
  china     = transformar(datos_modelo_nuevo$ln_china),
  fed       = transformar(datos_modelo_nuevo$tasa_fed),
  ipx       = transformar(datos_modelo_nuevo$ipx),
  trim2     = transformar(datos_modelo_nuevo$d_trim2),
  trim3     = transformar(datos_modelo_nuevo$d_trim3),
  trim4     = transformar(datos_modelo_nuevo$d_trim4)
)

formula_co <- y_t ~ constante + brasil + argentina + china + fed + ipx +
  trim2 + trim3 + trim4 - 1

modelo_transformado <- lm(formula_co, data = datos_co)
coef(modelo_transformado)

set.seed(123)

coef_boot_brasil <- NULL
coef_boot_argentina <- NULL
coef_boot_china <- NULL
for (i in 1:10000) {
  muestra_boot <- datos_co[sample(1:nrow(datos_co), nrow(datos_co), replace = TRUE), ]
  modelo_boot <- lm(formula_co, data = muestra_boot)
  coef_boot_brasil    <- c(coef_boot_brasil,    coef(modelo_boot)["brasil"])
  coef_boot_argentina <- c(coef_boot_argentina, coef(modelo_boot)["argentina"])
  coef_boot_china     <- c(coef_boot_china,     coef(modelo_boot)["china"])
}

# Intervalos de confianza empíricos al 95%
ic_boot <- rbind(
  brasil    = quantile(coef_boot_brasil,    c(0.025, 0.975), na.rm = TRUE),
  argentina = quantile(coef_boot_argentina, c(0.025, 0.975), na.rm = TRUE),
  china     = quantile(coef_boot_china,     c(0.025, 0.975), na.rm = TRUE)
)
ic_boot

# Comparamos con los intervalos t del modelo transformado
confint(modelo_transformado)[c("brasil", "argentina", "china"), ]

# Distribución bootstrap del coeficiente de Brasil
hist(coef_boot_brasil, breaks = 40, col = "lightyellow",
     main = "Distribución bootstrap de la elasticidad de Brasil", xlab = "Beta Brasil")
abline(v = quantile(coef_boot_brasil, c(0.025, 0.975), na.rm = TRUE), col = "darkred", lwd = 2)
abline(v = 0, lty = 2)

# ==============================================================
## 15. ESPECIFICACIÓN Y CAMBIO DE RÉGIMEN (RESET, Chow, CUSUM)
# ==============================================================

# --------------------------------------------------------------
# 15.i Test RESET
# --------------------------------------------------------------

# Agregamos los cuadrados, los cubos y los productos cruzados de las X
# (las dummies no: su cuadrado es la misma dummy). Ho: delta = 0 -> prueba F
datos_reset <- as.data.frame(datos_modelo_nuevo)[, c("ln_pbi", X_vars,
                                                     "d_trim2", "d_trim3", "d_trim4")]
for (v in X_vars) {
  datos_reset[[paste0(v, "_2")]] <- datos_reset[[v]]^2
  datos_reset[[paste0(v, "_3")]] <- datos_reset[[v]]^3
}
pares <- combn(X_vars, 2)
for (j in 1:ncol(pares)) {
  datos_reset[[paste0(pares[1, j], "_x_", pares[2, j])]] <-
    datos_reset[[pares[1, j]]] * datos_reset[[pares[2, j]]]
}

modelo_reset <- lm(ln_pbi ~ ., data = datos_reset)   # 8 regresores + 20 términos
reset_curso <- anova(modelo_1, modelo_reset)
reset_curso
qf(0.95, df1 = reset_curso$Df[2], df2 = reset_curso$Res.Df[2])   # F crítico

# ¿De dónde viene? Solo con los cuadrados:
terminos_2 <- paste0(X_vars, "_2")
modelo_reset2 <- lm(reformulate(c(X_vars, "d_trim2", "d_trim3", "d_trim4", terminos_2),
                                response = "ln_pbi"), data = datos_reset)
reset_cuadrados <- anova(modelo_1, modelo_reset2)
reset_cuadrados
qf(0.95, df1 = reset_cuadrados$Df[2], df2 = reset_cuadrados$Res.Df[2])
summary(modelo_reset2)$coefficients[terminos_2, ]

# --------------------------------------------------------------
# 15.ii Prueba de Chow: ¿cambian los betas con la pandemia?
# --------------------------------------------------------------

# Quiebre en II-2020. Período I: I-2016 a I-2020 ; período II: II-2020 en adelante
# F = [(SCR* - SCR) / K] / [SCR / (N - 2K)], con SCR = SCR_I + SCR_II

formula_modelo <- ln_pbi ~ ln_brasil + ln_argentina + ln_china + tasa_fed + ipx +
  d_trim2 + d_trim3 + d_trim4

periodo_I  <- datos_modelo_nuevo %>% filter(fecha <  as.Date("2020-04-01"))
periodo_II <- datos_modelo_nuevo %>% filter(fecha >= as.Date("2020-04-01"))

K <- length(coef(modelo_1))
N <- nrow(datos_modelo_nuevo)
data.frame(Parametros = K, Obs_periodo_I = nrow(periodo_I),
           Obs_periodo_II = nrow(periodo_II), Grados_libertad = N - 2 * K)

SCR_restringido <- sum(residuals(modelo_1)^2)
SCR_I  <- sum(residuals(lm(formula_modelo, data = periodo_I))^2)
SCR_II <- sum(residuals(lm(formula_modelo, data = periodo_II))^2)
SCR_no_restringido <- SCR_I + SCR_II

F_chow <- ((SCR_restringido - SCR_no_restringido) / K) /
  (SCR_no_restringido / (N - 2 * K))
F_chow
qf(0.95, df1 = K, df2 = N - 2 * K)
1 - pf(F_chow, df1 = K, df2 = N - 2 * K)

# --------------------------------------------------------------
# 15.iii Prueba CUSUM de estabilidad de parámetros
# --------------------------------------------------------------

cusum <- efp(formula_modelo, data = datos_modelo_nuevo, type = "Rec-CUSUM")
plot(cusum, main = "Prueba CUSUM")
sctest(cusum)

alfa <- 0.05

# --------------------------------------------------------------
# 16.i Linealidad: RESET (15.i)
# --------------------------------------------------------------

F_reset  <- reset_curso$F[2]
Fc_reset <- qf(0.95, reset_curso$Df[2], reset_curso$Res.Df[2])
c(F = F_reset, F_critico = Fc_reset, p = reset_curso$`Pr(>F)`[2])

# --------------------------------------------------------------
# 16.ii Multicolinealidad: resumen de la sección 10
# --------------------------------------------------------------

c(pares_mayores_0.6 = nrow(pares_altos), det = round(det_XX, 3),
  R2_aux_max = round(max(r2_auxiliar), 2))

# --------------------------------------------------------------
# 16.iii Exogeneidad: prueba de endogeneidad de Hausman
# --------------------------------------------------------------

# i)  X_i = Z*gamma + w  -> guardamos w_hat
# ii) y = X*beta + delta*w_hat + error ; Ho) delta = 0 (exogeneidad)
# Instrumentos (Z): el valor de cada X en el trimestre ANTERIOR
# (predeterminado: un shock de Uruguay en t no puede haberlo afectado).

rezagos <- brasil_trimestral %>%
  full_join(argentina_trimestral, by = c("año", "trimestre_num")) %>%
  full_join(china_trimestral,     by = c("año", "trimestre_num")) %>%
  full_join(fed_trimestral,       by = c("año", "trimestre_num")) %>%
  full_join(ipx_trimestral,       by = c("año", "trimestre_num")) %>%
  arrange(año, trimestre_num) %>%
  mutate(
    z_brasil    = lag(log(pbi_brasil), 1),
    z_argentina = lag(log(pbi_argentina), 1),
    z_china     = lag(log(pbi_china), 1),
    z_fed       = lag(tasa_fed, 1),
    z_ipx       = lag(ipx, 1)
  ) %>%
  select(año, trimestre_num, starts_with("z_"))

datos_hausman <- datos_modelo_nuevo %>%
  left_join(rezagos, by = c("año", "trimestre_num"))

Z_vars <- c("z_brasil", "z_argentina", "z_china", "z_fed", "z_ipx")
colSums(is.na(datos_hausman[, Z_vars]))   # no puede faltar ninguno

# Etapa i): cada X contra los instrumentos (y las dummies de trimestre)
relevancia <- data.frame()
for (v in X_vars) {
  etapa1 <- lm(reformulate(c(Z_vars, "d_trim2", "d_trim3", "d_trim4"), response = v),
               data = datos_hausman)
  datos_hausman[[paste0("w_", v)]] <- residuals(etapa1)
  prueba_z <- linearHypothesis(etapa1, paste(Z_vars, "= 0"))
  relevancia <- rbind(relevancia, data.frame(
    variable = v,
    F_instrumentos = round(prueba_z$F[2], 2),
    p_valor = signif(prueba_z$`Pr(>F)`[2], 3)
  ))
}
relevancia   # p < 0.05: los instrumentos son relevantes

# Etapa ii): agregamos los w_hat al modelo original
W_vars <- paste0("w_", X_vars)
modelo_hausman <- lm(
  reformulate(c(X_vars, "d_trim2", "d_trim3", "d_trim4", W_vars), response = "ln_pbi"),
  data = datos_hausman
)
summary(modelo_hausman)$coefficients[W_vars, ]

hausman_F <- linearHypothesis(modelo_hausman, paste(W_vars, "= 0"))
hausman_F
qf(0.95, df1 = length(W_vars), df2 = modelo_hausman$df.residual)

# --------------------------------------------------------------
# 16.iv Homocedasticidad: Breusch-Pagan y Goldfeld-Quandt (a mano)
# --------------------------------------------------------------

# Breusch-Pagan:
#  g_i = e_i^2 / sigma2, sigma2 = SCR/(N-K); regresar g contra z; Q = SCE/2 ~ Chi2(p)
e_mco  <- residuals(modelo_1)
N_bp   <- length(e_mco)
sigma2 <- sum(e_mco^2) / (N_bp - length(coef(modelo_1)))
g      <- e_mco^2 / sigma2
reg_bp <- lm(g ~ ., data = datos_modelo_nuevo[, X_vars])
SCE_bp <- sum((fitted(reg_bp) - mean(g))^2)
Q_bp   <- SCE_bp / 2
p_bp   <- 1 - pchisq(Q_bp, df = length(X_vars))
c(Q = Q_bp, chi2_critico = qchisq(0.95, length(X_vars)), p_valor = p_bp)

# Goldfeld-Quandt:
#  ordenar por los valores ajustados, omitir C observaciones centrales,
#  estimar con las primeras y las últimas (N-C)/2 y comparar las SCR
orden    <- order(fitted(modelo_1))
datos_gq <- datos_modelo_nuevo[orden, ]
C_gq     <- 12
n_grupo  <- (N_bp - C_gq) / 2               # 14 observaciones en cada grupo
reg_gq_I  <- lm(formula_modelo, data = head(datos_gq, n_grupo))
reg_gq_II <- lm(formula_modelo, data = tail(datos_gq, n_grupo))
SCR_gq_I  <- sum(residuals(reg_gq_I)^2)
SCR_gq_II <- sum(residuals(reg_gq_II)^2)
gl_I  <- df.residual(reg_gq_I)
gl_II <- df.residual(reg_gq_II)
R_gq  <- (SCR_gq_II / gl_II) / (SCR_gq_I / gl_I)
p_gq  <- 1 - pf(R_gq, gl_II, gl_I)
c(R = R_gq, F_critico = qf(0.95, gl_II, gl_I), gl_II = gl_II, gl_I = gl_I, p_valor = p_gq)

# --------------------------------------------------------------
# 16.v Autocorrelación: las cuatro pruebas
# --------------------------------------------------------------

# (a) Cambios de signo en los residuos (pocos = rachas)
cambios_signo <- sum(diff(sign(e_mco)) != 0)
c(cambios_de_signo = cambios_signo, maximo_posible = N_bp - 1)

# (b) Durbin-Watson antes y después de Cochrane-Orcutt
dw_mco <- dwtest(modelo_1)
dw_co  <- dwtest(modelo_co_manual)
dw_mco
dw_co

# (c) Prueba t sobre rho en e_t = rho * e_(t-1) + v_t
reg_rho <- lm(e_mco[2:N_bp] ~ e_mco[1:(N_bp - 1)] - 1)
summary(reg_rho)$coefficients

# (d) Prueba de Durbin: e_t contra las X y e_(t-1)
datos_durbin <- datos_modelo_nuevo[2:N_bp, c(X_vars, "d_trim2", "d_trim3", "d_trim4")]
datos_durbin$e_t   <- e_mco[2:N_bp]
datos_durbin$e_lag <- e_mco[1:(N_bp - 1)]
reg_durbin <- lm(reformulate(c(X_vars, "d_trim2", "d_trim3", "d_trim4", "e_lag"),
                             response = "e_t"), data = datos_durbin)
summary(reg_durbin)$coefficients["e_lag", ]

# Prueba t sobre rho en el modelo corregido
e_co <- residuals(modelo_co_manual)
reg_rho_co <- lm(e_co[-1] ~ e_co[-length(e_co)] - 1)
summary(reg_rho_co)$coefficients

# --------------------------------------------------------------
# 16.vi Tabla resumen del check
# --------------------------------------------------------------

p_rho  <- summary(reg_rho)$coefficients[1, 4]
p_durb <- summary(reg_durbin)$coefficients["e_lag", 4]
p_haus <- hausman_F$`Pr(>F)`[2]

check_supuestos <- data.frame(
  Supuesto = c("1. Linealidad", "2. No multicolinealidad", "3. Exogeneidad",
               "4. Homocedasticidad", "5. No autocorrelación", "6. Normalidad"),
  Pruebas = c("RESET del curso (X², X³, X·X)",
              "Corr. > 0,6 / mapa de calor / det / R2 aux.",
              "Hausman (residuos), Z = rezagos",
              "Goldfeld-Quandt / Breusch-Pagan",
              "DW / t sobre rho / Durbin",
              "Histograma + bootstrap"),
  Resultado = c(
    paste0("F = ", round(F_reset, 2), " vs. crítico ", round(Fc_reset, 2)),
    paste0(nrow(pares_altos), " par(es) > 0,6; det = ", round(det_XX, 3),
           "; R2 máx = ", round(max(r2_auxiliar), 2)),
    paste0("F = ", round(hausman_F$F[2], 2), " (p = ", round(p_haus, 3), ")"),
    paste0("GQ: R = ", round(R_gq, 2), " vs ", round(qf(0.95, gl_II, gl_I), 2),
           "; BP: Q = ", round(Q_bp, 2), " vs ", round(qchisq(0.95, length(X_vars)), 2)),
    paste0("DW = ", round(dw_mco$statistic, 2), "; p = ", signif(p_rho, 2), " / ",
           signif(p_durb, 2), " -> C-O: DW = ", round(dw_co$statistic, 2)),
    "Ver histograma 14.i"
  ),
  Cumple = c(
    ifelse(F_reset < Fc_reset, "Sí", "No"),
    ifelse(nrow(pares_altos) == 0, "Sí", "Aproximada (no exacta)"),
    ifelse(p_haus > alfa, "Sí", "No"),
    ifelse(min(p_bp, p_gq) > alfa, "Sí", "No"),
    ifelse(dw_mco$p.value > alfa, "Sí", "No -> corregido con C-O"),
    "Aproximada; bootstrap como respaldo"
  )
)
check_supuestos

# ==============================================================
## 17. VALORES EXTREMOS (regla del boxplot, 1,5 x RIC, sobre los residuos)
# ==============================================================

# En niveles, las variables tienen tendencia y estacionalidad, así que los
# valores extremos se buscan en los RESIDUOS: son los trimestres que el
# modelo no logra explicar.
boxplot(residuals(modelo_1), main = "Residuos del modelo", ylab = "Residuo")

q_e   <- quantile(residuals(modelo_1), c(0.25, 0.75))
ric_e <- q_e[2] - q_e[1]
extremos_residuos <- datos_modelo_nuevo$trimestre[
  residuals(modelo_1) < q_e[1] - 1.5 * ric_e | residuals(modelo_1) > q_e[2] + 1.5 * ric_e]
extremos_residuos

# Residuos estandarizados mayores a 2 en valor absoluto
datos_modelo_nuevo %>%
  mutate(res_est = residuos / sd(residuos)) %>%
  filter(abs(res_est) > 2) %>%
  select(trimestre, residuos, res_est)

# Outliers en los gráficos de dispersión de China y la FED (pedido del profesor):
# ¿qué trimestres se separan de la nube?
datos_modelo_nuevo %>%
  arrange(residuos) %>%
  select(trimestre, ln_china, tasa_fed, residuos) %>%
  head(5)

# Trimestres de 2020-2021 con la FED en casi 0% (período atípico de tasas)
datos_modelo_nuevo %>% filter(tasa_fed < 0.25) %>% select(trimestre, tasa_fed)

# ==============================================================
## 18. INTERPRETACIÓN Y APLICACIÓN (modelo de Cochrane-Orcutt)
# ==============================================================

confint(modelo_co_manual)

# Aplicación: si el PBI de Brasil y el de Argentina suben 2% más,
# efecto sobre el PBI uruguayo = 2*b_brasil + 2*b_argentina (en %),
# con Var = a' V a
a_vec <- c(0, 2, 2, 0, 0, 0, 0, 0, 0)
efecto <- sum(a_vec * coef(modelo_co_manual))
ee_efecto <- sqrt(as.numeric(t(a_vec) %*% vcov(modelo_co_manual) %*% a_vec))
t_c <- qt(0.975, modelo_co_manual$df.residual)
c(efecto = efecto, inferior = efecto - t_c * ee_efecto, superior = efecto + t_c * ee_efecto)

# Semi-elasticidad de la FED: +1 punto de tasa -> 100*beta % de PBI
100 * coef(modelo_co_manual)["X_transformadotasa_fed"]

# ==============================================================
## 19. COMPARACIÓN: MODELO CON DUMMY COVID (todo 2020)
# ==============================================================

modelo_covid <- update(modelo_1, . ~ . + d_covid)
summary(modelo_covid)

# a) ¿Es significativa la dummy?
summary(modelo_covid)$coefficients["d_covid", ]

# b) ¿Cambian las elasticidades?
round(cbind(sin_dummy = coef(modelo_1)[X_vars],
            con_dummy = coef(modelo_covid)[X_vars]), 3)

# c) RESET del curso con la dummy: ¿mejora la forma funcional?
datos_reset_covid <- datos_reset
datos_reset_covid$d_covid <- datos_modelo_nuevo$d_covid
modelo_reset_covid <- lm(ln_pbi ~ ., data = datos_reset_covid)
reset_covid <- anova(modelo_covid, modelo_reset_covid)
reset_covid
qf(0.95, df1 = reset_covid$Df[2], df2 = reset_covid$Res.Df[2])

# d) Autocorrelación con la dummy y Cochrane-Orcutt
dwtest(modelo_covid)

co_manual <- function(modelo) {
  e <- residuals(modelo); n <- length(e)
  rho <- cor(e[2:n], e[1:(n - 1)])
  y <- model.response(model.frame(modelo)); X <- model.matrix(modelo)
  y_t <- y[2:n] - rho * y[1:(n - 1)]
  X_t <- X[2:n, , drop = FALSE] - rho * X[1:(n - 1), , drop = FALSE]
  lm(y_t ~ X_t - 1)
}
modelo_covid_co <- co_manual(modelo_covid)
summary(modelo_covid_co)
F_region_covid <- linearHypothesis(modelo_covid_co, c("X_tln_brasil = 0", "X_tln_argentina = 0"))
F_region_covid

# e) Residuos de 2020 con y sin dummy
datos_modelo_nuevo %>%
  mutate(res_sin = residuals(modelo_1), res_con = residuals(modelo_covid)) %>%
  filter(año == 2020) %>%
  select(trimestre, res_sin, res_con)

# f) Tabla resumen
data.frame(
  Modelo = c("Sin dummy (Modelo 1)", "Con dummy COVID 2020"),
  R2_ajustado = round(c(summary(modelo_1)$adj.r.squared,
                        summary(modelo_covid)$adj.r.squared), 3),
  Brasil = round(c(coef(modelo_1)["ln_brasil"], coef(modelo_covid)["ln_brasil"]), 3),
  Argentina = round(c(coef(modelo_1)["ln_argentina"], coef(modelo_covid)["ln_argentina"]), 3),
  F_RESET = round(c(reset_curso$F[2], reset_covid$F[2]), 2),
  p_RESET = round(c(reset_curso$`Pr(>F)`[2], reset_covid$`Pr(>F)`[2]), 3)
)


