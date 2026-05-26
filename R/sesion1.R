# Introducción a R con datos de población municipal de Eustat
# Curso: Introducción a R - Asociación Vasca de Sociología
#
# Objetivos de esta sesión:
# - Leer un archivo CSV
# - Mirar la estructura de una tabla
# - Filtrar filas con filter()
# - Seleccionar columnas con select()
# - Crear variables con mutate()
# - Ordenar filas con arrange()
# - Agrupar y resumir con group_by() + summarise()
# - Hacer un primer gráfico con ggplot2

# -----------------------------------------------------------------------------
# 0. Paquetes
# -----------------------------------------------------------------------------

# Si todavía no tienes estos paquetes instalados, descomenta y ejecuta:
# install.packages(c("tidyverse", "janitor"))

library(tidyverse)
library(janitor)

# -----------------------------------------------------------------------------
# 1. Leer los datos
# -----------------------------------------------------------------------------

# Este archivo viene de Eustat: 
# https://www.eustat.eus/elementos/ele0011400/poblacion-de-la-ca-de-euskadi-por-ambitos-territoriales-segun-grandes-grupos-de-edad-y-sexo/tbl0011427_c.html

# pero esta pre-limpiado para empezar.
# Cada fila contiene población para:
#   municipio x sexo/género x grupo de edad
# en otro momento os puedo explicar como se ha limpiado 
# el fichero original

poblacion <- read_csv("data/clean/pobmun.csv")
# (explica asignacion, referencias)
# -----------------------------------------------------------------------------
# 2. Mirar los datos
# -----------------------------------------------------------------------------

# glimpse() enseña la estructura: nombres de columnas, tipos de datos y ejemplos.
glimpse(poblacion)

# View() abre una pestaña para mirar la tabla completa en RStudio.
View(poblacion)

# También podemos mirar solo las primeras filas.
head(poblacion)

# ¿Qué columnas tenemos?
names(poblacion)

# ¿Cuántas filas y columnas tiene la tabla?
dim(poblacion)

# EJERCICIO 1 ---------------------------------------------------------------
# Antes de seguir, responde mirando el resultado de glimpse():
# 1. ¿Cuál es la unidad de observación de esta tabla?
# 2. ¿Qué columna contiene la población?
# 3. ¿Qué columnas describen la geografía?
# 4. ¿Qué columnas describen sexo/género y edad?

# -----------------------------------------------------------------------------
# 3. Filtrar filas con filter()
# -----------------------------------------------------------------------------

# Empezamos con una versión sencilla: población total por municipio.
# Nos quedamos con:
#   genero == "Total"
#   edad_etiqueta == "Total"

pob_total_mun <- poblacion |>
  filter(genero == "Total", edad_etiqueta == "Total")

pob_total_mun |>
  head()

# EJERCICIO 2 ---------------------------------------------------------------
# Crea una tabla llamada pob_mujeres_total que contenga solo:
#   genero == "Mujeres"
#   edad_etiqueta == "Total"


# EJERCICIO 3 ---------------------------------------------------------------
# Crea una tabla con la población total de los municipios de Bizkaia.

# -----------------------------------------------------------------------------
# 4. Seleccionar columnas con select()
# -----------------------------------------------------------------------------

# A veces queremos una tabla más pequeña, solo con las columnas que necesitamos.

pob_total_mun_simple <- pob_total_mun |>
  select(prov, comarca, municipio, pob)

pob_total_mun_simple |>
  head()

# EJERCICIO 4 ---------------------------------------------------------------
# A partir de pob_total_mun, crea una tabla con solo estas columnas:
#   municipio, comarca, prov, pob
#
# ¿Qué cambia si pones las columnas en otro orden dentro de select()?

# ¿Hay problema si seleccionamos solo comarca, prov, y pob?

# -----------------------------------------------------------------------------
# 5. Ordenar filas con arrange()
# -----------------------------------------------------------------------------

# Municipios más poblados de la C.A. de Euskadi:

pob_total_mun |>
  arrange(desc(pob)) |>
  select(prov, comarca, municipio, pob) |>
  head(10)

# Municipios menos poblados:

pob_total_mun |>
  arrange(pob) |>
  select(prov, comarca, municipio, pob) |>
  head(10)

# EJERCICIO 5 ---------------------------------------------------------------
# ¿Cuáles son los 10 municipios más poblados de Gipuzkoa?
#
# Pistas:
# - empieza con pob_total_mun
# - usa filter(prov == "Gipuzkoa")
# - usa arrange(desc(pob))
# - termina con head(10)


# -----------------------------------------------------------------------------
# 6. Crear variables nuevas con mutate()
# -----------------------------------------------------------------------------

# Queremos calcular el porcentaje de población de 65 años o más en cada municipio.
# Para ello necesitamos dos tablas:
#   1. población total por municipio
#   2. población de 65+ por municipio

pob_65_mas <- poblacion |>
  filter(genero == "Total", edad_etiqueta == ">= 65") |>
  select(municipio_cod_full, pob_65_mas = pob)

pob_total_mun <- pob_total_mun |>
  # unir datos!
  left_join(pob_65_mas, by = "municipio_cod_full") |>
  mutate(pct_65_mas = pob_65_mas / pob * 100)

pob_total_mun |>
  select(prov, comarca, municipio, pob, pob_65_mas, pct_65_mas) |>
  arrange(desc(pct_65_mas)) |>
  head(10)

# EJERCICIO 6 ---------------------------------------------------------------
# Crea una variable llamada pct_0_19 con el porcentaje de población de 0-19 años.
#
# Pasos sugeridos:
# 1. Crea una tabla pob_0_19 filtrando edad_etiqueta == "0-19".
# 2. Selecciona municipio_cod_full y pob_0_19 = pob.
# 3. Haz left_join() con pob_total_mun.
# 4. Usa mutate(pct_0_19 = pob_0_19 / pob * 100).


# -----------------------------------------------------------------------------
# 7. Resumir datos con group_by() + summarise()
# -----------------------------------------------------------------------------

# Población total por provincia:

pob_total_mun |>
  group_by(prov) |>
  summarise(
    poblacion_total = sum(pob),
    municipios = n()
  )

# Porcentaje de 65+ por provincia.
# Ojo: para porcentajes agregados, mejor sumar numeradores y denominadores.

pob_total_mun |>
  group_by(prov) |>
  summarise(
    poblacion_total = sum(pob),
    poblacion_65_mas = sum(pob_65_mas),
    pct_65_mas = poblacion_65_mas / poblacion_total * 100
  )

# EJERCICIO 7 ---------------------------------------------------------------
# Calcula la población total por comarca.
#
# Pistas:
# - usa group_by(prov, comarca)
# - usa summarise(poblacion_total = sum(pob))
# - después usa arrange(desc(poblacion_total))


# EJERCICIO 8 ---------------------------------------------------------------
# Calcula el porcentaje de 65+ por comarca.
#
# Pistas:
# - agrupa por prov y comarca
# - suma pob y pob_65_mas
# - crea pct_65_mas dentro de summarise() o con mutate() después


# -----------------------------------------------------------------------------
# 8. Primeros gráficos con ggplot2
# -----------------------------------------------------------------------------

# Gráfico 1: población total por provincia

pob_total_mun |>
  group_by(prov) |>
  summarise(poblacion_total = sum(pob)) |>
  ggplot(aes(x = prov, y = poblacion_total)) +
  geom_col() +
  labs(
    title = "Población total por territorio histórico",
    subtitle = "C.A. de Euskadi, 2025",
    x = NULL,
    y = "Población"
  )

# Gráfico 2: municipios más poblados

pob_total_mun |>
  arrange(desc(pob)) |>
  slice_head(n = 10) |>
  ggplot(aes(x = reorder(municipio, pob), y = pob)) +
  geom_col() +
  coord_flip() +
  labs(
    title = "Los 10 municipios más poblados",
    subtitle = "C.A. de Euskadi, 2025",
    x = NULL,
    y = "Población"
  )

# Gráfico 3: porcentaje de población de 65+ por provincia

pob_total_mun |>
  group_by(prov) |>
  summarise(
    poblacion_total = sum(pob),
    poblacion_65_mas = sum(pob_65_mas),
    pct_65_mas = poblacion_65_mas / poblacion_total * 100
  ) |>
  ggplot(aes(x = prov, y = pct_65_mas)) +
  geom_col() +
  labs(
    title = "Porcentaje de población de 65 años o más",
    subtitle = "Territorios históricos, C.A. de Euskadi, 2025",
    x = NULL,
    y = "%"
  )

# EJERCICIO 9 ---------------------------------------------------------------
# Haz un gráfico de barras con las 10 comarcas con mayor porcentaje de 65+.
#
# Pistas:
# - empieza agrupando por prov y comarca
# - calcula pct_65_mas
# - ordena con arrange(desc(pct_65_mas))
# - usa slice_head(n = 10)
# - usa ggplot() + geom_col() + coord_flip()

# EJERCICIO 10 --------------------------------------------------------------
# Calcular la poblacion total de las provinvias desde el fichero inicial
# poblacion
#
# Pistas:
# - hay que eliminar algunas files antes?
# - hay que declarar grupos
# - y despues que hacemos? mutate() o summarize()?


# Resumen de functions:
# read_csv() 
# filter()
# select()
# mutate()
# group_by()
# summarize()

# ggplot()
# geom_col()
# coord_flip()
# labs()



