# Introducción a R con datos sociales
# Sesión 2 - Asociación Vasca de Sociología

library(tidyverse)
library(janitor)

# --------------------------------------
# 1. Recuperar el punto donde nos quedamos
# --------------------------------------

poblacion <- read_csv("data/clean/pobmun.csv", show_col_types = FALSE)


pob_total_mun <- poblacion |>
  filter(genero == "Total", edad_etiqueta == "Total")

pob_65_mas <- poblacion |>
  filter(genero == "Total", edad_etiqueta == ">= 65") |>
  select(municipio_cod_full, pob_65_mas = pob)

pob_total_mun <- pob_total_mun |>
  left_join(pob_65_mas, by = "municipio_cod_full") |>
  mutate(pct_65_mas = pob_65_mas / pob * 100)

# identico:
poblacion |> 
  filter(genero == "Total", edad_etiqueta != "Total") |>
  group_by(municipio_cod_full) |> 
  mutate(pob_65_mas = 100 * pob / sum(pob)) |> 
  filter(edad_etiqueta == ">= 65") |> 
  select(prov, comarca, municipio, pob, pob_65_mas)

# EJERCICIO 1 -----------------------------------------
# Crea una variable pct_0_19 con el porcentaje de 
# población de 0-19 años.


# -------------------------------------
# 2. Recodificar con mutate() + case_when()
# -------------------------------------

pob_total_mun <- pob_total_mun |>
  mutate(
    tamano_mun = case_when(
      pob < 1000 ~ "menos de 1.000",
      pob < 5000 ~ "1.000-4.999",
      pob < 20000 ~ "5.000-19.999",
      TRUE ~ "20.000 o más"
    )
  )

pob_total_mun |>
  count(tamano_mun)

# EJERCICIO 2 ---------------------------------------------
# Que ideas teneis para crear una variable para categorizar 
# la estructura de edad de los municipios en mas o menos
# envejecido?

pob_total_mun <- pob_total_mun |>
  mutate(
    envejecimiento_mun = case_when(
      pct_65_mas < 20 ~ "menos envejecido",
      pct_65_mas < 25 ~ "intermedio",
      TRUE ~ "más envejecido"
    )
  )

pob_total_mun |>
  count(envejecimiento_mun)

# -------------------------------------------------------
# 3. group_by() + summarise()
# -------------------------------------------------------
# Una tabla municipal se puede resumir a provincias o comarcas

resumen_prov <- poblacion |>
  filter(edad_etiqueta != "Total") |> 
  group_by(prov_cod, prov, edad, edad_etiqueta) |>
  summarise(
    pob = sum(pob),
    municipios = n(),
    .groups = "drop"
  ) |>
  group_by(prov_cod, prov) |> 
  mutate(
    pob_pct = pob / sum(pob)
  )

resumen_prov

# Se podria hacer lo mismo por comarcas.
# NOTA: hacer esto porque lo necesitamos luego!
resumen_comarcas <- poblacion |>
  filter(edad_etiqueta != "Total") |> 
  group_by(comarca, comarca_cod, comarca_cod_full, edad, edad_etiqueta) |>
  summarise(
    pob = sum(pob),
    .groups = "drop"
  ) |>
  group_by(comarca) |> 
  mutate(
    pob_pct = pob / sum(pob)
  )
# EJERCICIO 3 ------------------------------------------
# (reto, 5+ min)
# 1. empezar con poblacion - 
# 2. hacer una variable de tamaño municipio
# 3. agregar los muncipios por tamano (3 agregados), 
#          preservando el genero y la edad.
# objetivo: una tabla final que tiene 3 (tamaño) x 2 (genero) 
# x 3 (grupos edad) = 18 filas, y las columnas:
# tamaño | genero | edad | edad_etiqueta | pob.

# EJERCICIO 4 -----------------------------------------------
# ¿Cuáles son las 10 comarcas con mayor porcentaje de población de 65+?

# ---------------------------------------------------
# 4. Joins: añadir PIB provincial de INE
# ---------------------------------------------------

# esto he descargado de aqui: https://www.ine.es/jaxi/Tabla.htm?tpx=67284&L=0
# Archivo esperado: data/raw/67284.csv
# Es un TSV de INE. Lo usamos solo como ejemplo de join por código provincial.

pib_prov_raw <- read_tsv(
  "data/raw/67284.csv",
  locale = locale(
    encoding = "Latin1",
    decimal_mark = ",",
    grouping_mark = "."
  )
)

glimpse(pib_prov_raw)

# Si los nombres vienen con mayúsculas/acentos/espacios, clean_names() ayuda.

pib_prov <- pib_prov_raw |>
  clean_names()


# Ajusta estos nombres si INE cambia la descarga.
# Normalmente hay una columna de provincias, una de periodo/año y una de valor.

pib_prov <- pib_prov |>
  mutate(prov_cod = substr(provincias,1,2)) |> 
  select(prov_cod, pib = total)

resumen_prov_pib <- resumen_prov |>
  group_by(prov, prov_cod) |> 
  summarize(pob = sum(pob),
            .groups = "drop") |> 
  left_join(pib_prov, by = "prov_cod") |>
  mutate(pib_per_cap = pib / pob)

resumen_prov_pib

# EJERCICIO 5 -------------------------------------
# Cambia left_join() por right_join(), que cambia?
# prueba anti_join(), que cambia?

# la forma es:
# resumen_prov |>
#  *_join(pib_prov, by = "prov_cod")
# done * se cambia por diferentes tipos de join

# --------------------------------------------------
# 5. Más joins: una tabla auxiliar de comarcas
# --------------------------------------------------

# A veces una recodificación grande vive mejor en una tabla aparte.
# Esto es un simulacro: una clasificación simple de comarcas.

# nota: hay que hacer resumen comarca antes!!
comarca_tipo <- resumen_comarcas |>
  group_by(comarca, comarca_cod_full) |> 
  summarize(pob = sum(pob), 
            .groups = "drop") |> 
  mutate(
    tipo_comarca = case_when(
      str_detect(comarca, "Bilbo|Donostialdea") ~ "metropolitana",
      str_detect(comarca, "Arabako|Añana|Gorbeialdea") ~ "rural/interior",
      TRUE ~ "mixta"
    )
  )

comarca_tipo |>
  count(tipo_comarca)

resumen_comarca_tipo <- resumen_comarcas |>
  left_join(
    comarca_tipo |> select(comarca_cod_full, tipo_comarca),
    by = "comarca_cod_full"
  )

# porque es diferente?
resumen_comarca_tipo |>
  count(tipo_comarca)

# -------------------------------------------------
# 6. Pivotar: largo y ancho
# -------------------------------------------------

# La tabla original es larga: una fila por municipio x género x edad.
# Para comparar grupos de edad como columnas, usamos pivot_wider().

edad_pct_mun <- poblacion |>
  filter(genero == "Total", !is.na(edad)) |>
  group_by(municipio_cod_full, municipio) |>
  mutate(pct = pob / sum(pob) * 100) |>
  ungroup() |>
  select(municipio_cod_full, municipio, edad, pct) |>
  pivot_wider(names_from = edad, values_from = pct)

edad_pct_mun |>
  head()

# EJERCICIO 6 --------------------------------------
# Hemos olvidado anadir las columnas prov y comarca 
# a edad_pct_mun. Puedes anadir estas columnas (solo)?
# tips: utilia pob_total_mun, elige solo las columnas 
# que necesitamos, y hacer algun tipo de join.

# pivot_longer() hace el camino contrario.

edad_pct_largo <- edad_pct_mun |>
  pivot_longer(
    cols = c(`0`, `20`, `65`),
    names_to = "edad",
    values_to = "pct"
  )

edad_pct_largo |>
  head()

# EJERCICIO 8 -----------------------------------------
# Filtra edad_pct_largo para edad == "65" y ordena por pct descendente.

edad_pct_largo |>
  filter(edad == "65") |>
  arrange(desc(pct)) |>
  head(10)

# -----------------------------------------------------
# 7. ggplot2: gráficos básicos
# -----------------------------------------------------

# Barras: población por provincia.

resumen_prov |>
  group_by(prov) |> 
  summarize(pob = sum(pob)) |> 
  ggplot(aes(x = prov, y = pob)) +
  geom_col() +
  labs(
    title = "Población total por territorio histórico",
    subtitle = "CAPV, 2025",
    x = NULL,
    y = "Población"
  )

# Barras horizontales: 10 comarcas con más población.
resumen_comarcas |>
  group_by(comarca) |> 
  summarize(poblacion_total = sum(pob)) |> 
  arrange(desc(poblacion_total)) |>
  slice_head(n = 10) |>
  ggplot(aes(x = reorder(comarca, poblacion_total), y = poblacion_total)) +
  geom_col() +
  coord_flip() + # esto es clave
  labs(
    title = "Las 10 comarcas con más población",
    subtitle = "C.A. de Euskadi, 2025",
    x = NULL,
    y = "Población"
  )

# Puntos: juventud vs envejecimiento municipal.

edad_pct_mun |>
  ggplot(aes(x = `0`, y = `65`)) +
  geom_point() +
  labs(
    title = "Estructura por edad de los municipios",
    subtitle = "Porcentaje 0-19 frente a porcentaje 65+",
    x = "% 0-19",
    y = "% 65+"
  )

# EJERCICIO 9 ------------------------------------------
# Haz un gráfico de barras con las 10 comarcas con 
# mayor pct_65_mas.
# Pistas: resumen_comarca, arrange(desc(pct_65_mas)),
# slice_head(n = 10),
# reorder(comarca, pct_65_mas), geom_col(), coord_flip().

resumen_comarca |>
  arrange(desc(pct_65_mas)) |>
  slice_head(n = 10) |>
  ggplot(aes(x = reorder(comarca, pct_65_mas), y = pct_65_mas)) +
  geom_col() +
  coord_flip() +
  labs(
    title = "Comarcas con mayor porcentaje de población de 65+",
    subtitle = "C.A. de Euskadi, 2025",
    x = NULL,
    y = "% 65+"
  )

# ---------------------------------------
# 8. Guardar resultados
# ---------------------------------------

write_csv(resumen_comarcas, "data/clean/resumen_comarca_2025.csv")
write_csv(resumen_prov_pib, "data/clean/resumen_prov_pib_2025.csv")

ggsave(
  filename = "fig/comarcas_pct_65_mas.png",
  width = 8,
  height = 5,
  dpi = 300
)
