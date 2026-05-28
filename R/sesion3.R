# Introducción a R con datos sociales
# Sesión 3 - Visualización con ggplot2 y tasas de fecundidad
# Asociación Vasca de Sociología y Ciencia Política

library(tidyverse)
library(janitor)
library(scales)
library(colorspace)

# -----------------------------------------------
# 1. Datos limpios
# -----------------------------------------------
# he limpiado y armonizado estos datos antes
nacimientos <- read_csv("data/clean/nacimientos.csv", 
                        show_col_types = FALSE)
denom <- read_csv("data/clean/denominadores_fecundidad.csv", 
                  show_col_types = FALSE)

glimpse(nacimientos)
glimpse(denom)

# Nacimientos: provincia x edad de la madre x año
# Denominadores: provincia x edad x año x población femenina


# En nacimientos, edad viene como texto. Hay también filas "Total".
nacimientos
nac_edad <- nacimientos |>
  filter(edad != "Total") |>
  mutate(edad = parse_number(edad)) |>
  select(prov_cod, prov, periodo, edad, nac)

denom_edad <- denom |>
  select(prov_cod, periodo, edad, mujeres = pob)

# EJERCICIO 1 ---------------------------------------------------------------
# Mira los años disponibles en nac_edad y denom_edad.
# Pistas: distinct(periodo), arrange(periodo), range(periodo)

nac_edad |> pull(periodo) |> range()
denom_edad |> pull(periodo) |> range()
nac_edad |> 
  filter(edad == 20,periodo==2000,prov_cod=="01")
# -------------------------------------------------------
# 2. Calcular tasas por edad: ASFR
# -------------------------------------------------------

fec_edad <- nac_edad |>
  left_join(
    denom_edad,
    by = c("prov_cod", "periodo", "edad")
  ) |>
  mutate(
    asfr = nac / mujeres,
    asfr_1000 = 1000 * asfr
  )


fec_edad |>
  select(prov, periodo, edad, nac, mujeres, asfr_1000) |>
  head()

# ¿hay nacimientos sin denominador?

fec_edad |>
  filter(is.na(mujeres)) |>
  count(prov, periodo)

# EJERCICIO 2 ------------------------------------------
# ¿Cuál fue la tasa por 1.000 mujeres a los 30 años en Bizkaia en 2024?

# ------------------------------------------------------
# 3. ISF y edad media a la maternidad
# ------------------------------------------------------

isf_prov <- fec_edad |>
  group_by(prov_cod, prov, periodo) |>
  summarise(
    nacimientos = sum(nac, na.rm = TRUE),
    mujeres = sum(mujeres, na.rm = TRUE),
    isf = sum(asfr, na.rm = TRUE),
    emm = 0.5 + sum(edad * asfr) / isf,
    .groups = "drop"
  )

isf_prov |>
  arrange(prov, periodo) |>
  head()

# Euskadi total como suma de las tres provincias.
# No es la media de las provincias; 
# se suman nacimientos y mujeres por edad.

fec_euskadi <- fec_edad |>
  group_by(periodo, edad) |>
  summarise(
    prov_cod = "PV",
    prov = "C.A. de Euskadi",
    nac = sum(nac, na.rm = TRUE),
    mujeres = sum(mujeres, na.rm = TRUE),
    asfr = nac / mujeres,
    asfr_1000 = 1000 * asfr,
    .groups = "drop"
  )

isf_euskadi <- fec_euskadi |>
  group_by(prov_cod, prov, periodo) |>
  summarise(
    nacimientos = sum(nac, na.rm = TRUE),
    mujeres = sum(mujeres, na.rm = TRUE),
    isf = sum(asfr, na.rm = TRUE),
    emm = 0.5 + sum(edad * asfr) / isf,
    .groups = "drop"
  )

isf_todo <- bind_rows(isf_prov, isf_euskadi)

# EJERCICIO 3 ---------------------------------------------------------------
# ¿En qué año tuvo cada provincia su ISF más alto?
# Pistas: group_by(prov), slice_max(isf, n = 1)
# o sino group_by(prov) con filter(isf == max(isf))

# -------------------------------------------
# 4. Primer ggplot: líneas
# -------------------------------------------

isf_prov |>
  ggplot(aes(x = periodo, y = isf, color = prov)) +
  geom_line(linewidth = 1) +
  labs(
    title = "Índice sintético de fecundidad",
    subtitle = "Territorios históricos de la C.A. de Euskadi",
    x = NULL,
    y = "Hijos/as por mujer",
    color = "Territorio"
  )

# La estructura básica:
# ggplot(datos, aes(...)) + geom_algo() + labs(...)

# EJERCICIO 4 -----------------------------------------------
# Haz el mismo gráfico, pero usando la edad media a la maternidad (emm).

# -----------------------------------------------------------
# 5. Capas: líneas provinciales + línea destacada de Euskadi
# -----------------------------------------------------------
ggplot(mapping = aes(x = periodo, y = isf)) +
  geom_line(
    data = isf_prov,
    aes(color = prov),
    linewidth = 0.9,
    alpha = 0.8
  ) +
  geom_line(
    data = isf_euskadi,
    color = "black",
    linewidth = 1.4
  ) +
  labs(
    title = "Índice sintético de fecundidad",
    subtitle = "La línea negra resume el conjunto de la C.A. de Euskadi",
    x = NULL,
    y = "Hijos/as por mujer",
    color = "Territorio"
  )

# Idea clave:
# - aes(color = prov) mapea una variable a un color.
# - color = "black" fija un color manualmente.

# ----------------------------------------------------------
# 6. Escalas y etiquetas
# ----------------------------------------------------------

ggplot() +
  geom_line(
    data = isf_prov,
    aes(x = periodo, y = isf, color = prov),
    linewidth = 0.9
  ) +
  geom_line(
    data = isf_euskadi,
    aes(x = periodo, y = isf),
    color = "black",
    linewidth = 1.4
  ) +
  scale_x_continuous(
    breaks = seq(1995, 2025, by = 5)
  ) +
  scale_y_continuous(
    labels = label_number(decimal.mark = ",", accuracy = 0.1)
  ) +
  scale_color_discrete_qualitative(palette = "Dark 3") +
  labs(
    title = "ISF en el CAPV",
    subtitle = "Índice sintético de fecundidad, 1996-2024",
    x = NULL,
    y = "Hijos/as por mujer",
    color = "Territorio",
    caption = "Fuente: elaboración propia a partir de Eustat/INE."
  ) +
  theme_minimal()

# EJERCICIO 5 -------------------------------------------------
# Cambia el tema: prueba theme_classic(), theme_bw() o theme_light().
# ¿Cuál se lee mejor en una diapositiva?

# -------------------------------------------------------------
# 7. Superficie edad-año con geom_tile()
# -------------------------------------------------------------

fec_euskadi |>
  ggplot(aes(x = periodo, y = edad, fill = asfr_1000)) +
  geom_tile() +
  coord_equal() +
  scale_fill_continuous_sequential(
    palette = "YlOrRd",
    name = "Tasa por\n1.000 mujeres"
  ) +
  labs(
    title = "Tasas de fecundidad por edad",
    subtitle = "C.A. de Euskadi",
    x = NULL,
    y = "Edad"
  ) +
  theme_minimal()

# En geom_tile():
# - x e y son posiciones.
# - fill es color de relleno, normalmente una variable continua.

# EJERCICIO 6 ---------------------------------------------------
# Haz la misma superficie para Bizkaia.
# Pista: empieza con fec_edad |> filter(prov == "Bizkaia")

# ---------------------------------------------------------------
# 8. Añadir una capa: edad media sobre la superficie
# ---------------------------------------------------------------
fec_euskadi |>
  ggplot(aes(x = periodo, y = edad, fill = asfr_1000)) +
  geom_tile() +
  coord_equal() +
  geom_line(
    data = isf_euskadi,
    aes(x = periodo, y = emm),
    inherit.aes = FALSE,
    color = "black",
    linewidth = 1.2
  ) +
  scale_fill_continuous_sequential(
    palette = "YlOrRd",
    name = "Tasa por\n1.000 mujeres"
  ) +
  labs(
    title = "Tasas de fecundidad por edad y edad media",
    subtitle = "C.A. de Euskadi",
    x = NULL,
    y = "Edad"
  ) +
  theme_minimal()

# EJERCICIO 7 ---------------------------------------------------------------
# Cambia color = "black" por otro color fijo.
# Después prueba linewidth = 0.5 y linewidth = 2.
# ¿Qué cambia? ¿Qué es dato y qué es diseño?

# -------------------------------------------------------
# 9. Facetas: una superficie por provincia
# -------------------------------------------------------

fec_edad |>
  ggplot(aes(x = periodo, y = edad, fill = asfr_1000)) +
  geom_tile() +
  coord_equal() +
  facet_wrap(~ prov) +
  scale_fill_continuous_sequential(
    palette = "YlOrRd",
    name = "Tasa por\n1.000 mujeres"
  ) +
  labs(
    title = "Tasas de fecundidad por edad",
    subtitle = "Territorios históricos",
    x = NULL,
    y = "Edad"
  ) +
  theme_minimal()

# facet_wrap() repite el mismo gráfico para grupos.

# EJERCICIO 8 ---------------------------------------------
# Prueba facet_wrap(~ prov, ncol = 1).
# ¿Qué formato se lee mejor?

# ---------------------------------------------------------
# 10. Puntos: relación entre ISF y edad media
# ---------------------------------------------------------

isf_todo |>
  filter(prov_cod != "PV") |>
  ggplot(aes(x = emm, y = isf, color = prov)) +
  geom_point(alpha = 0.8, size = 2) +
  geom_smooth(se = FALSE, linewidth = 1) +
  scale_color_discrete_qualitative(palette = "Dark 3") +
  labs(
    title = "Fecundidad y calendario",
    subtitle = "Cada punto es un territorio-año",
    x = "Edad media a la maternidad",
    y = "ISF",
    color = "Territorio"
  ) +
  theme_minimal()

# EJERCICIO 9 ---------------------------------------------
# Cambia geom_point() para mapear size = nacimientos.
# Pista: aes(x = emm, y = isf, color = prov, size = nacimientos)

# ----------------------------------------------------------
# 11. Una mini historia visual
# ----------------------------------------------------------

ultimo_ano <- max(isf_todo$periodo, na.rm = TRUE)

isf_todo |>
  filter(periodo == ultimo_ano) |>
  arrange(desc(isf)) |>
  ggplot(aes(x = reorder(prov, isf), y = isf)) +
  geom_col() +
  coord_flip() +
  labs(
    title = paste("ISF en", ultimo_ano),
    subtitle = "C.A. de Euskadi y territorios históricos",
    x = NULL,
    y = "Hijos/as por mujer"
  ) +
  theme_minimal()

# EJERCICIO 10 -----------------------------------------
# Haz el mismo gráfico para 2008.
# ¿Qué cambia?

# ------------------------------------------------------
# 12. Guardar tablas y figuras
# ------------------------------------------------------

dir.create("fig", showWarnings = FALSE)
dir.create("data/clean", showWarnings = FALSE)

write_csv(fec_edad, "data/clean/fecundidad_asfr_prov.csv")
write_csv(isf_todo, "data/clean/fecundidad_isf_prov_euskadi.csv")

grafico_isf <- ggplot() +
  geom_line(
    data = isf_prov,
    aes(x = periodo, y = isf, color = prov),
    linewidth = 0.9
  ) +
  geom_line(
    data = isf_euskadi,
    aes(x = periodo, y = isf),
    color = "black",
    linewidth = 1.4
  ) +
  scale_color_discrete_qualitative(palette = "Dark 3") +
  labs(
    title = "Índice sintético de fecundidad",
    subtitle = "C.A. de Euskadi y territorios históricos",
    x = NULL,
    y = "Hijos/as por mujer",
    color = "Territorio"
  ) +
  theme_minimal()

grafico_isf

# no usamos jpeg para graficos estadisticos casi nunca
ggsave(
  filename = "fig/isf_euskadi_territorios.png",
  plot = grafico_isf,
  width = 8,
  height = 5,
  dpi = 300
)
ggsave(
  filename = "fig/isf_euskadi_territorios.pdf",
  plot = grafico_isf,
  width = 18,
  height = 12,
  units = "cm"
)

# fin
