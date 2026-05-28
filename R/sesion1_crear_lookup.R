# Inputs esperados:
#   data/raw/MunicipiosCAE_2025_19_02.xlsx
#   data/raw/Comarcas_de_la_C.A._de_Euskadi.xlsx
#
# Outputs:
#   data/clean/lookup_municipios_comarcas.csv
#   data/clean/lookup_comarcas.csv
#   data/clean/lookup_provincias.csv

library(tidyverse)
library(readxl)

# ----------------------------------------
# 0. urls, paths
# -----------------------------------------
# https://es.eustat.eus/estadisticas/opt_7/id_22070/clasificaciones.html
path_municipios <- "data/raw/MunicipiosCAE_2025_19_02.xlsx"
# https://es.eustat.eus/estadisticas/opt_7/id_22063/clasificaciones.html
path_comarcas   <- "data/raw/Comarcas_de_la_C.A._de_Euskadi.xlsx"

out_dir <- "data/clean"
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

# ------------------------------------------
# 1. Pequeñas funciones auxiliares
# -------------------------------------------

clean_chr <- function(x) {
  x |>
    as.character() |>
    str_squish() |>
    na_if("")
}

# Para nombres que se usarán en joins de diagnóstico. No sustituye a los códigos.
normalizar_nombre <- function(x) {
  x |>
    clean_chr() |>
    str_replace_all("\\s*/\\s*", " / ") |>
    str_replace_all("\\s+", " ")
}

# ---------------------------------------------
# 2. Territorios históricos / provincias
# ---------------------------------------------

lookup_provincias <- tibble(
  prov_cod = c("01", "20", "48"),
  prov = c("Araba/Álava", "Gipuzkoa", "Bizkaia")
)

# ---------------------------------------------
# 3. Comarcas
# ---------------------------------------------

lookup_comarcas <- read_excel(path_comarcas, sheet = 1, col_types = "text") |>
  transmute(
    prov_cod    = clean_chr(`Código Provincia`),
    comarca_cod = clean_chr(`Código Comarca`),
    comarca_es  = clean_chr(`Nombre español`),
    comarca_eu  = clean_chr(`Nombre euskera`),
    comarca     = comarca_es,
    comarca_cod_full = paste0(prov_cod, comarca_cod)
  ) |>
  filter(
    str_detect(prov_cod, "^\\d{2}$"),
    str_detect(comarca_cod, "^\\d{2}$")
  ) |>
  left_join(lookup_provincias, by = "prov_cod") |>
  select(
    prov_cod, prov,
    comarca_cod, comarca_cod_full,
    comarca, comarca_es, comarca_eu
  ) |>
  arrange(prov_cod, comarca_cod)

# ------------------------------------
# 4. Municipios
# ------------------------------------
#
# Este fichero tiene varias filas de título antes de la tabla. Leemos sin nombres
# de columnas, detectamos las filas reales por el patrón de códigos, y luego
# renombramos.

raw_municipios <- read_excel(
  path_municipios,
  sheet = 1,
  col_names = FALSE,
  col_types = "text"
)

lookup_municipios <- raw_municipios |>
  select(
    prov_cod      = 1,
    comarca_cod   = 2,
    municipio_cod = 3,
    municipio     = 4
  ) |>
  mutate(across(everything(), clean_chr)) |>
  filter(
    str_detect(prov_cod, "^\\d{2}$"),
    str_detect(comarca_cod, "^\\d{2}$"),
    str_detect(municipio_cod, "^\\d{3}$"),
    !is.na(municipio)
  ) |>
  mutate(
    comarca_cod_full = paste0(prov_cod, comarca_cod),
    municipio_cod_full = paste0(prov_cod, municipio_cod),
    municipio_norm = normalizar_nombre(municipio)
  ) |>
  left_join(lookup_provincias, by = "prov_cod") |>
  left_join(
    lookup_comarcas |>
      select(
        prov_cod, comarca_cod,
        comarca_cod_full,
        comarca, comarca_es, comarca_eu
      ),
    by = c("prov_cod", "comarca_cod", "comarca_cod_full")
  ) |>
  select(
    prov_cod, prov,
    comarca_cod, comarca_cod_full, comarca, comarca_es, comarca_eu,
    municipio_cod, municipio_cod_full,
    municipio, municipio_norm
  ) |>
  arrange(prov_cod, comarca_cod, municipio)

View(lookup_municipios)
# --------------------------
# 5. Escribir outputs
# --------------------------

write_csv(
  lookup_provincias,
  file.path(out_dir, "lookup_provincias.csv")
)

write_csv(
  lookup_comarcas,
  file.path(out_dir, "lookup_comarcas.csv")
)

write_csv(
  lookup_municipios,
  file.path(out_dir, "lookup_municipios_comarcas.csv")
)

