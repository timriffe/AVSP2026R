# 02_limpiar_eustat_poblacion_v7.R
#
# Limpia la tabla de población de Eustat y guarda un único fichero tidy,
# sin subtotales: una fila por municipio, sexo y grupo de edad.
#
# Inputs:
#   data/raw/xls0011427_c.xlsx
#   data/clean/lookup_municipios_comarcas.csv
#
# Output:
#   data/clean/pobmun.csv

library(tidyverse)
library(readxl)

file_pop <- "data/raw/xls0011427_c.xlsx"
file_lookup_mun <- "data/clean/lookup_municipios_comarcas.csv"
file_out <- "data/clean/pobmun.csv"

dir.create(dirname(file_out), recursive = TRUE, showWarnings = FALSE)

clean_chr <- function(x) {
  x |>
    as.character() |>
    str_replace_all("\\u00a0", " ") |>
    str_squish() |>
    na_if("")
}

normalizar_nombre <- function(x) {
  x |>
    clean_chr() |>
    str_to_lower() |>
    str_replace_all("\\.", "") |>
    str_replace_all("\\s*/\\s*", " / ") |>
    str_replace_all("\\s+", " ") |>
    str_squish()
}

normalizar_municipio <- function(x) {
  out <- normalizar_nombre(x)
  
  case_when(
    out == "arrankudiaga-zollo" ~ "arrankudiaga",
    out == "ribera baja / erriberabeitia" ~ "ribera baja / erribera beitia",
    TRUE ~ out
  )
}

fill_right <- function(x) {
  out <- as.character(x)
  last <- NA_character_
  
  for (i in seq_along(out)) {
    if (!is.na(out[i]) && out[i] != "") {
      last <- out[i]
    } else {
      out[i] <- last
    }
  }
  
  out
}

clean_age_label <- function(x) {
  x |>
    clean_chr() |>
    str_replace_all("0\\s*-\\s*19", "0-19") |>
    str_replace_all("20\\s*-\\s*64", "20-64") |>
    str_replace_all(">=\\s*65", "65+") |>
    str_to_lower()
}

clean_sex_label <- function(x) {
  x |>
    clean_chr() |>
    str_to_lower() |>
    str_replace_all("á", "a") |>
    str_replace_all("é", "e") |>
    str_replace_all("í", "i") |>
    str_replace_all("ó", "o") |>
    str_replace_all("ú", "u") |>
    str_replace_all("[^a-z0-9]+", "_") |>
    str_replace_all("^_|_$", "")
}

age_lower_bound <- function(x) {
  case_when(
    x == "0-19"  ~ 0L,
    x == "20-64" ~ 20L,
    x == "65+"   ~ 65L,
    TRUE ~ NA_integer_
  )
}

age_pretty <- function(x) {
  recode(
    x,
    "total" = "Total",
    "0-19"  = "0-19",
    "20-64" = "20-64",
    "65+"   = ">= 65"
  )
}

find_sex_row <- function(raw) {
  row_text <- apply(raw, 1, \(z) {
    z <- clean_chr(z)
    paste(z[!is.na(z)], collapse = "|")
  })
  
  sex_row <- which(str_detect(row_text, "Total.*Hombres.*Mujeres"))[1]
  
  if (is.na(sex_row)) {
    stop("No encuentro la fila de cabecera con Total, Hombres y Mujeres.")
  }
  
  sex_row
}

lookup_mun <- read_csv(file_lookup_mun, show_col_types = FALSE) |>
  mutate(municipio_norm = normalizar_municipio(municipio)) |>
  select(
    prov_cod, prov,
    comarca_cod, comarca_cod_full, comarca,
    municipio_cod, municipio_cod_full, municipio,
    municipio_norm
  )

raw <- read_excel(
  file_pop,
  sheet = 1,
  col_names = FALSE,
  .name_repair = "unique"
)

sex_row <- find_sex_row(raw)
age_row <- sex_row - 1L
data_start_row <- sex_row + 1L

header_age <- raw[age_row, , drop = TRUE] |>
  unlist(use.names = FALSE) |>
  fill_right()

header_sex <- raw[sex_row, , drop = TRUE] |>
  unlist(use.names = FALSE) |>
  as.character()

header_age[1] <- "ambito"

new_names <- map2_chr(header_age, header_sex, \(age, sex) {
  if (age == "ambito") {
    "ambito"
  } else {
    paste(clean_age_label(age), clean_sex_label(sex), sep = "__")
  }
})

new_names[1] <- "ambito"

pob_mun <- raw[data_start_row:nrow(raw), , drop = FALSE] |>
  set_names(new_names) |>
  mutate(ambito = clean_chr(ambito)) |>
  filter(!is.na(ambito), !str_detect(ambito, "^Fuente:")) |>
  mutate(
    tipo_ambito = case_when(
      ambito == "Territorios Históricos" ~ "territorio",
      ambito == "Comarcas" ~ "comarca",
      ambito == "Municipios" ~ "municipio",
      TRUE ~ NA_character_
    )
  ) |>
  fill(tipo_ambito, .direction = "down") |>
  filter(tipo_ambito == "municipio", ambito != "Municipios") |>
  mutate(municipio_norm = normalizar_municipio(ambito)) |>
  mutate(
    across(
      -c(ambito, tipo_ambito, municipio_norm),
      \(x) parse_number(
        as.character(x),
        locale = locale(decimal_mark = ",", grouping_mark = ".")
      )
    )
  ) |>
  pivot_longer(
    cols = -c(ambito, tipo_ambito, municipio_norm),
    names_to = c("edad_etiqueta", "genero"),
    names_sep = "__",
    values_to = "pob"
  ) |>
  mutate(
    genero = recode(
      genero,
      "total" = "Total",
      "hombres" = "Hombres",
      "mujeres" = "Mujeres"
    ),
    edad = age_lower_bound(edad_etiqueta),
    edad_etiqueta = age_pretty(edad_etiqueta)
  ) |>
  left_join(lookup_mun, by = "municipio_norm")

missing_mun <- pob_mun |>
  filter(is.na(municipio_cod)) |>
  distinct(ambito)

if (nrow(missing_mun) > 0) {
  stop(
    "Hay municipios sin emparejar en lookup_municipios_comarcas.csv: ",
    paste(missing_mun$ambito, collapse = ", ")
  )
}

pob_mun <- pob_mun |>
  select(
    prov_cod, prov,
    comarca_cod, comarca_cod_full, comarca,
    municipio_cod, municipio_cod_full, municipio,
    genero, edad, edad_etiqueta, pob
  ) |>
  arrange(prov_cod, comarca_cod, municipio_cod, genero, edad)

write_csv(pob_mun, file_out)
