# https://www.ine.es/jaxiT3/Tabla.htm?t=56934
library(tidyverse)

pop<-read_tsv("data/raw/56945.csv",
         locale = locale(
           encoding = "UTF-8",
           decimal_mark = ",",
           grouping_mark = "."
         ),
         col_types = cols(
           `Edad simple` = col_character(),
           Sexo = col_character(),
           Periodo = col_character(),
           Total = col_number(),
           Provincias = col_character()
           )
         )

denominadores <-
  pop |>
  janitor::clean_names() |> 
  filter(grepl("julio",periodo),
         edad_simple != "Todas las edades",
         sexo == "Mujeres") |> 
  mutate(edad = parse_number(edad_simple),
         periodo = stringr::word(periodo, -1) |> 
                     readr::parse_number()
         ) |> 
  select(prov = provincias, 
         periodo, 
         edad, 
         pob = total) |> 
  mutate(prov_cod = substr(prov,1,2))

denominadores |> write_csv("data/clean/denominadores_fecundidad.csv")
# (tiene mas años y mas provincias)

# processar nacimientos
nac <-read_csv("data/raw/enac01_20260528-104441.csv",
         locale = locale(
           encoding = "Latin1",
           decimal_mark = ",",
           grouping_mark = "."
         ),skip=2) |> 
  janitor::clean_names() |> 
  rename(prov = territorio_historico,
         edad = edad_madre,
         nac = total) |> 
  mutate(prov_cod = case_when(
    grepl("Ara", prov)~"01",
    grepl("Gip", prov)~"20",
    grepl("Biz", prov)~"48")
    ) |> 
  write_csv("data/clean/nacimientos.csv")


