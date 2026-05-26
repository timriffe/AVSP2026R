# 1. Leer los datos
poblacion <- readr::read_csv("data/clean/poblacion_euskadi_2025.csv")

# 2. Mirar los datos
glimpse(poblacion)
View(poblacion)

# 3. Crear variables nuevas
poblacion <- poblacion |>
  mutate(pct_65_mas = edad_65_mas / total * 100)

# 4. Ordenar y filtrar
poblacion |>
  filter(tipo_ambito == "territorio") |>
  arrange(desc(pct_65_mas))

# 5. Resumir
poblacion |>
  group_by(tipo_ambito) |>
  summarise(
    n = n(),
    poblacion_total = sum(total),
    pct_65_mas_media = mean(pct_65_mas)
  )

# 6. Graficar
poblacion |>
  filter(tipo_ambito == "territorio") |>
  ggplot(aes(x = ambito, y = pct_65_mas)) +
  geom_col() +
  labs(
    title = "Porcentaje de población de 65 años o más",
    subtitle = "Territorios históricos, C.A. de Euskadi, 2025",
    x = NULL,
    y = "%"
  )