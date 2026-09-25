library(dplyr)

# 1. Crear el dataset de ejemplo sobre agresiones sexuales
Datos_Sexual <- data.frame(
  ID = 1:8,
  EDAD_VICTIMA = c(19, 25, 17, 31, 22, 16, 28, 20),
  EDAD_AGRESOR = c(34, 40, 22, 45, 29, 38, 50, NA),
  TIPO_AGRESION = c("Abuso", "Violación", "Abuso", "Violación", "Violación", "Abuso", "Violación", "Abuso"),
  N_AÑOS = c(1, NA, 3, 0.5, 2, NA, 5, 1) # Años de violencia previa sufrida
)

# ------------------------------------------------------------------------------
# APLICACIÓN DE LAS 5 FUNCIONES
# ------------------------------------------------------------------------------

# FILTER: Filtrar solo casos de mayorías de edad o violencia prolongada
casos_graves <- Datos_Sexual %>%
  filter(EDAD_VICTIMA >= 18 & TIPO_AGRESION == "Violación")

# MUTATE: Crear nueva columna (ej. Brecha de edad entre agresor y víctima)
Datos_Sexual <- Datos_Sexual %>%
  mutate(BRECHA_EDAD = EDAD_AGRESOR - EDAD_VICTIMA)

# COUNT: Contar la frecuencia de agresiones por tipo
frecuencia_tipo <- Datos_Sexual %>%
  count(TIPO_AGRESION, sort = TRUE)

# SUMMARIZE: Resumen estadístico (omitido NAs al igual que tu código base)
resumen_edades <- Datos_Sexual %>%
  summarize(
    media_edad_victima = mean(EDAD_VICTIMA, na.rm = TRUE),
    mediana_edad_agresor = median(EDAD_AGRESOR, na.rm = TRUE),
    promedio_años_violencia = mean(N_AÑOS, na.rm = TRUE)
  )

# ARRANGE: Ordenar los datos por la brecha de edad de mayor a menor
casos_ordenados <- Datos_Sexual %>%
  arrange(desc(BRECHA_EDAD))

# ------------------------------------------------------------------------------
# PIPELINE INTEGRADO (Las 5 funciones juntas en un solo flujo)
# ------------------------------------------------------------------------------

reporte_consolidado <- Datos_Sexual %>%
  # 1. Filtramos registros válidos para el cálculo
  filter(!is.na(N_AÑOS)) %>%
  # 2. Creamos una categoría de riesgo según los años
  mutate(RIESGO = if_else(N_AÑOS > 2, "Prolongado", "Reciente")) %>%
  # 3. Agrupamos por tipo y riesgo para calcular estadísticas
  group_by(TIPO_AGRESION, RIESGO) %>%
  summarize(
    total_casos = n(),
    promedio_edad_victima = mean(EDAD_VICTIMA),
    .groups = "drop"
  ) %>%
  # 4. Ordenamos descendente por el promedio de edad
  arrange(desc(promedio_edad_victima))

print(reporte_consolidado)



