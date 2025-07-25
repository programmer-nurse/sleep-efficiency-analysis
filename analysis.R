# -----------------------------------------------------------------------------------
# PROYECTO: ¿Dormir bien importa?
# Autora: Cecilia Araya Ferreira
# Descripción: Análisis exploratorio de la eficiencia del sueño según hábitos de vida
# -----------------------------------------------------------------------------------

# PASO 1: Establecer directorio de trabajo
setwd("C:/Users/HOME/Documents/Estudio/proyectoR/proyectosueño")

# PASO 2: Cargar librerías
library(tidyverse)
library(janitor)   # Para limpiar nombres de columnas
library(ggplot2)

# PASO 3: Leer dataset, limpiar nombres y eliminar filas con NA
sleep_data <- read_csv("Sleep_Efficiency.csv") %>%
  clean_names() %>%           # Nombres en snake_case
  drop_na()                   # Eliminar filas con NA

# Verificar estructura del dataset limpio
glimpse(sleep_data)

# PASO 4: Transformar variables
sleep_data <- sleep_data %>%
  mutate(
    gender = factor(gender),
    smoking_status = factor(smoking_status),
    alcohol_consumption = as.numeric(alcohol_consumption),
    caffeine_consumption = as.numeric(caffeine_consumption)
  )

# PASO 5: Crear carpeta "plots" si no existe
if (!dir.exists("plots")) dir.create("plots")

# PASO 6: Gráficos --------------------------------------------------------------

# 1. Histograma de eficiencia del sueño
ggplot(sleep_data, aes(x = sleep_efficiency)) +
  geom_histogram(fill = "#69b3a2", color = "black", bins = 30) +
  labs(title = "Distribución de la eficiencia del sueño",
       x = "% Eficiencia", y = "Cantidad de personas") +
  theme_minimal()
ggsave("plots/1_hist_eficiencia.png")

# 2. Alcohol vs eficiencia
ggplot(sleep_data, aes(x = alcohol_consumption, y = sleep_efficiency)) +
  geom_point(alpha = 0.5, color = "darkblue") +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(title = "Relación entre alcohol y eficiencia del sueño",
       x = "Alcohol (24h)", y = "Eficiencia del sueño") +
  theme_minimal()
ggsave("plots/2_alcohol.png")

# 3. Fumar vs eficiencia
ggplot(sleep_data, aes(x = smoking_status, y = sleep_efficiency, fill = smoking_status)) +
  geom_boxplot() +
  labs(title = "¿Fumar afecta la eficiencia del sueño?",
       x = "Fumador", y = "Eficiencia del sueño") +
  theme_minimal()
ggsave("plots/3_fumar.png")

# PASO 7: Modelo de regresión ---------------------------------------------------

modelo <- lm(sleep_efficiency ~ alcohol_consumption + caffeine_consumption + 
               exercise_frequency + smoking_status, data = sleep_data)

summary(modelo)

#PASO 8: Subir al github
system('git init')
system('git remote add origin https://github.com/programmer-nurse/sleep-efficiency-analysis.git')
system('git branch -M main')
system('git add .')
system('git commit -m "Primer commit: análisis de eficiencia del sueño en R"')
system('git push -u origin main')

