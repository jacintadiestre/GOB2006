# GOB2006: Curso Evaluación de impacto de las políticas públicas. 
# Evaluación de Impacto en la Práctica
# Estudio de caso HISP. Paso a paso: 


# Limpiar el ambiente
rm(list = ls())

# Cargar librerías necesarias
library(tidyverse)
library(haven)      # Para leer archivos .dta
library(sandwich)   # Para errores estándar robustos
library(lmtest)     # Para pruebas de hipótesis
library(MatchIt)    # Para matching
library(pwr)        # Para cálculos de poder estadístico

# Configurar directorio de trabajo si es necesario
# setwd("tu_directorio")

# Función auxiliar para verificar columnas
check_columns <- function(data, required_cols) {
  missing_cols <- setdiff(required_cols, names(data))
  if (length(missing_cols) > 0) {
    stop(paste("Columnas faltantes:", paste(missing_cols, collapse = ", ")))
  }
}
# Abrir el conjunto de datos limpio
data <- read_dta("evaluation.dta")

# Verificar las columnas necesarias
required_columns <- c(
  "health_expenditures",
  "round",
  "treatment_locality",
  "eligible",
  "enrolled",  # Verifica si este es el nombre correcto
  "household_identifier",
  "age_hh",
  "age_sp",
  "educ_hh",
  "educ_sp",
  "female_hh",
  "indigenous",
  "hhsize",
  "dirtfloor",
  "bathroom",
  "land",
  "hospital_distance"
)

# Verificar columnas antes de proceder
check_columns(data, required_columns)

# Mostrar estructura de los datos
str(data)
summary(data)

# Definir variables globales
variables1 <- c("health_expenditures", "age_hh", "age_sp", "educ_hh", "educ_sp", 
                "female_hh", "indigenous", "hhsize", "dirtfloor", "bathroom", 
                "land", "hospital_distance")

controls <- c("age_hh", "age_sp", "educ_hh", "educ_sp", "female_hh", 
              "indigenous", "hhsize", "dirtfloor", "bathroom", "land", 
              "hospital_distance")

controls1 <- c("age_hh", "age_sp")
controls2 <- c("hhsize", "dirtfloor", "bathroom", "land", "hospital_distance")

# Caso 1: Antes y Después
# Q1
t.test(health_expenditures ~ round, 
       data = subset(data, eligible == 1 & treatment_locality == 1))

# Caso 2: Tratamientos Auto-seleccionados
# Q1
with(data, t.test(health_expenditures[round == 1 & treatment_locality == 1], 
                  conf.level = 0.95))

# Q2
for(var in controls) {
  print(paste("Variable:", var))
  print(t.test(data[[var]] ~ data$enrolled_rp, 
               data = subset(data, round == 1 & treatment_locality == 1)))
}

# Q3
model1 <- lm(health_expenditures ~ enrolled_rp, 
             data = subset(data, round == 1 & treatment_locality == 1))
model2 <- lm(paste("health_expenditures ~ enrolled_rp +", 
                   paste(controls1, collapse = " + ")),
             data = subset(data, round == 1 & treatment_locality == 1))

# Caso 3: Asignación Aleatoria
# Q1
for(var in c("health_expenditures", controls)) {
  print(paste("Variable:", var))
  print(t.test(data[[var]] ~ data$treatment_locality, 
               data = subset(data, round == 0 & eligible == 1)))
}

# Q2
t.test(health_expenditures ~ treatment_locality, 
       data = subset(data, eligible == 1 & round == 1))

# Q3
model3 <- lm(health_expenditures ~ treatment_locality, 
             data = subset(data, eligible == 1 & round == 1))
model4 <- lm(paste("health_expenditures ~ treatment_locality +", 
                   paste(controls, collapse = " + ")),
             data = subset(data, eligible == 1 & round == 1))

# Caso 5: RDD
# Normalizar el índice de pobreza en 0
data <- data %>%
  mutate(
    poverty_index_left = ifelse(poverty_index <= 58, poverty_index - 58, 0),
    poverty_index_right = ifelse(poverty_index > 58, poverty_index - 58, 0)
  )

# Q1
rdd_model <- lm(health_expenditures ~ poverty_index_left + poverty_index_right + eligible,
                data = subset(data, round == 1 & treatment_locality == 1))

# Caso 6: Diferencias en Diferencias
# Q1
data <- data %>%
  arrange(household_identifier, round) %>%
  group_by(household_identifier) %>%
  mutate(delta_he = health_expenditures - lead(health_expenditures)) %>%
  ungroup()

# Q2
t.test(delta_he ~ enrolled_rp, 
       data = subset(data, treatment_locality == 1))

# Q3
data$enrolled_round <- data$enrolled_rp * data$round

did_model <- lm(paste("health_expenditures ~ enrolled_round + round + enrolled_rp +",
                      paste(controls, collapse = " + ")),
                data = subset(data, treatment_locality == 1))

# Caso 7: Matching
# Q1
match_model <- glm(paste("enrolled_rp ~", paste(controls, collapse = " + ")),
                   family = binomial(),
                   data = subset(data, round == 0))

data$pscore <- predict(match_model, type = "response")

# Q2
data$quintiles <- cut(data$pscore, 
                      breaks = quantile(data$pscore, probs = seq(0, 1, 0.2), na.rm = TRUE),
                      labels = 1:5)

# Guardar resultados
# write.csv(data, "resultados_analisis.csv")

# Fin del script
