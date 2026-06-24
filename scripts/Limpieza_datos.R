# ===================================================
# Preparaciòn de datos 
# Reducciòn de dimensiones
# Equipo 8.
#====================================================

#---------Librerìas y data

library(tidyverse)
library(dplyr)

df_raw <- read_csv("data/water_potability.csv")

cat("\n*****ESTRUCTURA DATASET*****\n")
glimpse (df_raw)
cat("\n************\n")
print(head(df_raw))
cat("\n***** DIMENSIONES *****\n")
cat("Filas:", nrow(df_raw), "| Columnas:", ncol(df_raw), "\n")

# -------------- Valores nulos

cat("\n***** NAs rn el DATA********\n")
na_resumen <- df_raw %>%
  summarise(across(everything(), ~ sum(is.na(.)))) %>%
  pivot_longer(everything(),
               names_to  = "variable",
               values_to = "n_nulos") %>%
  mutate(pct_nulos = round(n_nulos / nrow(df_raw) * 100, 2))

print(na_resumen)


# ------- Imputaciòn por mediana dentro de clases
#  (0 = no potable, 1 = potable)


df_imputado <- df_raw %>%
  group_by(Potability) %>%
  mutate(
    ph              = ifelse(is.na(ph),
                             median(ph,              na.rm = TRUE), ph),
    Sulfate         = ifelse(is.na(Sulfate),
                             median(Sulfate,         na.rm = TRUE), Sulfate),
    Trihalomethanes = ifelse(is.na(Trihalomethanes),
                             median(Trihalomethanes, na.rm = TRUE), Trihalomethanes)
  ) %>%
  ungroup()

cat("\n***** NAs al imputar ****\n")
print(colSums(is.na(df_imputado)))

# --------- Separacion 
#X (predictores) ,  Y (etiqueta) 
X_sin_escalar <- df_imputado %>%
  select(-Potability)

y <- df_imputado$Potability %>%
  factor(levels = c(0, 1),
         labels = c("No potable", "Potable"))

cat("\n*****Distribucon por clases*******\n")
print(table(y))
cat("Proporción:\n")
print(round(prop.table(table(y)) * 100, 1))


#------ estandarizaciòn 
# uso (scale)

X_scaled <- scale(X_sin_escalar)

cat("\n ---Verifiaciòn----\n")
cat("Medias (deben ser ≈ 0):\n")
print(round(colMeans(X_scaled), 4))

cat("Desviaciones estándar (≈ 1):\n")
print(round(apply(X_scaled, 2, sd), 4))

# ------- guardar objetos
save(X_scaled,        # matrix estandarizada  
     X_sin_escalar,   # tibble sin escalar     
     y,               # factor con clases      
     df_imputado,     # dataset  limpio
     file = "data/water_potability_limpio.RData")

cat("\n")
cat("Archivos generados")

