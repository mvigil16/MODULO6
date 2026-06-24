#=====================================
# Análisis descriptivo
#=====================================

# ----- Librerías
library(tidyverse)
library(ggplot2)
library(corrplot)
library(gridExtra)

# ----- Carga de base limpia 
load("data/water_potability_limpio.RData")

# ----- Crear carpeta de salidas si no existe
dir.create("outputs/figuras", recursive = TRUE, showWarnings = FALSE)

cat("Datos cargados correctamente\n")
cat("Dimensiones X_scaled:", dim(X_scaled), "\n")
cat("Clases en y:\n")
print(table(y))

# ---- Estadísticas por clases
cat("\n*** Medias por clase ******\n")
df_imputado %>%
  group_by(Potability) %>%
  summarise(across(where(is.numeric), ~ round(mean(.), 3))) %>%
  print()

cat("\n***** Desviación estándar por clase ******\n")
df_imputado %>%
  group_by(Potability) %>%
  summarise(across(where(is.numeric), ~ round(sd(.), 3))) %>%
  print()

cat("\n**** Resumen ****\n")
summary(df_imputado)

# ------ BOXPLOTS 
df_largo <- df_imputado %>%
  mutate(Clase = factor(Potability,
                        levels = c(0, 1),
                        labels = c("No potable", "Potable"))) %>%
  select(-Potability) %>%
  pivot_longer(-Clase,
               names_to  = "Variable",
               values_to = "Valor")

p_boxplot <- ggplot(df_largo, aes(x = Clase, y = Valor, fill = Clase)) +
  geom_boxplot(alpha = 0.7, outlier.size = 0.5, outlier.alpha = 0.3) +
  facet_wrap(~ Variable, scales = "free_y", ncol = 3) +
  scale_fill_manual(values = c("No potable" = "#EF5350",
                               "Potable"    = "#42A5F5")) +
  labs(title    = "Distribución de variables por potabilidad",
       subtitle = "Water Potability",
       x = NULL, y = "Valor", fill = "Clase") +
  theme_minimal(base_size = 11) +
  theme(
    plot.title      = element_text(face = "bold", size = 13),
    plot.subtitle   = element_text(color = "gray50", size = 10),
    strip.text      = element_text(face = "bold"),
    legend.position = "bottom"
  )

ggsave("outputs/figuras/boxplots.png",
       plot = p_boxplot, width = 10, height = 8, dpi = 150)

cat("outputs/figuras/boxplots.png guardado\n")

# ---- Histogramas
p_hist <- ggplot(df_largo, aes(x = Valor, fill = Clase)) +
  geom_histogram(alpha = 0.6, bins = 30, position = "identity") +
  facet_wrap(~ Variable, scales = "free", ncol = 3) +
  scale_fill_manual(values = c("No potable" = "#EF5350",
                               "Potable"    = "#42A5F5")) +
  labs(title    = "Histogramas por variable y clase",
       subtitle = "Water Potability",
       x = "Valor", y = "Frecuencia", fill = "Clase") +
  theme_minimal(base_size = 11) +
  theme(
    plot.title      = element_text(face = "bold", size = 13),
    plot.subtitle   = element_text(color = "gray50", size = 10),
    strip.text      = element_text(face = "bold"),
    legend.position = "bottom"
  )

ggsave("outputs/figuras/histogramas.png",
       plot = p_hist, width = 10, height = 8, dpi = 150)

cat("outputs/figuras/histogramas.png guardado\n")

# ---- Mapa de correlación
cor_matrix <- cor(X_sin_escalar)

png("outputs/figuras/correlaciones.png",
    width = 800, height = 700, res = 120)

corrplot(cor_matrix,
         method      = "color",
         type        = "upper",
         addCoef.col = "black",
         number.cex  = 0.65,
         tl.col      = "black",
         tl.srt      = 45,
         col         = colorRampPalette(c("#EF5350", "white", "#42A5F5"))(200),
         title       = "Mapa de correlaciones — Water Potability",
         mar         = c(0, 0, 2, 0))

dev.off()

cat("outputs/figuras/correlaciones.png guardado\n")

# ----- Gráfica de proporción
df_clases <- as.data.frame(table(y)) %>%
  rename(Clase = y, n = Freq) %>%
  mutate(pct = round(n / sum(n) * 100, 1))

p_clases <- ggplot(df_clases, aes(x = Clase, y = n, fill = Clase)) +
  geom_col(width = 0.5, alpha = 0.85) +
  geom_text(aes(label = paste0(pct, "%\n(n=", n, ")")),
            vjust = -0.3, size = 4, fontface = "bold") +
  scale_fill_manual(values = c("No potable" = "#EF5350",
                               "Potable"    = "#42A5F5")) +
  scale_y_continuous(limits = c(0, max(df_clases$n) * 1.2)) +
  labs(title    = "Distribución de clases — Variable objetivo",
       subtitle = "Desbalance entre agua no potable y potable",
       x = NULL, y = "Número de muestras") +
  theme_minimal(base_size = 12) +
  theme(
    plot.title      = element_text(face = "bold", size = 13),
    plot.subtitle   = element_text(color = "gray50"),
    legend.position = "none"
  )

ggsave("outputs/figuras/clases.png",
       plot = p_clases, width = 6, height = 5, dpi = 150)

cat("outputs/figuras/clases.png guardado\n")
