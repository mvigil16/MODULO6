# ============================================================
#  t-SNE (t-Distributed Stochastic Neighbor Embedding)
# ============================================================

# ------librerìas
library(tidyverse)
library(ggplot2)
library(Rtsne)       # implementación de t-SNE en R
library(gridExtra)   

#----Carga de datos
load("data/water_potability_limpio.RData")
# X_scaled, X_sin_escalar, y, df_imputado

cat("Datos cargados\n")
cat("Dimensiones X_scaled:", dim(X_scaled), "\n")
cat("Clases en y:\n")
print(table(y))

# ----Eliminar duplicaods
# Rtsne() falla si existen filas duplicadas exactas
# check_duplicates = TRUE lo detecta, por eso los quitamos antes

duplicados <- duplicated(X_scaled)
cat("\nFilas duplicadas encontradas:", sum(duplicados), "\n")

X_tsne <- X_scaled[!duplicados, ]
y_tsne  <- y[!duplicados]

cat("Filas finales:", nrow(X_tsne), "\n")

# ----- t-SNE con tres valores de perplexity
# Perplexity controla el balance entre estructura local y global
# Regla general: entre 5 y 50, típico = 30
# Se exploran 3 valores para comparar

set.seed(42)   # reproducibilidad

cat("\nCalculando t-SNE con perplexity = 15 ...\n")
tsne_15 <- Rtsne(X_tsne, dims = 2, perplexity = 15,
                 max_iter = 1000, verbose = FALSE,
                 check_duplicates = FALSE)

cat("Calculando t-SNE con perplexity = 30 ...\n")
tsne_30 <- Rtsne(X_tsne, dims = 2, perplexity = 30,
                 max_iter = 1000, verbose = FALSE,
                 check_duplicates = FALSE)

cat("Calculando t-SNE con perplexity = 50 ...\n")
tsne_50 <- Rtsne(X_tsne, dims = 2, perplexity = 50,
                 max_iter = 1000, verbose = FALSE,
                 check_duplicates = FALSE)

cat(" t-SNE calculados\n")

#----Df de resultados
df_tsne15 <- data.frame(
  Dim1  = tsne_15$Y[, 1],
  Dim2  = tsne_15$Y[, 2],
  Clase = y_tsne,
  Perp  = "Perplexity = 15"
)

df_tsne30 <- data.frame(
  Dim1  = tsne_30$Y[, 1],
  Dim2  = tsne_30$Y[, 2],
  Clase = y_tsne,
  Perp  = "Perplexity = 30"
)

df_tsne50 <- data.frame(
  Dim1  = tsne_50$Y[, 1],
  Dim2  = tsne_50$Y[, 2],
  Clase = y_tsne,
  Perp  = "Perplexity = 50"
)

#Panel comparativo
df_todos <- bind_rows(df_tsne15, df_tsne30, df_tsne50) %>%
  mutate(Perp = factor(Perp,
                       levels = c("Perplexity = 15",
                                  "Perplexity = 30",
                                  "Perplexity = 50")))

# ----panel comparativo (3 perplexities) 
p_panel <- ggplot(df_todos, aes(x = Dim1, y = Dim2, color = Clase)) +
  geom_point(alpha = 0.4, size = 0.9) +
  facet_wrap(~ Perp, scales = "free", ncol = 3) +
  scale_color_manual(values = c("No potable" = "#EF5350",
                                "Potable"    = "#42A5F5")) +
  labs(title    = "t-SNE — Comparación de Perplexity",
       subtitle = "Cada panel usa un valor distinto de perplexity (15, 30, 50)",
       x = "Dimensión 1", y = "Dimensión 2", color = "Clase") +
  theme_minimal(base_size = 11) +
  theme(plot.title      = element_text(face = "bold", size = 13),
        plot.subtitle   = element_text(color = "gray50"),
        strip.text      = element_text(face = "bold"),
        legend.position = "bottom")

ggsave("outputs/figuras/tsne_comparacion.png",
       plot = p_panel, width = 13, height = 5, dpi = 150)
cat("✓ outputs/figuras/tsne_comparacion.png guardado\n")

# -----grafica principal (perplexity = 30) 
p_tsne30 <- ggplot(df_tsne30, aes(x = Dim1, y = Dim2, color = Clase)) +
  geom_point(alpha = 0.45, size = 1.2) +
  scale_color_manual(values = c("No potable" = "#EF5350",
                                "Potable"    = "#42A5F5")) +
  labs(title    = "t-SNE — Perplexity = 30 (recomendado)",
       subtitle = "Reducción a 2 dimensiones — Water Potability Dataset",
       x = "Dimensión 1 t-SNE",
       y = "Dimensión 2 t-SNE",
       color = "Clase") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray80") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray80") +
  theme_minimal(base_size = 12) +
  theme(plot.title      = element_text(face = "bold", size = 13),
        plot.subtitle   = element_text(color = "gray50"),
        legend.position = "bottom")

ggsave("outputs/figuras/tsne_perp30.png",
       plot = p_tsne30, width = 7, height = 6, dpi = 150)
cat("✓ outputs/figuras/tsne_perp30.png guardado\n")

# ---grafica con densidad
p_densidad <- ggplot(df_tsne30, aes(x = Dim1, y = Dim2, color = Clase)) +
  geom_point(alpha = 0.3, size = 0.9) +
  geom_density_2d(aes(color = Clase), linewidth = 0.5, alpha = 0.8) +
  scale_color_manual(values = c("No potable" = "#EF5350",
                                "Potable"    = "#42A5F5")) +
  labs(title    = "t-SNE — Densidad por clase (Perplexity = 30)",
       subtitle = "Las curvas muestran la concentración de cada clase",
       x = "Dimensión 1 t-SNE",
       y = "Dimensión 2 t-SNE",
       color = "Clase") +
  theme_minimal(base_size = 12) +
  theme(plot.title      = element_text(face = "bold", size = 13),
        plot.subtitle   = element_text(color = "gray50"),
        legend.position = "bottom")

ggsave("outputs/figuras/tsne_densidad.png",
       plot = p_densidad, width = 7, height = 6, dpi = 150)
cat("✓ outputs/figuras/tsne_densidad.png guardado\n")

# ----cordenadas de t-SNE
write.csv(df_tsne30,
          "outputs/tablas/tsne_coordenadas.csv",
          row.names = FALSE)
cat("✓ outputs/tablas/sne_coordenadas.csv guardado\n")

# -----Resultados
cat("\n")
cat("\nNota para el reporte:\n")
cat("→ t-SNE NO preserva distancias globales,\n")
cat("  solo estructura local entre vecinos.\n")
cat("→ Cambiar la seed cambia la forma del plot\n")
cat("  pero no la separabilidad entre clases.\n")