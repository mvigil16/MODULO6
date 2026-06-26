# ============================================================
# Análisis de Componentes Principales (PCA)
# ============================================================

# -----librerìas
library(tidyverse)
library(ggplot2)
library(factoextra)  
library(gridExtra)    

# ------cargad de datos
load("data/water_potability_limpio.RData")
# X_scaled, X_sin_escalar, y, df_imputado

cat("Datos cargados\n")
cat("Dimensiones X_scaled:", dim(X_scaled), "\n")

# ------Calcular PCA 

pca_result <- prcomp(X_scaled, scale. = FALSE)

cat("\n****** RESUMEN PCA *****\n")
summary(pca_result)

#-------Varianza
var_explicada    <- pca_result$sdev^2
prop_var         <- var_explicada / sum(var_explicada)
prop_var_acum    <- cumsum(prop_var)

tabla_varianza <- data.frame(
  PC            = paste0("PC", 1:9),
  Varianza      = round(var_explicada, 4),
  Prop_Var      = round(prop_var * 100, 2),
  Prop_Acum     = round(prop_var_acum * 100, 2)
)

cat("\n*****Tabla de varianza ******\n")
print(tabla_varianza)
# Guardar tabla
write.csv(tabla_varianza,
          "outputs/tablas/varianza_detalle.csv",
          row.names = FALSE)
cat("guardado\n")

# ----scree plot
p_scree <- fviz_eig(pca_result,
                    addlabels = TRUE,
                    ylim      = c(0, 20),
                    barfill   = "#42A5F5",
                    barcolor  = "#1565C0",
                    linecolor = "#E53935") +
  labs(title    = "Scree Plot — Varianza por componente",
       subtitle = "Water Potability Dataset",
       x = "Componente Principal",
       y = "% Varianza explicada") +
  theme_minimal(base_size = 12) +
  theme(plot.title    = element_text(face = "bold", size = 13),
        plot.subtitle = element_text(color = "gray50"))

ggsave("outputs/figuras/scree_plot.png",
       plot = p_scree, width = 8, height = 5, dpi = 150)
cat("scree_plot.png guardado\n")

# ----- Biplot PC1 / PC2
# Preparaciòn data frame con coordenadas PCA y etiqueta de clase
df_pca <- as.data.frame(pca_result$x[, 1:2])
df_pca$Clase <- y

p_biplot_manual <- ggplot(df_pca, aes(x = PC1, y = PC2, color = Clase)) +
  geom_point(alpha = 0.4, size = 1.2) +
  scale_color_manual(values = c("No potable" = "#EF5350",
                                "Potable"    = "#42A5F5")) +
  labs(title    = "PCA — PC1 vs PC2",
       subtitle = paste0("PC1: ", round(prop_var[1]*100, 1),
                         "%  |  PC2: ", round(prop_var[2]*100, 1), "%"),
       x = paste0("PC1 (", round(prop_var[1]*100, 1), "%)"),
       y = paste0("PC2 (", round(prop_var[2]*100, 1), "%)"),
       color = "Clase") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray70") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray70") +
  theme_minimal(base_size = 12) +
  theme(plot.title      = element_text(face = "bold", size = 13),
        plot.subtitle   = element_text(color = "gray50"),
        legend.position = "bottom")

ggsave("outputs/figuras/biplot_PC1_PC2.png",
       plot = p_biplot_manual, width = 7, height = 6, dpi = 150)
cat("biplot_PC1_PC2.png guardado\n")

#-----biplot con flechas
p_biplot_fviz <- fviz_pca_biplot(
  pca_result,
  geom.ind     = "point",
  col.ind      = y,
  palette      = c("#EF5350", "#42A5F5"),
  alpha.ind    = 0.4,
  col.var      = "#FFB300",
  repel        = TRUE,
  legend.title = "Clase"
) +
  labs(title    = "Biplot PCA — Variables y observaciones",
       subtitle = "Flechas = dirección de cada variable en el espacio PCA") +
  theme_minimal(base_size = 11) +
  theme(plot.title    = element_text(face = "bold", size = 13),
        plot.subtitle = element_text(color = "gray50"))

ggsave("outputs/figuras/biplot_flechas.png",
       plot = p_biplot_fviz, width = 8, height = 7, dpi = 150)
cat("biplot_flechas.png guardado\n")

# --- Tabla de loadings (primeras 5 PCs)
loadings <- as.data.frame(pca_result$rotation[, 1:5])
loadings$Variable <- rownames(loadings)
loadings <- loadings %>% select(Variable, everything())

cat("\n**** LOADINGS — Primeras 5 PCs *******\n")
print(round(loadings[, -1], 3))

write.csv(loadings,
          "outputs/tablas/loadings.csv",
          row.names = FALSE)
cat("✓ outputs/tablas/loadings.csv guardado\n")

# ------ Contribuciòn de variables a PC1 y PC2
p_contrib1 <- fviz_contrib(pca_result, choice = "var", axes = 1,
                           fill = "#42A5F5", color = "#1565C0") +
  labs(title = "Contribución de variables a PC1") +
  theme_minimal(base_size = 11)

p_contrib2 <- fviz_contrib(pca_result, choice = "var", axes = 2,
                           fill = "#EF5350", color = "#B71C1C") +
  labs(title = "Contribución de variables a PC2") +
  theme_minimal(base_size = 11)

p_contrib <- grid.arrange(p_contrib1, p_contrib2, ncol = 2)

ggsave("outputs/figuras/contribucion_variables.png",
       plot = p_contrib, width = 10, height = 5, dpi = 150)
cat("contribucion_variables.png guardado\n")

# ----- Resultados
cat("\n")
cat("Archivos generados")
cat("\n¿Cuántas PCs necesito para explicar el 80%?\n")
cat("→", which(prop_var_acum >= 0.80)[1], "componentes\n")