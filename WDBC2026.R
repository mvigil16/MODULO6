###WDBC2025

###
BrestCancerDiagnosticWisconsin

###Descripción
###El conjunto de datos proporciona información de 569 pacientes sobre 10 características de los núcleos celulares 
###obtenidos de una imagen digitalizada de una aspiración con aguja fina (FNA) de una masa mamaria. 
###Para cada paciente, el cáncer fue diagnosticado como maligno o benigno.

###Format
###A data frame with 569 observations on the following variables:

#ID: ID number

#diagnosis: cancer diagnosis: M = malignant, B = benign

#radius_mean: a numeric vector

#texture_mean: a numeric vector

#perimeter_mean: a numeric vector

#area_mean: a numeric vector

#smoothness_mean: a numeric vector

#compactness_mean: a numeric vector

#concavity_mean: a numeric vector

#nconcave_mean: a numeric vector

#symmetry_mean: a numeric vector

#fractaldim_mean: a numeric vector

###Detalles
###Las características registradas son:

#radius: Media de las distancias desde el centro a los puntos en el perímetro
#texture: Desviación estándar de los valores en escala de grises
#perimeter: Perímetro del núcleo celular
#area: Área del núcleo celular
#smoothness: Variación local en las longitudes de los radios
#compactness: Compacidad del núcleo celular, perímeter^2 / area - 1
#concavity: Severidad de las porciones cóncavas del contorno
#nconcave: Número de porciones cóncavas del contorno
#symmetry: Forma del núcleo celular
#fractaldim: Dimensión fractal, "coastline approximation" - 1

###Librerias

library(learnr)
library(tidyverse)
library(tidymodels)
library(embed)
library(corrr)
library(tidytext)
library(gradethis)
library(sortable)
library(learntidymodels)
library(rstatix)
theme_set(theme_bw(16))

df<-read.csv("C:/Users/Salvador/Desktop/Cursos 2026-II/Diplomado Introducción Analítica a la Ciencia de Datos2026/BreastCancerDiagnosisWisconsin.csv")

df %>% dim()

df %>% glimpse()

df %>% head()

df1<-df[,1:11]

df1 %>% head()


###Valores faltantes

colSums(is.na(df1))

df1 %>% summary()

###Variable de clasificacion: diagnosis

df1 <- df1 %>% 
  mutate(diagnosis = relevel(as.factor(diagnosis), "B", "M"))

df1  %>% count(diagnosis)

table(df1$diagnosis)

df1 %>% glimpse()

df1 %>%
  group_by(diagnosis) %>%
  summarise (n = n()) %>%
  mutate(prop = n / sum(n)) %>%
ggplot(aes(df1,x = diagnosis, y = n)) +
    geom_col(fill = c("#CC0033", "#e319dc")) +
    geom_text(aes(label = paste0(n, " | ", signif(n / nrow(df) * 100, digits = 4), '%')), nudge_y = 10) + ggtitle("Porcentajes de resultados de biopsia")
    theme_gray()

df1 %>%
  select(where(is.numeric)) %>%
  colMeans()

###Histogramas

df1 |> pivot_longer((!diagnosis), 
 names_to = "Variable", values_to="Score") |>
   ggplot(aes(x=Score)) + geom_histogram(aes(y = ..density..),bins=20,colour = 3, fill = "darkmagenta") +
     facet_wrap("Variable",ncol = 4,scales = "free" ) + theme_minimal()

###box-plot

df1 |> pivot_longer((!diagnosis), 
  values_to="Score",names_to = "Variable") |>
    ggplot(aes(y=Score)) + geom_boxplot(aes(fill="darkred"),colour = 3,show.legend = FALSE) +
      facet_wrap("Variable",ncol = 4,scales = "free" ) + theme_minimal()

###Densidad

df1 |> pivot_longer((!diagnosis), 
   names_to = "Variable",values_to="Score") |>
     ggplot(aes(x=Score)) + geom_density(aes(fill="darkred"),colour = 3,show.legend = FALSE) +
       facet_wrap("Variable",ncol = 4,scales = "free" ) + theme_minimal()

###Comparacion por la variable de clasificacion o respuesta

df1_long <- df1 %>% 
    pivot_longer(!diagnosis, names_to = "predictores", values_to = "values")

theme_set(theme_light())

df1_long %>% 
  ggplot(mapping = aes(x = diagnosis, y = values, fill = predictores)) +
  geom_boxplot() + 
  facet_wrap(~ predictores, scales = "free", ncol = 4) +
  scale_color_viridis_d(option = "plasma", end = .7) +
  theme(legend.position = "none") +
  labs(title = "Comparación vía box-plot")

df1_long |> ggplot(mapping = aes(values, fill = diagnosis)) +
  geom_histogram(color = "white") +
  facet_wrap(~predictores, scales = "free", ncol= 4) +
  scale_color_viridis_d(option = "plasma", end = .7) +
  labs(title = "Variables Distribution") +
  theme_light()+
  labs(title = "Comparación vía histograma")

df1_long |> ggplot(mapping = aes(values, fill = diagnosis)) +
  geom_density(color = "white") +
  facet_wrap(~predictores, scales = "free", ncol= 4) +
  scale_color_viridis_d(option = "plasma", end = .7) +
  labs(title = "Variables Distribution") +
  theme_light()+
  labs(title = "Comparación a través de densidad")

###

ggpairs(df1, mapping = aes(color = diagnosis),columns = seq(2,11))

### correlacion ###

wdbc_corr <- df1 %>%
  select(-diagnosis) %>%
  correlate(method = "spearman")

wdbc_corr 

cor_wdbc<-cor(df1[,-1],method="spearman")

cor_wdbc

ggcorrplot::ggcorrplot(corr = cor_wdbc,
                       type = "lower", 
                       show.diag = TRUE,
                       lab = TRUE, 
                       lab_size = 3)
det(cor_wdbc)

psych::KMO(cor_wdbc)


###Prueba de Bartlett

psych::cortest.bartlett(cor_wdbc,n=dim(df1)[1])

###Todas estas medidas indican que hay una estructura de asociacion fuerte

cor.df1 <- df1 %>% select(-diagnosis) %>% cor_mat()
cor.df1

options(scipen=999)
format(value, scientific=FALSE)
cor.df1 %>% cor_get_pval()

cor.df1 %>%
  cor_reorder() %>%
  pull_lower_triangle() %>%
  cor_plot(label = TRUE)

cor.df1 %>% cor_gather() %>% print(n=Inf)

###Confirmando (que es gerundio) que la estructura de correlación es simplemente BRUTAL

###PCA

### PCA ####

pca_wdbc <- recipe(data = df1, formula = ~ .) %>%
  update_role(diagnosis, new_role = "id") %>%
  step_dummy(all_nominal_predictors()) %>%  ###Mi base no contiene predictores discretos
  step_normalize(all_predictors()) %>%
  step_pca(all_predictors(), id = "pca")

pca_wdbc

pca_prep <- prep(pca_wdbc)

pca_loading <- tidy(pca_prep, id="pca")
pca_loading

pca_variances <- tidy(pca_prep, id = "pca", type = "variance")
pca_variances

pca_var_percent<- tidy(pca_prep, id = "pca", type = "variance")%>%
                filter(str_detect(terms, "percent variance"))

pca_var_percent

pca_var_cum_percent<-tidy(pca_prep, id = "pca", type = "variance")%>%
                filter(str_detect(terms, "cumulative percent variance"))
pca_var_cum_percent


pca_prep %>% 
  tidy(id = "pca", type = "variance") %>% 
  dplyr::filter(terms == "percent variance") %>% 
  ggplot(aes(x = component, y = value)) + 
  geom_col(fill = "#B53389") + 
  xlim(c(0, 10)) + 
  geom_point(size=3) +
  geom_line(color="darkblue", size=1.1)+
  labs(x="PC", y="% de varianza", title="Scree plot")

pca_prep %>% 
  tidy(id = "pca", type = "variance") %>% 
  dplyr::filter(terms == "cumulative percent variance") %>% 
  ggplot(aes(x = component, y = value)) + 
  geom_col(fill = "#F25E52") + 
  xlim(c(0, 10)) + 
  geom_point(size=3) +
  geom_line(color="darkblue", size=1.1)+
  labs(x="PC", y="% acumulado de varianza", title="Scree plot")

variance_exp <- tidy(pca_prep,id = "pca", type = "variance")%>%
                filter(str_detect(terms, "percent variance"))
variance_exp

datos_pca <- pca_prep %>% 
  bake(new_data = NULL)

###Grafica de los primeros dos componentes

VE <- paste("Varianza explicada por dos componentes:"
                       ,round(variance_exp$value[[1]]+variance_exp$value[[2]], digits = 2),"%")
VE

datos_pca %>%
  ggplot(aes(x = PC1, y = PC2))+
  geom_point()+
  labs(x = paste0("PC1: ",round(variance_exp$value[[1]],2), "%"),
       y = paste0("PC2: ",round(variance_exp$value[[2]],2), "%"))+
  ggtitle(paste0("WDBC: Gráfica de componentes principales","\n",VE))

datos_pca %>%
  ggplot(aes(x = PC1, y = PC2,color = factor(diagnosis)))+
  geom_point()+
  labs(x = paste0("PC1: ",round(variance_exp$value[[1]],2), "%"),
       y = paste0("PC2: ",round(variance_exp$value[[2]],2), "%"))+
  ggtitle(paste0("WDBC: Gráfica de componentes principales","\n",VE))

###

wdbc_recipe <-
  recipe(~., data = df1) %>% 
  update_role(diagnosis, new_role = "id") %>% 
  step_normalize(all_predictors()) %>%
  step_pca(all_predictors(), id = "pca") %>% 
  prep()

wdbc_pca <- 
  wdbc_recipe %>% 
  tidy(id = "pca") 

wdbc_pca

wdbc_pca %>%
  mutate(terms = tidytext::reorder_within(terms, 
                                          abs(value), 
                                          component)) %>%
  ggplot(aes(abs(value), terms, fill = value > 0)) +
  geom_col() +
  facet_wrap(~component, scales = "free_y") +
  tidytext::scale_y_reordered() +
  scale_fill_manual(values = c("#b6dfe2", "#0A537D")) +
  labs(
    x = "Absolute value of contribution",
    y = NULL, fill = "¿Positive?"
  ) 


pca_wdbc <- recipe(data = df1, formula = ~ .) %>%
  update_role(diagnosis, new_role = "id") %>%
  step_dummy(all_nominal_predictors()) %>%  ###Mi base no contiene predictores discretos
  step_normalize(all_predictors()) %>%
  step_pca(all_predictors(), id = "pca") %>%
  prep()

pca_wdbc

pca_prep <- prep(pca_wdbc)

pca_loading <- tidy(pca_prep, id="pca")
pca_loading

pca_variances <- tidy(pca_prep, id = "pca", type = "variance")
pca_variances

pca_var_percent<- tidy(pca_prep, id = "pca", type = "variance")%>%
                filter(str_detect(terms, "percent variance"))

pca_var_percent

pca_var_cum_percent<-tidy(pca_prep, id = "pca", type = "variance")%>%
                filter(str_detect(terms, "cumulative percent variance"))
pca_var_cum_percent

###Cambiando (que es gerundio)

wdbc_pca <- 
  pca_wdbc %>% 
  tidy(id = "pca") 

wdbc_pca

wdbc_pca %>%
  mutate(terms = tidytext::reorder_within(terms, 
                                          abs(value), 
                                          component)) %>%
  ggplot(aes(abs(value), terms, fill = value > 0)) +
  geom_col() +
  facet_wrap(~component, scales = "free_y") +
  tidytext::scale_y_reordered() +
  scale_fill_manual(values = c("#b6dfe2", "#0A537D")) +
  labs(
    x = "Absolute value of contribution",
    y = NULL, fill = "¿Positive?"
  ) 

wdbc_pca %>%
  filter(component %in% paste0("PC", 1:3)) %>%
  mutate(component = fct_inorder(component)) %>%
  ggplot(aes(value, terms, fill = terms)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~component, nrow = 1) +
  labs(y = NULL)













