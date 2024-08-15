setwd("D:/Variables_ambientales/R_variables_ambient")
getwd()

### Analisis riqueza de plantas rupicolas y variables amientales

# Cargo librerias que quiza use
library(readxl)
library(rsq)
library(MuMIn)
library(sjstats)
library(car)
library(MASS)
library(lmtest)
library(lme4)
library(lmerTest)
library(lmPerm)
library(ggplot2)
library(multcomp)
library(multcompView)
library(sjPlot)

## Anotaciones antes de empezar
#1. Las variables respuesta (riqueza) son DISCRETAS -> usar familia poisson

#2. Alguna fila que tengamos que quitar? Los pixeles/puntos proximos a la costa pueden tener 0s porque los raster no tenian datos ahi

#3. Elimino datos donde no hay litologia


### CARGO Y PROCESO LOS DATOS

# Leo los datos, exportados desde QGIS como un .csv
riqueza_amb_lito <- read.csv("D:/Variables_ambientales/R_variables_ambient/riqueza_amb_lito.csv")
View(riqueza_amb_lito)

# Quito los pixeles donde no hay datos, y los puntos donde no hay litologia
data_filtered <- subset(riqueza_amb_lito, !(X1 == 0 & X2 == 0 & X3 == 0) & LITO_RECL3 != "")
str(data_filtered)

#Convierto LITOLOGIA en factor
data_filtered$LITO_RECL3 <- as.factor(data_filtered$LITO_RECL3)

str(data_filtered)


# Cambiar los niveles de la variable LITO_RECL3
levels(data_filtered$LITO_RECL3) <- c("Areniscas", "Calizas", "Cuarcitas", "Granitos", "Serpentinas", "Vulcanitas")
# Verificar los cambios
levels(data_filtered$LITO_RECL3)
print(levels(data_filtered$LITO_RECL3))


#Defino la categoria de referencia para la litologia
data_filtered$LITO_RECL3 <- relevel(data_filtered$LITO_RECL3, ref = "Areniscas")





#########GLMs #######
str(data_filtered)

#Modelo con todo (NO UTILIZADO)
glm_1           <- glm(Riqueza_GB ~ X1 + X2 + X3 + X4 + X5 + X6 + X7 + X8 + X9 + X10
                       + X11 + X12 +  X13 + X14 + X15 + X16 + X17 + X18 + X19 + X20 +
                         LITO_RECL3, contrasts=list(LITO_RECL3=contr.sum),
                       data=data_filtered, family=poisson(link="log"))
summary(glm_1)
Anova(glm_1)
vif(glm_1)

step_glm1 <- stepAIC(glm_1, direction = "both")
plot(step_glm_1)


# Pruebo con una seleccion de variables bioclimaticas + la litologia
glm_2           <- glm(Riqueza_GB ~ X1 + X2 + X6 + X7 + X13 + X18 + LITO_RECL3,
                       contrasts=list(LITO_RECL3=contr.sum),
                       data=data_filtered, family=poisson(link="log"))

summary(glm_2)

## SELECCION DE MODELOS
options(na.action = "na.fail") # evita errores

ms_glm2           <- dredge(glm_2, extra = "adjR^2", rank = "AICc")
ms_glm2

confset_glm2      <- get.models(ms_glm2, subset=TRUE)
avgmod_glm2 <- model.avg(confset_glm2)
summary(avgmod_glm2)
sw(avgmod_glm2) #sw nos da la importancia relativa (weights) de las variables, si tienen peso > 0.5 las podemos considerar en el modelo final

# Miramos las asunciones del modelo glm_2 
vif(glm_2)
bptest(glm_2)
outlierTest(glm_2)
plot(glm_2)


library(pscl)
library(sjPlot)

# Crear una tabla de resumen del modelo con sjPlot
tab_model(glm_2, show.est = TRUE, show.ci = FALSE, show.se = TRUE, show.p = TRUE, transform = NULL,)



### GLM especies rupicolas especialistas
# Pruebo con una seleccion de variables bioclimaticas + la litologia
glm_sp           <- glm(Riqueza_sp ~ X1 + X2 + X6 + X7 + X13 + X18 + LITO_RECL3,
                       contrasts=list(LITO_RECL3=contr.sum),
                       data=data_filtered, family=poisson(link="log"))

## SELECCION DE MODELOS
options(na.action = "na.fail") # evita errores

ms_glm_sp           <- dredge(glm_sp, extra = "adjR^2", rank = "AICc")
ms_glm_sp

# Crear una tabla de resumen del modelo con sjPlot
tab_model(glm_2, glm_sp, show.est = TRUE , show.ci = FALSE, show.se = FALSE, show.p = TRUE, digits = 4)




########### REALIZAR GRAFICOs
library(ggplot2)


### 1. RUPICOLAS TOTALES

#Fit del modelo
fit_glm_2<- fitted(glm_2, type = "response")

x0_limits <- range(data_filtered$Riqueza_GB)
y0_limits <- range(fit_glm_2)

ggplot_mod_fin <- ggplot(data = data_filtered, aes(x=Riqueza_GB, y=fit_glm_2)) + 
  theme_classic() +  # Usar el tema clásico para obtener una caja alrededor del gráfico
  geom_point(color="#3E3E3E")+ theme(legend.position='none') +
  scale_x_continuous(limits = x0_limits, expand = c(0, 0)) +  # Ajustar los límites del eje X
  scale_y_continuous(limits = y0_limits, expand = c(0,0)) + # Ajustar los límites del eje Y
  coord_cartesian(clip = "on") +  # Asegurarse de que el gráfico no quede recortado
  theme(
    legend.position = 'none',
    axis.title.x = element_text(size = 15),
    axis.title.y = element_text(size = 15, angle = 90, vjust = 1),  # Alinear verticalmente el texto del eje Y
    axis.text.x = element_text(size = 12), margin = margin(t = 5),
    axis.text.y = element_text(size = 12, margin = margin(r = 5)),  # Separar los números del eje Y
    axis.line = element_line(color = "black"),
    axis.ticks.length = unit(-0.2, "cm"),
    axis.ticks = element_line(size = 0.5),
    panel.border = element_rect(color = "black", fill = NA, size = 0.5)  # Añadir bordes alrededor del panel
  ) +
  labs(x = "Riqueza de plantas rupícolas",y = "Predicción del modelo")  + 
  geom_smooth(method=lm, color = "#FF0000", se=TRUE)

ggplot_mod_fin

##VARIABLES

#Elevacion

x1_limits <- range(data_filtered$X1)

ggplot_elevacion <- ggplot(data = data_filtered, aes(x = X1, y = fit_glm_2)) +
  theme_classic() +
  geom_point(color="#3E3E3E")+ theme(legend.position='none') +
  scale_x_continuous(limits = x1_limits, expand = c(0, 0)) +  # Ajustar los límites del eje X
  scale_y_continuous(limits = y0_limits, expand = c(0,0)) +# Ajustar los límites del eje X
  coord_cartesian(clip = "on") +  # Asegurarse de que el gráfico no quede recortado
  theme(
    legend.position = 'none',
    axis.title.x = element_text(size = 15),
    axis.title.y = element_text(size = 14, angle = 90, vjust = 1),  # Alinear verticalmente el texto del eje Y
    axis.text.x = element_text(size = 12), margin = margin(r = 5),
    axis.text.y = element_text(size = 12, margin = margin(r = 5)),  # Separar los números del eje Y
    axis.line = element_line(color = "black"),
    axis.ticks.length = unit(-0.2, "cm"),
    axis.ticks = element_line(size = 0.5),
    panel.border = element_rect(color = "black", fill = NA, size = 0.5)
  ) +
  geom_smooth(method = lm, color = "#FF0000", se = TRUE) +
  labs(x = "Elevación (m)", y = "Riqueza de plantas rupícolas totales")

ggplot_elevacion

#Temperatura media
x2_limits <- range(data_filtered$X2)

ggplot_Tmedia <- ggplot(data = data_filtered, aes(x = X2, y = fit_glm_2)) +
  geom_point(color="#3E3E3E")+ theme(legend.position='none') +
  geom_smooth(method = lm, color = "#FF0000", se = TRUE) +
  theme_classic() +
  scale_x_continuous(limits= x2_limits, expand = c(0, 0)) +  # Ajustar los límites del eje X
  scale_y_continuous(limits = y0_limits, expand = c(0,0)) + # Ajustar los límites del eje Y
  coord_cartesian(clip = "on") +  # Asegurarse de que el gráfico no quede recortado
  theme(
    legend.position = 'none',
    axis.title.x = element_text(size = 15),
    axis.title.y = element_text(size = 14, angle = 90, vjust = 1),  # Alinear verticalmente el texto del eje Y
    axis.text.x = element_text(size = 12), margin = margin(t = 5),
    axis.text.y = element_text(size = 12, margin = margin(r = 5)),  # Separar los números del eje Y
    axis.line = element_line(color = "black"),
    axis.ticks.length = unit(-0.2, "cm"),
    axis.ticks = element_line(size = 0.5),
    panel.border = element_rect(color = "black", fill = NA, size = 0.5)
  ) +
  labs(x = "Temperatura media (ºC)", y = "Riqueza de plantas rupícolas totales")

ggplot_Tmedia

#Temperatura max periodo calido
x3_limits <- range(data_filtered$X6)

ggplot_Tmax <- ggplot(data = data_filtered, aes(x = X6, y = fit_glm_2)) +
  geom_point(color="#3E3E3E")+ theme(legend.position='none') +
  geom_smooth(method = lm, color = "#FF0000", se = TRUE) +
  theme_classic() +
  scale_x_continuous(limits= x3_limits, expand = c(0, 0)) +  # Ajustar los límites del eje X
  scale_y_continuous(limits = y0_limits, expand = c(0,0)) + # Ajustar los límites del eje Y
  coord_cartesian(clip = "on") +  # Asegurarse de que el gráfico no quede recortado
  theme(
    legend.position = 'none',
    axis.title.x = element_text(size = 15),
    axis.title.y = element_text(size = 14, angle = 90, vjust = 1),  # Alinear verticalmente el texto del eje Y
    axis.text.x = element_text(size = 12), margin = margin(t = 5),
    axis.text.y = element_text(size = 12, margin = margin(r = 5)),  # Separar los números del eje Y
    axis.line = element_line(color = "black"),
    axis.ticks.length = unit(-0.2, "cm"),
    axis.ticks = element_line(size = 0.5),
    panel.border = element_rect(color = "black", fill = NA, size = 0.5)
  ) +
  labs(x = "Temperatura máxima del mes cálido (ºC)", y = "Riqueza de plantas rupícolas totales")

ggplot_Tmax

#Temperatura min periodo frio
x4_limits <- range(data_filtered$X7)

ggplot_Tmin <- ggplot(data = data_filtered, aes(x = X7, y = fit_glm_2)) +
  geom_point(color="#3E3E3E")+ theme(legend.position='none') +
  geom_smooth(method = lm, color = "#FF0000", se = TRUE) +
  theme_classic() +
  scale_x_continuous(limits = x4_limits, expand = c(0, 0)) +  # Ajustar los límites del eje X
  scale_y_continuous(limits = y0_limits, expand = c(0,0)) + # Ajustar los límites del eje Y
  coord_cartesian(clip = "on") +
  theme(
    legend.position = 'none',
    axis.title.x = element_text(size = 15),
    axis.title.y = element_text(size = 14, angle = 90, vjust = 1),  # Alinear verticalmente el texto del eje Y
    axis.text.x = element_text(size = 12), margin = margin(t = 5),
    axis.text.y = element_text(size = 12, margin = margin(r = 5)),  # Separar los números del eje Y
    axis.line = element_line(color = "black"),
    axis.ticks.length = unit(-0.2, "cm"),
    axis.ticks = element_line(size = 0.5),
    panel.border = element_rect(color = "black", fill = NA, size = 0.5)
  ) +
  labs(x = "Temperatura mínima del mes frío (ºC)", y = "Riqueza de plantas rupícolas totales")

ggplot_Tmin

#Precipitacion anual
x5_limits <- range(data_filtered$X13)

ggplot_Precipitacion <- ggplot(data = data_filtered, aes(x = X13, y = fit_glm_2)) +
  geom_point(color="#3E3E3E")+ theme(legend.position='none') +
  geom_smooth(method = lm, color = "#FF0000", se = TRUE) +
  theme_classic() +
  scale_x_continuous(limits = x5_limits, expand = c(0, 0)) +  # Ajustar los límites del eje X
  scale_y_continuous(limits = y0_limits, expand = c(0, 0.5)) + # Ajustar los límites del eje Y
  coord_cartesian(clip = "on") +
  theme(
    legend.position = 'none',
    axis.title.x = element_text(size = 15),
    axis.title.y = element_text(size = 14, angle = 90, vjust = 1),  # Alinear verticalmente el texto del eje Y
    axis.text.x = element_text(size = 12), margin = margin(t = 5),
    axis.text.y = element_text(size = 12), margin = margin(r = 5),  # Separar los números del eje Y
    axis.line = element_line(color = "black"),
    axis.ticks.length = unit(-0.2, "cm"),
    axis.ticks = element_line(size = 0.5),
    panel.border = element_rect(color = "black", fill = NA, size = 0.5)
  ) +
  labs(x = "Precipitación anual (mm)", y = "Riqueza de plantas rupícolas totales")

ggplot_Precipitacion

#Precipitacion periodo seco
x6_limits <- range(data_filtered$X18)

ggplot_Precipitacion_seco <- ggplot(data = data_filtered, aes(x = X18, y = fit_glm_2)) +
  geom_point(color="#3E3E3E")+ theme(legend.position='none') +
  geom_smooth(method = lm, color = "#FF0000", se = TRUE) +
  theme_classic() +
  scale_x_continuous(limits = x6_limits, expand = c(0, 0)) +  # Ajustar los límites del eje X
  scale_y_continuous(limits = y0_limits, expand = c(0, -0.1)) + # Ajustar los límites del eje Y
  coord_cartesian(clip = "on") +
  theme(
    legend.position = 'none',
    axis.title.x = element_text(size = 15),
    axis.title.y = element_text(size = 14, angle = 90, vjust = 1),  # Alinear verticalmente el texto del eje Y
    axis.text.x = element_text(size = 12), margin = margin(t = 5),
    axis.text.y = element_text(size = 12, margin = margin(r = 5)),  # Separar los números del eje Y
    axis.line = element_line(color = "black"),
    axis.ticks.length = unit(-0.2, "cm"),
    axis.ticks = element_line(size = 0.5),
    panel.border = element_rect(color = "black", fill = NA, size = 0.5)
  ) +
  labs(x = "Precipitación de la estación seca (mm)", y = "Riqueza de plantas rupícolas totales")

ggplot_Precipitacion_seco








### 2. Rupicolas especialistas
fit_glm_sp<- fitted(glm_sp, type = "response")
y_sp_limits <- range(fit_glm_sp)


##VARIABLES

#Elevacion

x1_limits <- range(data_filtered$X1)

ggplot_elevacion_sp <- ggplot(data = data_filtered, aes(x = X1, y = fit_glm_sp)) +
  theme_classic() +
  geom_point(color="#3E3E3E")+ theme(legend.position='none') +
  scale_x_continuous(limits = x1_limits, expand = c(0, 0)) +  # Ajustar los límites del eje X
  scale_y_continuous(limits = y_sp_limits, expand = c(0,0)) +# Ajustar los límites del eje X
  coord_cartesian(clip = "on") +  # Asegurarse de que el gráfico no quede recortado
  theme(
    legend.position = 'none',
    axis.title.x = element_text(size = 15),
    axis.title.y = element_text(size = 12, angle = 90, vjust = 1),  # Alinear verticalmente el texto del eje Y
    axis.text.x = element_text(size = 12), margin = margin(r = 5),
    axis.text.y = element_text(size = 12, margin = margin(r = 5)),  # Separar los números del eje Y
    axis.line = element_line(color = "black"),
    axis.ticks.length = unit(-0.2, "cm"),
    axis.ticks = element_line(size = 0.5),
    panel.border = element_rect(color = "black", fill = NA, size = 0.5)
  ) +
  geom_smooth(method = lm, color = "#FF0000", se = TRUE) +
  labs(x = "Elevación (m)", y = "Riqueza de plantas rupícolas especialistas")

ggplot_elevacion_sp

#Temperatura media
x2_limits <- range(data_filtered$X2)

ggplot_Tmedia_sp <- ggplot(data = data_filtered, aes(x = X2, y = fit_glm_sp)) +
  geom_point(color="#3E3E3E")+ theme(legend.position='none') +
  geom_smooth(method = lm, color = "#FF0000", se = TRUE) +
  theme_classic() +
  scale_x_continuous(limits= x2_limits, expand = c(0, 0)) +  # Ajustar los límites del eje X
  scale_y_continuous(limits = y_sp_limits, expand = c(0,0)) + # Ajustar los límites del eje Y
  coord_cartesian(clip = "on") +  # Asegurarse de que el gráfico no quede recortado
  theme(
    legend.position = 'none',
    axis.title.x = element_text(size = 15),
    axis.title.y = element_text(size = 12, angle = 90, vjust = 1),  # Alinear verticalmente el texto del eje Y
    axis.text.x = element_text(size = 12), margin = margin(t = 5),
    axis.text.y = element_text(size = 12, margin = margin(r = 5)),  # Separar los números del eje Y
    axis.line = element_line(color = "black"),
    axis.ticks.length = unit(-0.2, "cm"),
    axis.ticks = element_line(size = 0.5),
    panel.border = element_rect(color = "black", fill = NA, size = 0.5)
  ) +
  labs(x = "Temperatura media (ºC)", y = "Riqueza de plantas rupícolas especialistas")

ggplot_Tmedia_sp

#Temperatura max periodo calido
x3_limits <- range(data_filtered$X6)

ggplot_Tmax_sp <- ggplot(data = data_filtered, aes(x = X6, y = fit_glm_sp)) +
  geom_point(color="#3E3E3E")+ theme(legend.position='none') +
  geom_smooth(method = lm, color = "#FF0000", se = TRUE) +
  theme_classic() +
  scale_x_continuous(limits= x3_limits, expand = c(0, 0)) +  # Ajustar los límites del eje X
  scale_y_continuous(limits = y_sp_limits, expand = c(0,0)) + # Ajustar los límites del eje Y
  coord_cartesian(clip = "on") +  # Asegurarse de que el gráfico no quede recortado
  theme(
    legend.position = 'none',
    axis.title.x = element_text(size = 15),
    axis.title.y = element_text(size = 12, angle = 90, vjust = 1),  # Alinear verticalmente el texto del eje Y
    axis.text.x = element_text(size = 12), margin = margin(t = 5),
    axis.text.y = element_text(size = 12, margin = margin(r = 5)),  # Separar los números del eje Y
    axis.line = element_line(color = "black"),
    axis.ticks.length = unit(-0.2, "cm"),
    axis.ticks = element_line(size = 0.5),
    panel.border = element_rect(color = "black", fill = NA, size = 0.5)
  ) +
  labs(x = "Temperatura máxima del mes cálido (ºC)", y = "Riqueza de plantas rupícolas especialistas")

ggplot_Tmax_sp

#Temperatura min periodo frio
x4_limits <- range(data_filtered$X7)

ggplot_Tmin_sp <- ggplot(data = data_filtered, aes(x = X7, y = fit_glm_sp)) +
  geom_point(color="#3E3E3E")+ theme(legend.position='none') +
  geom_smooth(method = lm, color = "#FF0000", se = TRUE) +
  theme_classic() +
  scale_x_continuous(limits = x4_limits, expand = c(0, 0)) +  # Ajustar los límites del eje X
  scale_y_continuous(limits = y_sp_limits, expand = c(0,0)) + # Ajustar los límites del eje Y
  coord_cartesian(clip = "on") +
  theme(
    legend.position = 'none',
    axis.title.x = element_text(size = 15),
    axis.title.y = element_text(size = 12, angle = 90, vjust = 1),  # Alinear verticalmente el texto del eje Y
    axis.text.x = element_text(size = 12), margin = margin(t = 5),
    axis.text.y = element_text(size = 12, margin = margin(r = 5)),  # Separar los números del eje Y
    axis.line = element_line(color = "black"),
    axis.ticks.length = unit(-0.2, "cm"),
    axis.ticks = element_line(size = 0.5),
    panel.border = element_rect(color = "black", fill = NA, size = 0.5)
  ) +
  labs(x = "Temperatura mínima del mes frío (ºC)", y = "Riqueza de plantas rupícolas especialistas")

ggplot_Tmin_sp

#Precipitacion anual
x5_limits <- range(data_filtered$X13)

ggplot_Precipitacion_sp <- ggplot(data = data_filtered, aes(x = X13, y = fit_glm_sp)) +
  geom_point(color="#3E3E3E")+ theme(legend.position='none') +
  geom_smooth(method = lm, color = "#FF0000", se = TRUE) +
  theme_classic() +
  scale_x_continuous(limits = x5_limits, expand = c(0, 0)) +  # Ajustar los límites del eje X
  scale_y_continuous(limits = y_sp_limits, expand = c(0, 0.5)) + # Ajustar los límites del eje Y
  coord_cartesian(clip = "on") +
  theme(
    legend.position = 'none',
    axis.title.x = element_text(size = 15),
    axis.title.y = element_text(size = 12, angle = 90, vjust = 1),  # Alinear verticalmente el texto del eje Y
    axis.text.x = element_text(size = 12), margin = margin(t = 5),
    axis.text.y = element_text(size = 12), margin = margin(r = 5),  # Separar los números del eje Y
    axis.line = element_line(color = "black"),
    axis.ticks.length = unit(-0.2, "cm"),
    axis.ticks = element_line(size = 0.5),
    panel.border = element_rect(color = "black", fill = NA, size = 0.5)
  ) +
  labs(x = "Precipitación anual (mm)", y = "Riqueza de plantas rupícolas especialistas")

ggplot_Precipitacion_sp

#Precipitacion periodo seco
x6_limits <- range(data_filtered$X18)

ggplot_Precipitacion_seco_sp <- ggplot(data = data_filtered, aes(x = X18, y = fit_glm_sp)) +
  geom_point(color="#3E3E3E")+ theme(legend.position='none') +
  geom_smooth(method = lm, color = "#FF0000", se = TRUE) +
  theme_classic() +
  scale_x_continuous(limits = x6_limits, expand = c(0, 0)) +  # Ajustar los límites del eje X
  scale_y_continuous(limits = y_sp_limits, expand = c(0, -0.1)) + # Ajustar los límites del eje Y
  coord_cartesian(clip = "on") +
  theme(
    legend.position = 'none',
    axis.title.x = element_text(size = 15),
    axis.title.y = element_text(size = 12, angle = 90, vjust = 1),  # Alinear verticalmente el texto del eje Y
    axis.text.x = element_text(size = 12), margin = margin(t = 5),
    axis.text.y = element_text(size = 12, margin = margin(r = 5)),  # Separar los números del eje Y
    axis.line = element_line(color = "black"),
    axis.ticks.length = unit(-0.2, "cm"),
    axis.ticks = element_line(size = 0.5),
    panel.border = element_rect(color = "black", fill = NA, size = 0.5)
  ) +
  labs(x = "Precipitación de la estación seca (mm)", y = "Riqueza de plantas rupícolas especialistas")

ggplot_Precipitacion_seco_sp


########### LITOLOGIA #############
library(MASS)

# Ajustar el modelo GLM con el factor LITO_RECL3
glm_lito <- glm(Riqueza_GB ~ LITO_RECL3,
                contrasts=list(LITO_RECL3=contr.sum),
                data = data_filtered,
                family = poisson(link = "log"),
                )

# Ver el resumen del modelo
summary(glm_lito)

#TEST POST-HOC A LA LITOLOGIA
library(multcomp)
library(multcompView)

# Realizar el análisis de Tukey HSD
tukey_glm_lito <- glht(glm_lito, linfct = mcp(LITO_RECL3 = "Tukey"))
tukey_results <- summary(tukey_glm_lito)
tukey_results


plot(tukey_results)

library(xtable)

tukey_df <- tidy(tukey_results)

# Convertir a una tabla en LaTeX
tukey_table <- xtable(tukey_df)

# Mostrar la tabla en formato LaTeX
print(tukey_table)

# También puedes exportar a HTML si prefieres
print(tukey_table, type = "html", file = "tukey_results.html")





###RUPICOLAS ESPECIALISTAS

# Ajustar el modelo GLM con el factor LITO_RECL3
glm_lito_rupicolas <- glm(Riqueza_sp ~ LITO_RECL3,
                contrasts=list(LITO_RECL3=contr.sum),
                data = data_filtered,
                family = poisson(link = "log"),
)

# Ver el resumen del modelo
summary(glm_lito_rupicolas)

# Realizar el análisis de Tukey HSD
tukey_glm_lito_rupicolas <- glht(glm_lito_rupicolas, linfct = mcp(LITO_RECL3 = "Tukey"))
tukey_results_rupicolas <- summary(tukey_glm_lito_rupicolas)
tukey_results_rupicolas
plot(tukey_results_rupicolas)

library(xtable)

tukey_df_rupicolas <- tidy(tukey_results_rupicolas)

# Convertir a una tabla en LaTeX
tukey_table_rupicolas <- xtable(tukey_df_rupicolas)

# Mostrar la tabla en formato LaTeX
print(tukey_table_rupicolas)

# También puedes exportar a HTML si prefieres
print(tukey_table_rupicolas, type = "html", file = "tukey_results_rupicolas.html", digits = 3)




###PLOTS LITOLOGIA

##lmp2

summary(lmp_2_fin)
##Para plotear, incluyo en el modelo GLM las variables ambientales que haya incluido en el lmp()
glm_lito_lmp2 <- glm(Riqueza_GB ~ X1 + X2 + X6 + X7 + X13 +LITO_RECL3,
                contrasts=list(LITO_RECL3=contr.sum),
                data = data_filtered,
                family = poisson(link = "log"),
)
summary(glm_lito_lmp2)

# Crear un boxplot para visualizar la distribución de Riqueza_GB por niveles de LITO_RECL3
fit_glm_lito_lmp2<- fitted(glm_lito_lmp2, type = "response")

ggplot_glm_lito_lmp2 <- ggplot(data = data_filtered, aes(x = LITO_RECL3, y = fit_glm_lito_lmp2)) +
  geom_boxplot(outlier.shape = NA) +
  theme_classic() +
  coord_cartesian(clip = "on") +
  theme(
    legend.position = 'none',
    axis.title.x = element_text(size = 15),
    axis.title.y = element_text(size = 12, angle = 90, vjust = 1),  # Alinear verticalmente el texto del eje Y
    axis.text.x = element_text(size = 10.5, color = "#3E3E3E"), margin = margin(t = 5),
    axis.text.y = element_text(size = 12, color = "#3E3E3E"), margin = margin(r = 5),  # Separar los números del eje Y
    axis.line = element_line(color = "black"),
    axis.ticks.length.y = unit(-0.2, "cm"),
    axis.ticks.y = element_line(size = 0.5),
    axis.ticks.length.x = unit(0.2, "cm"),
    axis.ticks.x = element_line(size = 0.5),
    panel.border = element_rect(color = "black", fill = NA, size = 0.5)
  ) +
  labs(x = "Litología", y = "Riqueza de plantas rupícolas totales")

ggplot_glm_lito_lmp2




###lmp4
summary(lmp_4_fin)
glm_lito_lmp4 <- glm(Riqueza_sp ~ X1 + X2 + X6 + X7+ X13 + X18 + LITO_RECL3,
                     contrasts=list(LITO_RECL3=contr.sum),
                     data = data_filtered,
                     family = poisson(link = "log"),
)
summary(glm_lito_lmp4)

# Crear un boxplot para visualizar la distribución de Riqueza_sp por niveles de LITO_RECL3
fit_glm_lito_lmp4<- fitted(glm_lito_lmp4, type = "response")

ggplot_glm_lito_lmp4 <- ggplot(data = data_filtered, aes(x = LITO_RECL3, y = fit_glm_lito_lmp4)) +
  geom_boxplot(outlier.shape = NA) +
  theme_classic() +
  coord_cartesian(clip = "on") +
  theme(
    legend.position = 'none',
    axis.title.x = element_text(size = 15),
    axis.title.y = element_text(size = 12, angle = 90, vjust = 1),  # Alinear verticalmente el texto del eje Y
    axis.text.x = element_text(size = 10.5, color = "#3E3E3E"), margin = margin(t = 5),
    axis.text.y = element_text(size = 10.5, color = "#3E3E3E"), margin = margin(r = 5),  # Separar los números del eje Y
    axis.line = element_line(color = "black"),
    axis.ticks.length.y = unit(-0.2, "cm"),
    axis.ticks.y = element_line(size = 0.5),
    axis.ticks.length.x = unit(0.2, "cm"),
    axis.ticks.x = element_line(size = 0.5),
    panel.border = element_rect(color = "black", fill = NA, size = 0.5)
  ) +
  labs(x = "Litología", y = "Riqueza de plantas rupícolas especialistas")

ggplot_glm_lito_lmp4
