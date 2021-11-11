# title: "Clase 13 Supuestos de la regresión lineal"
# author: Dr. José A. Gallardo
# Pontificia Universidad Católica de Valparaíso
# OCE 386 - Introducción al análisis de datos con R.


# Instala librerías
library(readxl)
library(dplyr)
library(ggplot2)
library(UsingR)
library(ggpmisc)
library(knitr)
library(lmtest)
library(car) 

# CASO DE ESTUDIO: CALENTAMIENTO GLOBAL.
# Fuente: climate.gov
# https://www.climate.gov/maps-data/dataset/global-temperature-anomalies-graphing-tool

My_Theme = theme(
  axis.title.x = element_text(size = 20),
  axis.text.x = element_text(size = 20),
  axis.title.y = element_text(size = 20),
  axis.text.y = element_text(size = 20))


Global_warming <- read_excel("Global_Warming.xlsx")
formula1 <- y ~ x
Global_warming$Year <- as.factor(Global_warming$Year)
Global_warming$CO2_ppm <- as.numeric(Global_warming$CO2_ppm)
Global_warming$`Global Temperature Anomalies` <- as.numeric(Global_warming$`Global Temperature Anomalies`)

s <- Global_warming %>% ggplot(aes(x = CO2_ppm, y = `Global Temperature Anomalies`)) + 
   geom_point() +
   stat_smooth(method = "lm", col = "red", show.legend=TRUE)+  stat_poly_eq(aes(label = paste0("atop(", ..eq.label.., ",", ..rr.label.., ")")), formula = formula1, parse = TRUE, size = 8)+scale_x_continuous(n.breaks = 8)

s+My_Theme


# INDEPENDENCIA: MÉTODO GRÁFICO
reg <- lm(`Global Temperature Anomalies` ~ CO2_ppm, 
          data = Global_warming)
plot(reg$residuals)
abline(h=0, col="red")


# LINEALIDAD: MÉTODO GRÁFICO
plot(reg, which=1)

# INDEPENDENCIA: Durbin Watson

# $H_0:$ No existe autocorrelación entre los datos (lo que deseamos).    
# $H_0:$ Existe autocorrelación entre los datos.  


# durbin watson test
durbinWatsonTest(reg) # library(car)

# Dado que p < 0,05 se rechaza independencia.

# HOMOGENEIDAD DE VARIANZAS: MÉTODO GRÁFICO

# **H~0~**:  $\sigma^2_1$ = $\sigma^2_2$  
# **H~A~**: $\sigma^2_1$ $\neq$ $\sigma^2_2$  
plot(reg, which=3)


# HOMOGENEIDAD DE VARIANZAS: PRUEBAS DE HIPÓTESIS

ncvTest(reg) # library(car) 
bptest(reg) # library(lmtest)

# p > .05, No tenemos eviencias para rechazar que nuestros datos son homocedasticos.


# NORMALIDAD: GRÁFICO DE CUANTILES
qqPlot(reg) # library(car)


# VALORES ATÌPICOS
plot(reg, which=4)

