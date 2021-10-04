# ----------------------------------------------------------
# R script anova
# Dr. Jose Gallardo
# 03 septiembre 2021
# Curso OCE 386 Introducción al Analisis de datos con R
# ----------------------------------------------------------

## R Documentation aov {stats}

help("aov")

# Crea objeto con datos de PlantGrowth {datasets}
my_data <- PlantGrowth

# Realiza análisis de varianza y almacena en objeto res.aov 

res.aov <- aov(weight ~ group, data = my_data)

# Imprime reporte del análisis de varianza

summary(res.aov)

## R Documentation TukeyHSD {stats}

help(TukeyHSD)

# Calcula diferencias entre medias luego de una ANOVA

TukeyHSD(res.aov)


## R Documentation aov {stats}
## Anova de dos vías con interacción

# Crea objeto con datos de ToothGrowth {datasets}
my_data1 <- ToothGrowth

# Realiza análisis de varianza y almacena en objeto res.aov2 

res.aov2 <- aov(len ~ supp * dose, data = my_data1)

# Imprime reporte del análisis de varianza

summary(res.aov2)
