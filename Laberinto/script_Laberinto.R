
# Grafica del Laberinto 
# 3 sesiones de P46, P74 Y P89 
#
# Creación: 26 de enero 2021
# Última modificación: 3 de febrero de 2021 

# librerias
library(scales)
library(dplyr)
library(viridis)
library(ggplot2)

# Carga la base de datos
laberinto <- read.csv('~/github/Analisis_ratas/Laberinto/Laberinto.csv')

# Convierte el tiempo a numeric
laberinto$TIEMPO <- as.numeric(laberinto$TIEMPO)

# Organizar datos del tiempo por sessión y lugar del laberinto 
laberinto.total <- aggregate(TIEMPO ~ SES + LUGAR, data = laberinto, mean)

# Agrega la desviacion estandar
laberinto.total$sd <- aggregate(TIEMPO ~ SES + LUGAR, data = laberinto, sd)[,3]

# Ordenar los datos por sesion
laberinto.total <- laberinto.total[order(laberinto.total$SES),]

# Grafica los datos
Col <- viridis(3, end=0.8)[as.factor(laberinto.total$LUGAR)]
barCenters <- barplot(height = laberinto.total$TIEMPO, ylim = c(0,300), names.arg = laberinto.total$LUGAR,
                      main = "Laberinto", xlab = "sesión", ylab = "Tiempo", border = Col, col = alpha(Col, 0.5))
arrows(barCenters, laberinto.total$TIEMPO-laberinto.total$sd, col = Col,
       barCenters, laberinto.total$TIEMPO+laberinto.total$sd,angle=90,code=3)
axis(1, at=barCenters, laberinto.total$SES, pos = -15, tick = NA, col = NA)

#### Spaguetti plot ####

## define base for the graphs and store in object 'p'
ggplot(data = laberinto.total, aes(x = SES, y = TIEMPO, group = LUGAR)) + 
  geom_line(aes(color=LUGAR)) + 
  geom_point(aes(color=LUGAR)) +
  # Inserta las desviaciones estándar ## 
  # geom_errorbar(aes(ymin=TIEMPO-sd, ymax=TIEMPO+sd, color=LUGAR), width=.1) +
  scale_color_manual(values = viridis(3, end=0.8)) + 
  labs(title = "Laberinto", x = "Sesión", y = "Tiempo (seg)")
  
#### Spaguetti plot con desviacion estandar como sombra
ggplot(data = laberinto.total, aes(x = SES, y = TIEMPO, group = LUGAR)) + 
  geom_line(aes(color=LUGAR)) +
  geom_point(aes(color=LUGAR)) +
  scale_color_manual(values=viridis(3, end=0.8)) +
  scale_fill_manual(values=viridis(3, end=0.8)) +
  geom_ribbon(aes(ymin = TIEMPO - sd,
                  ymax = TIEMPO + sd,
                  group=LUGAR, fill=LUGAR), alpha=.3, linetype=0)

#### Gráfica de todos los sujetos #### 
# --------------------------------------------------------------
# Mi intento #
# Subset de P46 y P89 en ABIERTOS
laberinto.subB <- subset(laberinto, laberinto$SES=="P46" | laberinto$SES=="P89")
laberinto.subC <- subset(laberinto.subB, laberinto.subB$LUGAR=="ABIERTOS")

# Subset con dplyr
# laberinto %>% 
#   filter(SES=="P46" | SES=="P89", LUGAR=="ABIERTOS")

# Droplevels de los factores
laberinto.subC <- droplevels(laberinto.subC)

# Grafica
ggplot(data = subset(laberinto.subC, laberinto$LUGAR=="ABIERTOS"), aes(x = SES, y = TIEMPO, group = ID, color=ID)) + 
  geom_line(aes(group=ID)) +
  geom_point(aes(group=ID)) +
  scale_color_manual(values=rep("#440154FF", 10)) +
  labs(title="Laberinto: ABIERTO", x ="Sesion", y = "Tiempo (seg)")
# FInaliza mi intento #
# --------------------------------------------------------------

# Tiepo por rata en brazo ABIERTOS
ggplot(data = subset(laberinto, laberinto$LUGAR=="ABIERTOS"), aes(x = SES, y = TIEMPO, group = ID, color=ID)) + 
    geom_line() +
    geom_point() +
    scale_color_manual(values=rep("#440154FF", 10)) +
    labs(title="Laberinto: ABIERTO", x ="Sesion", y = "Tiempo (seg)")

# Tiepo por rata en brazo CERRADOS
ggplot(data = subset(laberinto, laberinto$LUGAR=="CERRADOS"), aes(x = SES, y = TIEMPO, group = ID, color=ID)) + 
    geom_line(aes(color=ID)) +
    geom_point(aes(color=ID)) +
    scale_color_manual(values=rep("#21908CFF", 10)) +
    labs(title="Laberinto: CERRADOS", x ="Sesion", y = "Tiempo (seg)")
  
# Tiepo por rata en brazo CENTRO
ggplot(data = subset(laberinto, laberinto$LUGAR=="CENTRO"), aes(x = SES, y = TIEMPO, group = ID, color=ID)) + 
    geom_line(aes(color=ID)) +
    geom_point(aes(color=ID)) +
    scale_color_manual(values=rep("#7AD151FF", 10)) +
    labs(title="Laberinto: CENTRO", x ="Sesion", y = "Tiempo (seg)")







