# Grafica del Rotarod
# 3 sesiones de P35, P46 Y P89 
#
# Creación: 25 de enero 2021
# Última modificación: 25 de enero de 2021 

# Librerias 
library(ggplot2) # graficas de ggplot
library(viridis) # Mapa de color
library(tidyr) # Organiza datos
library(Hmisc)

# Asignar el csv a un objeto
rotarod <- read.csv("~/github/Analisis_ratas/Rotarod/ANALISIS_ROTAROD.csv")

# ---------------------------------------------------------------------- #
#### Procesa datos ####

# Valor maximo por rodillo
rotarod$maxT <- ifelse(rotarod$rodillo=="negro", 300, 120)

# Normaliza los datos del tiempo de acuerdo al maximo
rotarod[,c("s.1","s.2","s.3","s.4")] <- rotarod[,c("s.1","s.2","s.3","s.4")]/rotarod$maxT

# Promedio por dia
rotarod$Tmean <- apply(rotarod[,c("s.1","s.2","s.3","s.4")], 1, mean)

# Desviacion estandar por dia
rotarod$std <- apply(rotarod[,c("s.1","s.2","s.3","s.4")], 1, sd)

# ---------------------------------------------------------------------- #
#### Figura 2.a Comparacion de rodillos ####

# Submuestra para la comparacion de rodillos
rodillos <- subset(rotarod , rotarod$edad=="P35" | rotarod$edad=="P46")

# Tabla de Medias y desviacion estandar por rodillo
rodillos.m <- aggregate(cbind(Tmean, std) ~ rodillo + dia, data = rodillos, mean)

#### Figura 2.a.1 ####
# Grafica del Promedio y desviacion estandar de cada rodillo
ggplot(rodillos.m, aes(x=dia, y=Tmean, group=rodillo, colour=rodillo, fill=rodillo)) + 
  geom_point(aes(colour = rodillo)) + 
  geom_line(aes(colour = rodillo)) +
  geom_ribbon(aes(ymin = Tmean - std,
                  ymax = Tmean + std,
                  group=rodillo), alpha=.3, linetype=0) +
  labs(title="Comparación de Rodillos", x ="Dia", y = "Tiempo Normalizado")


#### Figura 2.a.2 ####
# Tiempo total por dia por Rodillo
rodillos$Ttotal <- apply(rodillos[,c("s.1","s.2","s.3","s.4")], 1, sum)

# Tabla de tiempo total por dia por sesion
rodillos.total <- aggregate(Ttotal ~ rodillo + dia, data = rodillos, sum)

# Grafica
Col <- c("#F8766D", "#00BFC4")[as.factor(rodillos.total$rodillo)]
barplot(rodillos.total$Ttotal, names.arg = "", main = "Tiempo total por rodillo", 
        xlab = "Tiempo total", ylab = "dia", col = alpha(Col, 0.6), border = Col, horiz = TRUE)
axis(side = 2, at= c(1.3, 3.7, 6.1, 8.5), labels = 1:4, tick = NA, col = NA, pos = -0.25, las=2)  
axis(side = 2, at= c(0.7, 1.9, 3.1, 4.3, 5.5, 6.7, 7.9, 9.1), cex.lab=0.5, las=2, pos=1.5, labels = rodillos.total$rodillo, tick = NA, col = NA)  

# ---------------------------------------------------------------------- #
#### Figura 2.b.1 Comparacion de Sesiones ####
# Submuestra para la comparacion de rodillos
rotarod.ses <- subset(rotarod, rotarod$rodillo=="blanco")

# Tabla de Medias y desviacion estandar por rodillo
rotarod.P <- aggregate(cbind(Tmean, std) ~ edad + dia, data = rotarod.ses, mean)

#### Figura 2.a.1 ####
# Grafica del Promedio y desviacion estandar de cada rodillo
ggplot(rotarod.P, aes(x=dia, y=Tmean, group=edad, colour=edad, fill=edad)) + 
  geom_point(aes(colour = edad)) + 
  geom_line(aes(colour = edad)) +
  geom_ribbon(aes(ymin = Tmean - std,
                  ymax = Tmean + std,
                  group=edad), alpha=.3, linetype=0) +
  labs(title="Comparación de Edad", x ="Dia", y = "Tiempo Normalizado")

#### Figura 2.b.2 Tiempo total por Sesion ####
# Tiempo total por dia por Rodillo
rotarod.ses$Ttotal <- apply(rotarod.ses[,c("s.1","s.2","s.3","s.4")], 1, sum)

# Tabla de tiempo total por dia por sesion
rotarod.total <- aggregate(Ttotal ~ edad + dia, data = rotarod.ses, sum)

# Grafica
Col <- c("#F8766D", "#00BFC4")[as.factor(rotarod.total$edad)]
barplot(rotarod.total$Ttotal, names.arg = "", main = "Tiempo total por Edad", 
        xlab = "Tiempo total (seg)", ylab = "dia", col = alpha(Col, 0.6), border = Col, horiz = TRUE)
axis(side = 2, at= c(1.3, 3.7, 6.1, 8.5), labels = 1:4, tick = NA, col = NA, pos = -0.25, las=2)  
axis(side = 2, at= c(0.7, 1.9, 3.1, 4.3, 5.5, 6.7, 7.9, 9.1), cex.lab=0.5, las=2, pos=1.5, labels = rotarod.total$edad, tick = NA, col = NA)  

# ------------------------------------------------------------------------- #
#### EJEMPLOS ####

# Valores unicos de un vector
unique(rotarod$id)

# Tabla de valores únicos
table(rotarod$id)

# Gráfica de barras de
barplot(table(rotarod$id)) 

# Slicing de TODAS la filas y la columna 3 y 6
rotarod[,c(3,6)]

# Slicing de TODAS la filas y la columna de la 3 a la 6
rotarod[,3:6]

# Equivalencias
3:6==c(3,4,5,6)==seq(3,6,1)
c(3,4,5,6)==seq(3,6,1)

# Cambia el tipo de clase a Matriz (numeric)
rotarod.mtx <- as.matrix(rotarod[,c('s.1','s.2','s.3','s.4')])

# Histograma de los valores de tiempo (seg) todos los animales
hist(rotarod.mtx, breaks = 20, xlab="Tiempo(seg)", ylab="Frecuencia de aparicion", col="gray85",
     main="Tiempo grupal")
abline(v=median(rotarod.mtx), col="red", lty=2)
abline(v=mean(rotarod.mtx), col="blue", lty=2)

# Slicing
rotarod[c(3,8,13,18),c(1,3:6)]

# Indexing & Slicing
for ( subj in unique(rotarod$id) ) {
  print(paste("Sujeto:", subj,""))
  X <- rotarod[rotarod$id==subj, c("s.1", "s.2", "s.3")]
  hist(as.matrix(X), main=subj, xlab="Tiempo(seg)", ylab="Frecuencia de aparicion", col="gray85")
}

# Rotarod en segunda muestra
rotarod$dia[rotarod$edad=="P89"] <- rotarod$dia[rotarod$edad=="P89"]+4

# -------------------------------------------------------------------- #
# Tydir::gather
# Numero unico de ratas
Nratas <- length(unique(rotarod$id))

# Asignar la base de datos reorganizada
df <- gather(rotarod[rotarod$id=="R017", 1:6], "Tiempo", "Segundos", 3:6)
# Guarda la columna dia como factor
df$dia <- as.factor(df$dia)

# Grafica de la rata R3 todos los ensayos todos los dias
ggplot(data=df, aes(x=Tiempo, y=Segundos, group=dia)) +
  geom_line(aes(color=dia))+
  geom_point(aes(color=dia))+
  scale_color_manual(values=viridis(Nratas, end = 0.8)) +
  labs(title=paste("Sujeto","R017"), x ="Ensayo", y = "Segundos")

# TAREA para mañana  graficar todas las ratas con un for Slicing e Indexing
for ( subj in unique(rotarod$id) ) {
  # Asignar la base de datos reorganizada
  print(subj)
  df <- gather(rotarod[rotarod$id==subj, 1:6], "Tiempo", "Segundos", 3:6)
  df$dia <- as.factor(df$dia)
  # Grafica de la rata R3 todos los ensayos todos los dias
  plot <- ggplot(data=df, aes(x=Tiempo, y=Segundos, group=dia)) +
    geom_line(aes(color=dia))+
    geom_point(aes(color=dia))+
    scale_color_manual(values=viridis(Nratas, end = 0.8)) +
    labs(title=paste("Sujeto",subj), x ="Ensayo", y = "Segundos")
  print(plot)
}

# Promedio por dia
rotarod$Tmean <- apply(rotarod[,3:6], 1, mean)
rotarod$Vmean <- apply(rotarod[,7:9], 1, mean)

# Velocidad promedio por dia
ggplot(data=rotarod, aes(x=dia, y=Vmean, group=id)) +
  geom_line(aes(color=id))+
  geom_point(aes(color=id))+
  scale_color_manual(values=plasma(Nratas, end = 0.8)) +
  labs(title="Velocidad (rpm)", x ="Dia", y = "Velocidad promedio")

# Tiempo promedio por dia
ggplot(data=rotarod, aes(x=dia, y=Tmean, group=id)) +
  geom_line(aes(color=id))+
  geom_point(aes(color=id))+
  scale_color_manual(values=plasma(Nratas, end = 0.8)) +
  labs(title="Tiempo (segundo)", x ="Dia", y = "Tiempo promedio")

