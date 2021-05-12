#ORT 
#10, 60, 240, 2880 Min 

# cargar datos
ORT <- read.csv('~/github/Analisis_ratas/ORT/ORT.csv')

# Organiza medias
ORT.ses <- aggregate(TIEMPO ~ SES + OBJETO, data = ORT, mean)

# Calcula la desviacion estandar
ORT.ses$sd <- aggregate(TIEMPO ~ SES + OBJETO, data = ORT, mean)[,3]

# Orgniza los datos por sesion
ORT.ses <- ORT.ses[order(ORT.ses$SES),]

# Grafica los datos
Col <- c("#7AD151FF", "#7AD151FF", "#440154FF", "#39568CFF", "#440154FF", "#39568CFF", "#440154FF", "#39568CFF")
barCenters <- barplot(height = ORT.ses$TIEMPO, ylim = c(0,32), names.arg = ORT.ses$OBJETO,
                      main = "Reconocimiento de Objetos", xlab = "Sesión", ylab = "Tiempo", border = Col, col = alpha(Col, 0.5))
arrows(barCenters, ORT.ses$TIEMPO-ORT.ses$sd, col = Col,
       barCenters, ORT.ses$TIEMPO+ORT.ses$sd,angle=90,code=3)
axis(1, at=barCenters, ORT.ses$SES, pos = -1, tick = NA, col = NA)


