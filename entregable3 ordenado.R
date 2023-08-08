#Paquetes a utilizar:
install.packages("foreign") #Principalmente lo usaremos para importar archivos Stata (versiones 12 o anteriores) y conserva etiquetas de valores
install.packages("readxl") # importar archivos Excel
install.packages("readr") # Para importar archivos con formato csv y delimitados por carácteres especiales
install.packages("summarytools") # Hacer la descripción de la base de datos
install.packages("dplyr") #Principal paquete para manipular datos
install.packages("table1") # Creación de tablas elegantes en R Studio
install.packages("ggplot2") #Creación de gráficos en R Studio
install.packages("compareGroups") # Comparación de grupos medidas descriptivas
install.packages("gtsummary") # Tablas descriptivas
install.packages("openxlsx") # Exportación a Excel
install.packages("ggplot2") #Gráficos elegantes en R
install.packages("gmodels") # Tablas de doble entrada
install.packages("car") #Test de Levene para varianzas

#Cargamos librerias:
library(foreign)
library(readxl)
library(readr)
library(summarytools)
library(dplyr)
library(table1)
library(ggplot2)
library(compareGroups)
library(gtsummary)
library(openxlsx)
library(ggplot2)
library(gmodels)
library(car)

#Declaramos la libreria de trabajo:
setwd("C:/R/ENTREGABLES/bbdd")

#Comprobamos que la ruta esté bien especificada:
getwd()

#Borramos la información previa:
rm(list = ls())

#importamos la base de datos 

EvaluacionContinuada_EstadisticaI_2022_Version2_1<-read.dta("EvaluacionContinuada_EstadisticaI_2022_version2 (1).dta")

#1.1.	Describir la base de datos y el tipo de variables
dfSummary(EvaluacionContinuada_EstadisticaI_2022_Version2_1)

###Generamos una base de datos para los hombres y para las mujeres separadamente
EvaluacionContinuada_EstadisticaI_2022_Version2_1_H<-filter(EvaluacionContinuada_EstadisticaI_2022_Version2_1, sexo =='Niño')
EvaluacionContinuada_EstadisticaI_2022_Version2_1_D<-filter(EvaluacionContinuada_EstadisticaI_2022_Version2_1, sexo =='Niña')

#Describir las principales medidas de tendencia central, posición, dispersión y distribución del número de visitas en centros de atención primaria y hospitalaria. 

##### PESOOOOOO#####
#Guardamos los estadísticos descriptivos como objetos
desc_peso_H<-descr(EvaluacionContinuada_EstadisticaI_2022_Version2_1_H$peso) #canviar el nombre de variable si es necesario
desc_peso_D<-descr(EvaluacionContinuada_EstadisticaI_2022_Version2_1_D$peso) #canviar el nombre de variable si es necesario

#El coeficiente de variación lo expresamos en %
desc_peso_H[10]<-desc_peso_H[10]*100 #canviar el nombre de variable si es necesario
desc_peso_D[10]<-desc_peso_D[10]*100 #canviar el nombre de variable si es necesario

#Creamos un vector con los estadísticos:
estadisticos<-c("Media","Desviación estándar","Mínimo","Q1","Mediana","Q3","Máximo","MAD","Rango intercuartílico","Coeficiente de variación (%)","Asimetria","Error estándar de la Kurtosi", "Kurtosi","Registros válidos","% registros válidos") #No cambiar

#Creamos una base de datos que contenga el nombre de los estadísticos y las columnas de cada variable:
BaseDatos<-data.frame(estadisticos, desc_peso_H, desc_peso_D)


#Exportamos los datos a un archivo Excel:
wb <- createWorkbook("Resultados") #No canviar
addWorksheet(wb, "Univariado peso y sexo", gridLines = TRUE) #Poner el nombre de pestaña que más os interese (1a hoja)
writeData(wb, sheet = 1, BaseDatos) #No canviar
saveWorkbook(wb, "Tabla_peso.xlsx", overwrite = TRUE) #Canviar el nombre "Tabla1.xlsx"  si es necesario

##Exploracion gráfica NIÑOS##
#Histograma
ggplot(EvaluacionContinuada_EstadisticaI_2022_Version2_1_H, aes(x=peso)) + # Nombre de la base de datos y la variable
  geom_histogram(color="grey", fill="#0000EE") + # podeis cambiar el color si lo deseais
  labs(title="Figura 1: Histogramade del peso en niños.", caption="Fuente: INE", x="Peso", y = "Frecuencia absoluta") + #títulos de los ejes
  coord_cartesian(xlim = c(1900, 4600))   #mirad los percentiles y valor máximo para decidir los límites
#Histograma +curva superpuesta
descr(EvaluacionContinuada_EstadisticaI_2022_Version2_1_H$peso)
ggplot(EvaluacionContinuada_EstadisticaI_2022_Version2_1_H, aes(x=peso)) + # Nombre de la base de datos y la variable
  geom_histogram(aes(y= ..density..), color="grey", fill="#0000EE") + # podeis cambiar el color si lo deseais
  stat_function(fun = dnorm, args = list(mean = 3250.00, sd = 475.98), col = "red", size =2) +  #cambiad la media y desviación  
  labs(x="Peso", y = "Densidad") + #títulos de los ejes
  coord_cartesian(xlim = c(1900, 4600)) + #mirad los percentiles y valor máximo para decidir los límites
  labs(title="Figura 1: Histograma con curva superpuesta del peso en niños.", caption="Fuente: INE")
#Diagrama de cajas
ggplot(data= EvaluacionContinuada_EstadisticaI_2022_Version2_1_H) + # Nombre de la base de datos 
  geom_boxplot(mapping=aes("var", peso), fill="#0000EE") + # nombre de variable y podeis cambiar el color si lo deseais
  xlab("") + # No cambiar
  ylab("Peso") + #título del eje y
  scale_x_discrete(breaks=NULL) +
  labs(title="Figura 2: Diagrama de cajas del peso de los niños.", caption="Fuente: INE") 

#Qq-plot
p <- ggplot(EvaluacionContinuada_EstadisticaI_2022_Version2_1_H, aes(sample = peso)) #cambiar base de datos y variable solamente
p + stat_qq() + stat_qq_line(color="red") +
  labs(x="Esperados", y = "Observados", title="Figura 3: Qq-plot del peso entre los niños.", caption="Fuente: INE")

#Normalidad en los datos -> #Varianzas iguales o desiguales: Test de levene
leveneTest( peso ~ sexo, EvaluacionContinuada_EstadisticaI_2022_Version2_1 )

#Se rechaza hipotesis nula --> T student con varianzas desiguales
t.test(peso ~ sexo, data = EvaluacionContinuada_EstadisticaI_2022_Version2_1, var.equal=FALSE, conf.level = 0.95, alternative = c("two.sided"))

##Exploracion gráfica NIÑAS##
#Histograma
ggplot(EvaluacionContinuada_EstadisticaI_2022_Version2_1_D, aes(x=peso)) + # Nombre de la base de datos y la variable
  geom_histogram(color="grey", fill="#00E5EE") + # podeis cambiar el color si lo deseais
  labs(title="Figura 4: Histogramade del peso y niñas.", caption="Fuente: INE", x="Peso", y = "Frecuencia absoluta") + #títulos de los ejes
  coord_cartesian(xlim = c(1900, 4600)) + #mirad los percentiles y valor máximo para decidir los límites
  labs(title="Figura 4: Histogramade del peso y niñas.", caption="Fuente: INE", x="Peso", y="Frecuencia absoluta") 

#Histograma+curva
descr(EvaluacionContinuada_EstadisticaI_2022_Version2_1_D$peso)
ggplot(EvaluacionContinuada_EstadisticaI_2022_Version2_1_D, aes(x=peso)) + # Nombre de la base de datos y la variable
  geom_histogram(aes(y= ..density..), color="grey", fill="#00E5EE") + # podeis cambiar el color si lo deseais
  stat_function(fun = dnorm, args = list(mean = 3380.0, sd = 502.06), col = "red", size =2) +  #cambiad la media y desviación  
  labs(x="Peso", y = "Densidad") + #títulos de los ejes
  coord_cartesian(xlim = c(1900, 4600)) + #mirad los percentiles y valor máximo para decidir los límites
  labs(title="Figura 4: Histograma con curva superpuesta del peso entre las niñas.", caption="Fuente: INE")

#Diagrama de cajas
ggplot(data= EvaluacionContinuada_EstadisticaI_2022_Version2_1_D) + # Nombre de la base de datos 
  geom_boxplot(mapping=aes("var", peso), fill="#00E5EE") + # nombre de variable y podeis cambiar el color si lo deseais
  xlab("") + # No cambiar
  ylab("Peso") + #título del eje y
  scale_x_discrete(breaks=NULL) +
  labs(title="Figura 5: Diagrama de cajas del peso de las niñas.", caption="Fuente: INE")

#Qp-plot
p <- ggplot(EvaluacionContinuada_EstadisticaI_2022_Version2_1_D, aes(sample = peso)) #cambiar base de datos y variable solamente
p + stat_qq() + stat_qq_line(color="red") +
  labs(x="Esperados", y = "Observados", title="Figura 6: Qq-plot del peso entre las niñas.", caption="Fuente: INE")

#Normalidad en los datos -> #Varianzas iguales o desiguales: Test de levene
leveneTest( peso ~ sexo, EvaluacionContinuada_EstadisticaI_2022_Version2_1 )

#Se rechaza hipotesis nula --> T student con varianzas desiguales
t.test(peso ~ sexo, data = EvaluacionContinuada_EstadisticaI_2022_Version2_1, var.equal=FALSE, conf.level = 0.95, alternative = c("two.sided"))

#####EDAD GESTACIONAL#####
#Guardamos los estadísticos descriptivos como objetos
desc_edadgest_H<-descr(EvaluacionContinuada_EstadisticaI_2022_Version2_1_H$edadgest) #canviar el nombre de variable si es necesario
desc_edadgest_D<-descr(EvaluacionContinuada_EstadisticaI_2022_Version2_1_D$edadgest) #canviar el nombre de variable si es necesario

#El coeficiente de variación lo expresamos en %
desc_edadgest_H[10]<-desc_edadgest_H[10]*100 #canviar el nombre de variable si es necesario
desc_edadgest_D[10]<-desc_edadgest_D[10]*100 #canviar el nombre de variable si es necesario

#Creamos una base de datos que contenga el nombre de los estadísticos y las columnas de cada variable:
BaseDatos<-data.frame(estadisticos, desc_edadgest_H, desc_edadgest_D)

#Exportamos los datos a un archivo Excel:
wb <- createWorkbook("Resultados") #No canviar
addWorksheet(wb, "Univariado edad gestacional y sexo", gridLines = TRUE) #Poner el nombre de pestaña que más os interese (1a hoja)
writeData(wb, sheet = 1, BaseDatos) #No canviar
saveWorkbook(wb, "Tabla_edadgestacional.xlsx", overwrite = TRUE) #Canviar el nombre "Tabla1.xlsx"  si es necesario

##Exploracion gráfica NIÑOS##
#Histograma
ggplot(EvaluacionContinuada_EstadisticaI_2022_Version2_1_H, aes(x=edadgest)) + # Nombre de la base de datos y la variable
  geom_histogram(color="grey", fill="#0000EE") + # podeis cambiar el color si lo deseais
  labs(title="Figura 7: Histogramade de la edad gestacional en niños.", caption="Fuente: INE", x="Edad gestacional", y = "Frecuencia absoluta") + #títulos de los ejes
  coord_cartesian(xlim = c(24, 40))   #mirad los percentiles y valor máximo para decidir los límites
#Histograma +curva superpuesta
descr(EvaluacionContinuada_EstadisticaI_2022_Version2_1_H$edadgest)
ggplot(EvaluacionContinuada_EstadisticaI_2022_Version2_1_H, aes(x=edadgest)) + # Nombre de la base de datos y la variable
  geom_histogram(aes(y= ..density..), color="grey", fill="#0000EE") + # podeis cambiar el color si lo deseais
  stat_function(fun = dnorm, args = list(mean = 39.00, sd = 2.12), col = "red", size =2) +  #cambiad la media y desviación  
  labs(x="Edad gestacional", y = "Densidad") + #títulos de los ejes
  coord_cartesian(xlim = c(24, 40)) + #mirad los percentiles y valor máximo para decidir los límites
  labs(title="Figura 7: Histograma con curva superpuesta de la edad gestacional en niños.", caption="Fuente: INE")
#Diagrama de cajas
ggplot(data= EvaluacionContinuada_EstadisticaI_2022_Version2_1_H) + # Nombre de la base de datos 
  geom_boxplot(mapping=aes("var", edadgest), fill="#0000EE") + # nombre de variable y podeis cambiar el color si lo deseais
  xlab("") + # No cambiar
  ylab("Edad gestacional") + #título del eje y
  scale_x_discrete(breaks=NULL) +
  labs(title="Figura 8: Diagrama de cajas de la edad gestacional de los niños.", caption="Fuente: INE") 

#Qq-plot
p <- ggplot(EvaluacionContinuada_EstadisticaI_2022_Version2_1_H, aes(sample = edadgest)) #cambiar base de datos y variable solamente
p + stat_qq() + stat_qq_line(color="red") +
  labs(x="Esperados", y = "Observados", title="Figura 9: Qq-plot de la edad gestacional entre los niños.")

#Anormalidad en los datos -> #Varianzas iguales o desiguales: Test de 
leveneTest( edadgest ~ sexo, EvaluacionContinuada_EstadisticaI_2022_Version2_1 )


##Exploracion gráfica NIÑAS##
#Histograma
ggplot(EvaluacionContinuada_EstadisticaI_2022_Version2_1_D, aes(x=edadgest)) + # Nombre de la base de datos y la variable
  geom_histogram(color="grey", fill="#00E5EE") + # podeis cambiar el color si lo deseais
  labs(title="Figura 10: Histogramade de la edad gestacional y niñas.", caption="Fuente: INE", x="Edad gestacional", y = "Frecuencia absoluta") + #títulos de los ejes
  coord_cartesian(xlim = c(24, 40)) + #mirad los percentiles y valor máximo para decidir los límites
  labs(title="Figura 10: Histogramade la edad gestacional y niñas.", caption="Fuente: INE", x="Edad gestacional", y="Frecuencia absoluta") 

#Histograma+curva
descr(EvaluacionContinuada_EstadisticaI_2022_Version2_1_D$edadgest)
ggplot(EvaluacionContinuada_EstadisticaI_2022_Version2_1_D, aes(x=edadgest)) + # Nombre de la base de datos y la variable
  geom_histogram(aes(y= ..density..), color="grey", fill="#00E5EE") + # podeis cambiar el color si lo deseais
  stat_function(fun = dnorm, args = list(mean = 38.93, sd = 2.20), col = "red", size =2) +  #cambiad la media y desviación  
  labs(x="Edad gestacional", y = "Densidad") + #títulos de los ejes
  coord_cartesian(xlim = c(24, 40)) + #mirad los percentiles y valor máximo para decidir los límites
  labs(title="Figura 10: Histogramade la edad gestacional y niñas.", caption="Fuente: INE")

#Diagrama de cajas
ggplot(data= EvaluacionContinuada_EstadisticaI_2022_Version2_1_D) + # Nombre de la base de datos 
  geom_boxplot(mapping=aes("var", edadgest), fill="#00E5EE") + # nombre de variable y podeis cambiar el color si lo deseais
  xlab("") + # No cambiar
  ylab("Eadd gestacional") + #título del eje y
  scale_x_discrete(breaks=NULL) +
  labs(title="Figura 10: Diagrama de cajas de la edad gestacional de las niñas.", caption="Fuente: INE")

#Qp-plot
p <- ggplot(EvaluacionContinuada_EstadisticaI_2022_Version2_1_D, aes(sample = edadgest)) #cambiar base de datos y variable solamente
p + stat_qq() + stat_qq_line(color="red") +
  labs(x="Esperados", y = "Observados", title="Figura 11: Qq-plot de la edad gestacional entre las niñas.", caption="Fuente: INE")

ANÁLISIS ESTADIIIIIIIIIIIIIIIIIIIIIIIIIIIIIS

t.test(edadgest ~ sexo, data = EvaluacionContinuada_EstadisticaI_2022_Version2_1, var.equal=TRUE, conf.level = 0.95, alternative = c("greater"))

wilcox.test(edadgest ~ sexo, data = EvaluacionContinuada_EstadisticaI_2022_Version2_1, var.equal=TRUE, conf.level = 0.95, alternative = c("greater"))

wilcox.test(edadgest ~ sexo, data = EvaluacionContinuada_EstadisticaI_2022_Version2_1, exact = FALSE)


