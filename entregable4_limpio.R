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
install.packages("ggpubr") # Gràfica de la recta de regresión con correlación, IC95% sobre la recta

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
library(ggpubr)

#Declaramos la libreria de trabajo:
setwd("C:/R/ENTREGABLES/bbdd")

#Comprobamos que la ruta esté bien especificada:
getwd()

#Borramos la información previa:
rm(list = ls())

#Importamos la base de datos:
EvaluacionContinuada_EstadisticaI_2022_1_<-read.dta("EvaluacionContinuada_EstadisticaI_2022 (1).dta")

#a)	Realizar el gráfico de dispersión de la esperanza de vida (variable dependiente) y el % de escolarización (variable independiente) en los 183 países
ggplot(EvaluacionContinuada_EstadisticaI_2022_1_, aes(x=edadm, y=peso)) + geom_point() +
  xlab("Edad de la madre") +
  ylab("Peso al nacer (gramos)")

#Determinamos los parámetros B0 y B1 de la recta de regresión
regresion <- lm(peso ~ edadm, data = EvaluacionContinuada_EstadisticaI_2022_1_)
summary(regresion)

#Ver la normalidad de la edad de la madre (edadm), al ser cuantitativa tenemos que hacer:
Desc_edadm<-descr(EvaluacionContinuada_EstadisticaI_2022_1_$edadm) #canviar el nombre de variable si es necesario

Desc_edadm[10]<-Desc_edadm[10]*100 #canviar el nombre de variable si es necesario

#Creamos un vector con los estadísticos:
estadisticos<-c("Media","Desviación estándar","Mínimo","Q1","Mediana","Q3","Máximo","MAD","Rango intercuartílico","Coeficiente de variación (%)","Asimetria","Error estándar de la Kurtosi", "Kurtosi","Registros válidos","% registros válidos") #No cambiar

#Creamos una base de datos que contenga el nombre de los estadísticos y las columnas de cada variable:
BaseDatos_edadmadre<-data.frame(estadisticos, Desc_edadm)

#Exportamos los datos a un archivo Excel:
wb <- createWorkbook("Resultados") #No canviar
addWorksheet(wb, "Univariado Peso género", gridLines = TRUE) #Poner el nombre de pestaña que más os interese (1a hoja)
writeData(wb, sheet = 1, BaseDatos_edadmadre) #No canviar
saveWorkbook(wb, "Descriptivos_entregable3.xlsx", overwrite = TRUE) #Canviar el nombre "Tabla1.xlsx"  si es necesario

#1.3.	Realizar la exploración gráfica de la edad de la madre (edadm): 
#Histograma y diagrama de cajas:
#EDAD MADRE
#Histograma:
ggplot(EvaluacionContinuada_EstadisticaI_2022_1_, aes(x=edadm)) + # Nombre de la base de datos y la variable
  geom_histogram(color="grey", fill="#C1FFC1") + # podeis cambiar el color si lo deseais
  labs(x="Edad de la madre", y = "Frecuencia absoluta") + #títulos de los ejes
  coord_cartesian(xlim = c(14, 48))  #mirad los percentiles y valor máximo para decidir los límites

# Diagrama de cajas:
#EDAD MADRE
ggplot(data= EvaluacionContinuada_EstadisticaI_2022_1_) + # Nombre de la base de datos 
  geom_boxplot(mapping=aes("var", edadm), fill="#C1FFC1") + # nombre de variable y podeis cambiar el color si lo deseais
  xlab("") + # No cambiar
  ylab("Edad de la madre") + #título del eje y
  scale_x_discrete(breaks=NULL) 

#superposición de la curvatura normal (teneis que introducir a mano la media y desviación estándar):
#Para saber la media y la desviación:
ggplot(EvaluacionContinuada_EstadisticaI_2022_1_, aes(x=edadm)) + # Nombre de la base de datos y la variable
  geom_histogram(aes(y= ..density..), color="grey", fill="#C1FFC1") + # podeis cambiar el color si lo deseais
  stat_function(fun = dnorm, args = list(mean = 32.31, sd = 4.65), col = "red", size =1) +  #cambiad la media y desviación  
  labs(x="Edad de la madre", y = "Frecuencia absoluta") + #títulos de los ejes
  coord_cartesian(xlim = c(14, 48)) #mirad los percentiles y valor máximo para decidir los límites

#Gráfico qqplot:
p <- ggplot(EvaluacionContinuada_EstadisticaI_2022_1_, aes(sample = edadm)) #cambiar base de datos y variable solamente
p + stat_qq() + stat_qq_line(color="red") +
  labs(x="Esperados", y = "Observados")


#CORRELACION DE PEARSON (peso y edad de la madre)
r<-cor(EvaluacionContinuada_EstadisticaI_2022_1_$edadm, EvaluacionContinuada_EstadisticaI_2022_1_$peso, method = "pearson", use ="complete.obs")

r

#Significación estadística del coeficiente de correlación:
cor.test(EvaluacionContinuada_EstadisticaI_2022_1_$edadm, EvaluacionContinuada_EstadisticaI_2022_1_$peso, method = "pearson", use ="complete.obs")


#EDAD GESTACIONAL (SEMANAS) Y EDAD DE LA MADRE:
ggplot(EvaluacionContinuada_EstadisticaI_2022_1_, aes(x=edadm, y=edadgest)) + geom_point() +
  xlab("Edad de la madre") +
  ylab("Edad gestacional (semanas)")


#CORRELACION DE SPEARMAN (edadgestacional y edad de la madre)
r<-cor(EvaluacionContinuada_EstadisticaI_2022_1_$edadm, EvaluacionContinuada_EstadisticaI_2022_1_$edadgest, method = "spearman", use ="complete.obs")

r

#Significación estadística del coeficiente de correlación:
cor.test(EvaluacionContinuada_EstadisticaI_2022_1_$edadm, EvaluacionContinuada_EstadisticaI_2022_1_$edadgest, method = "spearman", use ="complete.obs")

