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

#Declaramos la libreria de trabajo:
setwd("C:/R/ENTREGABLES/bbdd")

#Comprobamos que la ruta esté bien especificada:
getwd()

#Borramos la información previa:
rm(list = ls())

#importamos la base de datos 

EvaluacionContinuada_EstadisticaI_2022<-read.dta("EvaluacionContinuada_EstadisticaI_2022 (1).dta")


#1.1.	Describir la base de datos y el tipo de variables


dfSummary(EvaluacionContinuada_EstadisticaI_2022)


#
#Variables resultado cualitativas:  prematuridad (edad gestacional <37 semanas), bajo peso al nacer (peso al nacer <2.500 gramos), bajo peso al nacer a término (peso al nacer <2.500 gramos para los  nacidos en edad gestacional >37 semanas), pequeño para edad gestacional (peso al nacer <percentil 10th), grande para edad gestacional (peso al nacer >percentil 90th).
#Nos quedamos sólo con la variable sexo:
#Hacemos una tabla para ver el valor de texto y cuantos sujetos tenemos:
table(EvaluacionContinuada_EstadisticaI_2022$sexo)
#Para ello utilizamos la función filter: No tiene sentido filtrar nada!!!!!!!!!!!
#Niña<-filter(EvaluacionContinuada_EstadisticaI_2022, sexo == "Niña")
#Niño<-filter(EvaluacionContinuada_EstadisticaI_2022, sexo =="Niño")

#Renombrar variables en la tabla
table1::label(EvaluacionContinuada_EstadisticaI_2022$preterm) <- "Prematuridad"
table1::label(EvaluacionContinuada_EstadisticaI_2022$bajopeso) <- "Bajo peso"
table1::label(EvaluacionContinuada_EstadisticaI_2022$bajopesoterm) <- "Bajo peso a término"
table1::label(EvaluacionContinuada_EstadisticaI_2022$sga) <- "Pequeño por Edad Gestacional"
table1::label(EvaluacionContinuada_EstadisticaI_2022$lga) <- "Grande por Edad Gestacional"
table1::label(EvaluacionContinuada_EstadisticaI_2022$sexo) <- "Sexo"
 #omitimo missings de variable bajopesoterm
EvaluacionContinuada_EstadisticaI_2022_1_cc <- na.omit(EvaluacionContinuada_EstadisticaI_2022)

#1.2.	Hacer la tabla de contingencia

#Utilizaremos la función CrossTable. La variable independiente va primero.

#Por defecto y anulando la la contribución de ja Ji quadrado: #copiar a mano los valores a mano
#tabla pretérmino, frec abs, en %
CrossTable(EvaluacionContinuada_EstadisticaI_2022$preterm, EvaluacionContinuada_EstadisticaI_2022$sexo, prop.chisq = FALSE )
CrossTable(EvaluacionContinuada_EstadisticaI_2022$preterm, EvaluacionContinuada_EstadisticaI_2022$sexo, digits = 2, expected = FALSE, prop.r = FALSE, prop.c =TRUE, prop.t=FALSE,  prop.chisq = FALSE, chisq = FALSE, fisher =FALSE, mcnemar = FALSE, missing.include = FALSE )

CrossTable(EvaluacionContinuada_EstadisticaI_2022$preterm, EvaluacionContinuada_EstadisticaI_2022$sexo, digits = 4, expected = FALSE, prop.r = FALSE, prop.c =TRUE, prop.t=FALSE,  prop.chisq = FALSE, chisq = FALSE, fisher =FALSE, mcnemar = FALSE, missing.include = FALSE )

#Tablabajo peso al nacer
CrossTable(EvaluacionContinuada_EstadisticaI_2022$bajopeso, EvaluacionContinuada_EstadisticaI_2022$sexo, prop.chisq = FALSE )

CrossTable(EvaluacionContinuada_EstadisticaI_2022$bajopeso, EvaluacionContinuada_EstadisticaI_2022$sexo, digits = 2, expected = FALSE, prop.r = FALSE, prop.c =TRUE, prop.t=FALSE,  prop.chisq = FALSE, chisq = FALSE, fisher =FALSE, mcnemar = FALSE, missing.include = FALSE )

CrossTable(EvaluacionContinuada_EstadisticaI_2022$bajopeso, EvaluacionContinuada_EstadisticaI_2022$sexo, digits = 4, expected = FALSE, prop.r = FALSE, prop.c =TRUE, prop.t=FALSE,  prop.chisq = FALSE, chisq = FALSE, fisher =FALSE, mcnemar = FALSE, missing.include = FALSE )

#Tabla bajo peso al nacer a término
CrossTable(EvaluacionContinuada_EstadisticaI_2022_1_cc$bajopesoterm, EvaluacionContinuada_EstadisticaI_2022_1_cc$sexo, prop.chisq = FALSE )

CrossTable(EvaluacionContinuada_EstadisticaI_2022_1_cc$bajopesoterm, EvaluacionContinuada_EstadisticaI_2022_1_cc$sexo, digits = 2, expected = FALSE, prop.r = FALSE, prop.c =TRUE, prop.t=FALSE,  prop.chisq = FALSE, chisq = FALSE, fisher =FALSE, mcnemar = FALSE, missing.include = FALSE )

CrossTable(EvaluacionContinuada_EstadisticaI_2022_1_cc$bajopesoterm, EvaluacionContinuada_EstadisticaI_2022_1_cc$sexo, digits = 4, expected = FALSE, prop.r = FALSE, prop.c =TRUE, prop.t=FALSE,  prop.chisq = FALSE, chisq = FALSE, fisher =FALSE, mcnemar = FALSE, missing.include = FALSE )

#Tabla pequeño para edad gestacional
CrossTable(EvaluacionContinuada_EstadisticaI_2022$sga, EvaluacionContinuada_EstadisticaI_2022$sexo, prop.chisq = FALSE )

CrossTable(EvaluacionContinuada_EstadisticaI_2022$sga, EvaluacionContinuada_EstadisticaI_2022$sexo, digits = 2, expected = FALSE, prop.r = FALSE, prop.c =TRUE, prop.t=FALSE,  prop.chisq = FALSE, chisq = FALSE, fisher =FALSE, mcnemar = FALSE, missing.include = FALSE )

CrossTable(EvaluacionContinuada_EstadisticaI_2022$sga, EvaluacionContinuada_EstadisticaI_2022$sexo, digits = 4, expected = FALSE, prop.r = FALSE, prop.c =TRUE, prop.t=FALSE,  prop.chisq = FALSE, chisq = FALSE, fisher =FALSE, mcnemar = FALSE, missing.include = FALSE )

#Tabla grande para edad gestacional
CrossTable(EvaluacionContinuada_EstadisticaI_2022$lga, EvaluacionContinuada_EstadisticaI_2022$sexo, prop.chisq = FALSE )

CrossTable(EvaluacionContinuada_EstadisticaI_2022$lga, EvaluacionContinuada_EstadisticaI_2022$sexo, digits = 2, expected = FALSE, prop.r = FALSE, prop.c =TRUE, prop.t=FALSE,  prop.chisq = FALSE, chisq = FALSE, fisher =FALSE, mcnemar = FALSE, missing.include = FALSE )

CrossTable(EvaluacionContinuada_EstadisticaI_2022$lga, EvaluacionContinuada_EstadisticaI_2022$sexo, digits = 4, expected = FALSE, prop.r = FALSE, prop.c =TRUE, prop.t=FALSE,  prop.chisq = FALSE, chisq = FALSE, fisher =FALSE, mcnemar = FALSE, missing.include = FALSE )

#DIAGRAMA DE BARRAS #apilado
#Nacidos preterm
ggplot(EvaluacionContinuada_EstadisticaI_2022, aes(x = sexo, fill = preterm  )) + geom_bar() +
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1)) +
  labs(title="Figura 1: Diagrama de barras apilado según los nacimientos prematuros según el sexo.", caption="Fuente: INE", x="Sexo", y="Frecuencia absoluta") +
  scale_fill_discrete(name = "Nacidos pretérmino", labels = c("No", "Sí"))+   
  scale_fill_manual(values =c("#B9D3EE","#4A708B"),name = "Nacidos pretérmino")
  
#Bajopesoalnacer
  ggplot(EvaluacionContinuada_EstadisticaI_2022, aes(x = sexo, fill = bajopeso  )) + geom_bar() +
    theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1)) +
    labs(title="Figura 2: Diagrama de barras apilado según los nacimientos que presentaron bajo peso según el sexo.", caption="Fuente: INE", x="Sexo", y="Frecuencia absoluta") +
    scale_fill_discrete(name = "Bajo peso", labels = c("No", "Sí"))+
    scale_fill_manual(values =c("#FF8247","#8B4726"), name = "Bajo peso")


 #Bajopesoatérmino
    ggplot(EvaluacionContinuada_EstadisticaI_2022_1_cc, aes(x = sexo, fill = bajopesoterm  )) + geom_bar() +
      theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1)) +
      labs(title="Figura 3: Diagrama de barras apilado de nacimientos de bajo peso a término según el sexo.", caption="Fuente: INE", x="Sexo", y="Frecuencia absoluta") +
      scale_fill_discrete(name = "Bajo peso a término", labels = c("No", "Sí"))+
      scale_fill_manual(values =c("#00FF7F","#008B45"), name = "Bajo peso a término")
      
  #sga
      ggplot(EvaluacionContinuada_EstadisticaI_2022, aes(x = sexo, fill = sga  )) + geom_bar() +
        theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1)) +
        labs(title="Figura 4: Diagrama de barras apilado según los nacimientos un tamaño pequeños según el sexo.", caption="Fuente: INE", x="Sexo", y="Frecuencia absoluta") +
        scale_fill_discrete(name = "Pequeño según edad gestacional", labels = c("No", "Sí"))+
        scale_fill_manual(values =c("#EEA9B8","#8B475D"), name = "Pequeño según edad gestacional")
  
  #lga
        ggplot(EvaluacionContinuada_EstadisticaI_2022, aes(x = sexo, fill = lga  )) + geom_bar() +
          theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1)) +
          labs(title="Figura 5: Diagrama de barras apilado según los nacimientos un tamaño grande según el sexo.", caption="Fuente: INE", x="Sexo", y="Frecuencia absoluta") +
          scale_fill_discrete(name = "Grande según edad gestacional", labels = c("No", "Sí"))+
          scale_fill_manual(values =c("#FFA07A","#8B2500"), name = "Grande según edad gestacional")

#DIAGRAMA DE BARRAS #segmentado
#Nacidos preterm
 ggplot(EvaluacionContinuada_EstadisticaI_2022, aes(x = sexo, fill = preterm  )) + geom_bar(position = position_dodge()) +
    theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+
    labs(title="Figura 1: Diagrama de barras segmentado  de los nacimientos prematuros según el sexo.", caption="Fuente: INE", x="Sexo", y="Frecuencia absoluta") +
    scale_fill_discrete(name = "Nacidos prematuros", labels = c("No", "Sí"))+
   scale_fill_manual(values =c("#B9D3EE","#4A708B"), name = "Nacidos prematuros")
    

#Bajopesoalnacer
  ggplot(EvaluacionContinuada_EstadisticaI_2022, aes(x = sexo, fill = bajopeso  )) + geom_bar(position = position_dodge()) +
    theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1)) +
    labs(title="Figura 2: Diagrama de barras segmentado de los nacimientos de bajo peso según el sexo.", caption="Fuente: INE", x="Sexo", y="Frecuencia absoluta") +
    scale_fill_discrete(name = "Bajo peso", labels = c("No", "Sí"))+
    scale_fill_manual(values =c("#FF8247","#8B4726"), name = "Bajo peso")

          
#Bajopesoatérmino
  ggplot(EvaluacionContinuada_EstadisticaI_2022_1_cc, aes(x = sexo, fill = bajopesoterm  )) + geom_bar(position = position_dodge()) +
    theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1)) +
    labs(title="Figura 3: Diagrama de barras segmentado del bajo peso a término según el sexo.", caption="Fuente: INE", x="Sexo", y="Frecuencia absoluta") +
    scale_fill_discrete(name = "Bajo peso a término", labels = c("No", "Sí"))+
    scale_fill_manual(values =c("#00FF7F","#008B45"), name = "Bajo peso a término")
          
#sga
  ggplot(EvaluacionContinuada_EstadisticaI_2022, aes(x = sexo, fill = sga  )) + geom_bar(position = position_dodge()) +
    theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1)) +
    labs(title="Figura 4: Diagrama de barras segmentado del tamaño pequeños según el sexo del recién nacido.", caption="Fuente: INE", x="Sexo", y="Frecuencia absoluta") +
    scale_fill_discrete(name = "Pequeño según edad gestacional", labels = c("No", "Sí"))+
    scale_fill_manual(values =c("#EEA9B8","#8B475D"), name = "Pequeño según edad gestacional")
          
 #lga
  ggplot(EvaluacionContinuada_EstadisticaI_2022, aes(x = sexo, fill = lga  )) + geom_bar(position = position_dodge()) +
    theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1)) +
    labs(title="Figura 5: Diagrama de barras segmentado del tamaño grande según el sexo del recién nacido.", caption="Fuente: INE", x="Sexo", y="Frecuencia absoluta") +
    scale_fill_discrete(name = "Grande según edad gestacional", labels = c("No", "Sí"))+
    scale_fill_manual(values =c("#FFA07A","#8B2500"), name = "Grande según edad gestacional")
      
#VALORES ESPERADOS Y TEST
#preterm
CrossTable(EvaluacionContinuada_EstadisticaI_2022$preetrm, EvaluacionContinuada_EstadisticaI_2022$sexo, digits = 2, expected = TRUE, prop.r = FALSE, prop.c =FALSE, prop.t=FALSE,  prop.chisq = FALSE, chisq = FALSE, fisher =FALSE, mcnemar = FALSE, missing.include = FALSE )
#Plantead la hipótesis, realizad el test y dar la conclusión según los resultados obtenidos. Justificad las respuestas
CrossTable(EvaluacionContinuada_EstadisticaI_2022$preterm, EvaluacionContinuada_EstadisticaI_2022$sexo, digits = 2, expected = TRUE, prop.r = FALSE, prop.c =FALSE, prop.t=FALSE,  prop.chisq = FALSE, chisq = TRUE, fisher =FALSE, mcnemar = FALSE, missing.include = FALSE )

#Bajopesoalnacer
CrossTable(EvaluacionContinuada_EstadisticaI_2022$bajopeso, EvaluacionContinuada_EstadisticaI_2022$sexo, digits = 2, expected = TRUE, prop.r = FALSE, prop.c =FALSE, prop.t=FALSE,  prop.chisq = FALSE, chisq = FALSE, fisher =FALSE, mcnemar = FALSE, missing.include = FALSE )

CrossTable(EvaluacionContinuada_EstadisticaI_2022$bajopeso, EvaluacionContinuada_EstadisticaI_2022$sexo, digits = 2, expected = TRUE, prop.r = FALSE, prop.c =FALSE, prop.t=FALSE,  prop.chisq = FALSE, chisq = TRUE, fisher =FALSE, mcnemar = FALSE, missing.include = FALSE )

#bajopesoatermino
CrossTable(EvaluacionContinuada_EstadisticaI_2022_1_cc$bajopesoterm, EvaluacionContinuada_EstadisticaI_2022_1_cc$sexo, digits = 2, expected = TRUE, prop.r = FALSE, prop.c =FALSE, prop.t=FALSE,  prop.chisq = FALSE, chisq = FALSE, fisher =FALSE, mcnemar = FALSE, missing.include = FALSE )

CrossTable(EvaluacionContinuada_EstadisticaI_2022_1_cc$bajopesoterm, EvaluacionContinuada_EstadisticaI_2022_1_cc$sexo, digits = 2, expected = TRUE, prop.r = FALSE, prop.c =FALSE, prop.t=FALSE,  prop.chisq = FALSE, chisq = TRUE, fisher =FALSE, mcnemar = FALSE, missing.include = FALSE )

#sga
CrossTable(EvaluacionContinuada_EstadisticaI_2022$sga, EvaluacionContinuada_EstadisticaI_2022$sexo, digits = 2, expected = TRUE, prop.r = FALSE, prop.c =FALSE, prop.t=FALSE,  prop.chisq = FALSE, chisq = FALSE, fisher =FALSE, mcnemar = FALSE, missing.include = FALSE )

CrossTable(EvaluacionContinuada_EstadisticaI_2022$sga, EvaluacionContinuada_EstadisticaI_2022$sexo, digits = 2, expected = TRUE, prop.r = FALSE, prop.c =FALSE, prop.t=FALSE,  prop.chisq = FALSE, chisq = TRUE, fisher =FALSE, mcnemar = FALSE, missing.include = FALSE )

#lga
CrossTable(EvaluacionContinuada_EstadisticaI_2022$lga, EvaluacionContinuada_EstadisticaI_2022$sexo, digits = 2, expected = TRUE, prop.r = FALSE, prop.c =FALSE, prop.t=FALSE,  prop.chisq = FALSE, chisq = FALSE, fisher =FALSE, mcnemar = FALSE, missing.include = FALSE )

CrossTable(EvaluacionContinuada_EstadisticaI_2022$lga, EvaluacionContinuada_EstadisticaI_2022$sexo, digits = 2, expected = TRUE, prop.r = FALSE, prop.c =FALSE, prop.t=FALSE,  prop.chisq = FALSE, chisq = TRUE, fisher =FALSE, mcnemar = FALSE, missing.include = FALSE )

