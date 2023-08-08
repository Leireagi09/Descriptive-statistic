#Paquetes a utilizar:
install.packages("foreign") #Principalmente lo usaremos para importar archivos Stata (versiones 12 o anteriores) y conserva etiquetas de valores
install.packages("readxl") # importar archivos Excel
install.packages("readr") # Para importar archivos con formato csv y delimitados por carácteres especiales
install.packages("summarytools") # Hacer la descripción de la base de datos
install.packages("dplyr") #Principal paquete para manipular datos
install.packages("table1") # Creación de tablas elegantes en R Studio
install.packages("ggplot2") #Creación de gráficos en R Studio
options("install.lock"=FALSE)
install.packages("haven")
install.packages('rmarkdown', dependencies = TRUE)

#Cargamos librerias:
library(foreign)
library(readxl)
library(readr)
library(summarytools)
library(dplyr)
library(table1)
library(ggplot2)
library(haven)
library(rmarkdown)

#declaramos la ruta de trabajo
setwd("C:/R/ENTREGABLE 1")

#comprobar que la ruta esta bien instalada
getwd()

#Eliminar objetos anteriores
rm(list = ls())

#importacion de archivo stata
EvaluacionContinuada_EstadisticaI_2022_1_ <- read.dta("EvaluacionContinuada_EstadisticaI_2022 (1).dta")

#Entregable 1. ¿Cómo se distribuyen las variables resultado de este estudio?
#Pregunta 1: tipos de variables
dfSummary(EvaluacionContinuada_EstadisticaI_2022_1_)
#Id: variable cualitativa nominal politónica
#Peso: cuantitativa continua
#edadgest: cuantitiva continua
#preterm: v cualitativa nominal dicotómica
#bajopeso: cualitativa nominal dicotómica
#bajopesoterm: cuali nominal dicotomica
#sga: cuali nominal dicotómica
#lga: cuali nominal dicot
#sexo: cuali nominal dicot
#edadm: cuantitativa continua

#Pregunta 2: Realiza el análisis descriptivo univariado de cada una de las variables resultado cualitativas. 
#2.a.Presenta los resultados mediante una table descriptiva y gráficos. 
#Etiquetamos variables
table1::label(EvaluacionContinuada_EstadisticaI_2022_1_$preterm) <- "Prematuridad"
table1::label(EvaluacionContinuada_EstadisticaI_2022_1_$bajopeso) <- "Bajo peso"
table1::label(EvaluacionContinuada_EstadisticaI_2022_1_$bajopesoterm) <- "Bajo peso a término"
table1::label(EvaluacionContinuada_EstadisticaI_2022_1_$sga) <- "Pequeño por Edad Gestacional"
table1::label(EvaluacionContinuada_EstadisticaI_2022_1_$lga) <- "Grande por Edad Gestacional"
table1::label(EvaluacionContinuada_EstadisticaI_2022_1_$sexo) <- "Sexo"



#En el caso de que se quisieran hacer las tablas individualmente
table1::table1(~preterm,  data = EvaluacionContinuada_EstadisticaI_2022_1_)
table1::table1(~bajopeso,  data = EvaluacionContinuada_EstadisticaI_2022_1_)
table1::table1(~bajopesoterm,  data = EvaluacionContinuada_EstadisticaI_2022_1_)
table1::table1(~sga,  data = EvaluacionContinuada_EstadisticaI_2022_1_)
table1::table1(~lga,  data = EvaluacionContinuada_EstadisticaI_2022_1_)
table1::table1(~sexo,  data = EvaluacionContinuada_EstadisticaI_2022_1_)

#Tabla completa eliminando variable bajopesoterm
table1::table1(~ preterm + bajopeso + bajopesoterm+ sga + lga, data = EvaluacionContinuada_EstadisticaI_2022_1_)

#Tabla variable con NA   
#Omitimos missings
EvaluacionContinuada_EstadisticaI_2022_1_cc <- na.omit(EvaluacionContinuada_EstadisticaI_2022_1_)
#Etiquetamos en tabla sin NA
table1::label(EvaluacionContinuada_EstadisticaI_2022_1_cc$bajopesoterm) <- "Bajo peso al nacer"
#Convertir factor
EvaluacionContinuada_EstadisticaI_2022_1_cc$bajopesoterm<-factor(EvaluacionContinuada_EstadisticaI_2022_1_cc$bajopesoterm, levels=c(0, 1), labels=c('No', 'Si'))
#Tabla
table1::table1(~ bajopesoterm, data = EvaluacionContinuada_EstadisticaI_2022_1_cc, render.missing=NULL)

table1::label(EvaluacionContinuada_EstadisticaI_2022_1_cc$preterm) <- "Prematuridad"
table1::label(EvaluacionContinuada_EstadisticaI_2022_1_cc$bajopeso) <- "Bajo peso"
table1::label(EvaluacionContinuada_EstadisticaI_2022_1_cc$bajopesoterm) <- "Bajo peso a término"
table1::label(EvaluacionContinuada_EstadisticaI_2022_1_cc$sga) <- "Pequeño por Edad Gestacional"
table1::label(EvaluacionContinuada_EstadisticaI_2022_1_cc$lga) <- "Grande por Edad Gestacional"
table1::label(EvaluacionContinuada_EstadisticaI_2022_1_cc$sexo) <- "Sexo"

table1::table1(~ preterm + bajopeso + bajopesoterm+ sga + lga, data = EvaluacionContinuada_EstadisticaI_2022_1_cc)
#Gráficos por variable
#1
require(scales)

EvaluacionContinuada_EstadisticaI_2022_1_ %>% 
  count( preterm) %>% 
  mutate(pct = prop.table(n)) %>% 
  ggplot(aes(x = preterm, y = pct, fill = preterm, label = scales::percent(pct))) + 
  geom_col(position = 'dodge') + 
  geom_text(position = position_dodge(width = .9),    # move to center of bars
            vjust = -0.5,    # nudge above top of bar
            size = 4) + 
  scale_y_continuous(labels = scales::percent) +
  labs(title="Figura 1: Nacimientos prematuros",caption="Fuente: INE",x="Nacimientos prematuros",
       y="Porcentaje (%)") +
  theme(axis.text.x = element_text(angle = 0, hjust=1)) +
  guides(fill=guide_legend(title="Nacimiento prematuro")) +
  theme(legend.position="none") 


#2
require(scales)

EvaluacionContinuada_EstadisticaI_2022_1_ %>% 
  count( bajopeso) %>% 
  mutate(pct = prop.table(n)) %>% 
  ggplot(aes(x = bajopeso, y = pct, fill = bajopeso, label = scales::percent(pct))) + 
  geom_col(position = 'dodge') + 
  geom_text(position = position_dodge(width = .9),    # move to center of bars
            vjust = -0.5,    # nudge above top of bar
            size = 4) + 
  scale_y_continuous(labels = scales::percent) +
  labs(title="Figura 2: Bajo peso al nacer",caption="Fuente: INE",x="Bajo peso al nacer",
       y="Porcentaje (%)") +
  theme(axis.text.x = element_text(angle = 0, hjust=1)) +
  guides(fill=guide_legend(title="Bajo peso al nacer")) +
  theme(legend.position="none") 

#3
require(scales)


EvaluacionContinuada_EstadisticaI_2022_1_cc %>% 
  count( bajopesoterm) %>% 
  mutate(pct = prop.table(n)) %>% 
  ggplot(aes(x = bajopesoterm, y = pct, fill = bajopesoterm, label = scales::percent(pct))) + 
  geom_col(position = 'dodge') + 
  geom_text(position = position_dodge(width = .9),    # move to center of bars
            vjust = -0.5,    # nudge above top of bar
            size = 4) + 
  scale_y_continuous(labels = scales::percent) +
  labs(title="Figura 3: Bajo peso a término",caption="Fuente: INE",x="Bajo pero a término",
       y="Porcentaje (%)") +
  theme(axis.text.x = element_text(angle = 0, hjust=1)) +
  guides(fill=guide_legend(title="none")) +
  theme(legend.position="none")
 

#4
require(scales)

EvaluacionContinuada_EstadisticaI_2022_1_ %>% 
  count( sga) %>% 
  mutate(pct = prop.table(n)) %>% 
  ggplot(aes(x = sga, y = pct, fill = sga, label = scales::percent(pct))) + 
  geom_col(position = 'dodge') + 
  geom_text(position = position_dodge(width = .9),    # move to center of bars
            vjust = -0.5,    # nudge above top of bar
            size = 4) + 
  scale_y_continuous(labels = scales::percent) +
  labs(title="Figura 4: Pequeño según edad gestacional",caption="Fuente: INE",x="Pequeño según edad gestacional",
       y="Porcentaje (%)") +
  theme(axis.text.x = element_text(angle = 0, hjust=1)) +
  guides(fill=guide_legend(title="Bequeño por edad gestacional")) +
  theme(legend.position="none")



#5
require(scales)
EvaluacionContinuada_EstadisticaI_2022_1_ %>% 
  count( lga) %>% 
  mutate(pct = prop.table(n)) %>% 
  ggplot(aes(x = lga, y = pct, fill = lga, label = scales::percent(pct))) + 
  geom_col(position = 'dodge') + 
  geom_text(position = position_dodge(width = .9),    # move to center of bars
            vjust = -0.5,    # nudge above top of bar
            size = 4) + 
  scale_y_continuous(labels = scales::percent) +
  labs(title="Figura 5: Grande según edad gestacional",caption="Fuente: INE",x="Grande segun edad gestacional",
       y="Porcentaje (%)") +
  theme(axis.text.x = element_text(angle = 0, hjust=1)) +
  guides(fill=guide_legend(title="Grande según edad gestacional")) +
  theme(legend.position="none") 


#6

require(scales)

EvaluacionContinuada_EstadisticaI_2022_1_ %>% 
  count( sexo) %>% 
  mutate(pct = prop.table(n)) %>% 
  ggplot(aes(x = sexo, y = pct, fill = sexo, label = scales::percent(pct))) + 
  geom_col(position = 'dodge') + 
  geom_text(position = position_dodge(width = .9),    # move to center of bars
            vjust = -0.5,    # nudge above top of bar
            size = 4) + 
  scale_y_continuous(labels = scales::percent) +
  labs(title="Figura 6: Tamaño según edad gestacional",caption="Fuente: INE",x="Grande según edad gestacional",
       y="Porcentaje (%)") +
  theme(axis.text.x = element_text(angle = 0, hjust=1)) +
  guides(fill=guide_legend(title="Grande según edad gestacional")) +
  theme(legend.position="none") 





