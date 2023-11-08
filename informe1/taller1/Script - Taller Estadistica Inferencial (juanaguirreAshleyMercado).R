library(readr)
SaberProData <- read_delim("https://raw.githubusercontent.com/naticah21/DS_Education_Uninorte/master/SaberPro2016_2017.csv", 
                           delim = ";", escape_double = FALSE, trim_ws = TRUE)

SaberProDataGrING <- SaberProData[SaberProData$GRUPO_REFERENCIA=="INGENIERÍA",]




library(ggplot2)
library(dplyr)
library(viridis)
library(stringr)



#figura 1



library(ggplot2) 

library(viridis) 

library(dplyr) 



PROMEDIOPERCENTIL_GRUPOREFERENCIA <- SaberProData %>%  
  
  group_by(GRUPO_REFERENCIA) %>%  
  
  summarize(promedio = mean(PUNTAJE_GLOBAL)) 



PROMEDIO_PROMEDIOGLOBAL_GRUPOREFERENCIAS <- mean(PROMEDIOPERCENTIL_GRUPOREFERENCIA$promedio) 



colores <- viridis::viridis_pal(option = "turbo")(6) 



ggplot(data = PROMEDIOPERCENTIL_GRUPOREFERENCIA, aes(x = reorder(GRUPO_REFERENCIA, promedio), y = promedio, fill = promedio)) +  
  
  geom_bar(stat = "identity", position = "dodge") +  
  
  geom_hline(aes(yintercept = PROMEDIO_PROMEDIOGLOBAL_GRUPOREFERENCIAS, linetype = "Promedio General"),  
             
             linetype = "dashed", color = "red") +  
  
  labs(x = "Grupo de Referencia", y = "Promedio",  
       
       title = "Puntajes Globales por Grupo y Promedio General") +  
  
  scale_fill_gradientn(colors = colores, name = "Promedio") +  
  
  scale_linetype_manual(name = "Leyenda", values = c("Promedio General" = "dashed")) +  
  
  theme_minimal() +  
  
  theme( 
    
    axis.text.x = element_text(angle = 45, hjust = 1), 
    
    plot.title = element_text(size = 14), 
    
    plot.margin = margin(0.5, 0.5, 0.5, 0.5, "cm") 
    
  ) +  
  
  geom_text(aes(label = round(promedio, 2)), vjust = -0.5, position = position_dodge(width = 0.9)) +  
  
  guides(fill = FALSE) +  
  
  theme(legend.position = "none") +  
  
  annotate("text", x = 0.5, y = PROMEDIO_PROMEDIOGLOBAL_GRUPOREFERENCIAS + 2,  
           
           label = paste("Promedio General:", round(PROMEDIO_PROMEDIOGLOBAL_GRUPOREFERENCIAS, 2)),  
           
           
           
           color = "red") 




#figura 2

library(ggplot2) 



colores <- c("#849324", "#FFB30F", "#FD151B")   

ggplot() + 
  
  geom_boxplot(data = SaberProData, aes(x = "Lectura Crítica", y = PERCENTIL_NACIONALPRUEBA.LectCritica, fill = "Lectura Crítica"), width = 0.5) + 
  
  geom_boxplot(data = SaberProData, aes(x = "Razonamiento Cuantitativo", y = PERCENTIL_NACIONALPRUEBA.RazCuanti, fill = "Razonamiento Cuantitativo"), width = 0.5) +	 
  
  geom_boxplot(data = SaberProData, aes(x = "Competencias Ciudadanas", y = PERCENTIL_NACIONALPRUEBA.CCiudadanas, fill = "Competencias Ciudadanas"), width = 0.5) + 
  
  labs(title = "Distribuciones de Pruebas a nivel Nacional", 
       
       x = "Prueba", 
       
       y = "Percentil Nacional") + 
  
  scale_fill_manual(values = colores) + 
  
  theme_minimal() + 
  
  theme(legend.position = "bottom") 
#figura 3


library(ggplot2) 

library(viridis) 



ggplot(SaberProData, aes(x = PUNTAJE_PRUEBA.LectCritica, y = PUNTAJE_PRUEBA.RazCuanti)) + 
  
  geom_point(aes(color = PUNTAJE_PRUEBA.CCiudadanas), size = 3) + 
  
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "red") + 
  
  coord_fixed(ratio = 1, xlim = c(0, max(SaberProData[, variables_numericas])), ylim = c(0, max(SaberProData[, variables_numericas]))) + 
  
  scale_color_viridis_c(option = "cividis") +   
  
  labs(title = "Correlación entre Lectura Crítica y Razonamiento Cuantitativo", 
       
       x = "Puntaje Lectura Crítica", 
       
       y = "Puntaje Razonamiento Cuantitativo") + 
  
  theme_minimal() 

#figura 4
library(ggplot2) 

library(dplyr) 



ingenierias <- c("INGENIERIA INDUSTRIAL", "INGENIERIA MECANICA", 
                 
                 "INGENIERIA ELECTRONICA", "INGENIERIA CIVIL", 
                 
                 "INGENIERIA DE SISTEMAS Y COMPUTACION", "INGENIERIA ELECTRICA") 



SaberProDataIngenierias <- SaberProData %>% 
  
  filter(PROGRAMA %in% ingenierias) 



colores_ingenierias <- c("#1f77b4", "#ff7f0e", "#2ca02c", "#d62728", "#9467bd", "#8c564b") 



ggplot(SaberProDataIngenierias, aes(x = PROGRAMA, y = PUNTAJE_GLOBAL, fill = PROGRAMA)) + 
  
  geom_boxplot() + 
  
  labs(title = "Promedio del Rendimiento en las Tres Pruebas para Ingenierías", 
       
       x = "Carrera de Ingeniería", 
       
       y = "Promedio de Rendimiento") + 
  
  scale_fill_manual(values = colores_ingenierias) +      	theme_minimal() + 
  
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 

#figura 5
library(ggplot2) 

library(dplyr) 



promedios_por_carrera <- SaberProDataGrING %>% 
  
  group_by(PROGRAMA) %>% 
  
  summarise(Promedio = mean(PUNTAJE_GLOBAL)) 



ggplot(promedios_por_carrera, aes(x = PROGRAMA, y = Promedio, fill = PROGRAMA)) + 
  
  geom_bar(stat = "identity") + 
  
  geom_text(aes(label = round(Promedio, 2)), vjust = -0.5, size = 3) +   
  
  labs(title = "Promedio de Puntajes Globales por Carrera de Ingeniería", 
       
       x = "Carrera de Ingeniería", 
       
       y = "Promedio de Puntaje Global") + 
  
  scale_fill_viridis_d(option = "plasma") +   
  
  theme_minimal() + 
  
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 

#figura 6
library(ggplot2) 

library(dplyr) 

ingenierias <- c("INGENIERIA INDUSTRIAL", "INGENIERIA MECANICA", 
                 
                 "INGENIERIA ELECTRONICA", "INGENIERIA CIVIL", 
                 
                 "INGENIERIA DE SISTEMAS Y COMPUTACION", "INGENIERIA ELECTRICA") 



SaberProDataIngenierias <- SaberProData %>% 
  
  filter(PROGRAMA %in% ingenierias) 




ggplot(SaberProDataIngenierias, aes(x = PUNTAJE_PRUEBA.LectCritica, y = PUNTAJE_PRUEBA.RazCuanti)) + 
  
  stat_density_2d(aes(fill = after_stat(density)), geom = "tile", contour = FALSE, alpha = 0.5) + 
  
  geom_point(size = 3, shape = 21, color = "black") + 
  
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "red") + 
  
  labs(title = "Correlación entre Lectura Crítica y Razonamiento Cuantitativo para Ingenierías", 
       
       x = "Puntaje Prueba de Lectura Crítica", 
       
       y = "Puntaje Prueba de Razonamiento Cuantitativo") + 
  
  scale_fill_viridis_c(name = "Densidad") +  # Utilizar la paleta de colores de viridis 
  
  theme_minimal() 


#figura 7
library(ggplot2) 

library(dplyr) 

library(viridis) 



ingenierias <- c("INGENIERIA INDUSTRIAL", "INGENIERIA MECANICA", 
                 
                 "INGENIERIA ELECTRONICA", "INGENIERIA CIVIL", 
                 
                 "INGENIERIA DE SISTEMAS Y COMPUTACION", "INGENIERIA ELECTRICA") 



SaberProDataGrING <- SaberProData %>% 
  
  filter(PROGRAMA %in% ingenierias) 



estudiantes_por_carrera <- SaberProDataGrING %>% 
  
  group_by(PROGRAMA) %>% 
  
  summarise(num_estudiantes = n()) 



promedio_estudiantes <- mean(estudiantes_por_carrera$num_estudiantes) 



ggplot(estudiantes_por_carrera, aes(x = PROGRAMA, y = num_estudiantes, fill = num_estudiantes)) + 
  
  geom_bar(stat = "identity") + 
  
  geom_hline(yintercept = promedio_estudiantes, color = "red", linetype = "dashed") + 
  
  geom_text(aes(label = num_estudiantes), vjust = -0.5, size = 4) +   
  
  labs(title = "Número de Estudiantes por Carrera de Ingeniería", 
       
       x = "Carrera de Ingeniería", 
       
       y = "Número de Estudiantes") + 
  
  scale_fill_viridis(option = "D", name = "Número de Estudiantes") + 
  
  theme_minimal() + 
  
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 

#figura 8
color_palette <- colorRampPalette(c("red", "yellow", "green"))(100) 



hist(SaberProDataGrADMN$PUNTAJE_GLOBAL, breaks = 80,  
     
     main = "Distribución de Puntajes Globales",  
     
     col = color_palette,  
     
     density = 80,    
     
     border = "black") 

#figura 9
library(ggplot2) 

library(dplyr) 



SaberProDataGrADMN <- SaberProData[SaberProData$GRUPO_REFERENCIA == "ADMINISTRACIÓN Y AFINES",] 



programas_admn <- c("NEGOCIOS INTERNACIONALES", "ADMINISTRACION DE EMPRESAS") 



SaberProDataAdmnProgramas <- SaberProDataGrADMN %>% 
  
  filter(PROGRAMA %in% programas_admn) 



num_estudiantes_por_programa <- SaberProDataAdmnProgramas %>% 
  
  group_by(PROGRAMA) %>% 
  
  summarise(num_estudiantes = n()) 



promedio_num_estudiantes <- mean(num_estudiantes_por_programa$num_estudiantes) 



colores_viridis <- viridis::viridis_pal()(2) 



ggplot(num_estudiantes_por_programa, aes(x = reorder(PROGRAMA, -num_estudiantes), y = num_estudiantes, fill = PROGRAMA)) + 
  
  geom_bar(stat = "identity") + 
  
  geom_hline(yintercept = promedio_num_estudiantes, linetype = "dashed", color = "red") + 
  
  labs(title = "Número de Estudiantes por Programa (Administración y Afinidades)", 
       
       x = "Programa", 
       
       y = "Número de Estudiantes") + 
  
  scale_fill_manual(values = colores_viridis) +  # Asignar los colores 
  
  theme_minimal() + 
  
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  
  geom_text(aes(label = num_estudiantes), vjust = -0.5, size = 3) + 
  
  annotate("text", x = 0.5, y = promedio_num_estudiantes + 2, 
           
           label = paste("Promedio:", round(promedio_num_estudiantes, 2)), 
           
           color = "red") 

#figura 10
library(ggplot2) 

library(dplyr) 



SaberProDataGrADMN$PROGRAMA <- factor(SaberProDataGrADMN$PROGRAMA) 



levels(SaberProDataGrADMN$PROGRAMA) <- c("NI", "ADEMP") 



colores_viridis <- viridis::viridis_pal()(2) 



boxplot(SaberProDataGrADMN$PUNTAJE_GLOBAL ~ SaberProDataGrADMN$PROGRAMA, 
        
        horizontal = TRUE, las = 2, ylab = "", xlab = "Puntaje Global", 
        
        col = colores_viridis) 

#figura 11
library(ggplot2) 

library(dplyr) 



SaberProDataGrADMN <- SaberProData %>% 
  
  filter(GRUPO_REFERENCIA == "ADMINISTRACIÓN Y AFINES") 



programas_admn <- c("NEGOCIOS INTERNACIONALES", "ADMINISTRACION DE EMPRESAS") 



SaberProDataAdmnProgramas <- SaberProDataGrADMN %>% 
  
  filter(PROGRAMA %in% programas_admn) 



ggplot(SaberProDataAdmnProgramas, aes(x = PUNTAJE_PRUEBA.LectCritica, y = PUNTAJE_PRUEBA.RazCuanti)) + 
  
  stat_density_2d(aes(fill = after_stat(density)), geom = "tile", contour = FALSE, alpha = 0.5) + 
  
  geom_point(size = 3, shape = 21, color = "black") + 
  
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "red", size = 1) + 
  
  labs(title = "Correlación entre Lectura Crítica y Razonamiento Cuantitativo para Administración y Afinidades", 
       
       x = "Puntaje Prueba de Lectura Crítica", 
       
       y = "Puntaje Prueba de Razonamiento Cuantitativo") + 
  
  scale_fill_viridis_c(name = "Densidad") +   
  
  theme_minimal() 

#figura 12
library(ggplot2) 

library(dplyr) 

library(tidyr) 





filtered_data <- SaberProData %>% 
  
  filter(PERIODO %in% c("20162", "20163")) 



promedios_pruebas <- filtered_data %>% 
  
  group_by(PERIODO) %>% 
  
  summarize(promedio_Ciudadanas = mean(PUNTAJE_PRUEBA.CCiudadanas), 
            
            promedio_RazCuanti = mean(PUNTAJE_PRUEBA.RazCuanti), 
            
            promedio_LectCritica = mean(PUNTAJE_PRUEBA.LectCritica)) 



gathered_data <- promedios_pruebas %>% 
  
  pivot_longer(cols = starts_with("promedio_"), 
               
               names_to = "Prueba", 
               
               values_to = "Promedio") 



ggplot(data = gathered_data, aes(x = PERIODO, y = Promedio, fill = Prueba)) + 
  
  geom_bar(stat = "identity", position = "dodge", width = 0.2, color = "gray") +  # Agregar bordes 
  
  geom_text(aes(label = round(Promedio, 2)), position = position_dodge(width = 0.5), vjust = -0.5) +  # Agregar valores encima de las barras 
  
  labs(title = "Comparación de Promedios de Puntajes por Prueba y Categoría", 
       
       x = "Categoría", 
       
       y = "Promedio de Puntajes") + 
  
  scale_fill_manual(values = c("promedio_Ciudadanas" = "#EAC435", "promedio_RazCuanti" = "#345995", "promedio_LectCritica" = "#FB4D3D")) + 
  
  theme_minimal() + 
  
  theme(legend.position = "top") 

#Para la 8, solo cambia los valores de 20162, 20163 por 20172, 20173 en el primer filtro. 


#figura 13
library(ggplot2) 



SaberProData_20162 <- SaberProData[SaberProData$PERIODO == 20162,] 

SaberProData_20163 <- SaberProData[SaberProData$PERIODO == 20163,] 

SaberProData_20172 <- SaberProData[SaberProData$PERIODO == 20172,] 

SaberProData_20173 <- SaberProData[SaberProData$PERIODO == 20173,] 



#1 

ggplot(SaberProData_20162, aes(x = PERCENTIL_NACIONALGLOBAL, fill = PERCENTIL_NACIONALGLOBAL)) + 
  
  geom_histogram(binwidth = 5, color = "black") + 
  
  scale_fill_viridis(option = "magma") + 
  
  labs(title = "Distribución de Percentiles Nacionales Globales - Periodo 20162", 
       
       x = "Percentil Nacional Global", 
       
       y = "Frecuencia") + 
  
  theme_minimal() + 
  
  theme(legend.position = "none") 





#2 

ggplot(SaberProData_20163, aes(x = PERCENTIL_NACIONALGLOBAL, fill = PERCENTIL_NACIONALGLOBAL)) + 
  
  geom_histogram(binwidth = 5, color = "black") + 
  
  scale_fill_viridis(option = "magma") + 
  
  labs(title = "Distribución de Percentiles Nacionales Globales - Periodo 20163", 
       
       x = "Percentil Nacional Global", 
       
       y = "Frecuencia") + 
  
  theme_minimal() + 
  
  theme(legend.position = "none") 



#3 

ggplot(SaberProData_20172, aes(x = PERCENTIL_NACIONALGLOBAL, fill = PERCENTIL_NACIONALGLOBAL)) + 
  
  geom_histogram(binwidth = 5, color = "black") + 
  
  scale_fill_viridis(option = "magma") + 
  
  labs(title = "Distribución de Percentiles Nacionales Globales - Periodo 20172", 
       
       x = "Percentil Nacional Global", 
       
       y = "Frecuencia") + 
  
  theme_minimal() + 
  
  theme(legend.position = "none") 



#4 

ggplot(SaberProData_20173, aes(x = PERCENTIL_NACIONALGLOBAL, fill = PERCENTIL_NACIONALGLOBAL)) + 
  
  geom_histogram(binwidth = 5, color = "black") + 
  
  scale_fill_viridis(option = "magma") + 
  
  labs(title = "Distribución de Percentiles Nacionales Globales - Periodo 20173", 
       
       x = "Percentil Nacional Global", 
       
       y = "Frecuencia") + 
  
  theme_minimal() + 
  
  theme(legend.position = "none") 


#figura 14
Puntaje_global <- SaberProData$PUNTAJE_GLOBAL 

color_palette <- colorRampPalette(c("red","yellow", "green"))(100) 

hist(Puntaje_global, breaks = 80, 
     
     main = "Distribucion de Puntajes Globales", 
     
     col = color_palette, 
     
     density = 80,   
     
     border = "black")  

#figura 15
library(ggplot2) 

library(dplyr) 

library(viridis) 



SaberProData$PERIODO <- factor(SaberProData$PERIODO) 



estudiantes_por_semestre <- SaberProData %>% 
  
  group_by(PERIODO) %>% 
  
  summarise(num_estudiantes = n()) 



ggplot(estudiantes_por_semestre, aes(x = PERIODO, y = num_estudiantes, fill = num_estudiantes)) + 
  
  geom_bar(stat = "identity") + 
  
  scale_fill_viridis(option = "turbo") +   
  
  labs(title = "Cantidad de Estudiantes por Semestre", 
       
       x = "Semestre", 
       
       y = "Número de Estudiantes") + 
  
  theme_minimal() + 
  
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  
  geom_text(aes(label = num_estudiantes), vjust = -0.5, size = 3) 

