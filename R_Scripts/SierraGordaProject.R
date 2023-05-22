
install.packages("tidyverse")
library(tidyverse)


# IMPORT AND DATA CLEANING ------------------------------------------------


# Importar Datos Estacion 3
E3Data <- read_csv2("SGCSM/E-3_01-01-2018_a_31-12-2020.csv")

# Arreglar MP10
E3Data$MP10 <- E3Data$MP10/10

# Cambiar nombre de las columnas

names(E3Data) <- c("Fecha", "Hora", "DIR", "HR", "PR", "RS", "TEMP", "VV", "MP10")

# Eliminar filas que no tengan datos
E3Data <- na.omit(E3Data)
View(E3Data)
nrow(E3Data) ## 16 Enero 2018 hasta 31 Julio 2019. De 13842 paso a 7754 filas.

# Crear 24 dataframes, uno para cada hora.
for (i in 0:23) {
  assign(paste("E3_Hora", i, sep = ''), subset(E3Data, Hora == i)) 
}

# Importar Datos Estacion 5
E5Data <- read_csv2("SGCSM/E-5_01-01-2018_a_31-12-2020.csv")

# Cambiar nombre de las columnas

names(E5Data) <- c("Fecha", "Hora", "DIR", "VV", "MP10")


# Eliminar filas que no tengan datos
E5Data <- na.omit(E5Data)
View(E5Data)
nrow(E5Data) ## 13 Enero 2018 hasta 29 Abril 2020. De 20400 paso a 11580 filas.

# Crear 24 dataframes, uno para cada hora.
for (i in 0:23) {
  assign(paste("E5_Hora", i, sep = ''), subset(E5Data, Hora == i)) 
}

# Importar Datos MK
MKData <- read_csv2("SGCSM/MK_01-01-2018_a_31-12-2020.csv")

# Cambiar nombre de las columnas

names(MKData) <- c("Fecha", "Hora", "DIR", "HR", "TEMP", "VV", "MP10")


# Arreglar MP10
MKData$MP10 <- MKData$MP10/10


# Eliminar filas que no tengan datos
MKData <- na.omit(MKData)
View(MKData)
nrow(MKData) ## 07 Octubre 2020 hasta 31 Diciembre 2020. De 2051 paso a 1900 filas.

# Crear 24 dataframes, uno para cada hora.
for (i in 0:23) {
  assign(paste("MK_Hora", i, sep = ''), subset(MKData, Hora == i)) 
}

# Importar Datos Sierra Gorda
SGData <- read_csv2("SIERRA_GORDA/Sierra_Gorda_01-01-2018_a_31-12-2020.csv")

# Cambiar nombre de las columnas

names(SGData) <- c("Fecha", "Hora", "DIR", "TEMP", "VV", "MP10", "MP2.5")


#Arreglar MP10 Y MP2.5
SGData$MP10 <- SGData$MP10/10
SGData$MP2.5 <- SGData$MP2.5/10

# Eliminar filas que no tengan datos
SGData <- na.omit(SGData)
View(SGData)
nrow(SGData) ## 01 Enero 2018 hasta 28 Diciembre 2020. De 26304 paso a 25582 filas.

# Crear 24 dataframes, uno para cada hora.
for (i in 0:23) {
  assign(paste("SG10_Hora", i, sep = ''), subset(SGData, Hora == i)) 
}



# MODEL CREATION ----------------------------------------------------------

library(tree)

## Creacion de Modelos para todas horas de Estacion E3

for (i in 0:23) {
assign(paste("Tree_E3_Hora", i, sep = ''),
       tree(MP10 ~ DIR + HR + PR + RS + TEMP + VV,
            control = tree.control(nobs = nrow(E3Data %>% filter(Hora == i)),
                                   mincut = 5, minsize = 10, mindev = 0),
            data = E3Data %>% filter(Hora == i)))
}


## Creacion de Modelos para todas horas de Estacion E5

for (i in 0:23) {
  assign(paste("Tree_E5_Hora", i, sep = ''),
         tree(MP10 ~ DIR + VV,
              control = tree.control(nobs = nrow(E5Data %>% filter(Hora == i)),
                                     mincut = 5, minsize = 10, mindev = 0),
              data = E5Data %>% filter(Hora == i)))
}

## Creacion de Modelos para todas horas de Estacion MK

for (i in 0:23) {
  assign(paste("Tree_MK_Hora", i, sep = ''),
         tree(MP10 ~ DIR + HR + TEMP + VV,
              control = tree.control(nobs = nrow(MKData %>% filter(Hora == i)),
                                     mincut = 5, minsize = 10, mindev = 0),
              data = MKData %>% filter(Hora == i)))
}

## Creacion de Modelos para todas horas de Estacion Sierra Gorda PREDICCION MP10

for (i in 0:23) {
  assign(paste("Tree_SG10_Hora", i, sep = ''),
         tree(MP10 ~ DIR + TEMP + VV,
              control = tree.control(nobs = nrow(SGData %>% filter(Hora == i)),
                                     mincut = 5, minsize = 10, mindev = 0),
              data = SGData %>% filter(Hora == i)))
}


## Creacion de Modelos para todas horas de Estacion Sierra Gorda PREDICCION MP2.5

# Crear lista de horas, sin hora 12 porque hora 12 tira un error de max depth.
horas <- c(0:23)
horas <- horas[-13]

for (i in horas) {
  assign(paste("Tree_SG2.5_Hora", i, sep = ''),
         tree(MP2.5 ~ DIR + TEMP + VV,
              control = tree.control(nobs = nrow(SGData %>% filter(Hora == i)),
                                     mincut = 5, minsize = 10, mindev = 0),
              data = SGData %>% filter(Hora == i)))
}

## Crear manualmente modelo para hora 12.

Tree_SG2.5_Hora12 <- tree(MP2.5 ~ DIR + TEMP + VV,
               control = tree.control(nobs = 1063, mincut = 10, minsize = 20, mindev = 0),
               data = SG_Hora12)



# GUARDAR MODELOS ---------------------------------------------------------



## Estacion 3


for (i in 0:23) {
  
  mytreeframe <- tree(MP10 ~ DIR + HR + PR + RS + TEMP + VV,
                      control = tree.control(nobs = nrow(E3Data %>% filter(Hora == i)),
                                             mincut = 5, minsize = 10, mindev = 0),
                      data = E3Data %>% filter(Hora == i))$frame
  
  write.table(mytreeframe, file = paste(paste("SGCSM/Arboles/E3_Hora", i, sep = ''),
                                        ".csv", sep = ''), sep = ',')
  
}



## Estacion 5

for (i in 0:23) {
  
  mytreeframe <- tree(MP10 ~ DIR + VV,
                      control = tree.control(nobs = nrow(E5Data %>% filter(Hora == i)),
                                             mincut = 5, minsize = 10, mindev = 0),
                      data = E5Data %>% filter(Hora == i))$frame
  
  write.table(mytreeframe, file = paste(paste("SGCSM/Arboles/E5_Hora", i, sep = ''),
                                        ".csv", sep = ''), sep = ',')
  
}



## MK 

for (i in 0:23) {
  
  mytreeframe <- tree(MP10 ~ DIR + HR + TEMP + VV,
                      control = tree.control(nobs = nrow(MKData %>% filter(Hora == i)),
                                             mincut = 5, minsize = 10, mindev = 0),
                      data = MKData %>% filter(Hora == i))$frame
  
  write.table(mytreeframe, file = paste(paste("SGCSM/Arboles/MK_Hora", i, sep = ''),
                                        ".csv", sep = ''), sep = ',')
  
}


## Sierra Gorda MP10

for (i in 0:23) {
  
  mytreeframe <- tree(MP10 ~ DIR + TEMP + VV,
                      control = tree.control(nobs = nrow(SGData %>% filter(Hora == i)),
                                             mincut = 5, minsize = 10, mindev = 0),
                      data = SGData %>% filter(Hora == i))$frame
  
  write.table(mytreeframe, file = paste(paste("SGCSM/Arboles/SG10_Hora", i, sep = ''),
                                        ".csv", sep = ''), sep = ',')
  
}


## Sierra Gorda MP2.5

for (i in horas) {

mytreeframe <- tree(MP2.5 ~ DIR + TEMP + VV,
          control = tree.control(nobs = nrow(SGData %>% filter(Hora == i)),
                                 mincut = 5, minsize = 10, mindev = 0),
          data = SGData %>% filter(Hora == i))$frame

write.table(mytreeframe, file = paste(paste("SGCSM/Arboles/SG2.5_Hora", i, sep = ''),
                            ".csv", sep = ''), sep = ',')

}

## Hora 12 manualmente:

write.table(Tree_SG2.5_Hora12$frame, file = "SGCSM/Arboles/SG2.5_Hora12.csv", sep = '')



## Ver resultados del arbol (Residual Mean Deviance, y distribucion de residuos)

summary(Tree_E3_Hora0)

# Ver cada decision del arbol
View(Tree_E3_Hora0$frame)

# Graficos del arbol
text(Tree_E3_Hora0)

plot(Tree_E3_Hora0)




# ANALISIS DE DEVIANZA ----------------------------------------------------


Devianzas <- vector()

######################## ESTACION 3 ######################

for (i in 0:23) {
  
  
  TreeFrame <- tree(MP10 ~ DIR + HR + PR + RS + TEMP + VV,
                    control = tree.control(nobs = nrow(E3Data %>% filter(Hora == i)),
                                           mincut = 5, minsize = 10, mindev = 0),
                    data = E3Data %>% filter(Hora == i))$frame
  
  SampleSize <- TreeFrame$n[1]
  
  SummarizedFrame <- TreeFrame %>%
    group_by(var) %>% 
    summarise(TotalDeviance = sum(dev), NodosTerminales = n())
  
  ResMeanDev <- SummarizedFrame$TotalDeviance[1] / (SampleSize - SummarizedFrame$NodosTerminales[1])
  
  Devianzas[i+1] <- ResMeanDev
  
}

Devianzas_E3 <- data.frame(Horas = 0:23, Devianzas = MyVector)

Devianzas_E3 %>% 
  ggplot(aes(x = Horas, y = Devianzas)) +
  geom_point() +
  labs(y = "Residual Mean Deviance", title = "Devianzas Estacion 3") +
  geom_smooth()

################### ESTACION 5 ######################

for (i in 0:23) {
  
  
  TreeFrame <- tree(MP10 ~ DIR + VV,
                    control = tree.control(nobs = nrow(E5Data %>% filter(Hora == i)),
                                           mincut = 5, minsize = 10, mindev = 0),
                    data = E5Data %>% filter(Hora == i))$frame
  
  SampleSize <- TreeFrame$n[1]
  
  SummarizedFrame <- TreeFrame %>%
    group_by(var) %>% 
    summarise(TotalDeviance = sum(dev), NodosTerminales = n())
  
  ResMeanDev <- SummarizedFrame$TotalDeviance[1] / (SampleSize - SummarizedFrame$NodosTerminales[1])
  
  Devianzas[i+1] <- ResMeanDev
  
}

Devianzas_E5 <- data.frame(Horas = 0:23, Devianzas)

Devianzas_E5 %>% 
  ggplot(aes(x = Horas, y = Devianzas)) +
  geom_point() +
  labs(y = "Residual Mean Deviance", title = "Devianzas Estacion 5") +
  geom_smooth()


######################## ESTACION MK ######################



for (i in 0:23) {
  
  
  TreeFrame <- tree(MP10 ~ DIR + HR + TEMP + VV,
                    control = tree.control(nobs = nrow(MKData %>% filter(Hora == i)),
                                           mincut = 5, minsize = 10, mindev = 0),
                    data = MKData %>% filter(Hora == i))$frame
  
  SampleSize <- TreeFrame$n[1]
  
  SummarizedFrame <- TreeFrame %>%
    group_by(var) %>% 
    summarise(TotalDeviance = sum(dev), NodosTerminales = n())
  
  ResMeanDev <- SummarizedFrame$TotalDeviance[1] / (SampleSize - SummarizedFrame$NodosTerminales[1])
  
  Devianzas[i+1] <- ResMeanDev
  
}

Devianzas_MK <- data.frame(Horas = 0:23, Devianzas)

Devianzas_MK %>% 
  ggplot(aes(x = Horas, y = Devianzas)) +
  geom_point() +
  labs(y = "Residual Mean Deviance", title = "Devianzas Estacion MK") +
  geom_smooth()



######################## ESTACION SIERRA GORDA MP10 ######################




for (i in 0:23) {
  
  
  TreeFrame <- tree(MP10 ~ DIR + TEMP + VV,
                    control = tree.control(nobs = nrow(SGData %>% filter(Hora == i)),
                                           mincut = 5, minsize = 10, mindev = 0),
                    data = SGData %>% filter(Hora == i))$frame
  
  SampleSize <- TreeFrame$n[1]
  
  SummarizedFrame <- TreeFrame %>%
    group_by(var) %>% 
    summarise(TotalDeviance = sum(dev), NodosTerminales = n())
  
  ResMeanDev <- SummarizedFrame$TotalDeviance[1] / (SampleSize - SummarizedFrame$NodosTerminales[1])
  
  Devianzas[i+1] <- ResMeanDev
  
}

Devianzas_SG10 <- data.frame(Horas = 0:23, Devianzas)

Devianzas_SG10 %>% 
  ggplot(aes(x = Horas, y = Devianzas)) +
  geom_point() +
  labs(y = "Residual Mean Deviance", title = "Devianzas Estacion SG - Prediccion MP10") +
  geom_smooth()


######################## ESTACION SIERRA GORDA MP2.5 ######################

Devianzas <- vector()

for (i in horas) {
  
  
  TreeFrame <- tree(MP2.5 ~ DIR + TEMP + VV,
                    control = tree.control(nobs = nrow(SGData %>% filter(Hora == i)),
                                           mincut = 5, minsize = 10, mindev = 0),
                    data = SGData %>% filter(Hora == i))$frame
  
  SampleSize <- TreeFrame$n[1]
  
  SummarizedFrame <- TreeFrame %>%
    group_by(var) %>% 
    summarise(TotalDeviance = sum(dev), NodosTerminales = n())
  
  ResMeanDev <- SummarizedFrame$TotalDeviance[1] / (SampleSize - SummarizedFrame$NodosTerminales[1])
  
  Devianzas[i+1] <- ResMeanDev
  
}

## Armar el caso especial de la hora 12

TreeFrame <- tree(MP2.5 ~ DIR + TEMP + VV,
                  control = tree.control(nobs = nrow(SG_Hora12),
                                         mincut = 10, minsize = 20, mindev = 0),
                  data = SG_Hora12)$frame

SampleSize <- TreeFrame$n[1]

SummarizedFrame <- TreeFrame %>%
  group_by(var) %>% 
  summarise(TotalDeviance = sum(dev), NodosTerminales = n())

ResMeanDev <- SummarizedFrame$TotalDeviance[1] / (SampleSize - SummarizedFrame$NodosTerminales[1])

Devianzas[13] <- ResMeanDev


Devianzas_SG2.5 <- data.frame(Horas = 0:23, Devianzas)

Devianzas_SG2.5 %>% 
  ggplot(aes(x = Horas, y = Devianzas)) +
  geom_point() +
  labs(y = "Residual Mean Deviance", title = "Devianzas Estacion SG - Prediccion MP2.5") +
  geom_smooth()





# FORMATOS CORRECTOS PARA DANIEL ------------------------------------------



## 1) ESTACION 3 ##########################################################################

for (i in 0:23) {
  
  
  tree.dataframe <- tree(MP10 ~ DIR + HR + PR + RS + TEMP + VV,
                         control = tree.control(nobs = nrow(E3Data %>% filter(Hora == i)),
                                                mincut = 5, minsize = 10, mindev = 0),
                         data = E3Data %>% filter(Hora == i))$frame
  
  
  setDT(tree.dataframe, keep.rownames = TRUE)
  names(tree.dataframe)[1] <- "NodeNum"
  
  tree.dataframe$NodeNum <- as.numeric(tree.dataframe$NodeNum)
  
  tree.dataframe <- tree.dataframe %>% 
    mutate(spaces = case_when(
      1 == NodeNum ~ "   ",
      2 <= NodeNum & NodeNum <= 3 ~ "      ",
      4 <= NodeNum & NodeNum <= 7 ~ "        ",
      8 <= NodeNum & NodeNum <= 15 & nchar(NodeNum) == 1 ~ "          ",
      8 <= NodeNum & NodeNum <= 15 & nchar(NodeNum) == 2 ~ "         ",
      16 <= NodeNum & NodeNum <= 31 ~ "           ",
      32 <= NodeNum & NodeNum <= 63 ~ "             ",
      64 <= NodeNum & NodeNum <= 127 & nchar(NodeNum) == 2 ~ "               ",
      64 <= NodeNum & NodeNum <= 127 & nchar(NodeNum) == 3 ~ "              ",
      128 <= NodeNum & NodeNum <= 255 ~ "                ",
      256 <= NodeNum & NodeNum <= 511 ~ "                  ",
      512 <= NodeNum & NodeNum <= 1023 & nchar(NodeNum) == 3 ~ "                    ",
      512 <= NodeNum & NodeNum <= 1023 & nchar(NodeNum) == 4 ~ "                   ",
      1024 <= NodeNum & NodeNum <= 2047 ~ "                     ",
      2048 <= NodeNum & NodeNum <= 4095 ~ "                       ",
      4096 <= NodeNum & NodeNum <= 8191 ~ "                         ",
      8192 <= NodeNum & NodeNum <= 16383 & nchar(NodeNum) == 4 ~ "                           ",
      8192 <= NodeNum & NodeNum <= 16383 & nchar(NodeNum) == 5 ~ "                          ",
      16384 <= NodeNum & NodeNum <= 32767 ~ "                            ",
      32768 <= NodeNum & NodeNum <= 65535 ~ "                              ",
      65536 <= NodeNum & NodeNum <= 131071 & nchar(NodeNum) == 5 ~ "                                ",
      65536 <= NodeNum & NodeNum <= 131071 & nchar(NodeNum) == 6 ~ "                               ",
      131072 <= NodeNum & NodeNum <= 262143 ~ "                                 ",
      262144 <= NodeNum & NodeNum <= 524287 ~ "                                   ",
      524288 <= NodeNum & NodeNum <= 1048575 & nchar(NodeNum) == 6 ~ "                                     ",
      524288 <= NodeNum & NodeNum <= 1048575 & nchar(NodeNum) == 7 ~ "                                    ",
      1048576 <= NodeNum & NodeNum <= 2097151 ~ "                                      ",
      between(NodeNum, 2^21, 2^22-1) ~ strrep(" ", 47 - nchar(NodeNum)),
      between(NodeNum, 2^22, 2^23-1) ~ strrep(" ", 49 - nchar(NodeNum)),
      between(NodeNum, 2^23, 2^24-1) ~ strrep(" ", 51 - nchar(NodeNum)),
      between(NodeNum, 2^24, 2^25-1) ~ strrep(" ", 53 - nchar(NodeNum)),
      between(NodeNum, 2^25, 2^26-1) ~ strrep(" ", 55 - nchar(NodeNum)),
      between(NodeNum, 2^26, 2^27-1) ~ strrep(" ", 57 - nchar(NodeNum)),
      between(NodeNum, 2^27, 2^28-1) ~ strrep(" ", 59 - nchar(NodeNum))
    ))
  
  write.csv(as.matrix(tree.dataframe), file = paste(paste("SGCSM/Arboles/Estacion_3/E3_Hora", i, sep = ''),
                                                    ".csv", sep = ''), sep = ',')
  
  
  
  
}


## 2) ESTACION 5 #####################################################################################


for (i in 0:23) {
  
  
  tree.dataframe <- tree(MP10 ~ DIR + VV,
                         control = tree.control(nobs = nrow(E5Data %>% filter(Hora == i)),
                                                mincut = 5, minsize = 10, mindev = 0),
                         data = E5Data %>% filter(Hora == i))$frame
  
  
  setDT(tree.dataframe, keep.rownames = TRUE)
  names(tree.dataframe)[1] <- "NodeNum"
  
  tree.dataframe$NodeNum <- as.numeric(tree.dataframe$NodeNum)
  
  tree.dataframe <- tree.dataframe %>% 
    mutate(spaces = case_when(
      1 == NodeNum ~ "   ",
      2 <= NodeNum & NodeNum <= 3 ~ "      ",
      4 <= NodeNum & NodeNum <= 7 ~ "        ",
      8 <= NodeNum & NodeNum <= 15 & nchar(NodeNum) == 1 ~ "          ",
      8 <= NodeNum & NodeNum <= 15 & nchar(NodeNum) == 2 ~ "         ",
      16 <= NodeNum & NodeNum <= 31 ~ "           ",
      32 <= NodeNum & NodeNum <= 63 ~ "             ",
      64 <= NodeNum & NodeNum <= 127 & nchar(NodeNum) == 2 ~ "               ",
      64 <= NodeNum & NodeNum <= 127 & nchar(NodeNum) == 3 ~ "              ",
      128 <= NodeNum & NodeNum <= 255 ~ "                ",
      256 <= NodeNum & NodeNum <= 511 ~ "                  ",
      512 <= NodeNum & NodeNum <= 1023 & nchar(NodeNum) == 3 ~ "                    ",
      512 <= NodeNum & NodeNum <= 1023 & nchar(NodeNum) == 4 ~ "                   ",
      1024 <= NodeNum & NodeNum <= 2047 ~ "                     ",
      2048 <= NodeNum & NodeNum <= 4095 ~ "                       ",
      4096 <= NodeNum & NodeNum <= 8191 ~ "                         ",
      8192 <= NodeNum & NodeNum <= 16383 & nchar(NodeNum) == 4 ~ "                           ",
      8192 <= NodeNum & NodeNum <= 16383 & nchar(NodeNum) == 5 ~ "                          ",
      16384 <= NodeNum & NodeNum <= 32767 ~ "                            ",
      32768 <= NodeNum & NodeNum <= 65535 ~ "                              ",
      65536 <= NodeNum & NodeNum <= 131071 & nchar(NodeNum) == 5 ~ "                                ",
      65536 <= NodeNum & NodeNum <= 131071 & nchar(NodeNum) == 6 ~ "                               ",
      131072 <= NodeNum & NodeNum <= 262143 ~ "                                 ",
      262144 <= NodeNum & NodeNum <= 524287 ~ "                                   ",
      524288 <= NodeNum & NodeNum <= 1048575 & nchar(NodeNum) == 6 ~ "                                     ",
      524288 <= NodeNum & NodeNum <= 1048575 & nchar(NodeNum) == 7 ~ "                                    ",
      1048576 <= NodeNum & NodeNum <= 2097151 ~ "                                      ",
      between(NodeNum, 2^21, 2^22-1) ~ strrep(" ", 47 - nchar(NodeNum)),
      between(NodeNum, 2^22, 2^23-1) ~ strrep(" ", 49 - nchar(NodeNum)),
      between(NodeNum, 2^23, 2^24-1) ~ strrep(" ", 51 - nchar(NodeNum)),
      between(NodeNum, 2^24, 2^25-1) ~ strrep(" ", 53 - nchar(NodeNum)),
      between(NodeNum, 2^25, 2^26-1) ~ strrep(" ", 55 - nchar(NodeNum)),
      between(NodeNum, 2^26, 2^27-1) ~ strrep(" ", 57 - nchar(NodeNum)),
      between(NodeNum, 2^27, 2^28-1) ~ strrep(" ", 59 - nchar(NodeNum))
    ))
  
  write.csv(as.matrix(tree.dataframe), file = paste(paste("SGCSM/Arboles/Estacion_5/E5_Hora", i, sep = ''),
                                                    ".csv", sep = ''), sep = ',')
  
  
}





## 3) ESTACION MK ##########################################################################




for (i in 0:23) {
  
  
  tree.dataframe <- tree(MP10 ~ DIR + HR + TEMP + VV,
                         control = tree.control(nobs = nrow(MKData %>% filter(Hora == i)),
                                                mincut = 5, minsize = 10, mindev = 0),
                         data = MKData %>% filter(Hora == i))$frame
  
  
  setDT(tree.dataframe, keep.rownames = TRUE)
  names(tree.dataframe)[1] <- "NodeNum"
  
  tree.dataframe$NodeNum <- as.numeric(tree.dataframe$NodeNum)
  
  tree.dataframe <- tree.dataframe %>% 
    mutate(spaces = case_when(
      1 == NodeNum ~ "   ",
      2 <= NodeNum & NodeNum <= 3 ~ "      ",
      4 <= NodeNum & NodeNum <= 7 ~ "        ",
      8 <= NodeNum & NodeNum <= 15 & nchar(NodeNum) == 1 ~ "          ",
      8 <= NodeNum & NodeNum <= 15 & nchar(NodeNum) == 2 ~ "         ",
      16 <= NodeNum & NodeNum <= 31 ~ "           ",
      32 <= NodeNum & NodeNum <= 63 ~ "             ",
      64 <= NodeNum & NodeNum <= 127 & nchar(NodeNum) == 2 ~ "               ",
      64 <= NodeNum & NodeNum <= 127 & nchar(NodeNum) == 3 ~ "              ",
      128 <= NodeNum & NodeNum <= 255 ~ "                ",
      256 <= NodeNum & NodeNum <= 511 ~ "                  ",
      512 <= NodeNum & NodeNum <= 1023 & nchar(NodeNum) == 3 ~ "                    ",
      512 <= NodeNum & NodeNum <= 1023 & nchar(NodeNum) == 4 ~ "                   ",
      1024 <= NodeNum & NodeNum <= 2047 ~ "                     ",
      2048 <= NodeNum & NodeNum <= 4095 ~ "                       ",
      4096 <= NodeNum & NodeNum <= 8191 ~ "                         ",
      8192 <= NodeNum & NodeNum <= 16383 & nchar(NodeNum) == 4 ~ "                           ",
      8192 <= NodeNum & NodeNum <= 16383 & nchar(NodeNum) == 5 ~ "                          ",
      16384 <= NodeNum & NodeNum <= 32767 ~ "                            ",
      32768 <= NodeNum & NodeNum <= 65535 ~ "                              ",
      65536 <= NodeNum & NodeNum <= 131071 & nchar(NodeNum) == 5 ~ "                                ",
      65536 <= NodeNum & NodeNum <= 131071 & nchar(NodeNum) == 6 ~ "                               ",
      131072 <= NodeNum & NodeNum <= 262143 ~ "                                 ",
      262144 <= NodeNum & NodeNum <= 524287 ~ "                                   ",
      524288 <= NodeNum & NodeNum <= 1048575 & nchar(NodeNum) == 6 ~ "                                     ",
      524288 <= NodeNum & NodeNum <= 1048575 & nchar(NodeNum) == 7 ~ "                                    ",
      1048576 <= NodeNum & NodeNum <= 2097151 ~ "                                      ",
      between(NodeNum, 2^21, 2^22-1) ~ strrep(" ", 47 - nchar(NodeNum)),
      between(NodeNum, 2^22, 2^23-1) ~ strrep(" ", 49 - nchar(NodeNum)),
      between(NodeNum, 2^23, 2^24-1) ~ strrep(" ", 51 - nchar(NodeNum)),
      between(NodeNum, 2^24, 2^25-1) ~ strrep(" ", 53 - nchar(NodeNum)),
      between(NodeNum, 2^25, 2^26-1) ~ strrep(" ", 55 - nchar(NodeNum)),
      between(NodeNum, 2^26, 2^27-1) ~ strrep(" ", 57 - nchar(NodeNum)),
      between(NodeNum, 2^27, 2^28-1) ~ strrep(" ", 59 - nchar(NodeNum))
    ))
  
  write.csv(as.matrix(tree.dataframe), file = paste(paste("SGCSM/Arboles/Estacion_MK/MK_Hora", i, sep = ''),
                                                    ".csv", sep = ''), sep = ',')
  
  
}




## 4) ESTACION SIERRA GORDA MP 10 ############################################################



for (i in 0:23) {
  
  
  tree.dataframe <- tree(MP10 ~ DIR + TEMP + VV,
                         control = tree.control(nobs = nrow(SGData %>% filter(Hora == i)),
                                                mincut = 5, minsize = 10, mindev = 0),
                         data = SGData %>% filter(Hora == i))$frame
  
  
  setDT(tree.dataframe, keep.rownames = TRUE)
  names(tree.dataframe)[1] <- "NodeNum"
  
  tree.dataframe$NodeNum <- as.numeric(tree.dataframe$NodeNum)
  
  tree.dataframe <- tree.dataframe %>% 
    mutate(spaces = case_when(
      1 == NodeNum ~ "   ",
      2 <= NodeNum & NodeNum <= 3 ~ "      ",
      4 <= NodeNum & NodeNum <= 7 ~ "        ",
      8 <= NodeNum & NodeNum <= 15 & nchar(NodeNum) == 1 ~ "          ",
      8 <= NodeNum & NodeNum <= 15 & nchar(NodeNum) == 2 ~ "         ",
      16 <= NodeNum & NodeNum <= 31 ~ "           ",
      32 <= NodeNum & NodeNum <= 63 ~ "             ",
      64 <= NodeNum & NodeNum <= 127 & nchar(NodeNum) == 2 ~ "               ",
      64 <= NodeNum & NodeNum <= 127 & nchar(NodeNum) == 3 ~ "              ",
      128 <= NodeNum & NodeNum <= 255 ~ "                ",
      256 <= NodeNum & NodeNum <= 511 ~ "                  ",
      512 <= NodeNum & NodeNum <= 1023 & nchar(NodeNum) == 3 ~ "                    ",
      512 <= NodeNum & NodeNum <= 1023 & nchar(NodeNum) == 4 ~ "                   ",
      1024 <= NodeNum & NodeNum <= 2047 ~ "                     ",
      2048 <= NodeNum & NodeNum <= 4095 ~ "                       ",
      4096 <= NodeNum & NodeNum <= 8191 ~ "                         ",
      8192 <= NodeNum & NodeNum <= 16383 & nchar(NodeNum) == 4 ~ "                           ",
      8192 <= NodeNum & NodeNum <= 16383 & nchar(NodeNum) == 5 ~ "                          ",
      16384 <= NodeNum & NodeNum <= 32767 ~ "                            ",
      32768 <= NodeNum & NodeNum <= 65535 ~ "                              ",
      65536 <= NodeNum & NodeNum <= 131071 & nchar(NodeNum) == 5 ~ "                                ",
      65536 <= NodeNum & NodeNum <= 131071 & nchar(NodeNum) == 6 ~ "                               ",
      131072 <= NodeNum & NodeNum <= 262143 ~ "                                 ",
      262144 <= NodeNum & NodeNum <= 524287 ~ "                                   ",
      524288 <= NodeNum & NodeNum <= 1048575 & nchar(NodeNum) == 6 ~ "                                     ",
      524288 <= NodeNum & NodeNum <= 1048575 & nchar(NodeNum) == 7 ~ "                                    ",
      1048576 <= NodeNum & NodeNum <= 2097151 ~ "                                      ",
      between(NodeNum, 2^21, 2^22-1) ~ strrep(" ", 47 - nchar(NodeNum)),
      between(NodeNum, 2^22, 2^23-1) ~ strrep(" ", 49 - nchar(NodeNum)),
      between(NodeNum, 2^23, 2^24-1) ~ strrep(" ", 51 - nchar(NodeNum)),
      between(NodeNum, 2^24, 2^25-1) ~ strrep(" ", 53 - nchar(NodeNum)),
      between(NodeNum, 2^25, 2^26-1) ~ strrep(" ", 55 - nchar(NodeNum)),
      between(NodeNum, 2^26, 2^27-1) ~ strrep(" ", 57 - nchar(NodeNum)),
      between(NodeNum, 2^27, 2^28-1) ~ strrep(" ", 59 - nchar(NodeNum))
    ))
  
  write.csv(as.matrix(tree.dataframe), file = paste(paste("SGCSM/Arboles/Estacion_SG10/SG10_Hora", i, sep = ''),
                                                    ".csv", sep = ''), sep = ',')
  
  
}




## 4) ESTACION SIERRA GORDA MP 2.5 ############################################################


for (i in horas) {
  
  
  tree.dataframe <- tree(MP2.5 ~ DIR + TEMP + VV,
                         control = tree.control(nobs = nrow(SGData %>% filter(Hora == i)),
                                                mincut = 5, minsize = 10, mindev = 0),
                         data = SGData %>% filter(Hora == i))$frame
  
  
  setDT(tree.dataframe, keep.rownames = TRUE)
  names(tree.dataframe)[1] <- "NodeNum"
  
  tree.dataframe$NodeNum <- as.numeric(tree.dataframe$NodeNum)
  
  tree.dataframe <- tree.dataframe %>% 
    mutate(spaces = case_when(
      1 == NodeNum ~ "   ",
      2 <= NodeNum & NodeNum <= 3 ~ "      ",
      4 <= NodeNum & NodeNum <= 7 ~ "        ",
      8 <= NodeNum & NodeNum <= 15 & nchar(NodeNum) == 1 ~ "          ",
      8 <= NodeNum & NodeNum <= 15 & nchar(NodeNum) == 2 ~ "         ",
      16 <= NodeNum & NodeNum <= 31 ~ "           ",
      32 <= NodeNum & NodeNum <= 63 ~ "             ",
      64 <= NodeNum & NodeNum <= 127 & nchar(NodeNum) == 2 ~ "               ",
      64 <= NodeNum & NodeNum <= 127 & nchar(NodeNum) == 3 ~ "              ",
      128 <= NodeNum & NodeNum <= 255 ~ "                ",
      256 <= NodeNum & NodeNum <= 511 ~ "                  ",
      512 <= NodeNum & NodeNum <= 1023 & nchar(NodeNum) == 3 ~ "                    ",
      512 <= NodeNum & NodeNum <= 1023 & nchar(NodeNum) == 4 ~ "                   ",
      1024 <= NodeNum & NodeNum <= 2047 ~ "                     ",
      2048 <= NodeNum & NodeNum <= 4095 ~ "                       ",
      4096 <= NodeNum & NodeNum <= 8191 ~ "                         ",
      8192 <= NodeNum & NodeNum <= 16383 & nchar(NodeNum) == 4 ~ "                           ",
      8192 <= NodeNum & NodeNum <= 16383 & nchar(NodeNum) == 5 ~ "                          ",
      16384 <= NodeNum & NodeNum <= 32767 ~ "                            ",
      32768 <= NodeNum & NodeNum <= 65535 ~ "                              ",
      65536 <= NodeNum & NodeNum <= 131071 & nchar(NodeNum) == 5 ~ "                                ",
      65536 <= NodeNum & NodeNum <= 131071 & nchar(NodeNum) == 6 ~ "                               ",
      131072 <= NodeNum & NodeNum <= 262143 ~ "                                 ",
      262144 <= NodeNum & NodeNum <= 524287 ~ "                                   ",
      524288 <= NodeNum & NodeNum <= 1048575 & nchar(NodeNum) == 6 ~ "                                     ",
      524288 <= NodeNum & NodeNum <= 1048575 & nchar(NodeNum) == 7 ~ "                                    ",
      1048576 <= NodeNum & NodeNum <= 2097151 ~ "                                      ",
      between(NodeNum, 2^21, 2^22-1) ~ strrep(" ", 47 - nchar(NodeNum)),
      between(NodeNum, 2^22, 2^23-1) ~ strrep(" ", 49 - nchar(NodeNum)),
      between(NodeNum, 2^23, 2^24-1) ~ strrep(" ", 51 - nchar(NodeNum)),
      between(NodeNum, 2^24, 2^25-1) ~ strrep(" ", 53 - nchar(NodeNum)),
      between(NodeNum, 2^25, 2^26-1) ~ strrep(" ", 55 - nchar(NodeNum)),
      between(NodeNum, 2^26, 2^27-1) ~ strrep(" ", 57 - nchar(NodeNum)),
      between(NodeNum, 2^27, 2^28-1) ~ strrep(" ", 59 - nchar(NodeNum)),
      between(NodeNum, 2^28, 2^29-1) ~ strrep(" ", 61 - nchar(NodeNum)
      )))
  
  write.csv(as.matrix(tree.dataframe), file = paste(paste("SIERRA_GORDA/Arboles/Estacion_SG2.5_Test/SG25_Hora", i, sep = ''),
                                                    ".csv", sep = ''), sep = ',')
  
  
}


## caso especial (numero 12)


tree.dataframe <- tree(MP2.5 ~ DIR + TEMP + VV,
                       control = tree.control(nobs = nrow(SGData %>% filter(Hora == 12)),
                                              mincut = 10, minsize = 20, mindev = 0),
                       data = SGData %>% filter(Hora == 12))$frame


setDT(tree.dataframe, keep.rownames = TRUE)
names(tree.dataframe)[1] <- "NodeNum"

tree.dataframe$NodeNum <- as.numeric(tree.dataframe$NodeNum)

tree.dataframe <- tree.dataframe %>% 
  mutate(spaces = case_when(
    1 == NodeNum ~ "   ",
    2 <= NodeNum & NodeNum <= 3 ~ "      ",
    4 <= NodeNum & NodeNum <= 7 ~ "        ",
    8 <= NodeNum & NodeNum <= 15 & nchar(NodeNum) == 1 ~ "          ",
    8 <= NodeNum & NodeNum <= 15 & nchar(NodeNum) == 2 ~ "         ",
    16 <= NodeNum & NodeNum <= 31 ~ "           ",
    32 <= NodeNum & NodeNum <= 63 ~ "             ",
    64 <= NodeNum & NodeNum <= 127 & nchar(NodeNum) == 2 ~ "               ",
    64 <= NodeNum & NodeNum <= 127 & nchar(NodeNum) == 3 ~ "              ",
    128 <= NodeNum & NodeNum <= 255 ~ "                ",
    256 <= NodeNum & NodeNum <= 511 ~ "                  ",
    512 <= NodeNum & NodeNum <= 1023 & nchar(NodeNum) == 3 ~ "                    ",
    512 <= NodeNum & NodeNum <= 1023 & nchar(NodeNum) == 4 ~ "                   ",
    1024 <= NodeNum & NodeNum <= 2047 ~ "                     ",
    2048 <= NodeNum & NodeNum <= 4095 ~ "                       ",
    4096 <= NodeNum & NodeNum <= 8191 ~ "                         ",
    8192 <= NodeNum & NodeNum <= 16383 & nchar(NodeNum) == 4 ~ "                           ",
    8192 <= NodeNum & NodeNum <= 16383 & nchar(NodeNum) == 5 ~ "                          ",
    16384 <= NodeNum & NodeNum <= 32767 ~ "                            ",
    32768 <= NodeNum & NodeNum <= 65535 ~ "                              ",
    65536 <= NodeNum & NodeNum <= 131071 & nchar(NodeNum) == 5 ~ "                                ",
    65536 <= NodeNum & NodeNum <= 131071 & nchar(NodeNum) == 6 ~ "                               ",
    131072 <= NodeNum & NodeNum <= 262143 ~ "                                 ",
    262144 <= NodeNum & NodeNum <= 524287 ~ "                                   ",
    524288 <= NodeNum & NodeNum <= 1048575 & nchar(NodeNum) == 6 ~ "                                     ",
    524288 <= NodeNum & NodeNum <= 1048575 & nchar(NodeNum) == 7 ~ "                                    ",
    1048576 <= NodeNum & NodeNum <= 2097151 ~ "                                      ",
    between(NodeNum, 2^21, 2^22-1) ~ strrep(" ", 47 - nchar(NodeNum)),
    between(NodeNum, 2^22, 2^23-1) ~ strrep(" ", 49 - nchar(NodeNum)),
    between(NodeNum, 2^23, 2^24-1) ~ strrep(" ", 51 - nchar(NodeNum)),
    between(NodeNum, 2^24, 2^25-1) ~ strrep(" ", 53 - nchar(NodeNum)),
    between(NodeNum, 2^25, 2^26-1) ~ strrep(" ", 55 - nchar(NodeNum)),
    between(NodeNum, 2^26, 2^27-1) ~ strrep(" ", 57 - nchar(NodeNum)),
    between(NodeNum, 2^27, 2^28-1) ~ strrep(" ", 59 - nchar(NodeNum))
  ))

write.csv(as.matrix(tree.dataframe), file = paste(paste("SGCSM/Arboles/Estacion_SG2.5/SG25_Hora", 12, sep = ''),
                                                  ".csv", sep = ''), sep = ',')































