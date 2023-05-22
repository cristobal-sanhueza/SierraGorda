
library(readr)
library(dplyr)

# IMPORT DATA METEOROLOGICA
Sierra_Gorda_01_05_2016_a_31_03_2023 <- read_csv("GeoAire/SIERRA_GORDA/Observados/MachineLearning/Sierra_Gorda_01-05-2016_a_31-03-2023.csv", 
                                                 col_types = cols(Fecha = col_date(format = "%d/%m/%Y")), 
                                                 locale = locale(decimal_mark = ",", grouping_mark = "."))

# CREAR COLUMNA CON FECHA Y HORA JUNTOS
Sierra_Gorda_01_05_2016_a_31_03_2023 <- Sierra_Gorda_01_05_2016_a_31_03_2023 %>% 
  mutate(DateAndTime = as.POSIXct(paste(Fecha, Hora, sep = " "), format = "%Y-%m-%d %H"))


# ELMINAR COLUMNAS: FECHA Y HORA (POR SEPARADO)
Sierra_Gorda_01_05_2016_a_31_03_2023 <-  Sierra_Gorda_01_05_2016_a_31_03_2023 %>% 
  select(-c(1:2))

# REORDENAR COLUMNAS
Sierra_Gorda_01_05_2016_a_31_03_2023 <-  Sierra_Gorda_01_05_2016_a_31_03_2023 %>% 
  relocate(DateAndTime)

# RENAME COLUMNS
Sierra_Gorda_01_05_2016_a_31_03_2023 <- Sierra_Gorda_01_05_2016_a_31_03_2023 %>% 
  rename(DV = `Direccion viento (°)`,
         VV = `Velocidad del viento (m/s)`,
         TEMP = `Temperatura (°C)`)


# IMPORT MP10 DATA
calidad_aire_dia_hora_Sierra_Gorda_01_05_2016_a_31_03_2023 <- read_csv("GeoAire/SIERRA_GORDA/Observados/MachineLearning/calidad-aire-dia-hora_Sierra_Gorda_01-05-2016_a_31-03-2023.csv",
                                                                       col_types = cols(Fecha = col_date(format = "%d/%m/%Y")))

# CREAR COLUMNA CON FECHA Y HORA JUNTOS
calidad_aire_dia_hora_Sierra_Gorda_01_05_2016_a_31_03_2023 <- calidad_aire_dia_hora_Sierra_Gorda_01_05_2016_a_31_03_2023 %>% 
  mutate(DateAndTime = as.POSIXct(paste(Fecha, Hora, sep = " "), format = "%Y-%m-%d %H"))

# ELMINAR COLUMNAS: FECHA Y HORA (POR SEPARADO)
calidad_aire_dia_hora_Sierra_Gorda_01_05_2016_a_31_03_2023 <-  calidad_aire_dia_hora_Sierra_Gorda_01_05_2016_a_31_03_2023 %>% 
  select(-c(1:2))

# REORDENAR COLUMNAS
calidad_aire_dia_hora_Sierra_Gorda_01_05_2016_a_31_03_2023 <-  calidad_aire_dia_hora_Sierra_Gorda_01_05_2016_a_31_03_2023 %>% 
  relocate(DateAndTime)


# RENAME COLUMNS
calidad_aire_dia_hora_Sierra_Gorda_01_05_2016_a_31_03_2023 <- calidad_aire_dia_hora_Sierra_Gorda_01_05_2016_a_31_03_2023 %>% 
  rename(MP10 = `MP10 (µg/m³N)`)


# CREAR NUEVO DATAFRAME CON DATA COMBINADA DE METEO Y MP10
SG_Obs_MetMP10_2016_2022 <- Sierra_Gorda_01_05_2016_a_31_03_2023 %>% 
  inner_join(calidad_aire_dia_hora_Sierra_Gorda_01_05_2016_a_31_03_2023, by = "DateAndTime")


# EXPORT DATA
write_csv(SG_Obs_MetMP10_2016_2022, file = "SG_Obs_MetMP10_2016_2022.csv")












































































































































































