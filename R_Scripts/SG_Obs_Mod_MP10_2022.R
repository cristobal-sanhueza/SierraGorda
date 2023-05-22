library(readxl)
library(readr)





################################### DATA OBSERVADA ################################### 

# IMPORT OBSERVED DATA
SG_Obs <- read_csv("GeoAire/SIERRA_GORDA/Observados/2022/AñoCompleto/calidad-aire-dia-hora_Sierra Gorda.csv")

# CREAR DATE-TIME COLUMN
SG_Obs <- SG_Obs %>% 
  mutate(DateAndTime = as.POSIXct(paste(Fecha, Hora, sep = " "), format = "%d/%m/%Y %H"))

# RELOCATE DATE-TIME COLUMNS
SG_Obs <- SG_Obs %>% 
  relocate(DateAndTime)

# ELIMINAR USELESS COLUMNS
SG_Obs <- SG_Obs %>% 
  select(DateAndTime, MP10)

# IMPORT JANUARY OBSERVED DATA
SG_Obs_Enero2023 <- read_excel("GeoAire/SIERRA_GORDA/Observados/2023/1.Enero/MP10/Consultas en Linea.xlsx")

# CREAR DATE-TIME COLUMN
SG_Obs_Enero2023 <- SG_Obs_Enero2023 %>% 
  mutate(DateAndTime = as.POSIXct(paste(Fecha, Hora, sep = " "), format = "%d/%m/%Y %H"))

# RELOCATE DATE-TIME COLUMNS
SG_Obs_Enero2023 <- SG_Obs_Enero2023 %>% 
  relocate(DateAndTime)

# ELIMINAR USELESS COLUMNS
SG_Obs_Enero2023 <- SG_Obs_Enero2023 %>% 
  select(DateAndTime, MP_10)

# RENAME MP10 COLUMNS
SG_Obs_Enero2023 <- SG_Obs_Enero2023 %>% 
  rename(MP10 = MP_10)

# IMPORT FEBRUARY OBSERVED DATA
SG_Obs_Febrero2023 <- read_excel("GeoAire/SIERRA_GORDA/Observados/2023/2.Febrero/MP10/Consultas en Linea.xlsx")

# CREAR DATE-TIME COLUMN
SG_Obs_Febrero2023 <- SG_Obs_Febrero2023 %>% 
  mutate(DateAndTime = as.POSIXct(paste(Fecha, Hora, sep = " "), format = "%d/%m/%Y %H"))

# RELOCATE DATE-TIME COLUMNS
SG_Obs_Febrero2023 <- SG_Obs_Febrero2023 %>% 
  relocate(DateAndTime)

# ELIMINAR USELESS COLUMNS
SG_Obs_Febrero2023 <- SG_Obs_Febrero2023 %>% 
  select(DateAndTime, MP_10)

# RENAME MP10 COLUMNS
SG_Obs_Febrero2023 <- SG_Obs_Febrero2023 %>% 
  rename(MP10 = MP_10)


# ADD JANUARY AND FEBRUARY DATA
SG_Obs <- bind_rows(SG_Obs, SG_Obs_Enero2023, SG_Obs_Febrero2023)


# WRITE DATA

write_csv(SG_Obs, file = "SG_Obs_Marzo2022_Febrero2023.csv")

################################### DATA MODELADA ################################### 

# MARZO 2022
SG_Mod_Marzo2022 <- list.files(path = "Geoaire/SIERRA_GORDA/Modelados/pronostico-alerta/sg/2022/3.Marzo/MP10",    
                        pattern = "*.xlsx",
                        full.names = TRUE) %>% 
  lapply(read_excel)

for (i in 1:length(SG_Mod_Marzo2022)) {
  
  SG_Mod_Marzo2022[[i]] <- SG_Mod_Marzo2022[[i]] %>% 
    mutate(DateAndTime = as.POSIXct(paste(Año, mes, dia, hora, sep = " "), format = "%Y %m %d %H"))
  
  SG_Mod_Marzo2022[[i]] <- SG_Mod_Marzo2022[[i]] %>% 
    filter(dia == i)
  
}

SG_Mod_Marzo2022 <-  bind_rows(SG_Mod_Marzo2022)


# ABRIL 2022
SG_Mod_Abril2022 <- list.files(path = "Geoaire/SIERRA_GORDA/Modelados/pronostico-alerta/sg/2022/4.Abril/MP10",    
                               pattern = "*.xlsx",
                               full.names = TRUE) %>% 
  lapply(read_excel)

for (i in 1:length(SG_Mod_Abril2022)) {
  
  SG_Mod_Abril2022[[i]] <- SG_Mod_Abril2022[[i]] %>% 
    mutate(DateAndTime = as.POSIXct(paste(Año, mes, dia, hora, sep = " "), format = "%Y %m %d %H"))
  
  SG_Mod_Abril2022[[i]] <- SG_Mod_Abril2022[[i]] %>% 
    filter(dia == i)
  
}

SG_Mod_Abril2022 <-  bind_rows(SG_Mod_Abril2022)

# MAYO 2022
SG_Mod_Mayo2022 <- list.files(path = "Geoaire/SIERRA_GORDA/Modelados/pronostico-alerta/sg/2022/5.Mayo/MP10",    
                               pattern = "*.xlsx",
                               full.names = TRUE) %>% 
  lapply(read_excel)

for (i in 1:length(SG_Mod_Mayo2022)) {
  
  SG_Mod_Mayo2022[[i]] <- SG_Mod_Mayo2022[[i]] %>% 
    mutate(DateAndTime = as.POSIXct(paste(Año, mes, dia, hora, sep = " "), format = "%Y %m %d %H"))
  
  SG_Mod_Mayo2022[[i]] <- SG_Mod_Mayo2022[[i]] %>% 
    filter(dia == i)
  
}

SG_Mod_Mayo2022 <-  bind_rows(SG_Mod_Mayo2022)

# JUNIO 2022
SG_Mod_Junio2022 <- list.files(path = "Geoaire/SIERRA_GORDA/Modelados/pronostico-alerta/sg/2022/6.Junio/MP10",    
                               pattern = "*.xlsx",
                               full.names = TRUE) %>% 
  lapply(read_excel)

for (i in 1:length(SG_Mod_Junio2022)) {
  
  SG_Mod_Junio2022[[i]] <- SG_Mod_Junio2022[[i]] %>% 
    mutate(DateAndTime = as.POSIXct(paste(Año, mes, dia, hora, sep = " "), format = "%Y %m %d %H"))
  
  SG_Mod_Junio2022[[i]] <- SG_Mod_Junio2022[[i]] %>% 
    filter(dia == i)
  
}

SG_Mod_Junio2022 <-  bind_rows(SG_Mod_Junio2022)

# JULIO 2022
SG_Mod_Julio2022 <- list.files(path = "Geoaire/SIERRA_GORDA/Modelados/pronostico-alerta/sg/2022/7.Julio/MP10",    
                               pattern = "*.xlsx",
                               full.names = TRUE) %>% 
  lapply(read_excel)

for (i in 1:length(SG_Mod_Julio2022)) {
  
  SG_Mod_Julio2022[[i]] <- SG_Mod_Julio2022[[i]] %>% 
    mutate(DateAndTime = as.POSIXct(paste(Año, mes, dia, hora, sep = " "), format = "%Y %m %d %H"))
  
  SG_Mod_Julio2022[[i]] <- SG_Mod_Julio2022[[i]] %>% 
    filter(dia == i)
  
}

SG_Mod_Julio2022 <-  bind_rows(SG_Mod_Julio2022)

# AGOSTO 2022
SG_Mod_Agosto2022 <- list.files(path = "Geoaire/SIERRA_GORDA/Modelados/pronostico-alerta/sg/2022/8.Agosto/MP10",    
                               pattern = "*.xlsx",
                               full.names = TRUE) %>% 
  lapply(read_excel)

for (i in 1:length(SG_Mod_Agosto2022)) {
  
  SG_Mod_Agosto2022[[i]] <- SG_Mod_Agosto2022[[i]] %>% 
    mutate(DateAndTime = as.POSIXct(paste(Año, mes, dia, hora, sep = " "), format = "%Y %m %d %H"))
  
  SG_Mod_Agosto2022[[i]] <- SG_Mod_Agosto2022[[i]] %>% 
    filter(dia == i)
  
}

SG_Mod_Agosto2022 <-  bind_rows(SG_Mod_Agosto2022)

# SEPTIEMBRE 2022
SG_Mod_Septiembre2022 <- list.files(path = "Geoaire/SIERRA_GORDA/Modelados/pronostico-alerta/sg/2022/9.Septiembre/MP10",    
                               pattern = "*.xlsx",
                               full.names = TRUE) %>% 
  lapply(read_excel)

for (i in 1:length(SG_Mod_Septiembre2022)) {
  
  SG_Mod_Septiembre2022[[i]] <- SG_Mod_Septiembre2022[[i]] %>% 
    mutate(DateAndTime = as.POSIXct(paste(Año, mes, dia, hora, sep = " "), format = "%Y %m %d %H"))
  
  SG_Mod_Septiembre2022[[i]] <- SG_Mod_Septiembre2022[[i]] %>% 
    filter(dia == i)
  
}

SG_Mod_Septiembre2022 <-  bind_rows(SG_Mod_Septiembre2022)

# OCTUBRE 2022
SG_Mod_Octubre2022 <- list.files(path = "Geoaire/SIERRA_GORDA/Modelados/pronostico-alerta/sg/2022/10.Octubre/MP10",    
                               pattern = "*.xlsx",
                               full.names = TRUE) %>% 
  lapply(read_excel)

for (i in 1:length(SG_Mod_Octubre2022)) {
  
  SG_Mod_Octubre2022[[i]] <- SG_Mod_Octubre2022[[i]] %>% 
    mutate(DateAndTime = as.POSIXct(paste(Año, mes, dia, hora, sep = " "), format = "%Y %m %d %H"))
  
  SG_Mod_Octubre2022[[i]] <- SG_Mod_Octubre2022[[i]] %>% 
    filter(dia == i)
  
}

SG_Mod_Octubre2022 <-  bind_rows(SG_Mod_Octubre2022)

# NOVIEMBRE 2022
SG_Mod_Noviembre2022 <- list.files(path = "Geoaire/SIERRA_GORDA/Modelados/pronostico-alerta/sg/2022/11.Noviembre/MP10",    
                               pattern = "*.xlsx",
                               full.names = TRUE) %>% 
  lapply(read_excel)

for (i in 1:length(SG_Mod_Noviembre2022)) {
  
  SG_Mod_Noviembre2022[[i]] <- SG_Mod_Noviembre2022[[i]] %>% 
    mutate(DateAndTime = as.POSIXct(paste(Año, mes, dia, hora, sep = " "), format = "%Y %m %d %H"))
  
  SG_Mod_Noviembre2022[[i]] <- SG_Mod_Noviembre2022[[i]] %>% 
    filter(dia == i)
  
}

SG_Mod_Noviembre2022 <-  bind_rows(SG_Mod_Noviembre2022)

# DICIEMBRE 2022
SG_Mod_Diciembre2022 <- list.files(path = "Geoaire/SIERRA_GORDA/Modelados/pronostico-alerta/sg/2022/12.Diciembre/MP10",    
                               pattern = "*.xlsx",
                               full.names = TRUE) %>% 
  lapply(read_excel)

for (i in 1:length(SG_Mod_Diciembre2022)) {
  
  SG_Mod_Diciembre2022[[i]] <- SG_Mod_Diciembre2022[[i]] %>% 
    mutate(DateAndTime = as.POSIXct(paste(Año, mes, dia, hora, sep = " "), format = "%Y %m %d %H"))
  
  SG_Mod_Diciembre2022[[i]] <- SG_Mod_Diciembre2022[[i]] %>% 
    filter(dia == i)
  
}

SG_Mod_Diciembre2022 <-  bind_rows(SG_Mod_Diciembre2022)


# ENERO 2023
SG_Mod_Enero2023 <- list.files(path = "Geoaire/SIERRA_GORDA/Modelados/pronostico-alerta/sg/2023/1.Enero/MP10",    
                                   pattern = "*.xlsx",
                                   full.names = TRUE) %>% 
  lapply(read_excel)

for (i in 1:length(SG_Mod_Enero2023)) {
  
  SG_Mod_Enero2023[[i]] <- SG_Mod_Enero2023[[i]] %>% 
    mutate(DateAndTime = as.POSIXct(paste(Año, mes, dia, hora, sep = " "), format = "%Y %m %d %H"))
  
  SG_Mod_Enero2023[[i]] <- SG_Mod_Enero2023[[i]] %>% 
    filter(dia == i)
  
}

SG_Mod_Enero2023 <-  bind_rows(SG_Mod_Enero2023)

# FEBRERO 2023
SG_Mod_Febrero2023 <- list.files(path = "Geoaire/SIERRA_GORDA/Modelados/pronostico-alerta/sg/2023/2.Febrero/MP10",    
                                   pattern = "*.xlsx",
                                   full.names = TRUE) %>% 
  lapply(read_excel)

for (i in 1:length(SG_Mod_Febrero2023)) {
  
  SG_Mod_Febrero2023[[i]] <- SG_Mod_Febrero2023[[i]] %>% 
    mutate(DateAndTime = as.POSIXct(paste(Año, mes, dia, hora, sep = " "), format = "%Y %m %d %H"))
  
  SG_Mod_Febrero2023[[i]] <- SG_Mod_Febrero2023[[i]] %>% 
    filter(dia == i)
  
}

SG_Mod_Febrero2023 <-  bind_rows(SG_Mod_Febrero2023)


# COMBINE FINAL DF

FinalDF <- bind_rows(SG_Mod_Marzo2022,
                     SG_Mod_Abril2022,
                     SG_Mod_Mayo2022,
                     SG_Mod_Junio2022,
                     SG_Mod_Julio2022,
                     SG_Mod_Agosto2022,
                     SG_Mod_Septiembre2022,
                     SG_Mod_Octubre2022,
                     SG_Mod_Noviembre2022,
                     SG_Mod_Diciembre2022,
                     SG_Mod_Enero2023,
                     SG_Mod_Febrero2023)


# WRITE DATA

write_csv(FinalDF, file = "SG_Mod_Marzo2022_Febrero2023.csv")


## combine

MyFinal <- FinalDF %>% 
  select(DateAndTime, VALOR)

MyFinal_2 <- SG_Obs %>% 
  inner_join(MyFinal)


write_csv(MyFinal_2, file = "SG_Obs_Mod.csv")

library(ggplot2)


ObsModSO2_SanFdo <- AvgHourlySO2Obs_SanFdo %>% 
  mutate(VALOR_Mod = AvgHourlySO2Mod_SanFdo$VALOR)

ObsModSO2_SanFdo <- pivot_longer(ObsModSO2_SanFdo, cols = c("VALOR", "VALOR_Mod"))

ObsModSO2_SanFdo %>% 
  ggplot(aes(x = Hora, y = value, color = name)) +
  geom_line() +
  theme_bw() +
  labs(x = "Hora del día", y = "SO2 (µg/m3N)") +
  theme(legend.position = "top", legend.title = element_blank()) +
  scale_color_discrete(labels = c("Observado", "Modelado")) +
  scale_x_continuous(breaks = seq(0,23,1))


library(tidyr)

MyFinal_2$MP10 <- as.numeric(MyFinal_2$MP10)

MyFinal_2_melt <- pivot_longer(MyFinal_2, cols = c("MP10", "VALOR"))

MyFinal_2_melt %>% 
  ggplot(aes(x = DateAndTime, y = value, color = name)) +
  geom_line() +
  theme_bw()


### media diaria
library(lubridate)

df_avg <- MyFinal_2 %>%
  mutate(date = as.Date(DateAndTime)) %>%
  group_by(date) %>%
  summarise(avg_observed_pm10 = mean(MP10),
            avg_predicted_pm10 = mean(VALOR))


library(ggplot2)

# create the bar plot
ggplot(df_avg_clean, aes(x = date, y = avg_observed_pm10)) +
  geom_bar(stat = "identity", fill = "blue", alpha = 0.5) +
  geom_bar(aes(y = avg_predicted_pm10), stat = "identity", fill = "red", alpha = 0.5) +
  xlab("Date") +
  ylab("PM10") +
  ggtitle("Average Observed and Predicted PM10") +
  theme(plot.title = element_text(hjust = 0.5))



df_avg_clean <- na.omit(df_avg)



####

df_avg_clean_longer <- pivot_longer(df_avg_clean, cols = c("avg_observed_pm10", "avg_predicted_pm10"))

df_avg_clean_longer %>% 
  ggplot(aes(x = date, y = value, color = name)) +
  geom_line() +
  theme_bw() +
  labs(x = "Hora del día", y = "SO2 (µg/m3N)") +
  theme(legend.position = "top", legend.title = element_blank()) +
  scale_color_discrete(labels = c("Observado", "Modelado")) +
  scale_x_continuous(breaks = seq(0,23,1))





















































































































































































