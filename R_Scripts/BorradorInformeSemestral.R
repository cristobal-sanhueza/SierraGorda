

setwd("/Users/cristobal512/Desktop/geoaire/GeoaireProject/GeoAire")
library(openair)
library(pander)
library(tidyverse)
library(reshape2)
library(dygraphs)
library(xts)
library(readxl)
library(RColorBrewer)
library(data.table)
library(gplots)
library(scales)
library(DTWBI)
library(hydroGOF)
library(cvms)
library(lubridate)

# MP10 MOD #

MP10_Modelados <- list.files(path = "SIERRA_GORDA/Modelados/pronostico-alerta/sg/2022/Semestral/JulioDiciembre/MP10",
                             pattern = "*.xlsx",
                             full.names = TRUE) %>%
  lapply(read_excel)

for (i in 1:length(MP10_Modelados)) {
  MP10_Modelados[[i]] <- MP10_Modelados[[i]][1:24,]
}

MP10_Modelados <-  bind_rows(MP10_Modelados)

MeanMP10byHour_Modelados <- MP10_Modelados %>% 
  select(hora, VALOR) %>% 
  group_by(hora) %>% 
  summarise(VALOR = mean(VALOR, na.rm = TRUE))

MP10_Modelados <- MP10_Modelados %>% 
  mutate(DateAndTime = as.POSIXct(paste(Año, mes, dia, hora), format = "%Y %m %d %H"))

#MP10 OBS #

MP10_Observados <- list.files(path = "SIERRA_GORDA/Observados/Semestral/JulioDiciembre2022/MP10",
                              pattern = "*.xlsx",
                              full.names = TRUE) %>% 
  lapply(read_excel)

MP10_Observados <-  bind_rows(MP10_Observados)

MP10_Observados <- MP10_Observados %>% 
  mutate(MP_10 = as.numeric(MP_10))

MeanMP10byHour_Observados <- MP10_Observados %>% 
  mutate(Hora = as.numeric(Hora), MP_10 = as.numeric(MP_10)) %>% 
  select(Hora, MP_10) %>% 
  group_by(Hora) %>% 
  summarise(MP_10 = mean(MP_10, na.rm = TRUE))


MP10_Observados <-  MP10_Observados %>% 
  mutate(DateAndTime = as.POSIXct(paste(Fecha, Hora), format = "%d/%m/%Y %H")) %>% 
  relocate(DateAndTime, .after = MP_10)

MP10_Observados <- MP10_Observados %>% 
  inner_join(select(MP10_Modelados, DateAndTime), by = "DateAndTime")

MP10_Modelados <- MP10_Modelados %>% 
  inner_join(select(MP10_Observados, DateAndTime), by = "DateAndTime")

# MET MOD #

Met_Modelados <- list.files(path = "SIERRA_GORDA/Modelados/pronostico-alerta/sg/2022/Semestral/JulioDiciembre/Meteorologia",    
                            pattern = "*.xlsx",
                            full.names = TRUE) %>% 
  lapply(read_excel) 

for (i in 1:length(Met_Modelados)) {
  Met_Modelados[[i]] <- Met_Modelados[[i]][1:24,c(1,6:8)]
}

Met_Modelados <-  bind_rows(Met_Modelados)

Met_Modelados <- Met_Modelados %>% 
  arrange(FECHA)

Met_Modelados <- Met_Modelados[!duplicated(Met_Modelados$FECHA), ]

# MET OBS #

Met_Observados <- list.files("SIERRA_GORDA/Observados/Semestral/JulioDiciembre2022/Meteorologia",
                              pattern = "*.xlsx",
                              full.names = TRUE) %>% 
  lapply(read_excel, col_types = c("numeric", "text", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric")) %>% 
  bind_rows() %>% 
  mutate(DateAndTime = as.POSIXct(paste(Fecha, Hora), format = "%d/%m/%Y %H")) %>% 
  select(DateAndTime, DIR, VEL, TEMP) %>% 
  arrange(DateAndTime)

Met_Observados <- Met_Observados %>% 
  inner_join(select(Met_Modelados, FECHA), by = c("DateAndTime" = "FECHA"))

Met_Modelados <- Met_Modelados %>% 
  inner_join(select(Met_Observados, DateAndTime), by = c("FECHA" = "DateAndTime"))

pureba <- Met_Modelados %>% 
  mutate(dia = day(FECHA))



############################################################################################################

Temperatura <- Met_Observados[,c(1,4)] %>% 
  mutate(TEMP_MOD = Met_Modelados$TEMP)

Temperatura <- melt(Temperatura, id = c("DateAndTime"))

Temperatura %>%
  ggplot(aes(x = DateAndTime, y = value, color = variable)) +
  geom_line() +
  theme_bw() +
  theme(legend.position = "top", legend.title = element_blank(), axis.text.x = element_text(angle = 40, hjust = 1)) +
  labs(x = "", y = "Temperatura (°C)") +
  scale_color_discrete(labels = c("Observado", "Modelado")) +
  scale_x_datetime(date_labels = "%d %b", date_breaks = "12 day")



Velocidad <- Met_Observados[,c(1,3)] %>% 
  mutate(VEL_MOD = Met_Modelados$WSPEED)

Velocidad <- melt(Velocidad, id = c("DateAndTime"))

Velocidad %>%
  ggplot(aes(x = DateAndTime, y = value, color = variable)) +
  geom_line() +
  theme_bw() +
  theme(legend.position = "top", legend.title = element_blank(), axis.text.x = element_text(angle = 40, hjust = 1)) +
  labs(x = "", y = "Velocidad de Viento (m/s)") +
  scale_color_discrete(labels = c("Observado", "Modelado")) +
  scale_x_datetime(date_labels = "%d %b", date_breaks = "12 day")



