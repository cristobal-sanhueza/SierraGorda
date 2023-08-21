
library(openair)
library(pander)
library(tidyverse)
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

MP10_Modelados <- read_excel("Modelados/Geologger/SG_PRO_1JAN_30JUN_2023_MP10.xlsx")

MeanMP10byHour_Modelados <- MP10_Modelados %>% 
  select(Hora, MP10_MOD) %>% 
  group_by(Hora) %>% 
  summarise(MP10_MOD = mean(MP10_MOD, na.rm = TRUE))



#MP10 OBS #

MP10_Observados <- read_csv("Observados/Geologger/calidad-aire-dia-hora_Sierra_Gorda_01-01-2023_a_30-06-2023.csv")

MeanMP10byHour_Observados <- MP10_Observados %>%
  select(Hora, MP10) %>% 
  group_by(Hora) %>% 
  summarise(MP10 = mean(MP10, na.rm = TRUE))


# MET MOD #

Met_Modelados <- read_excel("Modelados/Geologger/SG_PRO_1JAN_30JUN_2023_MET.xlsx")

Met_Modelados <- Met_Modelados %>% 
  mutate(DateAndTime = as.POSIXct(paste(Fecha, Hora), format = "%d/%m/%Y %H")) %>% 
  select(DateAndTime, DV, VV, TEMP)


# MET OBS #

Met_Observados <- read_csv("Observados/Geologger/Sierra_Gorda_01-01-2023_a_30-06-2023.csv")

Met_Observados <- Met_Observados %>% 
  mutate(DateAndTime = as.POSIXct(paste(Fecha, Hora), format = "%d/%m/%Y %H")) %>% 
  select(DateAndTime, DV, VV, TEMP)


Temperatura <- Met_Observados[,c(1,4)] %>% 
  mutate(TEMP_MOD = Met_Modelados$TEMP)

Temperatura <- pivot_longer(Temperatura, cols = c("TEMP", "TEMP_MOD"))

Temperatura %>%
  ggplot(aes(x = DateAndTime, y = value, color = name)) +
  geom_line() +
  theme_bw() +
  theme(legend.position = "top", legend.title = element_blank(), axis.text.x = element_text(angle = 30, hjust = 1)) +
  labs(x = "", y = "Temperatura (°C)") +
  scale_color_discrete(labels = c("Observado", "Modelado")) +
  scale_x_datetime(date_labels = "%d %b", date_breaks = "10 day")


Velocidad <- Met_Observados[,c(1,3)] %>% 
  mutate(VEL_MOD = Met_Modelados$VV)

Velocidad <- pivot_longer(Velocidad, cols = c("VV","VEL_MOD"))

Velocidad %>%
  ggplot(aes(x = DateAndTime, y = value, color = name)) +
  geom_line() +
  theme_bw() +
  theme(legend.position = "top", legend.title = element_blank(), axis.text.x = element_text(angle = 30, hjust = 1)) +
  labs(x = "", y = "Velocidad de Viento (m/s)") +
  scale_color_discrete(labels = c("Observado", "Modelado")) +
  scale_x_datetime(date_labels = "%d %b", date_breaks = "10 day")



colnames(Met_Observados) <- c("date", "wd", "ws", "t")
colnames(Met_Modelados) <- c("date", "wd", "ws", "t")

windRose(Met_Observados, key.position="right", auto.text = FALSE, paddle = FALSE, annotate = c("Promedio", "Calma"), key.header = "Wind Speed (m/s)",
         key.footer = "")

windRose(Met_Modelados, key.position="right", auto.text = FALSE, paddle = FALSE, annotate = c("Promedio", "Calma"), key.header = "Wind Speed (m/s)",
         key.footer = "")


MetObs_Diurno <- selectByDate(Met_Observados, hour = 08:20)
MetMod_Diurno <- selectByDate(Met_Modelados, hour = 08:20)

windRose(MetObs_Diurno, key.position="right", auto.text = FALSE, paddle = FALSE, annotate = c("Promedio", "Calma"), key.header = "Wind Speed (m/s)",
         key.footer = "")

windRose(MetMod_Diurno, key.position="right", auto.text = FALSE, paddle = FALSE, annotate = c("Promedio", "Calma"), key.header = "Wind Speed (m/s)",
         key.footer = "")



rf <- colorRampPalette(rev(brewer.pal(11,'Spectral')))
r <- rf(32)

HoraYdireccion_Obs <-  Met_Observados %>% 
  mutate(hora = hour(date)) %>% 
  dplyr::select(hora, wd)

hist2d(HoraYdireccion_Obs, nbins = 25, col = r, xlab = "Horas del día", ylab = "Dirección del Viento")

HoraYdireccion_Mod <-  Met_Modelados %>% 
  mutate(hora = hour(date)) %>% 
  dplyr::select(hora, wd)

hist2d(HoraYdireccion_Mod, nbins = 25, col = r, xlab = "Horas del día", ylab = "Dirección del Viento")



MP10 <- MeanMP10byHour_Observados %>% 
  mutate(MP10_MOD = MeanMP10byHour_Modelados$MP10_MOD)

MP10 <- pivot_longer(MP10, cols = c("MP10", "MP10_MOD"))

MP10 %>% 
  ggplot(aes(x = Hora, y = value, color = name)) +
  geom_line() +
  theme_bw() +
  labs(x = "Hora del día", y = "MP10 (µg/m3N)") +
  theme(legend.position = "top", legend.title = element_blank()) +
  scale_color_discrete(labels = c("Observado", "Modelado")) +
  scale_x_continuous(breaks = seq(0,23,1))


FechayValor_MP10Obs <- MP10_Observados %>%
  dplyr::select(MP10)

FechayValor_MP10Mod <- MP10_Modelados %>%
  dplyr::select(MP10_MOD)

ExactitudMP10 <- FechayValor_MP10Obs %>% 
  mutate(MP10_MOD = FechayValor_MP10Mod$MP10_MOD,
         Bueno = case_when((between(MP10, 0, 130) & between(MP10_MOD, 0, 130)) | (!between(MP10, 0, 130) & (!between(MP10_MOD, 0, 130))) ~ 1,
                           TRUE ~ 0),
         Alerta1 = case_when((between(MP10, 130, 180) & between(MP10_MOD, 130, 180)) | (!between(MP10, 130, 180) & (!between(MP10_MOD, 130, 180))) ~ 1,
                             TRUE ~ 0),
         Alerta2 = case_when((MP10 > 180 & MP10_MOD > 180) | (MP10 < 180 & MP10_MOD < 180) ~ 1,
                             TRUE ~ 0))

pct_bueno = sum(ExactitudMP10$Bueno, na.rm = TRUE) / nrow(ExactitudMP10)
pct_alerta1 = sum(ExactitudMP10$Alerta1, na.rm = TRUE) / nrow(ExactitudMP10)
pct_alerta2 = sum(ExactitudMP10$Alerta2, na.rm = TRUE) / nrow(ExactitudMP10)
promedio = mean(c(pct_bueno, pct_alerta1, pct_alerta2))
































































































































































































