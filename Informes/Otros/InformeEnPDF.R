library(openair)
library(reshape2)
library(dygraphs)
library(xts)
library(gplots)
library(DTWBI)
library(hydroGOF)

# MET MOD #

Met_Modelados <- list.files(path = "SIERRA_GORDA/Modelados/pronostico-alerta/sg/2022/9.Septiembre/Meteorologia",    
                            pattern = "*.xlsx",
                            full.names = TRUE) %>% 
  lapply(read_excel) 

for (i in 1:30) {
  Met_Modelados[[i]] <- Met_Modelados[[i]][1:24,c(1,6:8)]
}

Met_Modelados <-  bind_rows(Met_Modelados)

# MET OBS #

Met_Observados <- read_excel("SIERRA_GORDA/Observados/2022/9.Septiembre/Meteorologia/Consultas en Linea.xlsx")

Met_Observados <- Met_Observados %>% 
  mutate(DateAndTime = as.POSIXct(paste(Fecha, Hora), format = "%d/%m/%Y %H"),
         DIR = as.numeric(DIR),
         VEL = as.numeric(VEL)) %>% 
  select(DateAndTime, DIR, VEL, TEMP)

# MP10 MOD #

MP10_Modelados <- list.files(path = "SIERRA_GORDA/Modelados/pronostico-alerta/sg/2022/9.Septiembre/MP10",    
                             pattern = "*.xlsx",
                             full.names = TRUE) %>% 
  lapply(read_excel) 

for (i in 1:30) {
  MP10_Modelados[[i]] <- MP10_Modelados[[i]][1:24,]
}

MP10_Modelados <-  bind_rows(MP10_Modelados)

MeanMP10byHour_Modelados <- MP10_Modelados %>% 
  select(hora, VALOR) %>% 
  group_by(hora) %>% 
  summarise(VALOR = mean(VALOR, na.rm = TRUE))

FechayValor_MP10Mod <- MP10_Modelados %>%
  mutate(DateAndTime = as.POSIXct(paste(Año, mes, dia, hora), format = "%Y %m %d %H"))%>%
  dplyr::select(DateAndTime, VALOR)

# MP10 OBS #

MP10_Observados <- read_excel("SIERRA_GORDA/Observados/2022/9.Septiembre/MP10/Consultas en Linea.xlsx")

MeanMP10byHour_Observados <- MP10_Observados %>% 
  mutate(Hora = as.numeric(Hora), MP_10 = as.numeric(MP_10)) %>% 
  select(Hora, MP_10) %>% 
  group_by(Hora) %>% 
  summarise(MP_10 = mean(MP_10, na.rm = TRUE))

FechayValor_MP10Obs <- MP10_Observados %>%
  mutate(DateAndTime = as.POSIXct(paste(Fecha, Hora), format = "%d/%m/%Y %H"),
         MP_10 = as.numeric(MP_10)) %>%
  dplyr::select(DateAndTime, MP_10)

ExactitudMP10 <- FechayValor_MP10Obs %>% 
  mutate(MP_10_MOD = FechayValor_MP10Mod$VALOR,
         Bueno = case_when((between(MP_10, 0, 100) & between(MP_10_MOD, 0, 100)) | (!between(MP_10, 0, 100) & (!between(MP_10_MOD, 0, 100))) ~ 1,
                           TRUE ~ 0),
         Alerta1 = case_when((between(MP_10, 100, 150) & between(MP_10_MOD, 100, 150)) | (!between(MP_10, 100, 150) & (!between(MP_10_MOD, 100, 150))) ~ 1,
                             TRUE ~ 0),
         Alerta2 = case_when((MP_10 > 150 & MP_10_MOD > 150) | (MP_10 < 150 & MP_10_MOD < 150) ~ 1,
                             TRUE ~ 0))








# COMBINE DATAFRAMES

MP10 <- MP10_Observados %>% 
  mutate(MP_10_MOD = MP10_Modelados$VALOR)

MP10 <- melt(MP10, id = c("Hora"))

MP10 %>% 
  ggplot(aes(x = Hora, y = value, color = variable)) +
  geom_line() +
  theme_bw() +
  labs(x = "Hora del día", y = "MP10 (µg/m3N)") +
  theme(legend.position = "top", legend.title = element_blank()) +
  scale_color_discrete(labels = c("Observado", "Modelado")) +
  scale_x_continuous(breaks = seq(0,23,1))
  


Temperatura <- Met_Observados[,c(1,4)] %>% 
  mutate(TEMP_MOD = Met_Modelados$TEMP)

Temperatura <- melt(Temperatura, id = c("DateAndTime"))

Temperatura %>%
  ggplot(aes(x = DateAndTime, y = value, color = variable)) +
  geom_line() +
  theme_bw() +
  theme(legend.position = "top", legend.title = element_blank(), axis.text.x = element_text(angle = 30, hjust = 1)) +
  labs(x = "", y = "Temperatura (°C)") +
  scale_color_discrete(labels = c("Observado", "Modelado")) +
  scale_x_datetime(date_labels = "%d %b", date_breaks = "4 day")

Velocidad <- Met_Observados[,c(1,3)] %>% 
  mutate(VEL_MOD = Met_Modelados$WSPEED)

Velocidad <- melt(Velocidad, id = c("DateAndTime"))

Velocidad %>%
  ggplot(aes(x = DateAndTime, y = value, color = variable)) +
  geom_line() +
  theme_bw() +
  theme(legend.position = "top", legend.title = element_blank(), axis.text.x = element_text(angle = 30, hjust = 1)) +
  labs(x = "", y = "Velocidad de Viento (m/s)") +
  scale_color_discrete(labels = c("Observado", "Modelado")) +
  scale_x_datetime(date_labels = "%d %b", date_breaks = "4 day")


colnames(Met_Observados) <- c("date", "wd", "ws", "t")

windRose(Met_Observados, angle=20, paddle=FALSE, offset=5, key.header= "Wind Speed (m/s)",
         key.footer="", key.position="right", auto.text = FALSE, annotate = FALSE)

windRose(Met_Observados, key.position="right", auto.text = FALSE, paddle = FALSE, annotate = c("Promedio", "Calma"), key.header = "Wind Speed (m/s)",
         key.footer = "")

colnames(Met_Modelados) <- c("date", "t", "wd", "ws")


MetObs_Diurno <- selectByDate(Met_Observados, hour = 08:20)

MetObs_Nocturno <- selectByDate(Met_Observados, hour = c(00:07, 21:23))

MetMod_Diurno <- selectByDate(Met_Modelados, hour = 08:20)

MetMod_Nocturno <- selectByDate(Met_Modelados, hour = c(00:07, 21:23))

# CICLO HORARIO DIRECCION DE VIENTO:

#Met_Observados %>% 
  # mutate(hora = hour(date)) %>% 
  # ggplot(aes(x = hora, y = wd)) +
  # geom_bin2d()

#filled.contour(kde2d(x = hour(Met_Modelados$date), y = Met_Modelados$wd))

#image(kde2d(x = hour(Met_Modelados$date), y = Met_Modelados$wd), col = r)

library(RColorBrewer)
rf <- colorRampPalette(rev(brewer.pal(11,'Spectral')))
r <- rf(32)

HoraYdireccion <-  Met_Observados %>% 
  mutate(hora = hour(date)) %>% 
  dplyr::select(hora, wd)

hist2d(HoraYdireccion, nbins = 25, col = r, xlab = "Horas del día", ylab = "Dirección del Viento")





## estadisticos

MFBias = 200 * mean((Met_Modelados$TEMP - Met_Observados$TEMP) / (Met_Modelados$TEMP + Met_Observados$TEMP), na.rm = TRUE)

round(MFBias, 2)



compute.fb(Met_Modelados$TEMP, Met_Observados$TEMP)

md(Met_Modelados$TEMP, Met_Observados$TEMP)





























