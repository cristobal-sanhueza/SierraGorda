
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

#MP10 MOD #

path_mp10_mod <- paste0('/Users/cristobal512/Desktop/geoaire/SIERRA_GORDA/Modelados/Geologger/calidad-aire-dia-hora_Sierra_Gorda-PRO_', start_date, '_a_', end_date, '.csv')

MP10_Modelados <- read_csv(path_mp10_mod, col_types = cols(Fecha = col_date(format = "%d/%m/%Y")))

MP10_Modelados <- MP10_Modelados %>% 
  rename(MP10_Mod = `MP10 (µg/m³N)`)

MP10_Modelados <- MP10_Modelados %>%
  mutate(Fecha = ymd_h(paste(Fecha, Hora)))

MeanMP10byHour_Modelados <- MP10_Modelados %>% 
  select(Hora, MP10_Mod) %>% 
  group_by(Hora) %>% 
  summarise(MP10_Mod = mean(MP10_Mod, na.rm = TRUE))


#MP10 OBS #

path_mp10_obs <- paste0('/Users/cristobal512/Desktop/geoaire/SIERRA_GORDA/Observados/Geologger/calidad-aire-dia-hora_Sierra_Gorda_', start_date, '_a_', end_date, '.csv')

MP10_Observados <- read_csv(path_mp10_obs, col_types = cols(Fecha = col_date(format = "%d/%m/%Y")))

MP10_Observados <- MP10_Observados %>% 
  rename(MP10_Obs = `MP10 (µg/m³N)`)

MP10_Observados <- MP10_Observados %>%
  mutate(Fecha = ymd_h(paste(Fecha, Hora)))

MeanMP10byHour_Observados <- MP10_Observados %>% 
  select(Hora, MP10_Obs) %>% 
  group_by(Hora) %>% 
  summarise(MP10_Obs = mean(MP10_Obs, na.rm = TRUE))


# MET MOD #

path_met_mod <- paste0('/Users/cristobal512/Desktop/geoaire/SIERRA_GORDA/Modelados/Geologger/Sierra_Gorda-PRO_', start_date, '_a_', end_date, '.csv')

Met_Modelados <- read_csv(path_met_mod,
                          col_types = cols(Fecha = col_date(format = "%d/%m/%Y")),
                          locale = locale(decimal_mark = ","))

Met_Modelados <- Met_Modelados %>% 
  rename(date = Fecha,
         wd = `Direccion viento (°)`,
         ws = `Velocidad del viento (m/s)`,
         t = `Temperatura (°C)`)

Met_Modelados <- Met_Modelados %>%
  mutate(date = ymd_h(paste(date, Hora)))


# MET OBS #

path_met_obs <- paste0('/Users/cristobal512/Desktop/geoaire/SIERRA_GORDA/Observados/Geologger/Sierra_Gorda_', start_date, '_a_', end_date, '.csv')

Met_Observados <- read_csv(path_met_obs, col_types = cols(Fecha = col_date(format = "%d/%m/%Y")), locale = locale(decimal_mark = ","))

Met_Observados <- Met_Observados %>% 
  rename(date = Fecha,
         wd = `Direccion viento (°)`,
         ws = `Velocidad del viento (m/s)`,
         t = `Temperatura (°C)`)

Met_Observados <- Met_Observados %>%
  mutate(date = ymd_h(paste(date, Hora)))

# TEMP

Temperatura <- inner_join(Met_Observados[c('date', 't')], Met_Modelados[c('date', 't')], by = "date")

Temperatura_longer <- pivot_longer(Temperatura, cols = c("t.x", "t.y"))

Temperatura_longer %>%
  ggplot(aes(x = date, y = value, color = name)) +
  geom_line() +
  theme_bw() +
  theme(legend.position = "top", legend.title = element_blank(), axis.text.x = element_text(angle = 30, hjust = 1)) +
  labs(x = "", y = "Temperatura (°C)") +
  scale_color_discrete(labels = c("Observado", "Modelado")) +
  scale_x_datetime(date_labels = "%d %b", date_breaks = "4 day")

# ESTADISTICOS TEMP

MFError = round(200 * mean(abs(Temperatura$t.y - Temperatura$t.x) / (Temperatura$t.y + Temperatura$t.x), na.rm = TRUE), 2)

MFBias <- round(200 * mean((Temperatura$t.y - Temperatura$t.x) / (Temperatura$t.y + Temperatura$t.x), na.rm = TRUE), 2)

IOAgreement = round(md(Temperatura$t.y, Temperatura$t.x), 2)

# VELOCIDAD

Velocidad <- inner_join(Met_Observados[c('date', 'ws')], Met_Modelados[c('date', 'ws')], by = "date")

Velocidad_longer <- pivot_longer(Velocidad, cols = c("ws.x", "ws.y"))

Velocidad_longer %>%
  ggplot(aes(x = date, y = value, color = name)) +
  geom_line() +
  theme_bw() +
  theme(legend.position = "top", legend.title = element_blank(), axis.text.x = element_text(angle = 30, hjust = 1)) +
  labs(x = "", y = "Velocidad de Viento (m/s)") +
  scale_color_discrete(labels = c("Observado", "Modelado")) +
  scale_x_datetime(date_labels = "%d %b", date_breaks = "4 day")

# ESTADISTICOS VELOCIDAD

MFError = round(200 * mean(abs(Velocidad$ws.y - Velocidad$ws.x) / (Velocidad$ws.y + Velocidad$ws.x), na.rm = TRUE), 2)

MFBias <- round(200 * mean((Velocidad$ws.y - Velocidad$ws.x) / (Velocidad$ws.y + Velocidad$ws.x), na.rm = TRUE), 2)

IOAgreement = round(md(Velocidad$ws.y, Velocidad$ws.x), 2)

# WINDROSE

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

# CICLIO HORARIO

rf <- colorRampPalette(rev(brewer.pal(11,'Spectral')))
r <- rf(32)

HoraYdireccion_Obs <-  Met_Observados %>% 
  dplyr::select(Hora, wd)

hist2d(HoraYdireccion_Obs, nbins = 24, col = r, xlab = "Horas del día", ylab = "Dirección del Viento")

HoraYdireccion_Mod <-  Met_Modelados %>% 
  dplyr::select(Hora, wd)

hist2d(HoraYdireccion_Mod, nbins = 24, col = r, xlab = "Horas del día", ylab = "Dirección del Viento")

# ESTADISTICOS DIRECCION DE VIENTO

Direccion <- inner_join(Met_Observados[c('date', 'wd')], Met_Modelados[c('date', 'wd')], by = "date")

MFError = round(200 * mean(abs(Direccion$wd.y - Direccion$wd.x) / (Direccion$wd.y + Direccion$wd.x), na.rm = TRUE), 2)

MFBias <- round(200 * mean((Direccion$wd.y - Direccion$wd.x) / (Direccion$wd.y + Direccion$wd.x), na.rm = TRUE), 2)

# MP10

MP10 <- inner_join(MeanMP10byHour_Observados, MeanMP10byHour_Modelados, by = "Hora")

MP10_longer <- pivot_longer(MP10, cols = c("MP10_Obs", "MP10_Mod"))

MP10_longer %>% 
  ggplot(aes(x = Hora, y = value, color = name)) +
  geom_line() +
  theme_bw() +
  labs(x = "Hora del día", y = "MP10 (µg/m3N)") +
  theme(legend.position = "top", legend.title = element_blank()) +
  scale_color_discrete(labels = c("Observado", "Modelado")) +
  scale_x_continuous(breaks = seq(0,23,1))

# ESTADISTICO MP10

MFError = round(200 * mean(abs(MP10$MP10_Mod - MP10$MP10_Obs) / (MP10$MP10_Mod + MP10$MP10_Obs), na.rm = TRUE), 2)

MFBias <- round(200 * mean((MP10$MP10_Mod - MP10$MP10_Obs) / (MP10$MP10_Mod + MP10$MP10_Obs), na.rm = TRUE), 2)


# EXACTITUD

ExactitudMP10 <- inner_join(MP10_Modelados[c('Fecha', 'MP10_Mod')], MP10_Observados[c('Fecha', 'MP10_Obs')], by = 'Fecha')

ExactitudMP10 <- ExactitudMP10 %>% 
  mutate(Bueno = case_when((between(MP10_Obs, 0, 130) & between(MP10_Mod, 0, 130)) | (!between(MP10_Obs, 0, 130) & (!between(MP10_Mod, 0, 130))) ~ 1,
                           TRUE ~ 0),
         Alerta1 = case_when((between(MP10_Obs, 130, 180) & between(MP10_Mod, 130, 180)) | (!between(MP10_Obs, 130, 180) & (!between(MP10_Mod, 130, 180))) ~ 1,
                             TRUE ~ 0),
         Alerta2 = case_when((MP10_Obs > 180 & MP10_Mod > 180) | (MP10_Obs < 180 & MP10_Mod < 180) ~ 1,
                             TRUE ~ 0))

pct_bueno = sum(ExactitudMP10$Bueno, na.rm = TRUE) / nrow(ExactitudMP10)
pct_alerta1 = sum(ExactitudMP10$Alerta1, na.rm = TRUE) / nrow(ExactitudMP10)
pct_alerta2 = sum(ExactitudMP10$Alerta2, na.rm = TRUE) / nrow(ExactitudMP10)
promedio = mean(c(pct_bueno, pct_alerta1, pct_alerta2))






























