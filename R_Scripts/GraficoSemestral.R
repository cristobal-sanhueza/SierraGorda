library("plyr")  
library("dplyr")                                                
library("readr")  
library("readxl")
library("reshape2")
library("ggplot2")
library("tidyverse")
library("M3")
library("lubridate")
library("ggpubr")
library("grid")


## import

SeptDic2021_Datos <- read_excel("SIERRA_GORDA/Bondad_De_Ajuste/Semestral/SeptDic2021_Datos.xlsx",
                                col_types = c("date", "numeric", "numeric", "numeric"))


MisDatos <- SeptDic2021_Datos %>% 
  mutate(agregado = case_when(hora < 10 ~ "0",
                          hora >= 10 ~ ""),
         hora = paste(agregado, hora, sep = ""),
         hora = paste(hora, ":00:00", sep = ""),
         MyDate = combine.date.and.time(date = MisDatos$fecha, time = MisDatos$hora))


MisDatos <- MisDatos[-c(1,2,5)]

MisDatos$MyDate <- as.POSIXct(MisDatos$MyDate)

names(MisDatos) <- c("Observado", "Modelado", "MyDate")

MyNewDF <- melt(MisDatos, id = "MyDate")

MyNewDF <- MyNewDF %>% 
  mutate(Month = month(MyDate))


SeptPlot <- ggplot(MyNewDF[MyNewDF$Month == 9,], aes(x = MyDate,y = value, colour = variable, group = variable)) +
  geom_line() +
  theme(axis.title.x = element_blank()) +
  theme_bw()
  

OctPlot <- ggplot(MyNewDF[MyNewDF$Month == 10,], aes(x = MyDate,y = value, colour = variable, group = variable)) +
  geom_line() +
  theme(axis.title.x = element_blank()) +
  theme_bw()

NovPlot <- ggplot(MyNewDF[MyNewDF$Month == 11,], aes(x = MyDate,y = value, colour = variable, group = variable)) +
  geom_line() +
  theme(axis.title.x = element_blank()) +
  theme_bw()

DicPlot <- ggplot(MyNewDF[MyNewDF$Month == 12,], aes(x = MyDate,y = value, colour = variable, group = variable)) +
  geom_line() +
  theme(axis.title.x = element_blank()) +
  theme_bw()

#ggarrange(SeptPlot, OctPlot, NovPlot, DicPlot, ncol = 1, nrow = 4)






figure <- ggarrange(SeptPlot + rremove("ylab") + rremove("xlab"),
                    OctPlot + rremove("ylab") + rremove("xlab"),
                    NovPlot + rremove("ylab") + rremove("xlab"),
                    DicPlot+ rremove("ylab") + rremove("xlab"),
                    labels = NULL,
                    ncol = 1, nrow = 4,
                    common.legend = TRUE, legend = "bottom")



annotate_figure(figure, left = textGrob("MP10 μg/m³", rot = 90, vjust = 1, gp = gpar(cex = 1.3)))
















































































































































































































































