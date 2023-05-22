library("dplyr")                                                
library("readr")  
library("readxl")
library("xlsx")

setwd("/Users/cristobal512/Desktop/geoaire/GeoaireProject/GeoAire")


## PARA LOS KPI

### DATAFRAME MODELADOS

Modelados <- list.files(path = "SIERRA_GORDA/Modelados/pronostico-alerta/sg/2023/4.Abril/MP10",    
                        pattern = ".xlsx",
                        full.names = TRUE) %>% 
  lapply(read_excel) %>%                                           
  bind_rows()                                               

View(Modelados)

NumberOfDaysInMonth = 21

MyLogicVector <- rep(c(T,F,F), NumberOfDaysInMonth) # T,F,F    T,F,F    T,F,F   ... 32 times (96 items)

MyLogicVector_2 <- rep(MyLogicVector, each = 24)  # T,T,T... 24 times, F,F,F,... 24 times, F,F,F,... 24 times, ...(96*24) times = 2304 times

Mysequence <- seq(1:(NumberOfDaysInMonth*24*3))  # 1,2,3,4...2304 (768 * 3) = 768 hours in a period of 32 days and 3 because each excel file has 3 days.

MyNewIndex <- Mysequence[MyLogicVector_2]  # 1,2,3,...24, 73,74,75,...96, 145,146,147,...168, 217,218,219,... ... 2182,2183,2184 (only correct indeces)

Modelados <- Modelados[MyNewIndex,]  # correct dataframe.

View(Modelados)

### -------------------------------------------------------------------------------------------------------------


write.xlsx(Modelados, file = "SIERRA_GORDA/Modelados/pronostico-alerta/sg/2023/4.Abril/MP10/Combinados/Abril_1al21_2023.xlsx")


write_excel_csv(Modelados, file = "SIERRA_GORDA/Modelados/pronostico-alerta/sg/2022/11.Noviembre/Mp10/Combinados/Nov2022.csv")






















































































