library("dplyr")                                           
library("readr")
library("readxl")
library("xlsx")

### DATAFRAME MODELADOS

Modelados <- list.files(path = "Modelados/pronostico-alerta/sg/2023/6.Junio/Meteorologia",    
                        pattern = "*.xlsx",
                        full.names = TRUE) %>% 
  lapply(read_excel) %>% 
  bind_rows


View(Modelados)

####################################################################################################################################

NumberOfDaysInMonth = 30

MyLogicVector <- rep(c(T,F,F), NumberOfDaysInMonth) # T,F,F    T,F,F    T,F,F   ... 31 times (93 items)

MyLogicVector_2 <- rep(MyLogicVector, each = 24)  # T,T,T... 24 times, F,F,F,... 24 times, F,F,F,... 24 times, ...(93*24) times = 2232 times

Mysequence <- seq(1:(NumberOfDaysInMonth*24*3))  # 1,2,3,4...2232 (744 * 3) = 744 hours in a period of 31 days and 3 because each excel file has 3 days.

MyNewIndex <- Mysequence[MyLogicVector_2]  # 1,2,3,...24, 73,74,75,...96, 145,146,147,...168, 217,218,219,... ... 2182,2183,2184 (only correct indeces)

Modelados <- Modelados[MyNewIndex,]  # correct dataframe.


View(Modelados)

####################################################################################################################################


write.xlsx(Modelados, file = "Modelados/pronostico-alerta/sg/2023/6.Junio/Meteorologia/Combinados/Junio2023.xlsx")


write_excel_csv(Modelados, file = "Modelados/pronostico-alerta/sg/2023/6.Junio/MP10/Combinados/Mayo2023_completo.csv")

















































































