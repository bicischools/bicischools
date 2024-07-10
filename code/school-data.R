library(tidyverse)
library(sf)
library(ggplot2)
library(tmap)
tmap_mode("view")

schools_basic = read_sf("../internal/SCHOOLS_basicsec.gpkg")

View(schools_basic)
table(schools_basic$Nivel, useNA = "ifany")
# Basico Secundario 
# 106         20 
summary(schools_basic$Alunos)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 29.0   151.8   234.0   355.6   476.8  1758.0 

ggplot(schools_basic |> filter(Nivel == "Basico"), aes(Alunos)) + 
  geom_histogram() + 
  ggtitle("Primary school") + 
  xlab("Students")

ggplot(schools_basic |> filter(Nivel == "Secundario"), aes(Alunos)) + 
  geom_histogram(binwidth = 100) +
  ggtitle("Secondary school") + 
  xlab("Students")

tm_shape(schools_basic) + tm_dots("Nivel", size = "Alunos")
