library(dplyr)
library(tidyr)
library(ggplot2)

campus <- read.csv("Placement_Data_Full_Class.csv", header = T, stringsAsFactors = T, na.strings = "")
head(campus)
str(campus)

campus_dp <- as_tibble(campus)
campus_dp$salary <- as.double(campus_dp$salary)

#Описательные статистики для ssc_p, hsc_p, degree_p, etest_p, mba_p. salary отдельно