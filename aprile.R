library(lubridate)
library(dplyr)
library(ggplot2)
library(pracma)
library(zoo)
library(readxl)

data <- read.csv("export_aprile.csv")
data <- rbind(data, read.csv("export_extra.csv"))

istat_pi <- read_excel("istat/istat_indice_prod_indus.xlsx")
istat_pi$month <- paste0(istat_pi$anno, "-", istat_pi$mese)

daily <- data %>% 
  filter(ORA == "Totale:") %>% 
  mutate(CONSUNTIVO = as.numeric(gsub(",", "", CONSUNTIVO))/1000, 
         PREVISIONE = as.numeric(gsub(",", "", PREVISIONE))/1000, 
         DATA = dmy(DATA), 
         Q = quarter(DATA, with_year = TRUE, fiscal_start = 1), 
         month = paste0(year(DATA), "-", month(DATA))) %>% 
  arrange(DATA) %>% 
  filter(DATA > dmy("31/03/2020"), DATA < dmy("1/05/2020"))


ggplot() + 
  geom_line(data=daily, aes(DATA, CONSUNTIVO))+ 
  theme_minimal()

monthly <- daily %>% 
  group_by(month) %>% 
  summarise(consumi = sum(CONSUNTIVO, na.rm=TRUE)) %>% 
  left_join(istat_pi) %>% 
  arrange(anno, mese)
