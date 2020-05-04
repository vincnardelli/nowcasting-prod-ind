library(lubridate)
library(dplyr)
library(ggplot2)
library(pracma)
library(zoo)
data <- read.csv("export.csv")
data <- rbind(data, read.csv("export_extra.csv"))

library(readxl)
istat <- read_excel("input.xlsx")
istat$Q <- as.numeric(paste0(istat$year, ".", istat$quarter))
istat$data <- as_date(istat$data)


library(readxl)
istat_pi <- read_excel("istat/istat_indice_prod_indus.xlsx")
istat_pi$month <- paste0(istat_pi$mese, "-", istat_pi$anno)

daily <- data %>% 
  filter(ORA == "Totale:") %>% 
  mutate(CONSUNTIVO = as.numeric(gsub(",", "", CONSUNTIVO))/1000, 
         PREVISIONE = as.numeric(gsub(",", "", PREVISIONE))/1000, 
         DATA = dmy(DATA), 
         Q = quarter(DATA, with_year = TRUE, fiscal_start = 1), 
         month = paste0(month(DATA), "-", year(DATA))) %>% 
        arrange(DATA) %>% 
  filter(DATA > dmy("31/12/2014"), DATA < dmy("01/04/2020"))

ggplot() + 
  geom_line(data=daily, aes(DATA, CONSUNTIVO))+ 
  theme_minimal()


daily %>% 
  mutate(ANNO=as.factor(year(DATA)), 
         ma3=movavg(CONSUNTIVO, 3),
         ma7=movavg(CONSUNTIVO, 90)) %>% 
  ggplot()+
  geom_line(aes(DATA, CONSUNTIVO), color="#8a8a8a")+
  geom_line(aes(DATA, ma7), color="#000000")+ 
  theme_minimal() + 
  ggtitle("Consumi elettrici nazionali", "MA7")




monthly <- daily %>% 
  group_by(month) %>% 
  summarise(consumi = sum(CONSUNTIVO, na.rm = TRUE)) 

monthly %>% 
  ggplot()+
  geom_line(aes(month, consumi), color="#8a8a8a") +
  theme_minimal() + 
  ggtitle("Consumi elettrici nazionali", "MA7")

 
  left_join(istat) %>% 
  select(Q, consumi, pil, data) %>% 
  mutate(consumi_w =rollapply(consumi, 5, function(x) weighted.mean(x, c(0.125, 0.25, 0.25, 0.25, 0.125)), fill = NA), 
         pil_w = rollapply(pil, 5, function(x) weighted.mean(x, c(0.125, 0.25, 0.25, 0.25, 0.125)), fill = NA))

quarter$data[13] <- as_date("2020-03-01")

ggplot(quarter) + 
  geom_line(aes(data, consumi_w, color="consumi")) + 
  geom_line(aes(data, pil_w, color="pil/6")) +
  theme_minimal() + 
  ylim(c(0, 90*10^3))


summary(lm(pil~consumi, data=quarter))






quarter <- daily %>% 
  group_by(Q) %>% 
  summarise(consumi = sum(CONSUNTIVO)) %>% 
  left_join(istat) %>% 
  select(Q, consumi, pil, data) %>% 
  mutate(consumi_w =rollapply(consumi, 5, function(x) weighted.mean(x, c(0.125, 0.25, 0.25, 0.25, 0.125)), fill = NA), 
         pil_w = rollapply(pil, 5, function(x) weighted.mean(x, c(0.125, 0.25, 0.25, 0.25, 0.125)), fill = NA))

quarter$data[13] <- as_date("2020-03-01")

ggplot(quarter) + 
  geom_line(aes(data, consumi_w, color="consumi")) + 
  geom_line(aes(data, pil_w, color="pil/6")) +
  theme_minimal() + 
  ylim(c(0, 90*10^3))


summary(lm(pil~consumi, data=quarter))




