rm(list=ls())
library(lubridate)
library(dplyr)
library(ggplot2)
library(pracma)
library(zoo)
library(readxl)

# Consumo elettrico oriario giornaliero
data <- read.csv("export.csv")

# Correzione dati
data[data$DATA == "11/04/2020" & data$ORA == "Totale:", ][2, ]$CONSUNTIVO <- 0


# Extra contiene dati consumo elettrico del 2015
data <- rbind(data, read.csv("export_extra.csv"))
# Extra contiene alcuni giorni di Aprile 2020
data <- rbind(data, read.csv("export_aprile.csv"))

# Produzione industriale
istat_pi <- read_excel("istat/istat_indice_prod_indus.xlsx")
istat_pi$month <- paste0(istat_pi$anno, "-", istat_pi$mese)

# Energia elettrica dal 2016 ad Aprile 2020
daily <- data %>% 
  filter(ORA == "Totale:") %>% 
  mutate(CONSUNTIVO = as.numeric(gsub(",", "", CONSUNTIVO))/1000, 
         PREVISIONE = as.numeric(gsub(",", "", PREVISIONE))/1000, 
         DATA = dmy(DATA), 
         Q = quarter(DATA, with_year = TRUE, fiscal_start = 1), 
         month = paste0(year(DATA), "-", month(DATA))) %>% 
  arrange(DATA) %>% 
  filter(DATA > dmy("31/12/2015"), DATA < dmy("30/04/2020"))

# Energia 2019 ad Aprile 2020
daily_export <- data %>% 
  filter(ORA == "Totale:") %>% 
  mutate(consumi = as.numeric(gsub(",", "", CONSUNTIVO))/1000, 
         DATA = dmy(DATA), 
         Q = quarter(DATA, with_year = TRUE, fiscal_start = 1), 
         month = paste0(year(DATA), "-", month(DATA))) %>% 
  arrange(DATA) %>% 
  filter(DATA > dmy("31/12/2018"), DATA < dmy("30/04/2020"))

# 2019 e 2020
# Consumi prelockdown: Lun. 9 Marzo
# Forse correzione sulla data? 9 invece che 10
daily_export$prelockdown <- NA
daily_export[daily_export$DATA < dmy("10/03/2020"),]$prelockdown <- daily_export[daily_export$DATA < dmy("10/03/2020"),]$consumi

# Consumi postlockdown
daily_export$postlockdown <- NA
daily_export[daily_export$DATA >= dmy("9/03/2020"),]$postlockdown <- daily_export[daily_export$DATA >= dmy("9/03/2020"),]$consumi

#writexl::write_xlsx(daily_export, "daily.xlsx")

# Mercoledì di marzo (??)
hourly_export <- data %>% 
  filter(ORA != "Totale:") %>% 
  mutate(consumi = as.numeric(gsub(",", "", CONSUNTIVO))/1000, 
         DATA = dmy(DATA), 
         Q = quarter(DATA, with_year = TRUE, fiscal_start = 1), 
         month = paste0(year(DATA), "-", month(DATA))) %>% 
  arrange(DATA) %>% 
  filter(DATA %in% c(dmy("04/03/2020"), 
                     dmy("11/03/2020"), 
                     dmy("18/03/2020"), 
                     dmy("25/03/2020"),
                     dmy("01/04/2020")))

writexl::write_xlsx(hourly_export, "hourly.xlsx")

# Dati delle domeniche di marzo e aprile
hourly_export <- data %>% 
  filter(ORA != "Totale:") %>% 
  mutate(consumi = as.numeric(gsub(",", "", CONSUNTIVO))/1000, 
         DATA = dmy(DATA), 
         Q = quarter(DATA, with_year = TRUE, fiscal_start = 1), 
         month = paste0(year(DATA), "-", month(DATA))) %>% 
  arrange(DATA) %>% 
  filter(DATA %in% c(dmy("08/03/2020"), 
                     dmy("15/03/2020"), 
                     dmy("22/03/2020"), 
                     dmy("29/03/2020"),
                     dmy("05/04/2020")))


writexl::write_xlsx(hourly_export, "hourlywe.xlsx")


# Grafico consumi da 2016 a 2020
ggplot() + 
  geom_line(data=daily, aes(DATA, CONSUNTIVO))+ 
  theme_minimal()

# Aprile 2020
aprile <- daily %>% 
  filter(DATA > dmy("31/03/2020"), DATA < dmy("30/04/2020")) 

writexl::write_xlsx(aprile, "aprile.xlsx")

# Giorni lavorativi Aprile 2020
aprile$weekdays <- weekdays(aprile$DATA)
aprile$wd <- 0

# Fine settimana Aprile 2020
aprile$wd[aprile$weekdays %in% c("Sabato", "Domenica")] <- 1
# Lunedì dell'Angelo
aprile$wd[aprile$DATA == "2020-04-13"] <- 1

# Media consumo giorni Festivi/non-Festivi Aprile 2020
mean_df <- aprile %>% group_by(wd) %>% summarise(m=mean(CONSUNTIVO, na.rm=TRUE))

mean_we <- mean_df$m[mean_df$wd == 1]
mean_wd <- mean_df$m[mean_df$wd == 0]

weekdays <- seq(max(aprile$DATA),ymd('2020-04-30'), by="days")
we <- sum(weekdays %in% c("Saturday", "Sunday"))
wd <- sum(!(weekdays %in% c("Saturday", "Sunday")))

# ???
aprile_total <- we*mean_we + wd*mean_wd + sum(aprile$CONSUNTIVO, na.rm = T)
aprile_total_deseas <- aprile_total - x.decomp$seasonal[4]

#monthly$consumi[monthly$month == "2020-4"] <- 19721.24739
#monthly$consumi_deseas[monthly$month == "2020-4"] <- 22453.27739


# Join tra produzione industriale e consumo energia elettrica
monthly <- daily %>% 
  group_by(month) %>% 
  summarise(consumi = sum(CONSUNTIVO, na.rm=TRUE)) %>% 
  left_join(istat_pi) %>% 
  arrange(anno, mese)

# Obbiettivo: predizione ogni mese della produzione industriale a un mese
# Lead time: 1 mese 
# Forecast interval: 1 mese
x <- ts(monthly$consumi, frequency=12, start=c(2015,1))
x.decomp <- decompose(x)
monthly$consumi_deseas <- x.decomp$x - x.decomp$seasonal

#sovrascrivi aprile

monthly$consumi[monthly$month == "2020-4"] <- aprile_total
monthly$consumi_deseas[monthly$month == "2020-4"] <- aprile_total_deseas


monthly$indice <- monthly$`0020: TOTALE INDUSTRIA ESCLUSE COSTRUZIONI (b-e)`
monthly$consumi_deseas2 <- monthly$consumi_deseas^2
model <- lm(indice~consumi_deseas, data=monthly)
monthly[(nrow(monthly)-1):nrow(monthly),]$indice <- predict(model, newdata = monthly[(nrow(monthly)-1):nrow(monthly),])   # Save the predicted values


predict(model, newdata = monthly[(nrow(monthly)-1):nrow(monthly),],  interval="predict") 

summary(model)

monthly$predicted <- predict(model, newdata = monthly)

monthly$month <- as.Date(as.yearmon(monthly$month))


monthly %>% 
  filter(month > dmy("31/12/2017"), month < dmy("30/04/2020")) %>% 
ggplot() + 
  geom_col(aes(month, consumi_deseas/250), fill="#c7c7c7") + 
  geom_line(aes(month, indice, color="real")) +
  geom_line(aes(month, predicted, color="predicted")) +
  coord_cartesian(ylim = c(90, 110)) +
  theme_minimal()

monthly %>% 
  filter(month > dmy("31/12/2017"), month < dmy("30/04/2020")) %>% 
  ggplot() + 
  geom_col(aes(month, consumi_deseas/250), fill="#c7c7c7") + 
  geom_line(aes(month, indice, color="real")) +
  coord_cartesian(ylim = c(90, 110)) +
  theme_minimal()


ggplot(monthly) + 
  geom_col(aes(month, consumi_deseas/250), fill="#c7c7c7") + 
  geom_line(aes(month, indice)) +
  geom_line(aes(month, predicted, color="predicted")) +
  coord_cartesian(ylim = c(90, 110)) +
  theme_minimal()


monthly$consumi_deseas_scalati <- monthly$consumi_deseas/250

monthly %>% 
  #filter(month > dmy("31/12/2017")) %>% 
write.csv("monthly.csv")

