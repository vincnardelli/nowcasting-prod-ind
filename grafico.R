data <- readxl::read_excel("modello_economico.xlsx")


library(ggplot2)
library(lubridate)


data$Date <- as.Date(as.yearmon(data$Date))


ggplot() + 
  geom_col(data = data, aes(Date, Indicatore), fill="#c7c7c7") + 
  geom_line(data = data, aes(Date, consumi_deseas/255)) +
  coord_cartesian(ylim = c(90, 110)) +
  theme_minimal()


