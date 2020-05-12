rm(list=ls())
library(readxl)
library(tidyverse)
library(lubridate)
library(zoo)
setwd("./data")
getwd()
files <- dir()
data <- vector(mode = "list", length = length(files))

for (i in seq_along(files)){
  data[[i]] <- read_excel(files[i], skip = 3, col_names = TRUE)
}

# Extract indexes of the rows containig "TOTALE"
# for each tibble in order to reach "CONSUMI TOTALI"
# as final objective.
# .[[1]] first column
indexes_tot <- data %>% 
  map(~which(str_detect(.[[1]], regex("^TOT"))))
indexes_tot

# indexes_consumi <- data %>% 
#   map(~which(str_detect(.[[1]], regex("^CONS"))))

# Iterate over the elements in the list data
# to reduce each tibble to have the rows with TOTALE
for (i in seq_along(data)){
  # Append names to each list
  names(data)[i] <- files[i]
  # Extract the rows which have "TOTALE"
  data[[i]] <- data[[i]][indexes_tot[[i]],]
}
data

cond_def <- vector(mode = "list", length = length(data))
cons_indexes <- vector(mode = "numeric", length = length(data))
years <- vector(mode = "numeric", length = length(data))
cols <- vector(mode = "list", length = length(data))
months_non_def <- vector(mode = "numeric")
non_def_indexes <- vector(mode = "numeric")

for (i in seq_along(data)){
  
  # Extract the years from the files' names
  years[i] <- str_extract(names(data)[i], "\\d{4}")
  
  # FOCUS FIRST ON TIBBLES WITH NAME "Definitivi"
  if (str_detect(names(data)[i], regex("Def")) == TRUE){
    #print(names(data)[i])
    
    # Select rows of with "CONSUMI" of those named "Definitivi" 
    # in the second column
    cond_def[[i]] <- str_detect(as.character(data[[i]][[2]]), "CONS")
    #print(cond_def[[i]])
    cons_indexes[i] <- which(cond_def[[i]])
    #print(cons_indexes[i])
    # Drop the columns with more than 4 characters
    cols[[i]] <- which(sapply(colnames(data[[i]]), str_detect, "^\\D{4}$"))
    cols[[i]] <- names(cols[[i]])
    #print(cols[[i]])
    data[[i]] <- subset(data[[i]], select = cols[[i]])
    #print(data[[i]])
    
    
    # FOCUS ON TIBBLES WHICH DOES NOT HAVE "Definitivi" IN THE NAME
    } else {
     cond_def[[i]] <- str_detect(as.character(data[[i]][[1]]), "CONS")
     #print(cond_def[[i]])
     cons_indexes[i] <- which(cond_def[[i]])
     #print(cons_indexes[i])
     
     cols[[i]] <- which(sapply(colnames(data[[i]]), str_detect, "3$"))
     cols[[i]] <- names(cols[[i]])
     #print(cols[[i]])
     data[[i]] <- subset(data[[i]], select = cols[[i]])
     #print(data[[i]])
     
     months_non_def[i] <- str_extract(names(data)[i], "[[:punct:]][0-9]{2}[[:punct:]]")
     months_non_def[i] <- str_extract(months_non_def[i], "\\d{2}")
     #print(months_non_def[i])
     
   }
  data[[i]] <- data[[i]][cons_indexes[i],]
  #print(data[[i]])
}

#### NON "Definitivi" ####
n <- length(data)
numb_def <- max(which(is.na(months_non_def)))
ym_non_def <- str_c(years, months_non_def, sep = "-")
tot_non_def <- n - numb_def

df_non_def <- matrix(ym_non_def[numb_def+1:n], ncol = 2, nrow = tot_non_def, byrow = FALSE)

for (i in 1:tot_non_def){#15
  df_non_def[i,2] <- data[[numb_def+i]][[1]]
}

#### DEFINITIVI ####
years <- sort(rep(years[1:numb_def], 12))
month <- str_pad(1:12, pad = 0, width = 2)
dates_def <- str_c(years, month, sep = "-")

df_def <- matrix(ncol = 2, nrow = numb_def*12)
df_def[,1] <- dates_def

mat_def <- matrix(ncol = 12, nrow = n-tot_non_def)
for(i in 1:numb_def){
  mat_def[i,] <- as.numeric(unlist(data[[i]]))
}
df_def[,2] <- as.vector(t(mat_def))

#### CONCATENATE  df_non_def df_def ###
df <- rbind(df_def, df_non_def)
colnames(df) <- c("Date", "Cons")
df %>% 
  as_tibble() %>% 
  mutate(Date = as.yearmon(Date, "%Y-%m")) %>%
  mutate(Cons = as.numeric(Cons)) %>% 
  ggplot(aes(x = Date, y = Cons)) + 
  geom_line() +
  theme_light() +
  xlab("Data") +
  ylab("(,000 Tonn.)") +
  theme(plot.title = element_text(hjust = 0.5),
        plot.caption = element_text(hjust = 0)) +
  labs(title = "Consumi petroliferi", 
       caption = "Source: https://dgsaie.mise.gov.it/consumi_petroliferi.php")
  
ggsave(filename = "../cons_petroliferi.png", plot = last_plot(), 
       width = 297, height = 210, units = "mm", device = "png", dpi = 320)
  




