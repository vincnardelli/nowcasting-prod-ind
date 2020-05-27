library(purrr)
library(tibble)
library(tidyr)
library(dplyr)
library(lubridate)
library(ggplot2)
library(stringr)
library(readxl)
library(gganimate)

# LOAD DATA
setwd(dir = "nowcasting-prod-ind/mise/data")
paths <- list.files()
data <- map(paths, read_excel, skip = 3)
data <- set_names(data, paths)

# EXCTRACT INDEX OF "TOTALE CONSUMI"
indexes_cons <- map_int(data, ~ which(.x[[1]] == "TOTALE" | .x[[1]] == "TOTALE  CONSUMI"))

# SUBSET DATA
data <- map2(data, indexes_cons, ~ .x[.y, ])

####################
#### DEFINITIVI ####
####################
last_def <- which(map_lgl(paths, ~ str_detect(.x, "Def"))) %>% max()
definitivi <- data[1:last_def]

columns_def <- map(data, ~ which(str_detect(colnames(.x), "^\\D{4}$")))
data_def <- map2(data, columns_def, ~ .x[.y])

# SET ELEMENTS NAMES 
dates_def <- map(paths[1:last_def], ~ str_extract(.x, "\\d{4}")) %>% 
  map(rep, 12) %>% 
  as_vector() %>% 
  str_c(str_pad(1:12, pad = 0, width = 2), sep = "-")

years <- map(paths, ~ str_extract(.x, "\\d{4}")) %>% map(rep, 12)
colnames_def <- map2(data_def, years, ~ str_c(names(.x), .y))
colnames_def <- colnames_def[1:last_def]

data_def <- map2(data_def[1:last_def], colnames_def, ~ set_names(.x, nm = .y)) %>% 
  flatten() %>% 
  map(as.numeric) %>% 
  enframe() %>% 
  unnest(cols = c("name", "value")) %>% 
  mutate(name = dates_def) %>% 
  mutate(name = str_c(name, "01", "-")) %>% 
  mutate(name = ymd(name)) %>% 
  print(n = Inf)

########################
#### NON DEFINITIVI ####
########################
data_nondef <- data[last_def + 1: length(data)] %>% compact()
data_nondef <- map(data_nondef, ~ .x[3])

# SET ELEMENTS NAMES 
names_nondef <- map(paths, ~ str_extract(.x, pattern = "\\d{4}[_]\\d{2}")) %>% discard(is.na)
data_nondef <- map2(data_nondef, names_nondef, ~ set_names(.x, nm = .y)) %>% 
  flatten() %>% 
  map(as.numeric) %>% 
  enframe() %>% 
  unnest(cols = c("name", "value")) %>% 
  mutate(name = str_replace(name, "_", "-")) %>% 
  mutate(name = str_c(name, "01", "-")) %>% 
  mutate(name = ymd(name)) %>% 
  print(n = Inf)

# COMBINE DEFINITIVI AND NON DEFINITIVI
line <- rbind(data_def ,data_nondef) %>% 
  ggplot(aes(x = name, y = value, group = 1)) + 
  geom_line() +
  theme_light() +
  xlab("Data") +
  ylab("(,000 Tonnellate)") +
  theme(plot.title = element_text(hjust = 0.5),
        plot.caption = element_text(hjust = 0)) +
  labs(title = "Consumi petroliferi", 
       caption = "Source: https://dgsaie.mise.gov.it/consumi_petroliferi.php") +
  transition_reveal(along = name)

# anim_save("fuel_mise_anim.gif", path = "/Users/Andrea/nowcasting-prod-ind/mise/images")
  

rbind(data_def ,data_nondef) %>% 
  count(cut_width(value, 140))
  
hist <- rbind(data_def ,data_nondef) %>% 
  ggplot(aes(value)) +
  geom_histogram(binwidth = 140) +
  theme_light() +
  xlab("(,000 Tonnellate)") +
  theme(plot.title = element_text(hjust = 0.5),
        plot.caption = element_text(hjust = 0)) +
  labs(title = "Consumi petroliferi", 
       caption = "Source: https://dgsaie.mise.gov.it/consumi_petroliferi.php") +
  transition_reveal(along = name)

anim_save("fuel_mise_anim_hist.gif", path = "/Users/Andrea/nowcasting-prod-ind/mise/images")



