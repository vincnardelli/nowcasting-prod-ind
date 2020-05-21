library(rvest)
library(stringr)
library(svMisc)
library(plyr)
library(dplyr)
library(here)
#-------------
here()
# Parse data
airbnb = url("http://insideairbnb.com/get-the-data.html")%>%read_html

# Function 1. 
get_city_in_a_country = function(country){
  
  # ''' Find the cities in a country.
  #     Input is a lower-case string ''' 
  
  # Takecities 
  cities = str_split(str_subset(tolower(airbnb %>% 
                                          html_nodes("h2") %>%
                                            html_text()),pattern = country),pattern=",", simplify = T)[,1]
  return(cities)
}

get_city_in_a_country('italy')

# Function 2. 
get_data_city = function(city){
  
  # ''' Function that returns 3 dataframes (listings, reviews, calendar) for the city selected.
  #     Input is a lower-case string '''
  
  # Pick current time 
  start_time <- Sys.time()
  
  # Take all the files 
  links = airbnb %>% 
            html_nodes(sprintf('table.table-hover.table-striped.%s > tbody > tr > td > a',city))  %>%
              html_attr("href")
  
  cat("-----------------------------\n City processed:",city,"\n----------------------------- \n")
  
  # Extract detailed listings data 
  links = str_subset(links, pattern="/data/")
  
  # Create empty dataframe 
  listings_df = NULL
  calendar_df = NULL
  reviews_df = NULL
  
  # Progress bar 
  pb = txtProgressBar(min = 0, max = length(links), initial = 0) 
  
  # Create a new folder named data
  data_folder = here::here("airbnb","data")
  dir.create(data_folder)

  # Create a sub-folder of data with the city selected
  city_folder = here::here("airbnb","data",city)
  dir.create(city_folder)
  
  # For loops that go through each links and download the content in combined dataframes
  for (i in 1:length(links)){
    
    if(str_detect(links[i], pattern="listings")){
      
      connection = gzcon(url(links[i]))
      txt = textConnection(readLines(connection))
      close(connection)
      dat <- read.csv(txt)
      close(txt)
      listings_df = rbind.fill(listings_df, dat)  
      
    } else if (str_detect(links[i], pattern="calendar")) {
      
      connection = gzcon(url(links[i]))
      txt = textConnection(readLines(connection))
      close(connection)
      dat <- read.csv(txt)
      close(txt)
      calendar_df = rbind.fill(calendar_df, dat)  
      
    } else if (str_detect(links[i], pattern="reviews")){
      
      connection = gzcon(url(links[i]))
      txt = textConnection(readLines(connection))
      close(connection)
      dat <- read.csv(txt)
      close(txt)
      reviews_df = rbind.fill(reviews_df, dat)  
      
    }
    
    setTxtProgressBar(pb,i)
    
  }
  
  # Write files 
  write.csv(listings_df,here::here("airbnb","data",city,sprintf("listings-%s.csv", city)))
  write.csv(calendar_df,here::here("airbnb","data",city, sprintf("calendar-%s.csv", city)))
  write.csv(reviews_df,here::here("airbnb","data",city, sprintf("reviews-%s.csv", city)))
  
  # Time 
  end_time <- Sys.time()
  diff = end_time - start_time
  print(diff)
  
}

# Example with trentino (fast download -> airbnb can also groups cities into regions if there are not many listings)
get_data_city("trentino")

