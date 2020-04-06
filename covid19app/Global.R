library(tidyverse)
library(shiny)
library(ggplot2)
library(RColorBrewer)
library(microbenchmark)
library(DT)
library(googleVis)
library(readxl)
library(dplyr)
library(httr)
library(car) 
library(forecast) 
library(reshape2)
library(cowplot)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(maps)
library(googleway)
library(ggrepel)
library(ggspatial)
library(corrplot)
library(tidyr)
library(tidyverse)
library(broom)
require(mosaic)


#create the URL where the dataset is stored with automatic updates every day

url <- paste("https://www.ecdc.europa.eu/sites/default/files/documents/COVID-19-geographic-disbtribution-worldwide-",format(Sys.time(), "%Y-%m-%d"), ".xlsx", sep = "")

#download the dataset from the website to a local temporary file

GET(url, authenticate(":", ":", type="ntlm"), write_disk(tf <- tempfile(fileext = ".xlsx")))

#read the Dataset sheet into “R”

data <- read_excel(tf)

# Changing colname

names(data)[7] <- "Country"



# Unique countries
countries <- data %>% group_by(Country) %>% 
    summarize(cases = sum(cases), deaths = sum(deaths))


total_cases <- data %>%  dplyr::group_by(Country) %>% 
    summarize(cases = sum(cases), deaths = sum(deaths))


#  Casesby country and comparision
US_c <- data %>%  filter( geoId == "US") %>% 
    transmute(US_cases = cases, dateRep)
IT_c <- data %>%  filter(geoId == "IT") %>% 
    transmute(IT_cases = cases)
CN_c <- data %>%  filter(geoId == "CN") %>% 
    transmute(CN_cases = cases)
ES_c <- data %>%  filter(geoId == "ES") %>% 
    transmute(ES_cases = cases)

cases <- bind_cols(US_c, IT_c, CN_c, ES_c)

melted_c <- melt(cases, id.vars = "dateRep", variable.name = "series_c", value.name = "value_c")
