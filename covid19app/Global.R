library(tidyverse)
library(ggplot2)
library(RColorBrewer)
library(microbenchmark)
library(DT)
library(readxl)
library(dplyr)
library(httr)
library(forecast) 
library(reshape2)
library(cowplot)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(ggrepel)
library(ggspatial)
library(corrplot)
library(tidyr)
library(tidyverse)
library(broom)
library(caTools)



#read the Dataset sheet into “R”

data <- read.csv("https://opendata.ecdc.europa.eu/covid19/casedistribution/csv", na.strings = "", fileEncoding = "UTF-8-BOM")

# Changing colname

names(data)[7] <- "Country"



# Unique countries ####
countries <- data %>% dplyr::group_by(Country) %>% 
    summarize(cases = sum(cases), deaths = sum(deaths))


total_cases <- data %>%  dplyr::group_by(Country) %>% 
    summarize(cases = sum(cases), deaths = sum(deaths))


#  Cases by country compared ####
US_c <- data %>%  dplyr::filter( geoId == "US") %>% 
    transmute(US_cases = cases, dateRep)
IT_c <- data %>%  dplyr::filter(geoId == "IT") %>% 
    transmute(IT_cases = cases)
CN_c <- data %>%  dplyr::filter(geoId == "CN") %>% 
    transmute(CN_cases = cases)
ES_c <- data %>%  dplyr::filter(geoId == "ES") %>% 
    transmute(ES_cases = cases)

cases <- bind_cols(US_c, IT_c, CN_c)

melted_c <- melt(cases, id.vars = "dateRep", variable.name = "series_c", value.name = "value_c")

# Deaths by country and comparision ####
US <- data %>%  filter( geoId == "US") %>% 
    transmute(US_Deaths = deaths, dateRep)
IT <- data %>%  filter(geoId == "IT") %>% 
    transmute(IT_Deaths = deaths)
CN <- data %>%  filter(geoId == "CN") %>% 
    transmute(CN_Deaths = deaths)
ES <- data %>%  filter(geoId == "ES") %>% 
    transmute(ES_Deaths = deaths)

deaths <- bind_cols(US, IT, CN)

melted <- melt(deaths, id.vars = "dateRep", variable.name = "series")



# calculating the rate of cases and cases per million on inhabitants ####
pop <- data %>% distinct(Country, popData2019)


cas_dea <- data %>% group_by(Country) %>% 
    summarize(cases = sum(cases), deaths = sum(deaths))

dat <- left_join(cas_dea, pop)

dat <- dat %>% mutate(rate_cases_per_100k_people= (dat$cases/ dat$popData2019)*100000)

#dounding rate_cases
dat["rate_cases_per_100k_people"] <- round(dat["rate_cases_per_100k_people"], digits = 4)

# calculating the rate of deaths and cases per million on inhabitants
dat <- dat %>% mutate(rate_deaths_per_100k_people = (dat$deaths/dat$popData2019)*100000)

# Rounding rate_deaths
dat["rate_deaths_per_100k_people"] <- round(dat["rate_deaths_per_100k_people"], digits = 4)


# loading animated gif
gif <- read.gif("covid19chart.gif", frame = 1)

