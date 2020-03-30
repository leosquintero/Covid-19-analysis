library(readxl)
library(dplyr)
library(httr)
library (ggplot2)
library(car) 
library(forecast) 
library(reshape2)
library(cowplot)
theme_set(theme_bw())
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(maps)
library(googleway)
library(ggrepel)
library(ggspatial)
library()

#create the URL where the dataset is stored with automatic updates every day

url <- paste("https://www.ecdc.europa.eu/sites/default/files/documents/COVID-19-geographic-disbtribution-worldwide-",format(Sys.time(), "%Y-%m-%d"), ".xlsx", sep = "")

#download the dataset from the website to a local temporary file

GET(url, authenticate(":", ":", type="ntlm"), write_disk(tf <- tempfile(fileext = ".xlsx")))

#read the Dataset sheet into “R”

data <- read_excel(tf)

# Changing colname

names(data)[7] <- "Country"


# EDA ####

head(data)

# taking a look at the dimentions
dim(data)

# taking a look at the structure
str(data)

# taking a look at the summary
summary(data)

colnames(data)

# Counting missing data
sum(is.na(data))

# Cases vs Deaths worldwide
data %>% summarize(deaths = sum(deaths), Cases = sum(cases))

# Exploratory Data Visualization ####

# cases worldwide
data %>%  group_by(Country, dateRep) %>% 
    summarize(deaths = sum(deaths), cases = sum(cases)) %>% 
    ggplot(aes(dateRep, cases, colour = deaths))+
    geom_jitter() +
    geom_smooth(method = "lm")+
    labs(title = "Cases by Date Worldwide",
                       subtitle = "2020", x = "Month")


# Deaths worldwide
ggplot(data, aes(x = dateRep, y = deaths)) +
    geom_jitter(stat = "identity", fill = "blue") +
    labs(title = "Deaths by Date Worldwide",
         subtitle = "2020", x = "Month")

# Cases worldwide geom_line
data %>% filter(deaths > 3) %>% 
    group_by(dateRep) %>% 
    summarize(cases  = sum(cases)) %>% 
    ggplot(aes(dateRep, cases))+
    geom_line()+
    labs(title = "Cases by Date Worldwide with geom_line",
         subtitle = "2020", x = "Month")


# Visualizing deaths by country > 50
data %>%
    filter(deaths > 50) %>%
    mutate(Country = reorder(Country, cases)) %>%
    ggplot(aes(Country, deaths)) +
    geom_col() +
    coord_flip() +
    labs(title = "Deaths by country > 50",
         subtitle = "2020", x = "Country")
                  
# Visualizing cases by country > 50
data %>%
    filter(cases > 1000) %>%
    mutate(Country = reorder(Country, cases)) %>%
    ggplot(aes(Country, cases)) +
    geom_col() +
    xlab(NULL) +
    coord_flip()+
    labs(title = "Ceses by country > 1000",
         subtitle = "2020", x = "Country")

# Visualizing cases by date
ggplot(data, aes(x = dateRep, y = cases)) +
    geom_col()+
    labs(title = "Cases by Date",
         subtitle = "2020", x = "Country")



#Cases by country ####

# cases in the united states
 
data %>% filter(geoId == "US") %>% 
ggplot(aes(dateRep, cases, colour = deaths)) +
    geom_point() +
    geom_smooth()+
    labs(title = "Cases by date in the US", 
         subtitle = "2020")

# Cases in Italy
data %>% filter(geoId == "IT") %>% 
    ggplot(aes(dateRep, cases, colour = deaths)) +
    geom_point() +
    geom_smooth()+
    labs(title = "Cases by date in Italy", 
         subtitle = "2020")

# Cases in China
data %>% filter(geoId == "CN") %>% 
    ggplot(aes(dateRep, cases, colour = deaths)) +
    geom_point() +
    geom_smooth() +
    labs(title = "Cases by date in China", 
         subtitle = "2020")

# Cases in Spain
data %>% filter(geoId == "ES") %>% 
    ggplot(aes(dateRep, cases, colour = deaths)) +
    geom_point() +
    geom_smooth()+
    labs(title = "Cases by date in Spain", 
         subtitle = "2020")

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

ggplot(melted_c, aes(dateRep, value_c, colour = series_c))+
    geom_point()+
    geom_smooth(se = F)+
    geom_rug()+
    labs(title = "Comparing cases's growth in US, IT, CN and ES", 
         subtitle = "2020", y = "Cases")


# Cases compared by country
ggplot(melted_c, aes(series_c, value_c)) +
    geom_col()+
    labs(title = "Cases by country compared", 
         subtitle =  "2020", x = "Country", y = "Cases")



# Deaths by country and comparision
US <- data %>%  filter( geoId == "US") %>% 
    transmute(US_Deaths = deaths, dateRep)
IT <- data %>%  filter(geoId == "IT") %>% 
    transmute(IT_Deaths = deaths)
CN <- data %>%  filter(geoId == "CN") %>% 
    transmute(CN_Deaths = deaths)
ES <- data %>%  filter(geoId == "ES") %>% 
    transmute(ES_Deaths = deaths)

deaths <- bind_cols(US, IT, CN, ES)

melted <- melt(deaths, id.vars = "dateRep", variable.name = "series")

ggplot(melted, aes(dateRep, value, colour = series))+
    geom_point()+
    geom_smooth(se = F)+
    geom_rug()+
    labs(title = "Comparing deaths growth in US, IT, CN and ES", 
         subtitle = "2020", y = "deaths")


# Deaths by country compared
ggplot(melted, aes(series, value)) +
    geom_col()+
    labs(title = "Deaths by country compared", 
         subtitle =  "2020", x = "Country", y = "Deaths")

