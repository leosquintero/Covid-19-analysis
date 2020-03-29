library(readxl)
library(dplyr)
library(httr)
library (ggplot2)
library(car) 
library(forecast) 
library(reshape2)
library(cowplot)

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



# Exploratory Data Visualization ####



# Cases vs Deaths worldwide
data %>% summarize(deaths = sum(deaths), Cases = sum(cases))


data %>%  group_by(Country, dateRep) %>% 
    summarize(deaths = sum(deaths), cases = sum(cases)) %>% 
    ggplot(aes(deaths, cases, colour = dateRep))+
    geom_jitter() +
    geom_smooth()


# cases worldwide
ggplot(data, aes(x = dateRep, y = cases)) +
    geom_jitter(stat = "identity", fill = "blue") +
    labs(title = "Cases by Date Worldwide",
         subtitle = "2020", x = "Month") 

# Cases worldwide geom_line
data %>% filter(deaths > 3) %>% 
    group_by(dateRep) %>% 
    summarize(cases  = sum(cases)) %>% 
    ggplot(aes(dateRep, cases))+
    geom_line()


# Deaths worldwide
ggplot(data, aes(x = dateRep, y = deaths)) +
    geom_jitter(stat = "identity", fill = "blue") +
    labs(title = "Cases by Date Worldwide",
         subtitle = "2020", x = "Month")


data %>% group_by(geoId) %>% 
    summarize()
    
ggplot(data) +
    geom_sf(aes(fill = deaths))+
    scale_fill_viridis_c(option = "plasma", trans = "sqrt")



# Visualizing deaths by country > 3
data %>%
    filter(deaths > 3) %>%
    mutate(Country = reorder(Country, cases)) %>%
    ggplot(aes(Country, deaths)) +
    geom_col() +
    xlab(NULL) +
    coord_flip()





# 
ggplot(data, aes(DateRep, Cases, color = Deaths)) +
    geom_point()

#Cases by country ####

# cases in the united states
 
data %>% filter(GeoId == "US") %>% 
ggplot(aes(DateRep, Cases, colour = Deaths)) +
    geom_point() +
    geom_smooth()

# Cases in Italy
data %>% filter(GeoId == "IT") %>% 
    ggplot(aes(DateRep, Cases, colour = Deaths)) +
    geom_point() +
    geom_smooth()

# Cases in China
data %>% filter(GeoId == "CN") %>% 
    ggplot(aes(DateRep, Cases, colour = Deaths)) +
    geom_point() +
    geom_smooth()

# Cases in Spain
data %>% filter(GeoId == "ES") %>% 
    ggplot(aes(DateRep, Cases, colour = Deaths)) +
    geom_point() +
    geom_smooth()



US <- data %>%  filter( geoId == "US") %>% 
    transmute(US_Deaths = deaths, US_Cases = cases, dateRep)
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
    geom_rug()
















data %>% group_by(Country, DateRep, Year, Deaths, Cases) %>% 
    transmute(GeoId == "US")






data %>%  group_by(sum(GeoId == "US")) 
           
ggplot(d, aes(GeoId, Deaths)) +
    geom_bar()


ggplot(data, aes(x = Month, y = Cases)) +
    geom_jitter()


data %>% 
    group_by(Country, Year, Month) %>%
    summarize(Deaths = sum(Deaths), Cases = sum(Cases))

plot(d$GeoId, d$Deaths)
