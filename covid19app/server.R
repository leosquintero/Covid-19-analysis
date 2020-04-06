#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#


library(DT)
library(googleVis)
library(dplyr)
library(ggplot2)

# Body ####
shinyServer(function(input, output) {
    
    # Data table with selected country
    output$countries <- renderTable({
        countries %>% filter(Country %in% input$countries)
        
    })
    # Showing raw data
    output$total_cases <-  renderDataTable({
        data
    })
    
    output$summa <- renderTable({
        
        summary(data)
    })
    
    output$worldwide <- renderPlot({
        data %>%  group_by(Country, dateRep) %>% 
            summarize(deaths = sum(deaths), cases = sum(cases)) %>% 
            ggplot(aes(dateRep, cases, colour = deaths))+
            geom_jitter() +
            geom_smooth(method = "lm")+
            labs(title = "Cases by Date Worldwide",
                 subtitle = "2020", x = "Month")
    })
    
    output$deaths_worldwide <- renderPlot({
        data %>% 
        ggplot(aes(x = dateRep, y = deaths, colour = cases)) +
            geom_jitter(stat = "identity", fill = "blue") +
            labs(title = "Deaths by Date Worldwide",
                 subtitle = "2020", x = "Month")
    })
    
    output$worldwide_cases <- renderPlot({
        data %>% filter(deaths > 3) %>% 
            group_by(dateRep) %>% 
            summarize(cases  = sum(cases)) %>% 
            ggplot(aes(dateRep, cases))+
            geom_line()+
            labs(title = "Cases by Date Worldwide with line",
                 subtitle = "2020", x = "Month")
    })
    
    output$deaths_by_country <- renderPlot({
        data %>%
            filter(deaths > 50) %>%
            mutate(Country = reorder(Country, cases)) %>%
            ggplot(aes(Country, deaths)) +
            geom_col() +
            coord_flip() +
            labs(title = "Deaths by country > 50",
                 subtitle = "2020", x = "Country")
    })
    
    
    # Visualizing cases by country > 1000
    output$cases_by_country <- renderPlot({
        data %>%
            filter(cases > 1000) %>%
            mutate(Country = reorder(Country, cases)) %>%
            ggplot(aes(Country, cases)) +
            geom_col() +
            xlab(NULL) +
            coord_flip()+
            labs(title = "Ceses by country > 1000",
                 subtitle = "2020", x = "Country")
    })
    
    output$march <- renderPlot({
        # March growth
        plot(data$dateRep[data$dateRep > strptime("2020-02-29", format = "%y%y-%m-%d")], 
             data$cases[data$dateRep > strptime("2020-02-29", format = "%y%y-%m-%d")], las = 1,
             xlab = "March", ylab = "")
    })
    
    # Cases compared by country
    output$compare <- renderPlot({
        ggplot(melted_c, aes(dateRep, value_c, colour = series_c))+
            geom_point()+
            geom_smooth(se = F)+
            geom_rug()+
            labs(title = "Comparing cases growth in US, IT, CN and ES", 
                 subtitle = "2020", y = "Cases")
        
    })
    
    
    # Deaths compared by country
    output$dcompare <- renderPlot({
        ggplot(melted, aes(dateRep, value, colour = series))+
            geom_point()+
            geom_smooth(se = F)+
            geom_rug()+
            labs(title = "Comparing deaths growth in US, IT, CN and ES", 
                 subtitle = "2020", y = "Deaths")
        
    })
    
    # cases in the united states
    output$UScases <- renderPlot({
        data %>% filter(geoId == "US") %>% 
            ggplot(aes(dateRep, cases, colour = deaths)) +
            geom_point() +
            geom_smooth()+
            labs(title = "Cases by date in the US", 
                 subtitle = "2020")
    })
    
    
    
    
    # cases in the united states
    output$USdeaths <- renderPlot({
        data %>% filter(geoId == "US") %>% 
            ggplot(aes(dateRep, deaths, colour = cases)) +
            geom_point() +
            geom_smooth()+
            labs(title = "Deaths by date in the US", 
                 subtitle = "2020")
    })
    
    # Cases in Italy
    
    output$ITcases <- renderPlot({
    data %>% filter(geoId == "IT") %>% 
        ggplot(aes(dateRep, cases, colour = deaths)) +
        geom_point() +
        geom_smooth()+
        labs(title = "Cases by date in Italy", 
             subtitle = "2020")
        
    })
    
    # Deaths in Italy
    output$ITdeaths <- renderPlot({
        data %>% filter(geoId == "IT") %>% 
            ggplot(aes(dateRep, deaths, colour = cases)) +
            geom_point() +
            geom_smooth()+
            labs(title = "Deaths by date in Italy", 
                 subtitle = "2020")
    
    })
    
    # Cases in Spain
    
    output$EScases <- renderPlot({
        data %>% filter(geoId == "ES") %>% 
            ggplot(aes(dateRep, cases, colour = deaths)) +
            geom_point() +
            geom_smooth()+
            labs(title = "Cases by date in Spain", 
                subtitle = "2020")
    
    })
    
    output$ESdeaths <- renderPlot({
        data %>% filter(geoId == "ES") %>% 
            ggplot(aes(dateRep, deaths, colour = cases)) +
            geom_point() +
            geom_smooth()+
            labs(title = "Deaths by date in Spain", 
                 subtitle = "2020")
    })
    
    
    # Cases in China
    
    output$CNcases <- renderPlot({
    data %>% filter(geoId == "CN") %>% 
        ggplot(aes(dateRep, cases, colour = deaths)) +
        geom_point() +
        geom_smooth() +
        labs(title = "Cases by date in China", 
             subtitle = "2020")
    })
    
    # Cases in China
    
    output$CNdeaths <- renderPlot({
        data %>% filter(geoId == "CN") %>% 
            ggplot(aes(dateRep, deaths, colour = cases)) +
            geom_point() +
            geom_smooth() +
            labs(title = "Deaths by date in China", 
                 subtitle = "2020")
    })
    
    output$plots <- renderPlot({
        data %>% filter(Country %in% input$select) %>% 
            ggplot(aes(x = dateRep, y = cases))+
            geom_point()+
            geom_smooth(se = F)+
            labs(title = "cases by country", subtitle = "2020", 
                 x = "Date", y = "Cases")
        
        
    })
    
    output$plotsd <- renderPlot({
        data %>% filter(Country %in% input$select) %>% 
            ggplot(aes(x = dateRep, y = deaths))+
            geom_point()+
            geom_smooth(se = F)+
            labs(title = "cases by country", subtitle = "2020", 
                 x = "Date", y = "Deaths")
    })
    
    
    
})
