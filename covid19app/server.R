#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#


library(DT)
library(dplyr)
library(ggplot2)



# Body ####
shinyServer(function(input, output) {
    
    # Data table with selected country
    output$dat <- renderTable({
        dat %>% dplyr::filter(Country %in% input$dat)
        
    })
    # Showing raw data
    output$total_cases <-  DT::renderDataTable({
        data
    })
    
    # calculating total cases
    output$cases <- renderTable({
        data %>% summarize(Cases = sum(cases))
    })
    
    # Calculating total deaths
    output$deaths <- renderTable({
        data %>% summarize(Deaths = sum(deaths))
    })
    
    
    # Country vs date worldwide
    output$worldwide <- renderPlot({
        data %>%  group_by(Country, dateRep) %>% 
            summarize(deaths = sum(deaths), cases = sum(cases)) %>% 
            ggplot(aes(dateRep, cases, colour = deaths))+
            geom_jitter() +
            geom_smooth(method = "lm")+
            labs(title = "Cases by Date Worldwide",
                 subtitle = "2020", x = "Month")
    })
    
    #count of worldwide deaths
    output$deaths_worldwide <- renderPlot({
        data %>% 
        ggplot(aes(x = dateRep, y = deaths, colour = cases)) +
            geom_jitter(stat = "identity", fill = "blue") +
            labs(title = "Deaths by Date Worldwide",
                 subtitle = "2020", x = "Month")
    })
    
    # Deaths and cases compared with lines
    output$worldwide_cases <- renderPlot({
        data %>% dplyr::filter(deaths > 3) %>% 
            ggplot(aes(dateRep, colour = variable))+
            geom_line(aes(y = cases, colour = "cases"))+
            geom_line(aes(y = deaths, colour = "deaths"))+
            labs(title = "Cases vs deaths Worldwide compared",
                 subtitle = "2020", x = "Month")
    })
    
    # Deaths by country
    output$deaths_by_country <- renderPlot({
        data %>%
            dplyr::filter(deaths > 50) %>%
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
            dplyr::filter(cases > 1000) %>%
            dplyr::mutate(Country = reorder(Country, cases)) %>%
            ggplot(aes(Country, cases)) +
            geom_col() +
            xlab(NULL) +
            coord_flip()+
            labs(title = "Ceses by country > 1000",
                 subtitle = "2020", x = "Country")
    })
    
    
    # March growth
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
        data %>% dplyr::filter(geoId == "US") %>% 
            ggplot(aes(dateRep, cases, colour = deaths)) +
            geom_point() +
            geom_smooth()+
            labs(title = "Cases by date in the US", 
                 subtitle = "2020")
    })
    
    
    
    
    # cases in the united states
    output$USdeaths <- renderPlot({
        data %>% dplyr::filter(geoId == "US") %>% 
            ggplot(aes(dateRep, deaths, colour = cases)) +
            geom_point() +
            geom_smooth()+
            labs(title = "Deaths by date in the US", 
                 subtitle = "2020")
    })
    
    # Cases in Italy
    
    output$ITcases <- renderPlot({
    data %>% dplyr::filter(geoId == "IT") %>% 
        ggplot(aes(dateRep, cases, colour = deaths)) +
        geom_point() +
        geom_smooth()+
        labs(title = "Cases by date in Italy", 
             subtitle = "2020")
        
    })
    
    # Deaths in Italy
    output$ITdeaths <- renderPlot({
        data %>% dplyr::filter(geoId == "IT") %>% 
            ggplot(aes(dateRep, deaths, colour = cases)) +
            geom_point() +
            geom_smooth()+
            labs(title = "Deaths by date in Italy", 
                 subtitle = "2020")
    
    })
    
    # Cases in Spain
    
    output$EScases <- renderPlot({
        data %>% dplyr::filter(geoId == "ES") %>% 
            ggplot(aes(dateRep, cases, colour = deaths)) +
            geom_point() +
            geom_smooth()+
            labs(title = "Cases by date in Spain", 
                subtitle = "2020")
    
    })
    
    # deaths in Spain
    output$ESdeaths <- renderPlot({
        data %>% dplyr::filter(geoId == "ES") %>% 
            ggplot(aes(dateRep, deaths, colour = cases)) +
            geom_point() +
            geom_smooth()+
            labs(title = "Deaths by date in Spain", 
                 subtitle = "2020")
    })
    
    
    # Cases in China
    output$CNcases <- renderPlot({
    data %>% dplyr::filter(geoId == "CN") %>% 
        ggplot(aes(dateRep, cases, colour = deaths)) +
        geom_point() +
        geom_smooth() +
        labs(title = "Cases by date in China", 
             subtitle = "2020")
    })
    
    # Cases in China
    output$CNdeaths <- renderPlot({
        data %>% dplyr::filter(geoId == "CN") %>% 
            ggplot(aes(dateRep, deaths, colour = cases)) +
            geom_point() +
            geom_smooth() +
            labs(title = "Deaths by date in China", 
                 subtitle = "2020")
    })
    
    # Cases by country dropdown
    output$plots <- renderPlot({
        data %>% dplyr::filter(Country %in% input$select) %>% 
            ggplot( aes(dateRep, cases)) +
            geom_line(aes(y = cases, colour = "cases"))+
            geom_line(aes(y = deaths, colour = "deaths"))+
            labs(title = "Cases and deaths by country compared", subtitle = "2020", 
                 x = "Date", y = "Count")
        
        
    })
    
    # Dynamic chart
    output$cv19 <- renderImage({
        outfile <- tempfile(fileext='.gif')
        list(src = "covid19chart.gif",
             contentType = 'image/gif')
        
    })
    
})
