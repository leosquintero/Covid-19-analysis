#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#


library(shinydashboard)
library(DT)


# Define UI for application that draws a histogram
shinyUI(
    dashboardPage(
        # Header ####
        dashboardHeader(title = "Covid-19 analysis"),
                        
        # Sidebar ####
        dashboardSidebar(sidebarUserPanel("Leonardo Quintero", 
                                          subtitle = '2020'),
                         sidebarMenu(
                             menuItem("Home", tabName = "Home", icon = icon("home")),
                             menuItem("Info", tabName = "Info", icon = icon("info")),
                             menuItem('Charts', tabName = "Charts", icon = icon("chart-bar")),
                             menuItem('Countries', tabName = "Countries", icon = icon("globe-americas")),
                             menuItem("Dataset", tabName = "Dataset", icon = icon("database")))),


        # boddy ####
        dashboardBody(
            tabItems(
                
                # Home and project description
                tabItem(tabName = "Home",
                        fluidPage(
                            column(width = 12, 
                                h1("Coronavirus analysis 2020"),
                                
                                h3("Introduction"),
                                p("Welcome to this interactive tool developed 
                                  to visualize data related to the Covid-19 pandemic"),
                                
                                h3("how it works"),
                                p("Select a tab from the list to your left, you will see 
                                    different tabs that will take you to visualize 
                                    information such as raw data, 
                                    worldwide interactive graphics and plots by country. You can also 
                                   find the raw data in wich you can search the total deaths and cases
                                  by country."), 
                                 
                                h3("Dataset"), 
                                a(href = "https://www.ecdc.europa.eu/en/publications-data/download-todays-data-geographic-distribution-covid-19-cases-worldwide",
                                "This dataset was obtained from 
                                  European Centre for Disease Prevention and
                                  Control"), 
                                h3("Code source"),
                                a(href = "https://github.com/leosquintero/Covid-19-analysis/tree/master/covid19app", "Find the code to this project 
                                  on Github")
                                
                            ))), 
                
                # Info section
                tabItem(tabName = "Info", 
                        fluidPage( 
                                         h1("2019–20 coronavirus pandemic"),
                                         p("The 2019–20 coronavirus pandemic is an ongoing pandemic of coronavirus disease 2019 (COVID-19), caused by severe acute respiratory syndrome coronavirus 2 (SARS-CoV-2).[6][b] The outbreak was first noted in Wuhan, Hubei province, China, in December 2019."),
                                         p("The World Health Organization (WHO) declared the outbreak to be a Public Health Emergency of International Concern on 30 January 2020 and recognized it as a pandemic on 11 March 2020.[8][9] As of 5 April 2020, more than 1.26 million cases of COVID-19 have been reported in over 200 countries and territories,[5] resulting in approximately 68,400 deaths.[4] More than 258,000 people have recovered.[4]"),
                                         p("The virus is mainly spread during close contact,[c] and by small droplets produced during coughing,[d] sneezing, or talking.[10][11][13] These small droplets may also be produced during breathing, but rapidly fall to the ground or surfaces and are not generally spread through the air over large distances.[10][14][15] People may also catch COVID-19 by touching a contaminated surface and then their face.[10][11]"),
                                         p("The virus can survive on surfaces for up to 72 hours.[16] It is most contagious during the first 3 days after symptom onset, although spread may be possible before symptoms appear and in later stages of the disease.[11][17] The time between exposure and symptom onset is typically around five days, but may range from 2 to 14 days.[18][19] Common symptoms include fever, cough, and shortness of breath.[18] Complications may include pneumonia and acute respiratory distress syndrome.[20] There is no known vaccine or specific antiviral treatment.[10] Primary treatment is symptomatic and supportive therapy.[21] Recommended preventive measures include hand washing, covering one's mouth when coughing, maintaining distance from other people, and monitoring and self-isolation for people who suspect they are infected.[10][22]"),
                                         p("Efforts to prevent the virus spread include travel restrictions, quarantines, curfews, workplace hazard controls, event postponements and cancellations, and facility closures. These include national or regional quarantines throughout the world (starting with the quarantine of Hubei), curfew measures in mainland China,[23][24][25] various border closures or incoming passenger restrictions,[26][27] screening at airports and train stations,[28] and outgoing passenger travel bans.[29][30][31]"),
                                         )),
                
                
                
                # charts section
                tabItem(tabName = "Dataset", 
                        fluidPage(titlePanel("Filter by country"),
                                  box(selectInput("dat", "Select a country", 
                                                  choices = unique(countries)),
                                      
                                    tableOutput(outputId = "dat"), width = 12, 
                                    DT::dataTableOutput(outputId = "total_cases")))),
                
                # Cases worldwide                    
                tabItem(tabName = "Charts", 
                    fluidPage(mainPanel(width = 12, 
                        tabsetPanel(
                            tabPanel("Numbers", titlePanel("Total number worldwide"), 
                                    flowLayout(
                                        box(width = 12, align = "center",
                                        tableOutput(outputId = "cases")) , 
                                        box(width = 12, align = "center",
                                        tableOutput(outputId = "deaths")))),
                            tabPanel("Worldwide", titlePanel("Cases Worldwide"), plotOutput("worldwide"),
                            tabPanel("Deaths worldwide", titlePanel("Deaths Worldwide"), plotOutput("deaths_worldwide"), 
                            tabPanel("Cases vs Deaths", titlePanel("Cases vs Deaths"), plotOutput("worldwide_cases")))), 
                            tabPanel("Cases and Deaths by country", titlePanel("Cases by country"), plotOutput("cases_by_country"), 
                            tabPanel("Deaths by country", titlePanel("Deaths by country"), plotOutput("deaths_by_country"))))
                            ))),
                
                
                # Visualization of deaths and cases by country
                tabItem(tabName = "Countries", 
                    fluidPage(mainPanel(width = 12, 
                        tabsetPanel(
                            tabPanel("Plot by country", fluidRow(titlePanel("Select country to be plotted"), 
                                                       selectInput("select", "Select Input", choices = data$Country)), 
                                     fluidPage(plotOutput("plots"), 
                                     fluidPage(plotOutput("plotsd")))),
                            tabPanel("Cases VS Deaths", titlePanel("Cases compared"), plotOutput("compare"),
                            tabPanel("Deaths compared", titlePanel("Deaths compared"), plotOutput("dcompare"))),
                            tabPanel("United States", titlePanel("Cases in teh US"), plotOutput("UScases"), 
                            tabPanel("Deaths in the US", titlePanel("Deaths in the US"), plotOutput("USdeaths"))),
                            tabPanel("Italy", titlePanel("Cases in Italy"), plotOutput("ITcases"),
                            tabPanel("Deaths in Italy", titlePanel("Deaths in Italy"), plotOutput("ITdeaths"))), 
                            tabPanel("Spain", titlePanel("Cases in Spain"), plotOutput("EScases"), 
                            tabPanel("Deaths in Spain", titlePanel("Deaths in Spain"), plotOutput("ESdeaths"))), 
                            tabPanel("China", titlePanel("Cases in China"), plotOutput("CNcases"),
                            tabPanel("Deaths in China", titlePanel("Deaths in China"), plotOutput("CNdeaths"))),
                            tabPanel("Dynamic chart", titlePanel("Dynamic Chart"), imageOutput("cv19"))
                            ))))
                ))))
