require(shiny)
require(hash)
require(ggplot2)
require(DT)
require(RSQLite)

source("InitDB.R")
source("LoadPreferences.R")
source("GetCountry.R")
source("GetLeagues.R")
source("GetSeasons.R")
source("GetLeagueTable.R")
source("UpdateLeagueL.R")
source("UpdateLeagueS.R")

InitDB()
country <- GetCountry()
leagues <- GetLeagues()
seasons <- rev(GetSeasons())
dbDisconnect(conn = ppConn)

# options(warn=-1)

shinyUI(

    navbarPage(

        a(href = "./", "PesPila"),

        tabPanel("home",

          fluidRow (

            column(10, offset = 1, class = "jumbotron text-center",

              tabsetPanel(

                tabPanel("Home",

                    h1("PesPila"),
                    h3("A tool to predict the outcome of football matches.")
                
                ),

                tabPanel("Data sources and literature",

                  h2("Source of the datasets"),
                  
                  p(class = "text-center", 

                    "Please visit: ", a(href = "http://www.football-data.co.uk", "Football-Data.co.uk", target = "blank")

                  )

                )
              
              )
          
            )
          )
        
        ),

        tabPanel("league data",

          fluidRow(

            column(3, offset = 1,

              sidebarPanel(width = 12,
                
                selectInput("DO",
                            label = "Do something",
                            choices = c("Kicker", "Bild"),
                            selected = "Kicker"),

                selectInput("leagueC",
                            label = "Choose a country",
                            choices = country,
                            selected = country[1]),
                
                selectInput("leagueL", 
                            label = "Choose a league",
                            choices = leagues,
                            selected = leagues[1]),

                selectInput("leagueS", 
                            label = "Choose a season",
                            choices = seasons,
                            selected = seasons[1])

                # p(class = "text-center",

                #   actionButton(inputId = "leagueGetData", label = "Get Data")

                # )

              ),

              sidebarPanel(width = 12, id = "legend",

                div("Description of the shortcuts:", id = "header"),
                br(),
                div("FTHG = Full Time Home Goals"),
                div("FTAG = Full Time Away Goals"),
                div("FTR = Full Time Result"),
                div("HTHG = Half Time Home Goals"),
                div("HTAG = Half Time Away Goals"),
                div("HTR = Half Time Result")
              
              )

            ),

            column(7,

                tabsetPanel(

                  tabPanel("Results", DT::dataTableOutput("leagueResults")),

                  tabPanel("Results", {

                    tabsetPanel(

                      tabPanel("Absolute", DT::dataTableOutput("results")),
                      tabPanel("in %", DT::dataTableOutput("resultsPercent"))

                    )

                  }),
                    
                    tabPanel("Match results", {

                      tabsetPanel(

                        tabPanel("Absolute", DT::dataTableOutput("results")),
                        tabPanel("in %", DT::dataTableOutput("resultsPercent"))

                      )

                    }),
                    tabPanel("Wins", {

                      tabsetPanel(

                        tabPanel("Absolute", DT::dataTableOutput("wins")),
                        tabPanel("in %", DT::dataTableOutput("winsPercent"))

                      )

                    }),
                    tabPanel("Goal Differences", {

                      tabsetPanel(

                        tabPanel("Absolute", DT::dataTableOutput("goalDifferences")),
                        tabPanel("in %", DT::dataTableOutput("goalDifferencesPercent"))

                      )

                    }),
                    tabPanel("Goal Sums", {

                      tabsetPanel(

                        tabPanel("Per Match (GSPM)", DT::dataTableOutput("gspm")),
                        tabPanel("Per Season (GSPS)", DT::dataTableOutput("gsps")),
                        tabPanel("Plot GSPM", plotOutput("gspmPlot")),
                        tabPanel("Plot GSPS", plotOutput("gspsPlot"))

                      )

                    }),
                    tabPanel("All Matches", DT::dataTableOutput("all"))
                )
            )
          )
        ),
        
        tags$head(
            tags$link(rel = "stylesheet", type = "text/css", href = "/css/bootstrap.min.css"),
            tags$link(rel = "stylesheet", type = "text/css", href = "/css/styles.css"),
            tags$script(src = "/js/scripts.js")
        )
    )
)