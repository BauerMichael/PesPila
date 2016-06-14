require(shiny)
require(shinydashboard)
require(hash)
require(ggplot2)
require(DT)
require(RSQLite)

source("InitDB.R")
source("LoadPreferences.R")
source("GetCountry.R")
source("GetLeagues.R")
source("GetSeasons.R")
source("GetTeams.R")
source("GetLeagueTable.R")
source("GetHomeVsAway.R")
source("UpdateLeagueL.R")
source("UpdateLeagueS.R")
source("UpdateLeagueT.R")
source("StatsTeamData.R")

seasons <- rev(GetSeasons())

InitDB()
country <- GetCountry()
leagues <- GetLeagues()
teams <- GetTeams()
dbDisconnect(conn = ppConn)

header <- dashboardHeader(

  title = "PesPila"

  # dropdownMenu(type = "messages",
    # messageItem(
    #   from = "Sales Dept",
    #   message = "Sales are steady this month."
    # ),
    # messageItem(
    #   from = "New User",
    #   message = "How do I register?",
    #   icon = icon("question"),
    #   time = "13:45"
    # ),
    # messageItem(
    #   from = "Support",
    #   message = "The new server is ready.",
    #   icon = icon("life-ring"),
    #   time = "2014-12-01"
    # ),
    # dropdownMenuOutput("messageMenu")
  # )

)

sidebar <- dashboardSidebar(

  sidebarMenu(
    menuItem("Stats Home Team", tabName = "statshometeam", icon = icon("dashboard")),
    menuItem("Stats Away Team", tabName = "statsawayteam", icon = icon("dashboard")),
    menuItem("Results Tables", tabName = "results", icon = icon("th")),
    menuItem("Widgets", icon = icon("th"), tabName = "widgets",
             badgeLabel = "new", badgeColor = "green")
  ),

  selectInput("leagueC",
              label = "Choose a Country",
              choices = country,
              selected = country[1]),
  
  selectInput("leagueL", 
              label = "Choose a League",
              choices = leagues,
              selected = leagues[1]),

  selectInput("leagueS", 
              label = "Choose a Season",
              choices = seasons,
              selected = seasons[1]),

  selectInput("leagueTH", 
              label = "Choose a Home Team",
              choices = teams,
              selected = "Bayern Munich"),

  selectInput("leagueTA", 
              label = "Choose a Away Team",
              choices = teams,
              selected = "Dortmund")

  # p(class = "text-center",

  #   actionButton(inputId = "leagueGetData", label = "Get Data")

  # )

)

# tabPanel("Results", DT::dataTableOutput("leagueResults"))

body <- dashboardBody(

  tabItems(

    tabItem(tabName = "statshometeam",

      fluidRow(

          box(width = 4, title = textOutput("seasonHeaderAllHome"),

            plotOutput("allSeasonGamesHome", height = 300)

          ),

          box(width = 4, title = textOutput("seasonHeaderHomeHome"),

            plotOutput("homeSeasonGamesHome", height = 300)

          ),

          box(width = 4, title = textOutput("seasonHeaderAwayHome"),

            plotOutput("awaySeasonGamesHome", height = 300)

          )

      ),

      fluidRow(

          box(width = 4, title = textOutput("overallHeaderAllHome"),

            plotOutput("allGamesHome", height = 300)

          ),

          box(width = 4, title = textOutput("overallHeaderHomeHome"),

            plotOutput("homeGamesHome", height = 300)

          ),

          box(width = 4, title = textOutput("overallHeaderAwayHome"),

            plotOutput("awayGamesHome", height = 300)

          )

      )

    ),

    tabItem(tabName = "statsawayteam",

      fluidRow(

          box(width = 4, title = textOutput("seasonHeaderAllAway"),

            plotOutput("allGamesAway", height = 300)

          ),

          box(width = 4, title = textOutput("seasonHeaderHomeAway"),

            plotOutput("homeGamesAway", height = 300)

          ),

          box(width = 4, title = textOutput("seasonHeaderAwayAway"),

            plotOutput("awayGamesAway", height = 300)

          )

      ),

      fluidRow(

          box(width = 4, title = textOutput("overallHeaderAllAway"),

            plotOutput("allSeasonGamesAway", height = 300)

          ),

          box(width = 4, title = textOutput("overallHeaderHomeAway"),

            plotOutput("homeSeasonGamesAway", height = 300)

          ),

          box(width = 4, title = textOutput("overallHeaderAwayAway"),

            plotOutput("awaySeasonGamesAway", height = 300)

          )

      )

    ),

    tabItem(tabName = "results",
    
      fluidRow(

        p(class = "text-center",

          uiOutput("buttons"),

          actionButton(inputId = "leagueGetData", label = "Games of the")
          # actionButton(inputId = "leagueGetData2", label = "Get Data")

        ),

        DT::dataTableOutput("leagueResults")

      )
    
    )

    # tabItem(tabName = "widgets",
    
    #   fluidRow(

    #     p(class = "text-center",

    #       actionButton(inputId = "leagueGetData", label = "Get Data")

    #     ),

    #     DT::dataTableOutput("leagueResults")

    #   )
    
    # )
  
  )

)

shinyUI(
  
  dashboardPage(header, sidebar, body, skin = "green")

)