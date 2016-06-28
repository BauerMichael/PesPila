require(shiny)
require(shinydashboard)
require(hash)
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
source("GetTableOfSeason.R")
source("UpdateLeagueL.R")
source("UpdateLeagueS.R")
source("UpdateLeagueT.R")
source("StatsTeamData.R")
source("Poisson.R")
source("ZeroInflatedPoisson.R")
source("Uniform.R")
source("Geometric.R")
source("NegativeBinomial.R")
source("Weibull.R")

seasons <- rev(GetSeasons())
country <- GetCountry()
leagues <- GetLeagues()
teams <- GetTeams()

header <- dashboardHeader(

  title = "PesPila"

)

sidebar <- dashboardSidebar(

  sidebarMenu(
    menuItem("Stats Home Team", tabName = "statshometeam", icon = icon("dashboard")),
    menuItem("Stats Away Team", tabName = "statsawayteam", icon = icon("dashboard")),
    menuItem("Tables", tabName = "results", icon = icon("th")),
    menuItem("Distributions", tabName = "dists", icon = icon("database")),
    menuItem("Forecast", tabName = "forecasts", icon = icon("database"))
    # menuItem("Widgets", icon = icon("th"), tabName = "widgets",
    #          badgeLabel = "new", badgeColor = "green")
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

)

body <- dashboardBody(

  tabItems(

    tabItem(tabName = "statshometeam",

      fluidRow(

        box(width = 4,
            title = textOutput("seasonHeaderAllHome"),
            background = "navy",
            solidHeader = TRUE,
            collapsible = TRUE,
            plotOutput("allSeasonGamesHome", height = 300)

        ),

        box(width = 4,
            title = textOutput("seasonHeaderHomeHome"),
            background = "navy",
            solidHeader = TRUE,
            collapsible = TRUE,
            plotOutput("homeSeasonGamesHome", height = 300)

        ),

        box(width = 4,
            title = textOutput("seasonHeaderAwayHome"),
            background = "navy",
            solidHeader = TRUE,
            collapsible = TRUE,
            plotOutput("awaySeasonGamesHome", height = 300)

        )

      ),

      fluidRow(

        box(width = 4,
            title = textOutput("overallHeaderAllHome"),
            background = "navy",
            solidHeader = TRUE,
            collapsible = TRUE,
            plotOutput("allGamesHome", height = 300)

        ),

        box(width = 4,
              title = textOutput("overallHeaderHomeHome"),
              background = "navy",
              solidHeader = TRUE,
              collapsible = TRUE,
              plotOutput("homeGamesHome", height = 300)

        ),

        box(width = 4,
              title = textOutput("overallHeaderAwayHome"),
              background = "navy",
              solidHeader = TRUE,
              collapsible = TRUE,
              plotOutput("awayGamesHome", height = 300)

        )

      )

    ),

    tabItem(tabName = "statsawayteam",

      fluidRow(

        box(width = 4,
            title = textOutput("seasonHeaderAllAway"),
            background = "navy",
            solidHeader = TRUE,
            collapsible = TRUE,
            plotOutput("allGamesAway", height = 300)

        ),

        box(width = 4,
              title = textOutput("seasonHeaderHomeAway"),
              background = "navy",
              solidHeader = TRUE,
              collapsible = TRUE,
              plotOutput("homeGamesAway", height = 300)

        ),

        box(width = 4,
              title = textOutput("seasonHeaderAwayAway"),
              background = "navy",
              solidHeader = TRUE,
              collapsible = TRUE,
              plotOutput("awayGamesAway", height = 300)

        )

      ),

      fluidRow(

        box(width = 4,
            title = textOutput("overallHeaderAllAway"),
            background = "navy",
            solidHeader = TRUE,
            collapsible = TRUE,
            plotOutput("allSeasonGamesAway", height = 300)

        ),

        box(width = 4,
              title = textOutput("overallHeaderHomeAway"),
              background = "navy",
              solidHeader = TRUE,
              collapsible = TRUE,
              plotOutput("homeSeasonGamesAway", height = 300)

        ),

        box(width = 4,
              title = textOutput("overallHeaderAwayAway"),
              background = "navy",
              solidHeader = TRUE,
              collapsible = TRUE,
              plotOutput("awaySeasonGamesAway", height = 300)

        )

      )

    ),

    tabItem(tabName = "results",

      fluidRow(

        column(10, offset = 1,

          tabsetPanel(

            tabPanel("Season Tables",

              h1(class = "text-center",

                textOutput(outputId = "seasonHeader")

              ),
              DT::dataTableOutput("TABLES")

            ),

            tabPanel("Season Games",

              h1(class = "text-center",

                textOutput(outputId = "gamesHeader")

              ),
              DT::dataTableOutput("GAMES")

            ),

            tabPanel("Home vs. Away",

              h1(class = "text-center",

                textOutput(outputId = "vsHeader")

              ),
              DT::dataTableOutput("VS")

            )

          )

        )

      )

    ),

    tabItem(tabName = "dists",

      fluidRow(

        column(10, offset = 1,

          tabsetPanel(

            tabPanel("Poisson",

              h1(class = "text-center",

                "Poisson Distribution"

              ),

              h3(class = "text-center",

                textOutput(outputId = "pvaluePoisson")

              ),

              plotOutput(outputId = "pDistPoisson"),
              DT::dataTableOutput("Poisson")

            ),

            tabPanel("Zero-Inflated-Poisson",

              h1(class = "text-center",

                "Zero-Inflated-Poisson Distribution"

              ),

              h3(class = "text-center",

                textOutput(outputId = "pvalueZIP")

              ),

              plotOutput(outputId = "pDistZIP"),
              DT::dataTableOutput("ZIP")

            ),

            tabPanel("Uniform",

              h1(class = "text-center",

                "Uniform Distribution"

              ),

              h3(class = "text-center",

                textOutput(outputId = "pvalueUniform")

              ),

              plotOutput(outputId = "pDistUniform"),
              DT::dataTableOutput("Uniform")

            ),

            tabPanel("Geometric",

              h1(class = "text-center",

                "Geometric Distribution"

              ),

              h3(class = "text-center",

                textOutput(outputId = "pvalueGeometric")

              ),

              plotOutput(outputId = "pDistGeometric"),
              DT::dataTableOutput("Geometric")

            ),

            tabPanel("Negative Binomial",

              h1(class = "text-center",

                "Negative Binomial Distribution"

              ),

              h3(class = "text-center",

                textOutput(outputId = "pvalueNegBinom")

              ),

              plotOutput(outputId = "pDistNegBinom"),
              DT::dataTableOutput("NegBinom")

            ),

            tabPanel("Weibull",

              h1(class = "text-center",

                "Weibull Distribution"

              ),

              h3(class = "text-center",

                textOutput(outputId = "pvalueWeibull")

              ),

              plotOutput(outputId = "pDistWeibull"),
              DT::dataTableOutput("Weibull")

            )

          )

        )

      )

    ),

    tabItem(tabName = "forecasts",

      fluidRow(

        column(6, class = "text-center",

          h1(textOutput("homeHeader"))

        ),

        column(6, class = "text-center",

          h1(textOutput("awayHeader"))

        )

      )

    )
  
  )

)

shinyUI(

  dashboardPage(header, sidebar, body, skin = "black")

)
