require(shiny)
require(shinydashboard)
require(hash)
require(DT)
require(ggplot2)
require(RSQLite)
require(reshape2)

options(scipen=999)

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
source("WinDrawLost.R")

seasons <- rev(GetSeasons())
country <- GetCountry()
leagues <- GetLeagues()
teams <- GetTeams()

header <- dashboardHeader(

  title = "PesPila"

)

sidebar <- dashboardSidebar(

  tags$link(rel = "stylesheet", type = "text/css", href = "/css/styles.css"),

  sidebarMenu(
    menuItem("Overview", tabName = "overview", icon = icon("dashboard")),
    menuItem("Tables", tabName = "results", icon = icon("th")),
    menuItem("Distributions", tabName = "dists", icon = icon("database")),
    menuItem("Forecast", tabName = "forecasts", icon = icon("database"))
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
              selected = teams[1]),

  selectInput("leagueTA",
              label = "Choose a Away Team",
              choices = teams,
              selected = teams[2])

)

body <- dashboardBody(

  tabItems(

    tabItem(tabName = "overview",

      fluidRow(

        box(width = 4,
            title = textOutput("allGamesOfHome"),
            background = "light-blue",
            solidHeader = TRUE,
            collapsible = TRUE,
            plotOutput("allGamesOfHomeStat", height = 300)

        ),

        box(width = 4,
            title = textOutput("homeGamesOfHome"),
            background = "light-blue",
            solidHeader = TRUE,
            collapsible = TRUE,
            plotOutput("homeGamesOfHomeStat", height = 300)

        ),

        box(width = 4,
            title = textOutput("awayGamesOfHome"),
            background = "light-blue",
            solidHeader = TRUE,
            collapsible = TRUE,
            plotOutput("awayGamesOfHomeStat", height = 300)

        )

      ),

      fluidRow(

        box(width = 4,
            title = textOutput("allGamesOfAway"),
            background = "navy",
            solidHeader = TRUE,
            collapsible = TRUE,
            plotOutput("allGamesOfAwayStat", height = 300)

        ),

        box(width = 4,
              title = textOutput("homeGamesOfAway"),
              background = "navy",
              solidHeader = TRUE,
              collapsible = TRUE,
              plotOutput("homeGamesOfAwayStat", height = 300)

        ),

        box(width = 4,
              title = textOutput("awayGamesOfAway"),
              background = "navy",
              solidHeader = TRUE,
              collapsible = TRUE,
              plotOutput("awayGamesOfAwayStat", height = 300)

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
              DT::dataTableOutput("Tables")

            ),

            tabPanel("Season Games",

              h1(class = "text-center",

                textOutput(outputId = "gamesHeader")

              ),
              DT::dataTableOutput("Games")

            ),

            tabPanel("Home vs. Away",

              h1(class = "text-center",

                textOutput(outputId = "vsHeader")

              ),
              DT::dataTableOutput("vs")

            )

          )

        )

      )

    ),

    tabItem(tabName = "dists",

      fluidRow(

        column(12,

          tabsetPanel(

            tabPanel("Goals Scored",

              fluidRow(

                h1(class = "text-center",

                  "Distributions"

                ),

                column(9,

                  plotOutput("goalScored", height = 500,
                    dblclick = "Scored_dblclick",
                    brush = brushOpts(
                      id = "Scored_brush",
                      resetOnNew = TRUE
                    )
                  )

                ),

                column(3,

                  DT::dataTableOutput("gScored")

                )

              )

            ),

            tabPanel("Goals Conceded",

              fluidRow(

                h1(class = "text-center",

                  "Distributions"

                ),

                column(9,

                  plotOutput("goalConceded", height = 500,
                    dblclick = "Coneded_dblclick",
                    brush = brushOpts(
                      id = "Coneded_brush",
                      resetOnNew = TRUE
                    )
                  )

                ),

                column(3,

                  DT::dataTableOutput("gConceded")

                )

              )

              # tabsetPanel(

              #   tabPanel("Poisson",

              #     h1(class = "text-center",

              #       "Poisson Distribution"

              #     ),

              #     h3(class = "text-center",

              #       textOutput(outputId = "pvaluePoissonA")

              #     ),

              #     plotOutput(outputId = "pDistPoissonA"),
              #     DT::dataTableOutput("PoissonA")

              #   ),

              #   tabPanel("Zero-Inflated-Poisson",

              #     h1(class = "text-center",

              #       "Zero-Inflated-Poisson Distribution"

              #     ),

              #     h3(class = "text-center",

              #       textOutput(outputId = "pvalueZIPA")

              #     ),

              #     plotOutput(outputId = "pDistZIPA"),
              #     DT::dataTableOutput("ZIPA")

              #   ),

              #   tabPanel("Uniform",

              #     h1(class = "text-center",

              #       "Uniform Distribution"

              #     ),

              #     h3(class = "text-center",

              #       textOutput(outputId = "pvalueUniformA")

              #     ),

              #     plotOutput(outputId = "pDistUniformA"),
              #     DT::dataTableOutput("UniformA")

              #   ),

              #   tabPanel("Geometric",

              #     h1(class = "text-center",

              #       "Geometric Distribution"

              #     ),

              #     h3(class = "text-center",

              #       textOutput(outputId = "pvalueGeometricA")

              #     ),

              #     plotOutput(outputId = "pDistGeometricA"),
              #     DT::dataTableOutput("GeometricA")

              #   ),

              #   tabPanel("Negative Binomial",

              #     h1(class = "text-center",

              #       "Negative Binomial Distribution"

              #     ),

              #     h3(class = "text-center",

              #       textOutput(outputId = "pvalueNegBinomA")

              #     ),

              #     plotOutput(outputId = "pDistNegBinomA"),
              #     DT::dataTableOutput("NegBinomA")

              #   )

              # )

            )

            # tabPanel("P-Value Prediction",

            #   fluidRow(

            #     h1(class = "text-center",

            #       "Poisson P-Value"

            #     ),

            #     column(9,

            #       plotOutput("poissonPval", height = 500,
            #         dblclick = "plot_dblclick",
            #         brush = brushOpts(
            #           id = "plot_brush",
            #           resetOnNew = TRUE
            #         )
            #       )

            #     ),

            #     column(3,

            #       DT::dataTableOutput("pPval")

            #     )

            #   )

            # )

          )

        )

      )

    ),

    tabItem(tabName = "forecasts",

      fluidRow(

        column(8, offset = 2, class = "text-center",

          h1("Forecast"),
          dataTableOutput("predictScored")

        )

      )

    )
  
  )

)

shinyUI(

  dashboardPage(header, sidebar, body, skin = "black")

)
