library(shiny)

shinyServer(

  function(input, output, session) {

    observe({input$DO,

      print('Blubb')

    })

    observe({input$leagueC

      UpdateLeagueL(session = session, country = input$leagueC)
      UpdateLeagueS(session = session, country = input$leagueC, league = input$leagueL)

    })

    observe({input$leagueL

      UpdateLeagueS(session = session, country = input$leagueC, league = input$leagueL)

    })

  }

)
