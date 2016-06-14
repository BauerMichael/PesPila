shinyServer(function(input, output, session) {

  observeEvent(input$leagueC, {

    UpdateLeagueL(session = session, country = input$leagueC)
    UpdateLeagueS(session = session, country = input$leagueC, league = input$leagueL)
    UpdateLeagueT(session = session, country = input$leagueC)

  })

  observeEvent(input$leagueL, {

    UpdateLeagueS(session = session, country = input$leagueC, league = input$leagueL)
    UpdateLeagueT(session = session, country = input$leagueC)

  })

  observeEvent(input$seasonGames, {

    data <- GetLeagueTable(country = input$leagueC, league = input$leagueL, season = input$leagueS)
    output$leagueResults <- DT::renderDataTable({

      datatable(data,
          rownames = TRUE, escape = FALSE, caption = 'My first datatable try.',
          extensions = c('TableTools', 'ColReorder', 'ColVis', 'FixedColumns'),
          options = list(pageLength = -1,
              lengthMenu = list(c(-1, 50, 100), list('All', '50', '150')),
              deferRender = TRUE, colVis = list(exclude = c(0, 1), activate = 'mouseover'),
              searchHighlight = TRUE,
              dom = 'T<"clear">RC<"clear">lfrtipS',
              colReorder = list(realtime = TRUE),
              TableTools = list(sSwfPath = copySWF('www', pdf = TRUE))
          )
      )

    })

  })

  observeEvent(input$homeVSaway, {

    data <- GetHomeVsAway(input = input, country = input$leagueC, home = input$leagueTH, away = input$leagueTA)
    output$leagueResults <- DT::renderDataTable({

      datatable(data,
          rownames = TRUE, escape = FALSE, caption = 'My first datatable try.',
          extensions = c('TableTools', 'ColReorder', 'ColVis', 'FixedColumns'),
          options = list(pageLength = -1,
              lengthMenu = list(c(-1, 50, 100), list('All', '50', '150')),
              deferRender = TRUE, colVis = list(exclude = c(0, 1), activate = 'mouseover'),
              searchHighlight = TRUE,
              dom = 'T<"clear">RC<"clear">lfrtipS',
              colReorder = list(realtime = TRUE),
              TableTools = list(sSwfPath = copySWF('www', pdf = TRUE))
          )
      )

    })

  })

  output$buttons <- renderUI({

    p(class = "text-center",
    
      actionButton(inputId = "seasonGames", label = paste0("Get Games of Season ", input$leagueS)),
      actionButton(inputId = "homeVSaway", label = paste0("Get Games of ", input$leagueTH, " vs. ", input$leagueTA))
    
    )
  
  })

  output$allGamesHome <- renderPlot({

    home <- StatsTeamData(country = input$leagueC, team = input$leagueTH, locus = "HomeTeam")
    away <- StatsTeamData(country = input$leagueC, team = input$leagueTH, locus = "AwayTeam")
    all <- home + away
    barplot(all, ylab = "Number of games", col = c("#00AA00", "#8CA3B1", "#FF0000"), ylim = c(0, max(all)+50))

    seasons <- GetSeasons(country = input$leagueC, league = input$leagueL)
    header <- paste0("All Games from ", seasons[1], " to ", seasons[length(seasons)])
    output$overallHeaderAllHome <- renderText({header})

  })

  output$homeGamesHome <- renderPlot({

    home <- StatsTeamData(country = input$leagueC, team = input$leagueTH, locus = "HomeTeam")
    barplot(home, ylab = "Number of games", col = c("#00AA00", "#8CA3B1", "#FF0000"), ylim = c(0, max(home)+50))

    seasons <- GetSeasons(country = input$leagueC, league = input$leagueL)
    header <- paste0("Home Games from ", seasons[1], " to ", seasons[length(seasons)])
    output$overallHeaderHomeHome <- renderText({header})

  })

  output$awayGamesHome <- renderPlot({

    away <- StatsTeamData(country = input$leagueC, team = input$leagueTH, locus = "AwayTeam")
    barplot(away, ylab = "Number of games", col = c("#00AA00", "#8CA3B1", "#FF0000"), ylim = c(0, max(away)+50))

    seasons <- GetSeasons(country = input$leagueC, league = input$leagueL)
    header <- paste0("Away Games from ", seasons[1], " to ", seasons[length(seasons)])
    output$overallHeaderAwayHome <- renderText({header})

  })

  output$allSeasonGamesHome <- renderPlot({

    home <- StatsTeamData(country = input$leagueC, team = input$leagueTH, locus = "HomeTeam", season = input$leagueS)
    away <- StatsTeamData(country = input$leagueC, team = input$leagueTH, locus = "AwayTeam", season = input$leagueS)
    all <- home + away
    barplot(all, ylab = "Number of games", col = c("#00AA00", "#8CA3B1", "#FF0000"), ylim = c(0, max(all)+10))

    header <- paste0("All Games in ", input$leagueS)
    output$seasonHeaderAllHome <- renderText({header})

  })

  output$homeSeasonGamesHome <- renderPlot({

    home <- StatsTeamData(country = input$leagueC, team = input$leagueTH, locus = "HomeTeam", season = input$leagueS)
    barplot(home, ylab = "Number of games", col = c("#00AA00", "#8CA3B1", "#FF0000"), ylim = c(0, max(home)+10))

    header <- paste0("Home Games in ", input$leagueS)
    output$seasonHeaderHomeHome <- renderText({header})

  })

  output$awaySeasonGamesHome <- renderPlot({

    away <- StatsTeamData(country = input$leagueC, team = input$leagueTH, locus = "AwayTeam", season = input$leagueS)
    barplot(away, ylab = "Number of games", col = c("#00AA00", "#8CA3B1", "#FF0000"), ylim = c(0, max(away)+10))

    header <- paste0("Away Games in ", input$leagueS)
    output$seasonHeaderAwayHome <- renderText({header})

  })

  output$allSeasonGamesAway <- renderPlot({

    home <- StatsTeamData(country = input$leagueC, team = input$leagueTA, locus = "HomeTeam")
    away <- StatsTeamData(country = input$leagueC, team = input$leagueTA, locus = "AwayTeam")
    all <- home + away
    barplot(all, ylab = "Number of games", col = c("#00AA00", "#8CA3B1", "#FF0000"), ylim = c(0, max(all)+50))

    seasons <- GetSeasons(country = input$leagueC, league = input$leagueL)
    header <- paste0("All Games from ", seasons[1], " to ", seasons[length(seasons)])
    output$overallHeaderAllAway <- renderText({header})

  })

  output$homeSeasonGamesAway <- renderPlot({

    home <- StatsTeamData(country = input$leagueC, team = input$leagueTA, locus = "HomeTeam")
    barplot(home, ylab = "Number of games", col = c("#00AA00", "#8CA3B1", "#FF0000"), ylim = c(0, max(home)+50))

    seasons <- GetSeasons(country = input$leagueC, league = input$leagueL)
    header <- paste0("Home Games from ", seasons[1], " to ", seasons[length(seasons)])
    output$overallHeaderHomeAway <- renderText({header})

  })

  output$awaySeasonGamesAway <- renderPlot({

    away <- StatsTeamData(country = input$leagueC, team = input$leagueTA, locus = "AwayTeam")
    barplot(away, ylab = "Number of games", col = c("#00AA00", "#8CA3B1", "#FF0000"), ylim = c(0, max(away)+50))

    seasons <- GetSeasons(country = input$leagueC, league = input$leagueL)
    header <- paste0("Away Games from ", seasons[1], " to ", seasons[length(seasons)])
    output$overallHeaderAwayAway <- renderText({header})

  })

  output$allGamesAway <- renderPlot({

    home <- StatsTeamData(country = input$leagueC, team = input$leagueTA, locus = "HomeTeam", season = input$leagueS)
    away <- StatsTeamData(country = input$leagueC, team = input$leagueTA, locus = "AwayTeam", season = input$leagueS)
    all <- home + away
    barplot(all, ylab = "Number of games", col = c("#00AA00", "#8CA3B1", "#FF0000"), ylim = c(0, max(all)+10))

    header <- paste0("All Games in ", input$leagueS)
    output$seasonHeaderAllAway <- renderText({header})

  })

  output$homeGamesAway <- renderPlot({

    home <- StatsTeamData(country = input$leagueC, team = input$leagueTA, locus = "HomeTeam", season = input$leagueS)
    barplot(home, ylab = "Number of games", col = c("#00AA00", "#8CA3B1", "#FF0000"), ylim = c(0, max(home)+10))

    header <- paste0("Home Games in ", input$leagueS)
    output$seasonHeaderHomeAway <- renderText({header})

  })

  output$awayGamesAway <- renderPlot({

    away <- StatsTeamData(country = input$leagueC, team = input$leagueTA, locus = "AwayTeam", season = input$leagueS)
    barplot(away, ylab = "Number of games", col = c("#00AA00", "#8CA3B1", "#FF0000"), ylim = c(0, max(away)+10))

    header <- paste0("Away Games in ", input$leagueS)
    output$seasonHeaderAwayAway <- renderText({header})

  })

})