shinyServer(function(input, output, session) {

  observe({input$leagueC

    UpdateLeagueL(session = session, country = input$leagueC, league = input$leagueL)
    UpdateLeagueS(session = session, country = input$leagueC, league = input$leagueL, season = input$leagueS)
    UpdateLeagueT(session = session, country = input$leagueC, league = input$leagueL, season = input$leagueS)

  })

  observe({input$leagueL

    UpdateLeagueS(session = session, country = input$leagueC, league = input$leagueL, season = input$leagueS)
    UpdateLeagueT(session = session, country = input$leagueC, league = input$leagueL, season = input$leagueS)

  })

  output$Poisson <- DT::renderDataTable({

    data <- Poisson(country = input$leagueC, team = input$leagueTH, season = input$leagueS)

    output$pDistPoisson <- renderPlot({

      plot(data$Table$Goals, data$Normal, type = "l", col = "blue",
           ylim = c(min(data$Normal, data$Table$Probabilities), max(data$Normal, data$Table$Probabilities)))
      lines(data$Table$Goals, data$Table$Probabilities, col = "red")

    })

    output$pvaluePoisson <- renderText({

      h0 <- ""
      if (data$ChiSquare$p.value > 0.05) {

        h0 <- "Accepting null-hypothesis with p-value = "

      } else {

        h0 <- "Rejecting null-hypothesis with p-value = "

      }

      paste0(h0, round(data$ChiSquare$p.value, 2))

    })

    datatable(data$Table,
        rownames = FALSE, escape = FALSE,
        extensions = c('ColReorder', 'ColVis', 'Responsive'),
        options = list(pageLength = -1,
            lengthMenu = list(c(-1, 50, 100), list('All', '50', '150')),
            deferRender = TRUE, colVis = list(exclude = c(0, 1), activate = 'mouseover'),
            searchHighlight = TRUE,
            dom = 'TRCSlfrtip<"clear">',
            colReorder = list(realtime = TRUE)
        )
    )
  
  })

  output$ZIP <- DT::renderDataTable({

    data <- ZIP(country = input$leagueC, team = input$leagueTH, season = input$leagueS)

    output$pDistZIP <- renderPlot({

      plot(data$Table$Goals, data$Normal, type = "l", col = "blue",
           ylim = c(min(data$Normal, data$Table$NewProbs), max(data$Normal, data$Table$NewProbs)))
      lines(data$Table$Goals, data$Table$NewProbs, col = "red")

    })

    output$pvalueZIP <- renderText({

      h0 <- ""
      if (data$ChiSquare$p.value > 0.05) {

        h0 <- "Accepting null-hypothesis with p-value = "

      } else {

        h0 <- "Rejecting null-hypothesis with p-value = "

      }

      paste0(h0, round(data$ChiSquare$p.value, 2), " with lambda = ", round(data$lambda, 2), " and phi = ", round(data$phi, 2), ".")

    })

    datatable(data$Table,
        rownames = FALSE, escape = FALSE,
        extensions = c('ColReorder', 'ColVis', 'Responsive'),
        options = list(pageLength = -1,
            lengthMenu = list(c(-1, 50, 100), list('All', '50', '150')),
            deferRender = TRUE, colVis = list(exclude = c(0, 1), activate = 'mouseover'),
            searchHighlight = TRUE,
            dom = 'TRCSlfrtip<"clear">',
            colReorder = list(realtime = TRUE)
        )
    )
  
  })

  output$Uniform <- DT::renderDataTable({

    data <- Uniform(country = input$leagueC, team = input$leagueTH, season = input$leagueS)

    output$pDistUniform <- renderPlot({

      plot(data$Table$Goals, data$Normal, type = "l", col = "blue",
           ylim = c(min(data$Normal, data$Table$Probabilities), max(data$Normal, data$Table$Probabilities)))
      lines(data$Table$Goals, data$Table$Probabilities, col = "red")

    })

    output$pvalueUniform <- renderText({

      h0 <- ""
      if (data$ChiSquare > 0.05) {

        h0 <- "Accepting null-hypothesis with p-value = "

      } else {

        h0 <- "Rejecting null-hypothesis with p-value = "

      }

      paste0(h0, round(data$ChiSquare, 2), " and a = ", data$a, ", b = ", data$b)

    })

    datatable(data$Table,
        rownames = FALSE, escape = FALSE,
        extensions = c('ColReorder', 'ColVis', 'Responsive'),
        options = list(pageLength = -1,
            lengthMenu = list(c(-1, 50, 100), list('All', '50', '150')),
            deferRender = TRUE, colVis = list(exclude = c(0, 1), activate = 'mouseover'),
            searchHighlight = TRUE,
            dom = 'TRCSlfrtip<"clear">',
            colReorder = list(realtime = TRUE)
        )
    )
  
  })

  output$Geometric <- DT::renderDataTable({

    data <- Geometric(country = input$leagueC, team = input$leagueTH, season = input$leagueS)

    output$pDistGeometric <- renderPlot({

      plot(data$Table$Goals, data$Normal, type = "l", col = "blue",
           ylim = c(min(data$Normal, data$Table$Probabilities), max(data$Normal, data$Table$Probabilities)))
      lines(data$Table$Goals, data$Table$Probabilities, col = "red")

    })

    output$pvalueGeometric <- renderText({

      h0 <- ""
      if (data$ChiSquare$p.value > 0.05) {

        h0 <- "Accepting null-hypothesis with p-value = "

      } else {

        h0 <- "Rejecting null-hypothesis with p-value = "

      }

      paste0(h0, round(data$ChiSquare$p.value, 2))

    })

    datatable(data$Table,
        rownames = FALSE, escape = FALSE,
        extensions = c('ColReorder', 'ColVis', 'Responsive'),
        options = list(pageLength = -1,
            lengthMenu = list(c(-1, 50, 100), list('All', '50', '150')),
            deferRender = TRUE, colVis = list(exclude = c(0, 1), activate = 'mouseover'),
            searchHighlight = TRUE,
            dom = 'TRCSlfrtip<"clear">',
            colReorder = list(realtime = TRUE)
        )
    )
  
  })

  output$NegBinom <- DT::renderDataTable({

    data <- NBD(country = input$leagueC, team = input$leagueTH, season = input$leagueS)

    output$pDistNegBinom <- renderPlot({

      plot(data$Table$Goals, data$Normal, type = "l", col = "blue",
           ylim = c(min(data$Normal, data$Table$NewProbs), max(data$Normal, data$Table$NewProbs)))
      lines(data$Table$Goals, data$Table$NewProbs, col = "red")

    })

    output$pvalueNegBinom <- renderText({

      h0 <- ""
      if (data$ChiSquare$p.value > 0.05) {

        h0 <- "Accepting null-hypothesis with p-value = "

      } else {

        h0 <- "Rejecting null-hypothesis with p-value = "

      }

      paste0(h0, round(data$ChiSquare$p.value, 2), " with k = ", round(data$k, 2), " and p = ", round(data$p, 2), ".")

    })

    datatable(data$Table,
        rownames = FALSE, escape = FALSE,
        extensions = c('ColReorder', 'ColVis', 'Responsive'),
        options = list(pageLength = -1,
            lengthMenu = list(c(-1, 50, 100), list('All', '50', '150')),
            deferRender = TRUE, colVis = list(exclude = c(0, 1), activate = 'mouseover'),
            searchHighlight = TRUE,
            dom = 'TRCSlfrtip<"clear">',
            colReorder = list(realtime = TRUE)
        )
    )
  
  })

  output$ZIW <- DT::renderDataTable({

    data <- ZIW(country = input$leagueC, team = input$leagueTH, season = input$leagueS)

    output$pDistWeibull <- renderPlot({

      plot(data$Table$Goals, data$Normal, type = "l", col = "blue",
           ylim = c(min(data$Normal, data$Table$NewProbs), max(data$Normal, data$Table$NewProbs)))
      lines(data$Table$Goals, data$Table$NewProbs, col = "red")

    })

    output$pvalueWeibull <- renderText({

      h0 <- ""
      if (data$ChiSquare$p.value > 0.05) {

        h0 <- "Accepting null-hypothesis with p-value = "

      } else {

        h0 <- "Rejecting null-hypothesis with p-value = "

      }

      paste0(h0, round(data$ChiSquare$p.value, 2), " with shape = ", round(data$shape, 2), ", scale = ", round(data$scale, 2), " and phi = ", round(data$phi, 2), ".")

    })

    datatable(data$Table,
        rownames = FALSE, escape = FALSE,
        extensions = c('ColReorder', 'ColVis', 'Responsive'),
        options = list(pageLength = -1,
            lengthMenu = list(c(-1, 50, 100), list('All', '50', '150')),
            deferRender = TRUE, colVis = list(exclude = c(0, 1), activate = 'mouseover'),
            searchHighlight = TRUE,
            dom = 'TRCSlfrtip<"clear">',
            colReorder = list(realtime = TRUE)
        )
    )
  
  })

  output$GAMES <- DT::renderDataTable({

    data <- GetLeagueTable(country = input$leagueC, league = input$leagueL, season = input$leagueS)

    datatable(data[, 4:ncol(data)],
        rownames = FALSE, escape = FALSE,
        extensions = c('ColReorder', 'ColVis', 'Responsive'),
        options = list(pageLength = -1,
            lengthMenu = list(c(-1, 50, 100), list('All', '50', '150')),
            deferRender = TRUE, colVis = list(exclude = c(0, 1), activate = 'mouseover'),
            searchHighlight = TRUE,
            dom = 'TRCSlfrtip<"clear">',
            colReorder = list(realtime = TRUE)
        )
    )

  })


  output$TABLES <- DT::renderDataTable({

    data <- GetLeagueTable(country = input$leagueC, league = input$leagueL, season = input$leagueS)
    table <- GetTableOfSeason(data = data)
    table <- cbind("Place" = 1:nrow(table), table)

    datatable(table,
        rownames = FALSE, escape = FALSE,
        extensions = c('ColReorder', 'ColVis', 'Responsive'),
        options = list(pageLength = -1,
            lengthMenu = list(c(-1, 50, 100), list('All', '50', '150')),
            deferRender = TRUE, colVis = list(exclude = c(0, 1), activate = 'mouseover'),
            searchHighlight = TRUE,
            dom = 'TRCSlfrtip<"clear">',
            colReorder = list(realtime = TRUE)
        )
    )

  })

  output$VS <- DT::renderDataTable({

    data <- GetHomeVsAway(input = input, country = input$leagueC, home = input$leagueTH, away = input$leagueTA)

    datatable(data,
        rownames = FALSE, escape = FALSE,
        extensions = c('ColReorder', 'ColVis', 'Responsive'),
        options = list(pageLength = -1,
            lengthMenu = list(c(-1, 50, 100), list('All', '50', '150')),
            deferRender = TRUE, colVis = list(exclude = c(0, 1), activate = 'mouseover'),
            searchHighlight = TRUE,
            dom = 'TRCSlfrtip<"clear">',
            colReorder = list(realtime = TRUE)
        )
    )

  })

  output$gamesHeader <- renderText({

    paste0("Games of ", input$leagueL, " - Season ", input$leagueS)

  })

  output$seasonHeader <- renderText({

    paste0("Table of ", input$leagueL, " - Season ", input$leagueS)

  })

  output$vsHeader <- renderText({

    paste0(input$leagueTH, " vs. ", input$leagueTA)

  })

  output$tabPanel <- renderUI({

    tabPanel("Absolute", DT::dataTableOutput("goalDifferences"))
    tabPanel("in %", DT::dataTableOutput("goalDifferencesPercent"))

  })

  output$allGamesHome <- renderPlot({

    home <- StatsTeamData(country = input$leagueC, team = input$leagueTH, locus = "HomeTeam")
    away <- StatsTeamData(country = input$leagueC, team = input$leagueTH, locus = "AwayTeam")
    all <- home + away
    barplot(all, ylab = "Number of games", col = c("#00AA00", "#8CA3B1", "#FF0000"), ylim = c(0, max(all)+50))

    seasons <- GetSeasons(country = input$leagueC, league = input$leagueL)
    header <- paste0("Games of ", input$leagueTH, " ", seasons[1], " to ", seasons[length(seasons)])
    output$overallHeaderAllHome <- renderText({header})

  })

  output$homeGamesHome <- renderPlot({

    home <- StatsTeamData(country = input$leagueC, team = input$leagueTH, locus = "HomeTeam")
    barplot(home, ylab = "Number of games", col = c("#00AA00", "#8CA3B1", "#FF0000"), ylim = c(0, max(home)+50))

    seasons <- GetSeasons(country = input$leagueC, league = input$leagueL)
    header <- paste0("Home Games of ", input$leagueTH, " ", seasons[1], " to ", seasons[length(seasons)])
    output$overallHeaderHomeHome <- renderText({header})

  })

  output$awayGamesHome <- renderPlot({

    away <- StatsTeamData(country = input$leagueC, team = input$leagueTH, locus = "AwayTeam")
    barplot(away, ylab = "Number of games", col = c("#00AA00", "#8CA3B1", "#FF0000"), ylim = c(0, max(away)+50))

    seasons <- GetSeasons(country = input$leagueC, league = input$leagueL)
    header <- paste0("Away Games of ", input$leagueTH, " ", seasons[1], " to ", seasons[length(seasons)])
    output$overallHeaderAwayHome <- renderText({header})

  })

  output$allSeasonGamesHome <- renderPlot({

    home <- StatsTeamData(country = input$leagueC, team = input$leagueTH, locus = "HomeTeam", season = input$leagueS)
    away <- StatsTeamData(country = input$leagueC, team = input$leagueTH, locus = "AwayTeam", season = input$leagueS)
    all <- home + away
    barplot(all, ylab = "Number of games", col = c("#00AA00", "#8CA3B1", "#FF0000"), ylim = c(0, max(all)+10))

    header <- paste0("Games of ", input$leagueTH, " ", input$leagueS)
    output$seasonHeaderAllHome <- renderText({header})

  })

  output$homeSeasonGamesHome <- renderPlot({

    home <- StatsTeamData(country = input$leagueC, team = input$leagueTH, locus = "HomeTeam", season = input$leagueS)
    barplot(home, ylab = "Number of games", col = c("#00AA00", "#8CA3B1", "#FF0000"), ylim = c(0, max(home)+10))

    header <- paste0("Home Games of ", input$leagueTH, " ", input$leagueS)
    output$seasonHeaderHomeHome <- renderText({header})

  })

  output$awaySeasonGamesHome <- renderPlot({

    away <- StatsTeamData(country = input$leagueC, team = input$leagueTH, locus = "AwayTeam", season = input$leagueS)
    barplot(away, ylab = "Number of games", col = c("#00AA00", "#8CA3B1", "#FF0000"), ylim = c(0, max(away)+10))

    header <- paste0("Away Games of ", input$leagueTH, " in ", input$leagueS)
    output$seasonHeaderAwayHome <- renderText({header})

  })

  output$allSeasonGamesAway <- renderPlot({

    home <- StatsTeamData(country = input$leagueC, team = input$leagueTA, locus = "HomeTeam")
    away <- StatsTeamData(country = input$leagueC, team = input$leagueTA, locus = "AwayTeam")
    all <- home + away
    barplot(all, ylab = "Number of games", col = c("#00AA00", "#8CA3B1", "#FF0000"), ylim = c(0, max(all)+50))

    seasons <- GetSeasons(country = input$leagueC, league = input$leagueL)
    header <- paste0("Games of ", input$leagueTA, " ", seasons[1], " to ", seasons[length(seasons)])
    output$overallHeaderAllAway <- renderText({header})

  })

  output$homeSeasonGamesAway <- renderPlot({

    home <- StatsTeamData(country = input$leagueC, team = input$leagueTA, locus = "HomeTeam")
    barplot(home, ylab = "Number of games", col = c("#00AA00", "#8CA3B1", "#FF0000"), ylim = c(0, max(home)+50))

    seasons <- GetSeasons(country = input$leagueC, league = input$leagueL)
    header <- paste0("Home Games of ", input$leagueTA, " ", seasons[1], " to ", seasons[length(seasons)])
    output$overallHeaderHomeAway <- renderText({header})

  })

  output$awaySeasonGamesAway <- renderPlot({

    away <- StatsTeamData(country = input$leagueC, team = input$leagueTA, locus = "AwayTeam")
    barplot(away, ylab = "Number of games", col = c("#00AA00", "#8CA3B1", "#FF0000"), ylim = c(0, max(away)+50))

    seasons <- GetSeasons(country = input$leagueC, league = input$leagueL)
    header <- paste0("Away Games of ", input$leagueTA, " ", seasons[1], " to ", seasons[length(seasons)])
    output$overallHeaderAwayAway <- renderText({header})

  })

  output$allGamesAway <- renderPlot({

    home <- StatsTeamData(country = input$leagueC, team = input$leagueTA, locus = "HomeTeam", season = input$leagueS)
    away <- StatsTeamData(country = input$leagueC, team = input$leagueTA, locus = "AwayTeam", season = input$leagueS)
    all <- home + away
    barplot(all, ylab = "Number of games", col = c("#00AA00", "#8CA3B1", "#FF0000"), ylim = c(0, max(all)+10))

    header <- paste0("Games of ", input$leagueTA, " ", input$leagueS)
    output$seasonHeaderAllAway <- renderText({header})

  })

  output$homeGamesAway <- renderPlot({

    home <- StatsTeamData(country = input$leagueC, team = input$leagueTA, locus = "HomeTeam", season = input$leagueS)
    barplot(home, ylab = "Number of games", col = c("#00AA00", "#8CA3B1", "#FF0000"), ylim = c(0, max(home)+10))

    header <- paste0("Home Games of ", input$leagueTA, " ", input$leagueS)
    output$seasonHeaderHomeAway <- renderText({header})

  })

  output$awayGamesAway <- renderPlot({

    away <- StatsTeamData(country = input$leagueC, team = input$leagueTA, locus = "AwayTeam", season = input$leagueS)
    barplot(away, ylab = "Number of games", col = c("#00AA00", "#8CA3B1", "#FF0000"), ylim = c(0, max(away)+10))

    header <- paste0("Away Games in ", input$leagueS)
    output$seasonHeaderAwayAway <- renderText({header})

  })

  output$homeHeader <- renderText({input$leagueTH})
  output$awayHeader <- renderText({input$leagueTA})

})