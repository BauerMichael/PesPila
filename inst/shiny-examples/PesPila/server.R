shinyServer(function(input, output, session) {

  observe({input$leagueC

    withProgress(message = 'Calculation in progress', detail = 'This may take a while...', min = 0, max = 2, {

      UpdateLeagueL(session = session, country = input$leagueC, league = input$leagueL)
      UpdateLeagueS(session = session, country = input$leagueC, league = input$leagueL, season = input$leagueS)
      UpdateLeagueT(session = session, country = input$leagueC, league = input$leagueL, season = input$leagueS)

    })

  })

  observe({input$leagueL

    withProgress(message = 'Calculation in progress', detail = 'This may take a while...', min = 0, max = 2, {

      UpdateLeagueS(session = session, country = input$leagueC, league = input$leagueL, season = input$leagueS)
      UpdateLeagueT(session = session, country = input$leagueC, league = input$leagueL, season = input$leagueS)

    })

  })

  output$Poisson <- DT::renderDataTable({

    withProgress(message = 'Calculation in progress', detail = 'This may take a while...', min = 0, max = 2, {

      if (input$leagueC != "" && input$leagueS != "" && input$leagueTH != "" && input$leagueL != "") {

        data <- Poisson(country = input$leagueC, team = input$leagueTH, season = input$leagueS)

        output$pDistPoisson <- renderPlot({

          plot(data$Table$Goals, data$Normal, type = "l", col = "blue",
               ylim = c(min(data$Normal, data$Table$NewProbs), max(data$Normal, data$Table$NewProbs)))
          lines(data$Table$Goals, data$Table$NewProbs, col = "red")

        })

        output$pvaluePoisson <- renderText({

          h0 <- ""
          if (data$ChiSquare$p.value > 0.05) {

            h0 <- "Accepting null-hypothesis for "

          } else {

            h0 <- "Rejecting null-hypothesis for "

          }

          paste0(h0, " ", input$leagueTH, " with p-value = ", round(data$ChiSquare$p.value, 2))

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

      }

    })
  
  })

  output$ZIP <- DT::renderDataTable({

    withProgress(message = 'Calculation in progress', detail = 'This may take a while...', min = 0, max = 2, {

      if (input$leagueC != "" && input$leagueS != "" && input$leagueTH != "" && input$leagueL != "") {

        data <- ZIP(country = input$leagueC, team = input$leagueTH, season = input$leagueS)

        output$pDistZIP <- renderPlot({

          plot(data$Table$Goals, data$Normal, type = "l", col = "blue",
               ylim = c(min(data$Normal, data$Table$NewProbs), max(data$Normal, data$Table$NewProbs)))
          lines(data$Table$Goals, data$Table$NewProbs, col = "red")

        })

        output$pvalueZIP <- renderText({

          h0 <- ""
          if (data$ChiSquare$p.value > 0.05) {

            h0 <- "Accepting null-hypothesis for "

          } else {

            h0 <- "Rejecting null-hypothesis for "

          }

          paste0(h0, " ", input$leagueTH, " with p-value = ", round(data$ChiSquare$p.value, 2), " with lambda = ", round(data$lambda, 2), " and phi = ", round(data$phi, 2), ".")

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

      }

    })
  
  })

  output$Uniform <- DT::renderDataTable({

    withProgress(message = 'Calculation in progress', detail = 'This may take a while...', min = 0, max = 2, {

      if (input$leagueC != "" && input$leagueS != "" && input$leagueTH != "" && input$leagueL != "") {

        data <- Uniform(country = input$leagueC, team = input$leagueTH, season = input$leagueS)

        output$pDistUniform <- renderPlot({

          plot(data$Table$Goals, data$Normal, type = "l", col = "blue",
               ylim = c(min(data$Normal, data$Table$NewProbs), max(data$Normal, data$Table$NewProbs)))
          lines(data$Table$Goals, data$Table$NewProbs, col = "red")

        })

        output$pvalueUniform <- renderText({

          h0 <- ""
          if (data$ChiSquare$p.value > 0.05) {

            h0 <- "Accepting null-hypothesis for "

          } else {

            h0 <- "Rejecting null-hypothesis for "

          }

          paste0(h0, " ", input$leagueTH, " with p-value = ", round(data$ChiSquare$p.value, 2), " and a = ", data$a, ", b = ", data$b)

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

      }

    })
  
  })

  output$Geometric <- DT::renderDataTable({

    withProgress(message = 'Calculation in progress', detail = 'This may take a while...', min = 0, max = 2, {

      if (input$leagueC != "" && input$leagueS != "" && input$leagueTH != "" && input$leagueL != "") {

        data <- Geometric(country = input$leagueC, team = input$leagueTH, season = input$leagueS)

        output$pDistGeometric <- renderPlot({

          plot(data$Table$Goals, data$Normal, type = "l", col = "blue",
               ylim = c(min(data$Normal, data$Table$NewProbs), max(data$Normal, data$Table$NewProbs)))
          lines(data$Table$Goals, data$Table$NewProbs, col = "red")

        })

        output$pvalueGeometric <- renderText({

          h0 <- ""
          if (data$ChiSquare$p.value > 0.05) {

            h0 <- "Accepting null-hypothesis for "

          } else {

            h0 <- "Rejecting null-hypothesis for "

          }

          paste0(h0, " ", input$leagueTH, " with p-value = ", round(data$ChiSquare$p.value, 2))

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

      }

    })
  
  })

  output$NegBinom <- DT::renderDataTable({

    withProgress(message = 'Calculation in progress', detail = 'This may take a while...', min = 0, max = 2, {

      if (input$leagueC != "" && input$leagueS != "" && input$leagueTH != "" && input$leagueL != "") {

        data <- NBD(country = input$leagueC, team = input$leagueTH, season = input$leagueS)

        output$pDistNegBinom <- renderPlot({

          plot(data$Table$Goals, data$Normal, type = "l", col = "blue",
               ylim = c(min(data$Normal, data$Table$NewProbs), max(data$Normal, data$Table$NewProbs)))
          lines(data$Table$Goals, data$Table$NewProbs, col = "red")

        })

        output$pvalueNegBinom <- renderText({

          h0 <- ""
          if (data$ChiSquare$p.value > 0.05) {

            h0 <- "Accepting null-hypothesis for "

          } else {

            h0 <- "Rejecting null-hypothesis for "

          }

          paste0(h0, " ", input$leagueTH, " with p-value = ", round(data$ChiSquare$p.value, 2), " with k = ", round(data$k, 2), " and p = ", round(data$p, 2), ".")

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

      }

    })
  
  })

  output$Games <- DT::renderDataTable({

    withProgress(message = 'Calculation in progress', detail = 'This may take a while...', min = 0, max = 2, {

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

  })


  output$Tables <- DT::renderDataTable({

    withProgress(message = 'Calculation in progress', detail = 'This may take a while...', min = 0, max = 2, {

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

  })

  output$vs <- DT::renderDataTable({

    withProgress(message = 'Calculation in progress', detail = 'This may take a while...', min = 0, max = 2, {

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

  })

  output$gamesHeader <- renderText({paste0("Games of ", input$leagueL, " - Season ", input$leagueS)})
  output$seasonHeader <- renderText({paste0("Table of ", input$leagueL, " - Season ", input$leagueS)})
  output$vsHeader <- renderText({paste0(input$leagueTH, " vs. ", input$leagueTA)})

  observeEvent(eventExpr = c(input$leagueC, input$leagueL, input$leagueS, input$leagueTH, input$leagueTA), {

    withProgress(message = 'Calculation in progress', detail = 'This may take a while...', min = 0, max = 2, {
    
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

        header <- paste0("Away Games of ", input$leagueTH, " ", input$leagueS)
        output$seasonHeaderAwayAway <- renderText({header})

      })

    })

  })

  output$homeHeader <- renderText({input$leagueTH})
  output$awayHeader <- renderText({input$leagueTA})

  output$homeHTML <- DT::renderDataTable({

    withProgress(message = 'Calculation in progress', detail = 'This may take a while...', min = 0, max = 2, {

      val <- round(c(
        Poisson(country = input$leagueC, team = input$leagueTH, season = input$leagueS)$ChiSquare$p.value,
        ZIP(country = input$leagueC, team = input$leagueTH, season = input$leagueS)$ChiSquare$p.value,
        Uniform(country = input$leagueC, team = input$leagueTH, season = input$leagueS)$ChiSquare,
        Geometric(country = input$leagueC, team = input$leagueTH, season = input$leagueS)$ChiSquare$p.value,
        NBD(country = input$leagueC, team = input$leagueTH, season = input$leagueS)$ChiSquare$p.value
      ), 3)
      data <- data.frame("Model" = c("Poisson Distribution", "Zero-Inflated-Poisson Distribution", "Uniform Distribution", "Geometric Distribution", "Negative-Binomial Distribution"),
                         "p-value" = val)
      datatable(data,
          rownames = FALSE,
          extensions = c('Responsive')
      )

    })

  })

  output$awayHTML <- DT::renderDataTable({

    withProgress(message = 'Calculation in progress', detail = 'This may take a while...', min = 0, max = 2, {

      val <- round(c(
        Poisson(country = input$leagueC, team = input$leagueTA, season = input$leagueS)$ChiSquare$p.value,
        ZIP(country = input$leagueC, team = input$leagueTA, season = input$leagueS)$ChiSquare$p.value,
        Uniform(country = input$leagueC, team = input$leagueTA, season = input$leagueS)$ChiSquare,
        Geometric(country = input$leagueC, team = input$leagueTA, season = input$leagueS)$ChiSquare$p.value,
        NBD(country = input$leagueC, team = input$leagueTA, season = input$leagueS)$ChiSquare$p.value
      ), 3)
      data <- data.frame("Model" = c("Poisson Distribution", "Zero-Inflated-Poisson Distribution", "Uniform Distribution", "Geometric Distribution", "Negative-Binomial Distribution"),
                         "p-value" = val)
      datatable(data,
          rownames = FALSE,
          extensions = c('Responsive')
      )

    })

  })

  output$matchingTable <- DT::renderDataTable({

    withProgress(message = 'Calculation in progress', detail = 'This may take a while...', min = 0, max = 2, {

      away <- list(
        "Poisson" = Poisson(country = input$leagueC, team = input$leagueTA, season = input$leagueS),
        "ZIP" = ZIP(country = input$leagueC, team = input$leagueTA, season = input$leagueS),
        "Uniform" = Uniform(country = input$leagueC, team = input$leagueTA, season = input$leagueS),
        "Geometric" = Geometric(country = input$leagueC, team = input$leagueTA, season = input$leagueS),
        "NBD" = NBD(country = input$leagueC, team = input$leagueTA, season = input$leagueS)
      )

      home <- list(
        "Poisson" = Poisson(country = input$leagueC, team = input$leagueTH, season = input$leagueS),
        "ZIP" = ZIP(country = input$leagueC, team = input$leagueTH, season = input$leagueS),
        "Uniform" = Uniform(country = input$leagueC, team = input$leagueTH, season = input$leagueS),
        "Geometric" = Geometric(country = input$leagueC, team = input$leagueTH, season = input$leagueS),
        "NBD" = NBD(country = input$leagueC, team = input$leagueTH, season = input$leagueS)
      )

      pHome <- 0
      pAway <- 0
      iHome <- 0
      iAway <- 0
      hVec <- c()
      aVec <- c()
      for (i in 1:length(home)) {
        hVec <- c(hVec, home[[i]]$ChiSquare$p.value)
        aVec <- c(aVec, away[[i]]$ChiSquare$p.value)
        if (home[[i]]$ChiSquare$p.value > pHome) {
          iHome <- i
          pHome <- home[[i]]$ChiSquare$p.value
        }
        if (away[[i]]$ChiSquare$p.value > pAway) {
          iAway <- i
          pAway <- away[[i]]$ChiSquare$p.value
        }
      }

      nrow <- nrow(home[[iHome]]$Table)
      ncol <- nrow(away[[iAway]]$Table)
      mat <- data.frame(matrix(0, nrow = nrow, ncol = ncol))
      hWin <- 0
      aWin <- 0
      draw <- 0
      for (i in 1:nrow) {
        for (j in 1:ncol) {
          mat[i, j] <- round(home[[iHome]]$Table$NewProbs[i]*away[[iAway]]$Table$NewProbs[j], 4)
          if (i == j) {
            draw <- draw + mat[i, j]*100
          } else if (j > i) {
            aWin <- aWin + mat[i, j]*100
          } else {
            hWin <- hWin + mat[i, j]*100
          }
        }
      }
      draw <- round(draw, 2)
      aWin <- round(aWin, 2)
      hWin <- round(hWin, 2)

      print(max(mat))
      colnames(mat) <- 0:5
      rownames(mat) <- 0:5
      mat$Probs <- round(c(NA, NA, NA, hWin, draw, aWin), 2)
      mat$Odds <- round(c(NA, NA, NA, (100/hWin)-1, (100/draw)-1, (100/aWin)-1), 2)

      # data <- data.frame("Model" = c("Poisson Distribution", "Zero-Inflated-Poisson Distribution", "Uniform Distribution", "Geometric Distribution", "Negative-Binomial Distribution"),
      #                    "Home" = hVec, "Away" = aVec, "Probs" = c(NA, NA, hWin, draw, aWin),
      #                    "Odds" = c(NA, NA, (1/hWin)-1, (1/draw)-1, (1/aWin)-1))
      # colnames(data) <- c("Model", input$leagueTH, input$leagueTA)

      datatable(mat,
          rownames = TRUE,
          extensions = c('Responsive')
      )

      # data <- MatchingTable()
      # val <- round(c(
      #   Poisson(country = input$leagueC, team = input$leagueTA, season = input$leagueS)$ChiSquare$p.value,
      #   ZIP(country = input$leagueC, team = input$leagueTA, season = input$leagueS)$ChiSquare$p.value,
      #   Uniform(country = input$leagueC, team = input$leagueTA, season = input$leagueS)$ChiSquare,
      #   Geometric(country = input$leagueC, team = input$leagueTA, season = input$leagueS)$ChiSquare$p.value,
      #   NBD(country = input$leagueC, team = input$leagueTA, season = input$leagueS)$ChiSquare$p.value
      # ), 3)
      # data <- data.frame("Model" = c("Poisson Distribution", "Zero-Inflated-Poisson Distribution", "Uniform Distribution", "Geometric Distribution", "Negative-Binomial Distribution"),
      #                    "p-value" = val)
      # datatable(data,
      #     rownames = FALSE,
      #     extensions = c('Responsive')
      # )

    })

  })

})