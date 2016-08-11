shinyServer(function(input, output, session) {

  # ranges <- reactiveValues(x = NULL, y = NULL)
  # ranges <- reactiveValues(x = NULL, y = NULL)
  # rangesP <- reactiveValues(x = NULL, y = NULL)
  # rangesZ <- reactiveValues(x = NULL, y = NULL)
  # rangesU <- reactiveValues(x = NULL, y = NULL)
  # rangesG <- reactiveValues(x = NULL, y = NULL)
  # rangesN <- reactiveValues(x = NULL, y = NULL)

  # observeEvent(input$plot_dblclick, {
  #   brush <- input$plot_brush
  #   if (!is.null(brush)) {
  #     ranges$x <- c(brush$xmin, brush$xmax)
  #     ranges$y <- c(brush$ymin, brush$ymax)
  #   } else {
  #     ranges$x <- NULL
  #     ranges$y <- NULL
  #   }
  # })
  # observeEvent(input$plot_dblclickP, {
  #   brush <- input$plot_brushP
  #   if (!is.null(brush)) {
  #     rangesP$x <- c(brush$xmin, brush$xmax)
  #     rangesP$y <- c(brush$ymin, brush$ymax)
  #   } else {
  #     rangesP$x <- NULL
  #     rangesP$y <- NULL
  #   }
  # })
  # observeEvent(input$plot_dblclickZ, {
  #   brush <- input$plot_brushZ
  #   if (!is.null(brush)) {
  #     rangesZ$x <- c(brush$xmin, brush$xmax)
  #     rangesZ$y <- c(brush$ymin, brush$ymax)
  #   } else {
  #     rangesZ$x <- NULL
  #     rangesZ$y <- NULL
  #   }
  # })
  # observeEvent(input$plot_dblclickU, {
  #   brush <- input$plot_brushU
  #   if (!is.null(brush)) {
  #     rangesU$x <- c(brush$xmin, brush$xmax)
  #     rangesU$y <- c(brush$ymin, brush$ymax)
  #   } else {
  #     rangesU$x <- NULL
  #     rangesU$y <- NULL
  #   }
  # })
  # observeEvent(input$plot_dblclickG, {
  #   brush <- input$plot_brushG
  #   if (!is.null(brush)) {
  #     rangesG$x <- c(brush$xmin, brush$xmax)
  #     rangesG$y <- c(brush$ymin, brush$ymax)
  #   } else {
  #     rangesG$x <- NULL
  #     rangesG$y <- NULL
  #   }
  # })
  # observeEvent(input$plot_dblclickN, {
  #   brush <- input$plot_brushN
  #   if (!is.null(brush)) {
  #     rangesN$x <- c(brush$xmin, brush$xmax)
  #     rangesN$y <- c(brush$ymin, brush$ymax)
  #   } else {
  #     rangesN$x <- NULL
  #     rangesN$y <- NULL
  #   }
  # })

  observeEvent(eventExpr = input$getSetting, {
    withProgress(message = 'Calculation in progress', detail = 'This may take a while...', min = 0, max = 2, {
      Updateteam(session = session, country = input$country, league = input$league, season = input$season)
    })
  })

  observeEvent(eventExpr = input$getLeagues, {
    withProgress(message = 'Calculation in progress', detail = 'This may take a while...', min = 0, max = 2, {
      Updateleague(session = session, country = input$country, league = input$league)
    })
  })

  observeEvent(eventExpr = input$getSeasons, {
    withProgress(message = 'Calculation in progress', detail = 'This may take a while...', min = 0, max = 2, {
      Updateseason(session = session, country = input$country, league = input$league, season = input$season)
    })
  })

  observeEvent(eventExpr = input$getForecasts, {
    withProgress(message = 'Calculation in progress', detail = 'This may take a while...', min = 0, max = 2, {
      output$gScored <- DT::renderDataTable({

        withProgress(message = 'Calculation in progress', detail = 'This may take a while...', min = 0, max = 2, {

          if (input$country != "" && input$season != "" && input$home != "" && input$league != "") {

            InitDB()
            query <- paste0("select A.* from Scored A, Seasons B, Teams C, Countries D where B.Season = '", input$season, "' and A.Season_ID = B.Season_ID and C.Team = '", input$team, "' and A.Team_ID = C.Team_ID and D.Country = '", input$country, "' and A.Country_ID = D.Country_ID")
            data <- dbGetQuery(conn = ppConn, statement = query)
            dbDisconnect(conn = ppConn)

            goals <- 0:5
            a <- data[1, "Uniform_A"]
            b <- data[1, "Uniform_B"]
            unif <- rep(x = 0, times = length(goals))
            unif[which(goals <= b)] <- 1/(b-a+1)

            df <- data.frame("Goals" = goals,
                             "Frequencies" = round(as.numeric(data[1, c("Zero", "One", "Two", "Three", "Four", "Five")]), 5),
                             "Poisson" = round(dpois(x = goals, lambda = data[1, "Poisson_Lambda"]), 5),
                             "Zero.Inflated.Poisson" = round(dzipois(x = goals, lambda = data[1, "ZIP_Lambda"], phi = data[1, "ZIP_Phi"]), 5),
                             "Uniform" = round(unif, 5),
                             "Geometric" = round(dgeom(x = goals, prob = data[1, "Geometric_P"]), 5),
                             "Negative.Binomial" = round(dnbinom(x = goals, mu = data[1, "NBD_K"], size = data[1, "NBD_P"]), 5),
                             "Model" = c("Poisson", "Zero-Inflated-Poisson", "Uniform", "Geometric", "Negative-Binomial", NA),
                             "Pval" = c(100*round(data[1, "Poisson_Pval"], 4), 100*round(data[1, "ZIP_Pval"], 4), 100*round(data[1, "Uniform_Pval"], 4), 100*round(data[1, "Geometric_Pval"], 4), 100*round(data[1, "NBD_Pval"], 4), NA)
            )

            output$goalScored <- renderPlot({

              ggplot(df, aes(x = Goals)) + 
                    geom_line(aes(y = Frequencies), colour = "red", size = 1.2) + 
                    geom_line(aes(y = Poisson), colour = "blue") + 
                    geom_line(aes(y = Zero.Inflated.Poisson), colour = "green") + 
                    geom_line(aes(y = Uniform), colour = "black") + 
                    geom_line(aes(y = Geometric), colour = "purple") + 
                    geom_line(aes(y = Negative.Binomial), colour = "orange") + 
                    geom_point(aes(y = Frequencies)) + 
                    geom_point(aes(y = Poisson)) + 
                    geom_point(aes(y = Zero.Inflated.Poisson)) + 
                    geom_point(aes(y = Uniform)) + 
                    geom_point(aes(y = Geometric)) + 
                    geom_point(aes(y = Negative.Binomial)) +
                    # coord_cartesian(xlim = rangesP$x, ylim = rangesP$y) + 
                    theme(axis.title.x = element_text(face="bold", colour="black", size=15), axis.title.y = element_text(face="bold", colour="black", size=15)) + 
                    ylab("Frequencies")# + 
                    # coord_cartesian(xlim = ranges$x, ylim = ranges$y)
              # ggplot(df, aes(x = Goals)) + geom_line(aes(y = Frequencies), colour = "red", size = 1.2) + geom_line(aes(y = Poisson), colour = "blue") + geom_line(aes(y = Zero.Inflated.Poisson), colour = "green") + geom_line(aes(y = Uniform), colour = "black") + geom_line(aes(y = Geometric), colour = "purple") + geom_line(aes(y = Negative.Binomial), colour = "orange") + geom_point(aes(y = Frequencies)) + geom_point(aes(y = Poisson)) + geom_point(aes(y = Zero.Inflated.Poisson)) + geom_point(aes(y = Uniform)) + geom_point(aes(y = Geometric)) + geom_point(aes(y = Negative.Binomial)) + coord_cartesian(xlim = rangesP$x, ylim = rangesP$y) + theme(axis.title.x = element_text(face="bold", colour="black", size=15), axis.title.y = element_text(face="bold", colour="black", size=15)) + ylab("Frequencies") + coord_cartesian(xlim = ranges$x, ylim = ranges$y) + scale_colour_manual(values = c("red", "blue", "green", "black", "orange", "purple"))
              # ggplot(df, aes(x = Goals)) + geom_line(aes(y = Frequencies), colour = "red", size = 1.2) + geom_line(aes(y = Poisson), colour = "blue") + geom_line(aes(y = Zero.Inflated.Poisson), colour = "green") + geom_line(aes(y = Uniform), colour = "black") + geom_line(aes(y = Geometric), colour = "purple") + geom_line(aes(y = Negative.Binomial), colour = "orange") + geom_point(aes(y = Frequencies)) + geom_point(aes(y = Poisson)) + geom_point(aes(y = Zero.Inflated.Poisson)) + geom_point(aes(y = Uniform)) + geom_point(aes(y = Geometric)) + geom_point(aes(y = Negative.Binomial)) + coord_cartesian(xlim = rangesP$x, ylim = rangesP$y) + theme(axis.title.x = element_text(face="bold", colour="black", size=15), axis.title.y = element_text(face="bold", colour="black", size=15)) + ylab("Frequencies") + coord_cartesian(xlim = ranges$x, ylim = ranges$y) + theme(legend.position = c(0, 0), legend.justification = c(0, 0))
              # ggplot(df, aes(x = Goals)) + geom_line(aes(y = Frequencies), colour = "red", size = 1.2) + geom_line(aes(y = Poisson), colour = "blue") + geom_line(aes(y = Zero.Inflated.Poisson), colour = "green") + geom_line(aes(y = Uniform), colour = "black") + geom_line(aes(y = Geometric), colour = "purple") + geom_line(aes(y = Negative.Binomial), colour = "orange") + geom_point(aes(y = Frequencies)) + geom_point(aes(y = Poisson)) + geom_point(aes(y = Zero.Inflated.Poisson)) + geom_point(aes(y = Uniform)) + geom_point(aes(y = Geometric)) + geom_point(aes(y = Negative.Binomial)) + coord_cartesian(xlim = rangesP$x, ylim = rangesP$y) + theme(axis.title.x = element_text(face="bold", colour="black", size=15), axis.title.y = element_text(face="bold", colour="black", size=15)) + ylab("Frequencies") + coord_cartesian(xlim = ranges$x, ylim = ranges$y) + guides(fill = guide_legend(title = "Distributions"))
              # ggplot(df, aes(x = Goals)) + geom_line(aes(y = Frequencies), colour = "red", size = 1.2) + geom_line(aes(y = Poisson), colour = "blue") + geom_line(aes(y = Zero.Inflated.Poisson), colour = "green") + geom_line(aes(y = Uniform), colour = "black") + geom_line(aes(y = Geometric), colour = "purple") + geom_line(aes(y = Negative.Binomial), colour = "orange") + geom_point(aes(y = Frequencies)) + geom_point(aes(y = Poisson)) + geom_point(aes(y = Zero.Inflated.Poisson)) + geom_point(aes(y = Uniform)) + geom_point(aes(y = Geometric)) + geom_point(aes(y = Negative.Binomial)) + coord_cartesian(xlim = rangesP$x, ylim = rangesP$y) + theme(axis.title.x = element_text(face="bold", colour="black", size=15), axis.title.y = element_text(face="bold", colour="black", size=15)) + ylab("Frequencies") + coord_cartesian(xlim = ranges$x, ylim = ranges$y) + scale_fill_discrete(name = "TEST")
              # ggplot(data$Table, aes(Goals, Freq/sum(Freq))) + geom_line(colour = "blue") + geom_point() + geom_line(aes(Goals, NewProbs), colour = "red") + theme(axis.title.x = element_text(face="bold", colour="black", size=15), axis.title.y = element_text(face="bold", colour="black", size=15)) + ylab("Frequencies") + coord_cartesian(xlim = rangesP$x, ylim = rangesP$y)

            })

            datatable(df[1:5, c("Model", "Pval")],
                rownames = FALSE,
                escape = FALSE
            )

          }

        })
      
      })

      output$gConceded <- DT::renderDataTable({

        withProgress(message = 'Calculation in progress', detail = 'This may take a while...', min = 0, max = 2, {

          if (input$country != "" && input$season != "" && input$home != "" && input$league != "") {

            InitDB()
            query <- paste0("select A.* from Conceded A, Seasons B, Teams C, Countries D where B.Season = '", input$season, "' and A.Season_ID = B.Season_ID and C.Team = '", input$team, "' and A.Team_ID = C.Team_ID and D.Country = '", input$country, "' and A.Country_ID = D.Country_ID")
            data <- dbGetQuery(conn = ppConn, statement = query)
            dbDisconnect(conn = ppConn)

            goals <- 0:5
            a <- data[1, "Uniform_A"]
            b <- data[1, "Uniform_B"]
            unif <- rep(x = 0, times = length(goals))
            unif[which(goals <= b)] <- 1/(b-a+1)

            df <- data.frame("Goals" = goals,
                             "Frequencies" = round(as.numeric(data[1, c("Zero", "One", "Two", "Three", "Four", "Five")]), 5),
                             "Poisson" = round(dpois(x = goals, lambda = data[1, "Poisson_Lambda"]), 5),
                             "Zero.Inflated.Poisson" = round(dzipois(x = goals, lambda = data[1, "ZIP_Lambda"], phi = data[1, "ZIP_Phi"]), 5),
                             "Uniform" = round(unif, 5),
                             "Geometric" = round(dgeom(x = goals, prob = data[1, "Geometric_P"]), 5),
                             "Negative.Binomial" = round(dnbinom(x = goals, mu = data[1, "NBD_K"], size = data[1, "NBD_P"]), 5),
                             "Model" = c("Poisson", "Zero-Inflated-Poisson", "Uniform", "Geometric", "Negative-Binomial", NA),
                             "Pval" = c(100*round(data[1, "Poisson_Pval"], 4), 100*round(data[1, "ZIP_Pval"], 4), 100*round(data[1, "Uniform_Pval"], 4), 100*round(data[1, "Geometric_Pval"], 4), 100*round(data[1, "NBD_Pval"], 4), NA)
            )

            output$goalConceded <- renderPlot({

              ggplot(df, aes(x = Goals)) + 
                    geom_line(aes(y = Frequencies), colour = "red", size = 1.2) + 
                    geom_line(aes(y = Poisson), colour = "blue") + 
                    geom_line(aes(y = Zero.Inflated.Poisson), colour = "green") + 
                    geom_line(aes(y = Uniform), colour = "black") + 
                    geom_line(aes(y = Geometric), colour = "purple") + 
                    geom_line(aes(y = Negative.Binomial), colour = "orange") + 
                    geom_point(aes(y = Frequencies)) + geom_point(aes(y = Poisson)) + 
                    geom_point(aes(y = Zero.Inflated.Poisson)) + 
                    geom_point(aes(y = Uniform)) + 
                    geom_point(aes(y = Geometric)) + 
                    geom_point(aes(y = Negative.Binomial)) + 
                    # coord_cartesian(xlim = rangesP$x, ylim = rangesP$y) + 
                    theme(axis.title.x = element_text(face="bold", colour="black", size=15), axis.title.y = element_text(face="bold", colour="black", size=15)) + 
                    ylab("Frequencies")# + 
                    # coord_cartesian(xlim = ranges$x, ylim = ranges$y)

            })

            datatable(df[1:5, c("Model", "Pval")],
                rownames = FALSE,
                escape = FALSE
            )

          }

        })
      
      })
    })
  })

  # output$pPval <- DT::renderDataTable({

  #   withProgress(message = 'Calculation in progress', detail = 'This may take a while...', min = 0, max = 2, {

  #     if (input$country != "" && input$season != "" && input$home != "" && input$league != "") {

  #       InitDB()
  #       query <- paste0("select A.Dist, round(A.Zero, 2), round(A.One, 2), round(A.Two, 2), round(A.Three, 2), round(A.Four, 2), round(A.Five, 2) from Scored A, Teams C, Countries D where C.Team = '", input$home, "' and A.Team_ID = C.Team_ID and D.Country = '", input$country, "' and A.Country_ID = D.Country_ID")
  #       data <- dbGetQuery(conn = ppConn, statement = query)
  #       dbDisconnect(conn = ppConn)
  #       data <- melt(data, id.vars = "Dist")
  #       data$variable <- as.numeric(data$variable)

  #       output$poissonPval <- renderPlot({
  #         ggplot(data, aes(x = variable, y = value, color = Dist)) + geom_point() + geom_smooth() + coord_cartesian(xlim = ranges$x, ylim = ranges$y)
  #       })

  #       datatable(data,
  #           rownames = FALSE,
  #           escape = FALSE
  #       )

  #     }

  #   })
  
  # })

  output$predictScored <- DT::renderDataTable({

    withProgress(message = 'Calculation in progress', detail = 'This may take a while...', min = 0, max = 2, {

      InitDB()
      home <- paste0("select A.Dist from Scored A, Seasons B, Teams C, Countries D where B.Season = '", input$season, "' and A.Season_ID = B.Season_ID and C.Team = '", input$home, "' and A.Team_ID = C.Team_ID and D.Country = '", input$country, "' and A.Country_ID = D.Country_ID")
      home <- as.character(dbGetQuery(conn = ppConn, statement = home)[1, "Dist"])
      hStr <- strsplit(home, split = "_")[[1]]
      hProb <- paste0(hStr, "_Pval")
      home <- paste0("select ", home, ", ", hProb, " from Scored")
      home <- as.character(dbGetQuery(conn = ppConn, statement = home)[1, home])
      away <- paste0("select A.Dist from Scored A, Seasons B, Teams C, Countries D where B.Season = '", input$season, "' and A.Season_ID = B.Season_ID and C.Team = '", input$away, "' and A.Team_ID = C.Team_ID and D.Country = '", input$country, "' and A.Country_ID = D.Country_ID")
      away <- as.character(dbGetQuery(conn = ppConn, statement = away)[1, "Dist"])
      aStr <- strsplit(away, split = "_")[[1]]
      aProb <- paste0(aStr, "_Pval")
      away <- paste0("select ", away, ", ", aProb, " from Scored")
      away <- as.character(dbGetQuery(conn = ppConn, statement = away)[1, away])
      dbDisconnect(conn = ppConn)

      goals <- 0:5

      probs <- c()
      if (hProb == "Poisson") {
        probs <- dpois(x = goals, lambda = paste0(hProb, ""))
      }

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

      colnames(mat) <- 0:5
      rownames(mat) <- 0:5

      med <- (max(mat) - min(mat))/2

      for (i in 1:nrow) {
        for (j in 1:ncol) {
          if (mat[i,j] > med) {
            mat[i,j] <- paste0('<font style="background-color:red;color:white;">', mat[i,j], '</font>')
          } else {
            mat[i,j] <- paste0('<font style="background-color:#3c8dbc;color:white;">', mat[i,j], '</font>')
          }
        }
      }

      homeOdd <- if (round((100/hWin)-1, 2) < 1) {1.05} else {round((100/hWin)-1, 2)}
      drawOdd <- if (round((100/draw)-1, 2) < 1) {1.05} else {round((100/draw)-1, 2)}
      awayOdd <- if (round((100/aWin)-1, 2) < 1) {1.05} else {round((100/aWin)-1, 2)}

      mat$Probs <- c("Home Win", hWin, "Draw", draw, "Away Win", aWin)
      mat$Odds <- c("Home Win", homeOdd, "Draw", drawOdd, "Away Win", awayOdd)

      datatable(mat,
          rownames = TRUE,
          escape = FALSE,
          extensions = c('Responsive')
      )

    })

  })

  # output$Poisson <- DT::renderDataTable({

  #   withProgress(message = 'Calculation in progress', detail = 'This may take a while...', min = 0, max = 2, {

  #     if (input$country != "" && input$season != "" && input$home != "" && input$league != "") {

  #       data <- Poisson(country = input$country, team = input$home, season = input$season)

  #       output$pDistPoisson <- renderPlot({

  #         ggplot(data$Table, aes(Goals, Freq/sum(Freq))) + geom_line(colour = "blue") + geom_point() + geom_line(aes(Goals, NewProbs), colour = "red") + theme(axis.title.x = element_text(face="bold", colour="black", size=15), axis.title.y = element_text(face="bold", colour="black", size=15)) + ylab("Frequencies") + coord_cartesian(xlim = ranges$x, ylim = ranges$y)

  #       })

  #       output$pvaluePoisson <- renderText({

  #         h0 <- ""
  #         if (data$ChiSquare$p.value > 0.05) {

  #           h0 <- "Accepting null-hypothesis for "

  #         } else {

  #           h0 <- "Rejecting null-hypothesis for "

  #         }

  #         paste0(h0, " ", input$home, " with p-value = ", round(data$ChiSquare$p.value, 2))

  #       })

  #       datatable(data$Table,
  #           rownames = FALSE, escape = FALSE,
  #           extensions = c('ColReorder', 'ColVis', 'Responsive'),
  #           options = list(pageLength = -1,
  #               lengthMenu = list(c(-1, 50, 100), list('All', '50', '150')),
  #               deferRender = TRUE, colVis = list(exclude = c(0, 1), activate = 'mouseover'),
  #               searchHighlight = TRUE,
  #               dom = 'TRCSlfrtip<"clear">',
  #               colReorder = list(realtime = TRUE)
  #           )
  #       )

  #     }

  #   })
  
  # })

  # output$ZIP <- DT::renderDataTable({

  #   withProgress(message = 'Calculation in progress', detail = 'This may take a while...', min = 0, max = 2, {

  #     if (input$country != "" && input$season != "" && input$home != "" && input$league != "") {

  #       data <- ZIP(country = input$country, team = input$home, season = input$season)

  #       output$pDistZIP <- renderPlot({

  #         ggplot(data$Table, aes(Goals, Freq/sum(Freq))) + geom_line(colour = "blue") + geom_point() + geom_line(aes(Goals, NewProbs), colour = "red") + theme(axis.title.x = element_text(face="bold", colour="black", size=15), axis.title.y = element_text(face="bold", colour="black", size=15)) + ylab("Frequencies") + coord_cartesian(xlim = rangesZ$x, ylim = rangesZ$y)

  #       })

  #       output$pvalueZIP <- renderText({

  #         h0 <- ""
  #         if (data$ChiSquare$p.value > 0.05) {

  #           h0 <- "Accepting null-hypothesis for "

  #         } else {

  #           h0 <- "Rejecting null-hypothesis for "

  #         }

  #         paste0(h0, " ", input$home, " with p-value = ", round(data$ChiSquare$p.value, 2), " with lambda = ", round(data$lambda, 2), " and phi = ", round(data$phi, 2), ".")

  #       })

  #       datatable(data$Table,
  #           rownames = FALSE, escape = FALSE,
  #           extensions = c('ColReorder', 'ColVis', 'Responsive'),
  #           options = list(pageLength = -1,
  #               lengthMenu = list(c(-1, 50, 100), list('All', '50', '150')),
  #               deferRender = TRUE, colVis = list(exclude = c(0, 1), activate = 'mouseover'),
  #               searchHighlight = TRUE,
  #               dom = 'TRCSlfrtip<"clear">',
  #               colReorder = list(realtime = TRUE)
  #           )
  #       )

  #     }

  #   })
  
  # })

  # output$Uniform <- DT::renderDataTable({

  #   withProgress(message = 'Calculation in progress', detail = 'This may take a while...', min = 0, max = 2, {

  #     if (input$country != "" && input$season != "" && input$home != "" && input$league != "") {

  #       data <- Uniform(country = input$country, team = input$home, season = input$season)

  #       output$pDistUniform <- renderPlot({

  #         ggplot(data$Table, aes(Goals, Freq/sum(Freq))) + geom_line(colour = "blue") + geom_point() + geom_line(aes(Goals, NewProbs), colour = "red") + theme(axis.title.x = element_text(face="bold", colour="black", size=15), axis.title.y = element_text(face="bold", colour="black", size=15)) + ylab("Frequencies") + coord_cartesian(xlim = rangesU$x, ylim = rangesU$y)

  #       })

  #       output$pvalueUniform <- renderText({

  #         h0 <- ""
  #         if (data$ChiSquare$p.value > 0.05) {

  #           h0 <- "Accepting null-hypothesis for "

  #         } else {

  #           h0 <- "Rejecting null-hypothesis for "

  #         }

  #         paste0(h0, " ", input$home, " with p-value = ", round(data$ChiSquare$p.value, 2), " and a = ", data$a, ", b = ", data$b)

  #       })

  #       datatable(data$Table,
  #           rownames = FALSE, escape = FALSE,
  #           extensions = c('ColReorder', 'ColVis', 'Responsive'),
  #           options = list(pageLength = -1,
  #               lengthMenu = list(c(-1, 50, 100), list('All', '50', '150')),
  #               deferRender = TRUE, colVis = list(exclude = c(0, 1), activate = 'mouseover'),
  #               searchHighlight = TRUE,
  #               dom = 'TRCSlfrtip<"clear">',
  #               colReorder = list(realtime = TRUE)
  #           )
  #       )

  #     }

  #   })
  
  # })

  # output$Geometric <- DT::renderDataTable({

  #   withProgress(message = 'Calculation in progress', detail = 'This may take a while...', min = 0, max = 2, {

  #     if (input$country != "" && input$season != "" && input$home != "" && input$league != "") {

  #       data <- Geometric(country = input$country, team = input$home, season = input$season)

  #       output$pDistGeometric <- renderPlot({

  #         ggplot(data$Table, aes(Goals, Freq/sum(Freq))) + geom_line(colour = "blue") + geom_point() + geom_line(aes(Goals, NewProbs), colour = "red") + theme(axis.title.x = element_text(face="bold", colour="black", size=15), axis.title.y = element_text(face="bold", colour="black", size=15)) + ylab("Frequencies") + coord_cartesian(xlim = rangesG$x, ylim = rangesG$y)

  #       })

  #       output$pvalueGeometric <- renderText({

  #         h0 <- ""
  #         if (data$ChiSquare$p.value > 0.05) {

  #           h0 <- "Accepting null-hypothesis for "

  #         } else {

  #           h0 <- "Rejecting null-hypothesis for "

  #         }

  #         paste0(h0, " ", input$home, " with p-value = ", round(data$ChiSquare$p.value, 2))

  #       })

  #       datatable(data$Table,
  #           rownames = FALSE, escape = FALSE,
  #           extensions = c('ColReorder', 'ColVis', 'Responsive'),
  #           options = list(pageLength = -1,
  #               lengthMenu = list(c(-1, 50, 100), list('All', '50', '150')),
  #               deferRender = TRUE, colVis = list(exclude = c(0, 1), activate = 'mouseover'),
  #               searchHighlight = TRUE,
  #               dom = 'TRCSlfrtip<"clear">',
  #               colReorder = list(realtime = TRUE)
  #           )
  #       )

  #     }

  #   })
  
  # })

  # output$NegBinom <- DT::renderDataTable({

  #   withProgress(message = 'Calculation in progress', detail = 'This may take a while...', min = 0, max = 2, {

  #     if (input$country != "" && input$season != "" && input$home != "" && input$league != "") {

  #       data <- NBD(country = input$country, team = input$home, season = input$season)

  #       output$pDistNegBinom <- renderPlot({

  #         ggplot(data$Table, aes(Goals, Freq/sum(Freq))) + geom_line(colour = "blue") + geom_point() + geom_line(aes(Goals, NewProbs), colour = "red") + theme(axis.title.x = element_text(face="bold", colour="black", size=15), axis.title.y = element_text(face="bold", colour="black", size=15)) + ylab("Frequencies") + coord_cartesian(xlim = rangesN$x, ylim = rangesN$y)

  #       })

  #       output$pvalueNegBinom <- renderText({

  #         h0 <- ""
  #         if (data$ChiSquare$p.value > 0.05) {

  #           h0 <- "Accepting null-hypothesis for "

  #         } else {

  #           h0 <- "Rejecting null-hypothesis for "

  #         }

  #         paste0(h0, " ", input$home, " with p-value = ", round(data$ChiSquare$p.value, 2), " with k = ", round(data$k, 2), " and p = ", round(data$p, 2), ".")

  #       })

  #       datatable(data$Table,
  #           rownames = FALSE, escape = FALSE,
  #           extensions = c('ColReorder', 'ColVis', 'Responsive'),
  #           options = list(pageLength = -1,
  #               lengthMenu = list(c(-1, 50, 100), list('All', '50', '150')),
  #               deferRender = TRUE, colVis = list(exclude = c(0, 1), activate = 'mouseover'),
  #               searchHighlight = TRUE,
  #               dom = 'TRCSlfrtip<"clear">',
  #               colReorder = list(realtime = TRUE)
  #           )
  #       )

  #     }

  #   })
  
  # })

  # output$PoissonA <- DT::renderDataTable({

  #   withProgress(message = 'Calculation in progress', detail = 'This may take a while...', min = 0, max = 2, {

  #     if (input$country != "" && input$season != "" && input$home != "" && input$league != "") {

  #       data <- Poisson(country = input$country, team = input$home, season = input$season, against = TRUE)

  #       output$pDistPoissonA <- renderPlot({

  #         plot(data$Table$Goals, data$Normal, type = "l", col = "blue",
  #              ylim = c(min(data$Normal, data$Table$NewProbs), max(data$Normal, data$Table$NewProbs)))
  #         lines(data$Table$Goals, data$Table$NewProbs, col = "red")

  #       })

  #       output$pvaluePoissonA <- renderText({

  #         h0 <- ""
  #         if (data$ChiSquare$p.value > 0.05) {

  #           h0 <- "Accepting null-hypothesis for "

  #         } else {

  #           h0 <- "Rejecting null-hypothesis for "

  #         }

  #         paste0(h0, " ", input$home, " with p-value = ", round(data$ChiSquare$p.value, 2))

  #       })

  #       datatable(data$Table,
  #           rownames = FALSE, escape = FALSE,
  #           extensions = c('ColReorder', 'ColVis', 'Responsive'),
  #           options = list(pageLength = -1,
  #               lengthMenu = list(c(-1, 50, 100), list('All', '50', '150')),
  #               deferRender = TRUE, colVis = list(exclude = c(0, 1), activate = 'mouseover'),
  #               searchHighlight = TRUE,
  #               dom = 'TRCSlfrtip<"clear">',
  #               colReorder = list(realtime = TRUE)
  #           )
  #       )

  #     }

  #   })
  
  # })

  # output$ZIPA <- DT::renderDataTable({

  #   withProgress(message = 'Calculation in progress', detail = 'This may take a while...', min = 0, max = 2, {

  #     if (input$country != "" && input$season != "" && input$home != "" && input$league != "") {

  #       data <- ZIP(country = input$country, team = input$home, season = input$season, against = TRUE)

  #       output$pDistZIPA <- renderPlot({

  #         plot(data$Table$Goals, data$Normal, type = "l", col = "blue",
  #              ylim = c(min(data$Normal, data$Table$NewProbs), max(data$Normal, data$Table$NewProbs)))
  #         lines(data$Table$Goals, data$Table$NewProbs, col = "red")

  #       })

  #       output$pvalueZIPA <- renderText({

  #         h0 <- ""
  #         if (data$ChiSquare$p.value > 0.05) {

  #           h0 <- "Accepting null-hypothesis for "

  #         } else {

  #           h0 <- "Rejecting null-hypothesis for "

  #         }

  #         paste0(h0, " ", input$home, " with p-value = ", round(data$ChiSquare$p.value, 2), " with lambda = ", round(data$lambda, 2), " and phi = ", round(data$phi, 2), ".")

  #       })

  #       datatable(data$Table,
  #           rownames = FALSE, escape = FALSE,
  #           extensions = c('ColReorder', 'ColVis', 'Responsive'),
  #           options = list(pageLength = -1,
  #               lengthMenu = list(c(-1, 50, 100), list('All', '50', '150')),
  #               deferRender = TRUE, colVis = list(exclude = c(0, 1), activate = 'mouseover'),
  #               searchHighlight = TRUE,
  #               dom = 'TRCSlfrtip<"clear">',
  #               colReorder = list(realtime = TRUE)
  #           )
  #       )

  #     }

  #   })
  
  # })

  # output$UniformA <- DT::renderDataTable({

  #   withProgress(message = 'Calculation in progress', detail = 'This may take a while...', min = 0, max = 2, {

  #     if (input$country != "" && input$season != "" && input$home != "" && input$league != "") {

  #       data <- Uniform(country = input$country, team = input$home, season = input$season, against = TRUE)

  #       output$pDistUniformA <- renderPlot({

  #         plot(data$Table$Goals, data$Normal, type = "l", col = "blue",
  #              ylim = c(min(data$Normal, data$Table$NewProbs), max(data$Normal, data$Table$NewProbs)))
  #         lines(data$Table$Goals, data$Table$NewProbs, col = "red")

  #       })

  #       output$pvalueUniformA <- renderText({

  #         h0 <- ""
  #         if (data$ChiSquare$p.value > 0.05) {

  #           h0 <- "Accepting null-hypothesis for "

  #         } else {

  #           h0 <- "Rejecting null-hypothesis for "

  #         }

  #         paste0(h0, " ", input$home, " with p-value = ", round(data$ChiSquare$p.value, 2), " and a = ", data$a, ", b = ", data$b)

  #       })

  #       datatable(data$Table,
  #           rownames = FALSE, escape = FALSE,
  #           extensions = c('ColReorder', 'ColVis', 'Responsive'),
  #           options = list(pageLength = -1,
  #               lengthMenu = list(c(-1, 50, 100), list('All', '50', '150')),
  #               deferRender = TRUE, colVis = list(exclude = c(0, 1), activate = 'mouseover'),
  #               searchHighlight = TRUE,
  #               dom = 'TRCSlfrtip<"clear">',
  #               colReorder = list(realtime = TRUE)
  #           )
  #       )

  #     }

  #   })
  
  # })

  # output$GeometricA <- DT::renderDataTable({

  #   withProgress(message = 'Calculation in progress', detail = 'This may take a while...', min = 0, max = 2, {

  #     if (input$country != "" && input$season != "" && input$home != "" && input$league != "") {

  #       data <- Geometric(country = input$country, team = input$home, season = input$season, against = TRUE)

  #       output$pDistGeometricA <- renderPlot({

  #         plot(data$Table$Goals, data$Normal, type = "l", col = "blue",
  #              ylim = c(min(data$Normal, data$Table$NewProbs), max(data$Normal, data$Table$NewProbs)))
  #         lines(data$Table$Goals, data$Table$NewProbs, col = "red")

  #       })

  #       output$pvalueGeometricA <- renderText({

  #         h0 <- ""
  #         if (data$ChiSquare$p.value > 0.05) {

  #           h0 <- "Accepting null-hypothesis for "

  #         } else {

  #           h0 <- "Rejecting null-hypothesis for "

  #         }

  #         paste0(h0, " ", input$home, " with p-value = ", round(data$ChiSquare$p.value, 2))

  #       })

  #       datatable(data$Table,
  #           rownames = FALSE, escape = FALSE,
  #           extensions = c('ColReorder', 'ColVis', 'Responsive'),
  #           options = list(pageLength = -1,
  #               lengthMenu = list(c(-1, 50, 100), list('All', '50', '150')),
  #               deferRender = TRUE, colVis = list(exclude = c(0, 1), activate = 'mouseover'),
  #               searchHighlight = TRUE,
  #               dom = 'TRCSlfrtip<"clear">',
  #               colReorder = list(realtime = TRUE)
  #           )
  #       )

  #     }

  #   })
  
  # })

  # output$NegBinomA <- DT::renderDataTable({

  #   withProgress(message = 'Calculation in progress', detail = 'This may take a while...', min = 0, max = 2, {

  #     if (input$country != "" && input$season != "" && input$home != "" && input$league != "") {

  #       data <- NBD(country = input$country, team = input$home, season = input$season, against = TRUE)

  #       output$pDistNegBinomA <- renderPlot({

  #         plot(data$Table$Goals, data$Normal, type = "l", col = "blue",
  #              ylim = c(min(data$Normal, data$Table$NewProbs), max(data$Normal, data$Table$NewProbs)))
  #         lines(data$Table$Goals, data$Table$NewProbs, col = "red")

  #       })

  #       output$pvalueNegBinomA <- renderText({

  #         h0 <- ""
  #         if (data$ChiSquare$p.value > 0.05) {

  #           h0 <- "Accepting null-hypothesis for "

  #         } else {

  #           h0 <- "Rejecting null-hypothesis for "

  #         }

  #         paste0(h0, " ", input$home, " with p-value = ", round(data$ChiSquare$p.value, 2), " with k = ", round(data$k, 2), " and p = ", round(data$p, 2), ".")

  #       })

  #       datatable(data$Table,
  #           rownames = FALSE, escape = FALSE,
  #           extensions = c('ColReorder', 'ColVis', 'Responsive'),
  #           options = list(pageLength = -1,
  #               lengthMenu = list(c(-1, 50, 100), list('All', '50', '150')),
  #               deferRender = TRUE, colVis = list(exclude = c(0, 1), activate = 'mouseover'),
  #               searchHighlight = TRUE,
  #               dom = 'TRCSlfrtip<"clear">',
  #               colReorder = list(realtime = TRUE)
  #           )
  #       )

  #     }

  #   })
  
  # })

  observeEvent(eventExpr = input$getVS, {
    withProgress(message = 'Calculation in progress', detail = 'This may take a while...', min = 0, max = 2, {

      output$gamesHeader <- renderText({paste0("Games of ", input$league, " - Season ", input$season)})
      output$seasonHeader <- renderText({paste0("Table of ", input$league, " - Season ", input$season)})
      output$vsHeader <- renderText({paste0(input$home, " vs. ", input$away)})

      output$Games <- DT::renderDataTable({

        withProgress(message = 'Calculation in progress', detail = 'This may take a while...', min = 0, max = 2, {

          data <- GetLeagueTable(country = input$country, league = input$league, season = input$season)

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

          data <- GetLeagueTable(country = input$country, league = input$league, season = input$season)
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

          data <- GetHomeVsAway(country = input$country, home = input$home, away = input$away)

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

      WinDrawLost(output = output, team = input$home, season = input$season, country = input$country,
                  stat = "All", plot = "allGamesOfHomeStat", head = "allGamesOfHome")
      WinDrawLost(output = output, team = input$home, season = input$season, country = input$country,
                        stat = "Home", plot = "homeGamesOfHomeStat", head = "homeGamesOfHome")
      WinDrawLost(output = output, team = input$home, season = input$season, country = input$country,
                        stat = "Away", plot = "awayGamesOfHomeStat", head = "awayGamesOfHome")
      WinDrawLost(output = output, team = input$away, season = input$season, country = input$country,
                        stat = "All", plot = "allGamesOfAwayStat", head = "allGamesOfAway")
      WinDrawLost(output = output, team = input$away, season = input$season, country = input$country,
                        stat = "Home", plot = "homeGamesOfAwayStat", head = "homeGamesOfAway")
      WinDrawLost(output = output, team = input$away, season = input$season, country = input$country,
                        stat = "Away", plot = "awayGamesOfAwayStat", head = "awayGamesOfAway")

    })
  })

  # observeEvent(eventExpr = input$home, {
  #   withProgress(message = 'Calculation in progress', detail = 'This may take a while...', min = 0, max = 2, {
  #     WinDrawLost(output = output, team = input$home, season = input$season, country = input$country,
  #                 stat = "All", plot = "allGamesOfHomeStat", head = "allGamesOfHome")
  #   })
  # })

  # observeEvent(eventExpr = input$home, {
  #   withProgress(message = 'Calculation in progress', detail = 'This may take a while...', min = 0, max = 2, {
  #     WinDrawLost(output = output, team = input$home, season = input$season, country = input$country,
  #                 stat = "Home", plot = "homeGamesOfHomeStat", head = "homeGamesOfHome")
  #   })
  # })

  # observeEvent(eventExpr = input$home, {
  #   withProgress(message = 'Calculation in progress', detail = 'This may take a while...', min = 0, max = 2, {
  #     WinDrawLost(output = output, team = input$home, season = input$season, country = input$country,
  #                 stat = "Away", plot = "awayGamesOfHomeStat", head = "awayGamesOfHome")
  #   })
  # })

  # observeEvent(eventExpr = input$away, {
  #   withProgress(message = 'Calculation in progress', detail = 'This may take a while...', min = 0, max = 2, {
  #     WinDrawLost(output = output, team = input$away, season = input$season, country = input$country,
  #                 stat = "All", plot = "allGamesOfAwayStat", head = "allGamesOfAway")
  #   })

  # })

  # observeEvent(eventExpr = input$away, {
  #   withProgress(message = 'Calculation in progress', detail = 'This may take a while...', min = 0, max = 2, {
  #     WinDrawLost(output = output, team = input$away, season = input$season, country = input$country,
  #                 stat = "Home", plot = "homeGamesOfAwayStat", head = "homeGamesOfAway")
  #   })
  # })
  # observeEvent(eventExpr = input$away, {
  #   withProgress(message = 'Calculation in progress', detail = 'This may take a while...', min = 0, max = 2, {
  #     WinDrawLost(output = output, team = input$away, season = input$season, country = input$country,
  #                 stat = "Away", plot = "awayGamesOfAwayStat", head = "awayGamesOfAway")
  #   })
  # })

  # output$homeHTML <- DT::renderDataTable({

  #   withProgress(message = 'Calculation in progress', detail = 'This may take a while...', min = 0, max = 2, {

  #     val <- round(c(
  #       Poisson(country = input$country, team = input$home, season = input$season)$ChiSquare$p.value,
  #       ZIP(country = input$country, team = input$home, season = input$season)$ChiSquare$p.value,
  #       Uniform(country = input$country, team = input$home, season = input$season)$ChiSquare,
  #       Geometric(country = input$country, team = input$home, season = input$season)$ChiSquare$p.value,
  #       NBD(country = input$country, team = input$home, season = input$season)$ChiSquare$p.value
  #     ), 3)
  #     data <- data.frame("Model" = c("Poisson Distribution", "Zero-Inflated-Poisson Distribution", "Uniform Distribution", "Geometric Distribution", "Negative-Binomial Distribution"),
  #                        "p-value" = val)
  #     datatable(data,
  #         rownames = FALSE,
  #         extensions = c('Responsive')
  #     )

  #   })

  # })

  # output$awayHTML <- DT::renderDataTable({

  #   withProgress(message = 'Calculation in progress', detail = 'This may take a while...', min = 0, max = 2, {

  #     val <- round(c(
  #       Poisson(country = input$country, team = input$away, season = input$season)$ChiSquare$p.value,
  #       ZIP(country = input$country, team = input$away, season = input$season)$ChiSquare$p.value,
  #       Uniform(country = input$country, team = input$away, season = input$season)$ChiSquare,
  #       Geometric(country = input$country, team = input$away, season = input$season)$ChiSquare$p.value,
  #       NBD(country = input$country, team = input$away, season = input$season)$ChiSquare$p.value
  #     ), 3)
  #     data <- data.frame("Model" = c("Poisson Distribution", "Zero-Inflated-Poisson Distribution", "Uniform Distribution", "Geometric Distribution", "Negative-Binomial Distribution"),
  #                        "p-value" = val)
  #     datatable(data,
  #         rownames = FALSE,
  #         extensions = c('Responsive')
  #     )

  #   })

  # })

  # output$matchingTable <- DT::renderDataTable({

  #   withProgress(message = 'Calculation in progress', detail = 'This may take a while...', min = 0, max = 2, {

  #     away <- list(
  #       "Poisson" = Poisson(country = input$country, team = input$away, season = input$season),
  #       "ZIP" = ZIP(country = input$country, team = input$away, season = input$season),
  #       "Uniform" = Uniform(country = input$country, team = input$away, season = input$season),
  #       "Geometric" = Geometric(country = input$country, team = input$away, season = input$season),
  #       "NBD" = NBD(country = input$country, team = input$away, season = input$season)
  #     )

  #     home <- list(
  #       "Poisson" = Poisson(country = input$country, team = input$home, season = input$season),
  #       "ZIP" = ZIP(country = input$country, team = input$home, season = input$season),
  #       "Uniform" = Uniform(country = input$country, team = input$home, season = input$season),
  #       "Geometric" = Geometric(country = input$country, team = input$home, season = input$season),
  #       "NBD" = NBD(country = input$country, team = input$home, season = input$season)
  #     )

  #     pHome <- 0
  #     pAway <- 0
  #     iHome <- 0
  #     iAway <- 0
  #     hVec <- c()
  #     aVec <- c()
  #     for (i in 1:length(home)) {
  #       hVec <- c(hVec, home[[i]]$ChiSquare$p.value)
  #       aVec <- c(aVec, away[[i]]$ChiSquare$p.value)
  #       if (home[[i]]$ChiSquare$p.value > pHome) {
  #         iHome <- i
  #         pHome <- home[[i]]$ChiSquare$p.value
  #       }
  #       if (away[[i]]$ChiSquare$p.value > pAway) {
  #         iAway <- i
  #         pAway <- away[[i]]$ChiSquare$p.value
  #       }
  #     }

  #     nrow <- nrow(home[[iHome]]$Table)
  #     ncol <- nrow(away[[iAway]]$Table)
  #     mat <- data.frame(matrix(0, nrow = nrow, ncol = ncol))
  #     hWin <- 0
  #     aWin <- 0
  #     draw <- 0
  #     for (i in 1:nrow) {
  #       for (j in 1:ncol) {
  #         mat[i, j] <- round(home[[iHome]]$Table$NewProbs[i]*away[[iAway]]$Table$NewProbs[j], 4)
  #         if (i == j) {
  #           draw <- draw + mat[i, j]*100
  #         } else if (j > i) {
  #           aWin <- aWin + mat[i, j]*100
  #         } else {
  #           hWin <- hWin + mat[i, j]*100
  #         }
  #       }
  #     }

  #     colnames(mat) <- 0:5
  #     rownames(mat) <- 0:5

  #     med <- (max(mat) - min(mat))/2

  #     for (i in 1:nrow) {
  #       for (j in 1:ncol) {
  #         if (mat[i,j] > med) {
  #           mat[i,j] <- paste0('<font style="background-color:red;color:white;">', mat[i,j], '</font>')
  #         } else {
  #           mat[i,j] <- paste0('<font style="background-color:#3c8dbc;color:white;">', mat[i,j], '</font>')
  #         }
  #       }
  #     }

  #     homeOdd <- if (round((100/hWin)-1, 2) < 1) {1.05} else {round((100/hWin)-1, 2)}
  #     drawOdd <- if (round((100/draw)-1, 2) < 1) {1.05} else {round((100/draw)-1, 2)}
  #     awayOdd <- if (round((100/aWin)-1, 2) < 1) {1.05} else {round((100/aWin)-1, 2)}

  #     mat$Probs <- c("Home Win", hWin, "Draw", draw, "Away Win", aWin)
  #     mat$Odds <- c("Home Win", homeOdd, "Draw", drawOdd, "Away Win", awayOdd)

  #     datatable(mat,
  #         rownames = TRUE,
  #         escape = FALSE,
  #         extensions = c('Responsive')
  #     )

  #   })

  # })

  # output$matchingTableA <- DT::renderDataTable({

  #   withProgress(message = 'Calculation in progress', detail = 'This may take a while...', min = 0, max = 2, {

  #     away <- list(
  #       "Poisson" = Poisson(country = input$country, team = input$away, season = input$season, against = TRUE),
  #       "ZIP" = ZIP(country = input$country, team = input$away, season = input$season, against = TRUE),
  #       "Uniform" = Uniform(country = input$country, team = input$away, season = input$season, against = TRUE),
  #       "Geometric" = Geometric(country = input$country, team = input$away, season = input$season, against = TRUE),
  #       "NBD" = NBD(country = input$country, team = input$away, season = input$season, against = TRUE)
  #     )

  #     home <- list(
  #       "Poisson" = Poisson(country = input$country, team = input$home, season = input$season, against = TRUE),
  #       "ZIP" = ZIP(country = input$country, team = input$home, season = input$season, against = TRUE),
  #       "Uniform" = Uniform(country = input$country, team = input$home, season = input$season, against = TRUE),
  #       "Geometric" = Geometric(country = input$country, team = input$home, season = input$season, against = TRUE),
  #       "NBD" = NBD(country = input$country, team = input$home, season = input$season, against = TRUE)
  #     )

  #     pHome <- 0
  #     pAway <- 0
  #     iHome <- 0
  #     iAway <- 0
  #     hVec <- c()
  #     aVec <- c()
  #     for (i in 1:length(home)) {
  #       hVec <- c(hVec, home[[i]]$ChiSquare$p.value)
  #       aVec <- c(aVec, away[[i]]$ChiSquare$p.value)
  #       if (home[[i]]$ChiSquare$p.value > pHome) {
  #         iHome <- i
  #         pHome <- home[[i]]$ChiSquare$p.value
  #       }
  #       if (away[[i]]$ChiSquare$p.value > pAway) {
  #         iAway <- i
  #         pAway <- away[[i]]$ChiSquare$p.value
  #       }
  #     }

  #     nrow <- nrow(home[[iHome]]$Table)
  #     ncol <- nrow(away[[iAway]]$Table)
  #     mat <- data.frame(matrix(0, nrow = nrow, ncol = ncol))
  #     hLoose <- 0
  #     aLoose <- 0
  #     draw <- 0
  #     for (i in 1:nrow) {
  #       for (j in 1:ncol) {
  #         mat[i, j] <- round(home[[iHome]]$Table$NewProbs[i]*away[[iAway]]$Table$NewProbs[j], 4)
  #         if (i == j) {
  #           draw <- draw + mat[i, j]*100
  #         } else if (j > i) {
  #           aLoose <- aLoose + mat[i, j]*100
  #         } else {
  #           hLoose <- hLoose + mat[i, j]*100
  #         }
  #       }
  #     }

  #     colnames(mat) <- 0:5
  #     rownames(mat) <- 0:5

  #     med <- (max(mat) - min(mat))/2

  #     for (i in 1:nrow) {
  #       for (j in 1:ncol) {
  #         if (mat[i,j] > med) {
  #           mat[i,j] <- paste0('<font style="background-color:red;color:white;">', mat[i,j], '</font>')
  #         } else {
  #           mat[i,j] <- paste0('<font style="background-color:#3c8dbc;color:white;">', mat[i,j], '</font>')
  #         }
  #       }
  #     }

  #     homeOdd <- if (round((100/hLoose)-1, 2) < 1) {1.05} else {round((100/hLoose)-1, 2)}
  #     drawOdd <- if (round((100/draw)-1, 2) < 1) {1.05} else {round((100/draw)-1, 2)}
  #     awayOdd <- if (round((100/aLoose)-1, 2) < 1) {1.05} else {round((100/aLoose)-1, 2)}

  #     mat$Probs <- c("Home Loose", hLoose, "Draw", draw, "Away Loose", aLoose)
  #     mat$Odds <- c("Home Loose", homeOdd, "Draw", drawOdd, "Away Loose", awayOdd)

  #     datatable(mat,
  #         rownames = TRUE,
  #         escape = FALSE,
  #         extensions = c('Responsive')
  #     )

  #   })

  # })

})