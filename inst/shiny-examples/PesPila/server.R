TestPredict <- function(session, input, output, homeTable, awayTable) {
  hParm <- ""
  aParm <- ""
  hProb <- rep(x = 0, times = 6)
  aProb <- rep(x = 0, times = 6)
  goals <- 0:5
  ngoal <- length(goals)
  mat <- data.frame(matrix(data = 0, nrow = ngoal, ncol = ngoal))
  colnames(mat) <- 0:5
  rownames(mat) <- 0:5
  hWin <- 0
  aWin <- 0
  draw <- 0
  tMax <- 0
  tInd <- c()

  InitDB()
  
  home <- paste0("select A.Dist from ", homeTable, " A, Seasons B, Teams C, Countries D where B.Season = '", input$season, "' and A.Season_ID = B.Season_ID and C.Team = '", input$fHome, "' and A.Team_ID = C.Team_ID and D.Country = '", input$country, "' and A.Country_ID = D.Country_ID")
  home <- as.character(dbGetQuery(conn = ppConn, statement = home)[1, "Dist"])
  hStr <- strsplit(home, split = "_")[[1]]

  if (hStr[1] == "Poisson") {
    hParm <- paste0(hStr[1], "_Lambda")
  } else if (hStr[1] == "ZIP") {
    hParm <- paste0(hStr[1], c("_Phi", "_Lambda"))
  } else if (hStr[1] == "Uniform") {
    hParm <- paste0(hStr[1], c("_A", "_B"))
  } else if (hStr[1] == "Geometric") {
    hParm <- paste0(hStr[1], "_P")
  } else if (hStr[1] == "NBD") {
    hParm <- paste0(hStr[1], c("_K", "_P"))
  }

  query <- paste0("select ", paste0("A.", home), ", ", paste("A.", hParm, collapse = ","), " from ", homeTable, " A, Seasons B, Teams C, Countries D where B.Season = '", input$season, "' and A.Season_ID = B.Season_ID and C.Team = '", input$fHome, "' and A.Team_ID = C.Team_ID and D.Country = '", input$country, "' and A.Country_ID = D.Country_ID")
  home <- dbGetQuery(conn = ppConn, statement = query)[1, ]
  
  away <- paste0("select A.Dist from ", awayTable, " A, Seasons B, Teams C, Countries D where B.Season = '", input$season, "' and A.Season_ID = B.Season_ID and C.Team = '", input$fAway, "' and A.Team_ID = C.Team_ID and D.Country = '", input$country, "' and A.Country_ID = D.Country_ID")
  away <- as.character(dbGetQuery(conn = ppConn, statement = away)[1, "Dist"])
  aStr <- strsplit(away, split = "_")[[1]]
  
  if (aStr[1] == "Poisson") {
    aParm <- paste0(aStr[1], "_Lambda")
  } else if (aStr[1] == "ZIP") {
    aParm <- paste0(aStr[1], c("_Phi", "_Lambda"))
  } else if (aStr[1] == "Uniform") {
    aParm <- paste0(aStr[1], c("_A", "_B"))
  } else if (aStr[1] == "Geometric") {
    aParm <- paste0(aStr[1], "_P")
  } else if (aStr[1] == "NBD") {
    aParm <- paste0(aStr[1], c("_K", "_P"))
  }

  query <- paste0("select ", paste0("A.", away), ", ", paste("A.", aParm, collapse = ","), " from ", awayTable, " A, Seasons B, Teams C, Countries D where B.Season = '", input$season, "' and A.Season_ID = B.Season_ID and C.Team = '", input$fAway, "' and A.Team_ID = C.Team_ID and D.Country = '", input$country, "' and A.Country_ID = D.Country_ID")
  away <- dbGetQuery(conn = ppConn, statement = query)[1, ]

  dbDisconnect(conn = ppConn)

  if (hStr[1] == "Poisson") {
    hProb <- dpois(x = goals, lambda = home[1, "Poisson_Lambda"])
  } else if (hStr[1] == "ZIP") {
    hProb <- dzipois(x = goals, lambda = home[1, "ZIP_Lambda"], phi = home[1, "ZIP_Phi"])
  } else if (hStr[1] == "Uniform") {
    hProb[which(goals <= home[1, "Uniform_B"])] <- 1/(home[1, "Uniform_B"] - home[1, "Uniform_A"] + 1)
  } else if (hStr[1] == "Geometric") {
    hProb <- dgeom(x = goals, prob = home[1, "Geometric_P"])
  } else if (hStr[1] == "NBD") {
    hProb <- dnbinom(x = goals, mu = home[1, "NBD_K"], size = home[1, "NBD_P"])
  }

  if (aStr[1] == "Poisson") {
    aProb <- dpois(x = goals, lambda = away[, "Poisson_Lambda"])
  } else if (aStr[1] == "ZIP") {
    aProb <- dzipois(x = goals, lambda = away[1, "ZIP_Lambda"], phi = away[1, "ZIP_Phi"])
  } else if (aStr[1] == "Uniform") {
    aProb[which(goals <= away[1, "Uniform_B"])] <- 1/(away[1, "Uniform_B"] - away[1, "Uniform_A"] + 1)
  } else if (aStr[1] == "Geometric") {
    aProb <- dgeom(x = goals, prob = away[1, "Geometric_P"])
  } else if (aStr[1] == "NBD") {
    aProb <- dnbinom(x = goals, mu = away[1, "NBD_K"], size = away[1, "NBD_P"])
  }

  # hProb <- 3/2 * hProb
  # aProb <- 2/3 * aProb

  for (i in 1:ngoal) {
    for (j in 1:ngoal) {
      mat[i, j] <- hProb[i] * aProb[j]
      
      if (i == j) {
        draw <- draw + mat[i, j] * 100
      } else if (j > i) {
        aWin <- aWin + mat[i, j] * 100
      } else {
        hWin <- hWin + mat[i, j] * 100
      }

      if (mat[i, j] > tMax) {
        # mat[i, j] <- paste0('<font style="background-color:red;color:white;">', mat[i, j], '</font>')
        tMax <- mat[i, j]
        tInd <- c(i-1, j-1)
      }
    }
  }

  homeOdd <- if ((100/hWin) - 1 < 1) {1.10} else {(100/hWin) - 1}
  drawOdd <- if ((100/draw) - 1 < 1) {1.10} else {(100/draw) - 1}
  awayOdd <- if ((100/aWin) - 1 < 1) {1.10} else {(100/aWin) - 1}

  out <- data.frame("hWin" = hWin, "draw" = draw, "aWin" = aWin, "homeOdd" = homeOdd, "drawOdd" = drawOdd,
                    "awayOdd" = awayOdd, "FTHG" = tInd[1], "FTAG" = tInd[2])

  # test <- GetHomeVsAway(country = input$country, home = input$fHome, away = input$fAway)

  # output$prediction <- renderUI({
  #   HTML(
  #     paste(c(
  #         '<table style="font-size:20px;">
  #           <tr>
  #             <td style="padding:5px;"><u>Actual</u></td>
  #             <td style="padding:5px;"><b>', input$fHome, '</b></td>
  #             <td style="padding:5px;">', round(as.numeric(test[2, "FTHG"]), 0), '</td>
  #             <td style="padding:5px;">', round(as.numeric(test[2, "FTAG"]), 0), '</td>
  #             <td style="padding:5px;" colspan="2"><b>', input$fAway, '</b></td>
  #           </tr>
  #           <tr>
  #             <td style="padding:5px;" colspan="2"><b>', input$fHome, '</b></td>
  #             <td style="padding:5px;">', round(as.numeric(test[1, "FTHG"]), 0), '</td>
  #             <td style="padding:5px;">', round(as.numeric(test[1, "FTAG"]), 0), '</td>
  #             <td style="padding:5px;" colspan="2"><b>', input$fAway, '</b></td>
  #           </tr>
  #           <tr>
  #             <td style="padding:5px;"><b>Home Win:</b></td>
  #             <td style="padding:5px;">', round(hWin, 2), ' %</td>
  #             <td style="padding:5px;"><b>Draw:</b></td>
  #             <td style="padding:5px;">', round(draw, 2), ' %</td>
  #             <td style="padding:5px;"><b>Away Win:</b></td>
  #             <td style="padding:5px;">', round(aWin, 2), ' %</td>
  #           </tr>
  #           <tr>
  #             <td style="padding:5px;"><b>Home Odd:</b></td>
  #             <td style="padding:5px;">', round(homeOdd, 2), '</td>
  #             <td style="padding:5px;"><b>Draw Odd:</b></td>
  #             <td style="padding:5px;">', round(drawOdd, 2), '</td>
  #             <td style="padding:5px;"><b>Away Odd:</b></td>
  #             <td style="padding:5px;">', round(awayOdd, 2), '</td>
  #           </tr>
  #           <tr>
  #             <td style="padding:5px;" colspan="2"><b>', input$fHome, '</b></td>
  #             <td style="padding:5px;">', tInd[1], '</td>
  #             <td style="padding:5px;">', tInd[2], '</td>
  #             <td style="padding:5px;" colspan="2"><b>', input$fAway, '</b></td>
  #           </tr>
  #         </table>'
  #       )
  #     )
  #   )
  # })

  return (out)
}

shinyServer(function(input, output, session) {

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

  observeEvent(eventExpr = input$getDistributions, {
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

  # output$predictScored <- DT::renderDataTable({
  output$prediction <- renderUI({

    withProgress(message = 'Calculation in progress', detail = 'This may take a while...', min = 0, max = 2, {


      test <- GetHomeVsAway(country = input$country, home = input$fHome, away = input$fAway)
      SvS <- TestPredict(session = session, input = input, output = output, homeTable = "Scored", awayTable = "Scored")
      CvC <- TestPredict(session = session, input = input, output = output, homeTable = "Conceded", awayTable = "Conceded")
      SvC <- TestPredict(session = session, input = input, output = output, homeTable = "Scored", awayTable = "Conceded")
      CvS <- TestPredict(session = session, input = input, output = output, homeTable = "Conceded", awayTable = "Scored")

      H <- 1.5 * (SvS[1, "FTHG"] + CvC[1, "FTHG"] + SvC[1, "FTHG"] + CvS[1, "FTHG"] + as.numeric(test[1, "FTHG"])) / 5
      A <- (SvS[1, "FTAG"] + CvC[1, "FTAG"] + SvC[1, "FTAG"] + CvS[1, "FTAG"] + as.numeric(test[1, "FTAG"])) / 5
      H <- round(H, 0)
      A <- round(A, 0)

      # output$prediction <- renderUI({
        HTML(
          paste(c(
              '<table style="font-size:20px;">
                <tr>
                  <td style="padding:5px;"><u>Actual</u></td>
                  <td style="padding:5px;"><b>', input$fHome, '</b></td>
                  <td style="padding:5px;">', round(as.numeric(test[2, "FTHG"]), 0), '</td>
                  <td style="padding:5px;">', round(as.numeric(test[2, "FTAG"]), 0), '</td>
                  <td style="padding:5px;" colspan="2"><b>', input$fAway, '</b></td>
                </tr>
                <tr>
                  <td style="padding:5px;" colspan="2"><b>', input$fHome, '</b></td>
                  <td style="padding:5px;">', H, '</td>
                  <td style="padding:5px;">', A, '</td>
                  <td style="padding:5px;" colspan="2"><b>', input$fAway, '</b></td>
                </tr>
                <tr>
                  <td style="padding:5px;" colspan="2"><b>', input$fHome, '</b></td>
                  <td style="padding:5px;">', round(as.numeric(test[1, "FTHG"]), 0), '</td>
                  <td style="padding:5px;">', round(as.numeric(test[1, "FTAG"]), 0), '</td>
                  <td style="padding:5px;" colspan="2"><b>', input$fAway, '</b></td>
                </tr>
                <tr>
                  <td style="padding:5px;"><b>Home Win:</b></td>
                  <td style="padding:5px;">', round(SvS[1, "hWin"], 2), ' %</td>
                  <td style="padding:5px;"><b>Draw:</b></td>
                  <td style="padding:5px;">', round(SvS[1, "draw"], 2), ' %</td>
                  <td style="padding:5px;"><b>Away Win:</b></td>
                  <td style="padding:5px;">', round(SvS[1, "aWin"], 2), ' %</td>
                </tr>
                <tr>
                  <td style="padding:5px;"><b>Home Odd:</b></td>
                  <td style="padding:5px;">', round(SvS[1, "homeOdd"], 2), '</td>
                  <td style="padding:5px;"><b>Draw Odd:</b></td>
                  <td style="padding:5px;">', round(SvS[1, "drawOdd"], 2), '</td>
                  <td style="padding:5px;"><b>Away Odd:</b></td>
                  <td style="padding:5px;">', round(SvS[1, "awayOdd"], 2), '</td>
                </tr>
                <tr>
                  <td style="padding:5px;" colspan="2"><b>', input$fHome, '</b></td>
                  <td style="padding:5px;">', SvS[1, "FTHG"], '</td>
                  <td style="padding:5px;">', SvS[1, "FTAG"], '</td>
                  <td style="padding:5px;" colspan="2"><b>', input$fAway, '</b></td>
                </tr>
                <tr>
                  <td style="padding:5px;"><b>Home Win:</b></td>
                  <td style="padding:5px;">', round(CvC[1, "aWin"], 2), ' %</td>
                  <td style="padding:5px;"><b>Draw:</b></td>
                  <td style="padding:5px;">', round(CvC[1, "draw"], 2), ' %</td>
                  <td style="padding:5px;"><b>Away Win:</b></td>
                  <td style="padding:5px;">', round(CvC[1, "hWin"], 2), ' %</td>
                </tr>
                <tr>
                  <td style="padding:5px;"><b>Home Odd:</b></td>
                  <td style="padding:5px;">', round(CvC[1, "awayOdd"], 2), '</td>
                  <td style="padding:5px;"><b>Draw Odd:</b></td>
                  <td style="padding:5px;">', round(CvC[1, "drawOdd"], 2), '</td>
                  <td style="padding:5px;"><b>Away Odd:</b></td>
                  <td style="padding:5px;">', round(CvC[1, "homeOdd"], 2), '</td>
                </tr>
                <tr>
                  <td style="padding:5px;" colspan="2"><b>', input$fHome, '</b></td>
                  <td style="padding:5px;">', CvC[1, "FTAG"], '</td>
                  <td style="padding:5px;">', CvC[1, "FTHG"], '</td>
                  <td style="padding:5px;" colspan="2"><b>', input$fAway, '</b></td>
                </tr>
                <tr>
                  <td style="padding:5px;"><b>Home Win:</b></td>
                  <td style="padding:5px;">', round(SvC[1, "hWin"], 2), ' %</td>
                  <td style="padding:5px;"><b>Draw:</b></td>
                  <td style="padding:5px;">', round(SvC[1, "draw"], 2), ' %</td>
                  <td style="padding:5px;"><b>Away Win:</b></td>
                  <td style="padding:5px;">', round(SvC[1, "aWin"], 2), ' %</td>
                </tr>
                <tr>
                  <td style="padding:5px;"><b>Home Odd:</b></td>
                  <td style="padding:5px;">', round(SvC[1, "homeOdd"], 2), '</td>
                  <td style="padding:5px;"><b>Draw Odd:</b></td>
                  <td style="padding:5px;">', round(SvC[1, "drawOdd"], 2), '</td>
                  <td style="padding:5px;"><b>Away Odd:</b></td>
                  <td style="padding:5px;">', round(SvC[1, "awayOdd"], 2), '</td>
                </tr>
                <tr>
                  <td style="padding:5px;" colspan="2"><b>', input$fHome, '</b></td>
                  <td style="padding:5px;">', SvC[1, "FTHG"], '</td>
                  <td style="padding:5px;">', SvC[1, "FTAG"], '</td>
                  <td style="padding:5px;" colspan="2"><b>', input$fAway, '</b></td>
                </tr>
                <tr>
                  <td style="padding:5px;"><b>Home Win:</b></td>
                  <td style="padding:5px;">', round(CvS[1, "aWin"], 2), ' %</td>
                  <td style="padding:5px;"><b>Draw:</b></td>
                  <td style="padding:5px;">', round(CvS[1, "draw"], 2), ' %</td>
                  <td style="padding:5px;"><b>Away Win:</b></td>
                  <td style="padding:5px;">', round(CvS[1, "hWin"], 2), ' %</td>
                </tr>
                <tr>
                  <td style="padding:5px;"><b>Home Odd:</b></td>
                  <td style="padding:5px;">', round(CvS[1, "awayOdd"], 2), '</td>
                  <td style="padding:5px;"><b>Draw Odd:</b></td>
                  <td style="padding:5px;">', round(CvS[1, "drawOdd"], 2), '</td>
                  <td style="padding:5px;"><b>Away Odd:</b></td>
                  <td style="padding:5px;">', round(CvS[1, "homeOdd"], 2), '</td>
                </tr>
                <tr>
                  <td style="padding:5px;" colspan="2"><b>', input$fHome, '</b></td>
                  <td style="padding:5px;">', CvS[1, "FTAG"], '</td>
                  <td style="padding:5px;">', CvS[1, "FTHG"], '</td>
                  <td style="padding:5px;" colspan="2"><b>', input$fAway, '</b></td>
                </tr>
              </table>'
            )
          )
        )
      # })

      # pHome <- 0
      # pAway <- 0
      # iHome <- 0
      # iAway <- 0
      # hVec <- c()
      # aVec <- c()
      # for (i in 1:length(home)) {
      #   hVec <- c(hVec, home[[i]]$ChiSquare$p.value)
      #   aVec <- c(aVec, away[[i]]$ChiSquare$p.value)
      #   if (home[[i]]$ChiSquare$p.value > pHome) {
      #     iHome <- i
      #     pHome <- home[[i]]$ChiSquare$p.value
      #   }
      #   if (away[[i]]$ChiSquare$p.value > pAway) {
      #     iAway <- i
      #     pAway <- away[[i]]$ChiSquare$p.value
      #   }
      # }

      # nrow <- nrow(home[[iHome]]$Table)
      # ncol <- nrow(away[[iAway]]$Table)
      # mat <- data.frame(matrix(0, nrow = nrow, ncol = ncol))
      # hWin <- 0
      # aWin <- 0
      # draw <- 0
      # for (i in 1:nrow) {
      #   for (j in 1:ncol) {
      #     mat[i, j] <- round(home[[iHome]]$Table$NewProbs[i]*away[[iAway]]$Table$NewProbs[j], 4)
      #     if (i == j) {
      #       draw <- draw + mat[i, j]*100
      #     } else if (j > i) {
      #       aWin <- aWin + mat[i, j]*100
      #     } else {
      #       hWin <- hWin + mat[i, j]*100
      #     }
      #   }
      # }

      # colnames(mat) <- 0:5
      # rownames(mat) <- 0:5

      # med <- (max(mat) - min(mat))/2

      # for (i in 1:nrow) {
      #   for (j in 1:ncol) {
      #     if (mat[i,j] > med) {
      #       mat[i,j] <- paste0('<font style="background-color:red;color:white;">', mat[i,j], '</font>')
      #     } else {
      #       mat[i,j] <- paste0('<font style="background-color:#3c8dbc;color:white;">', mat[i,j], '</font>')
      #     }
      #   }
      # }

      # homeOdd <- if (round((100/hWin)-1, 2) < 1) {1.05} else {round((100/hWin)-1, 2)}
      # drawOdd <- if (round((100/draw)-1, 2) < 1) {1.05} else {round((100/draw)-1, 2)}
      # awayOdd <- if (round((100/aWin)-1, 2) < 1) {1.05} else {round((100/aWin)-1, 2)}

      # mat$Probs <- c("Home Win", hWin, "Draw", draw, "Away Win", aWin)
      # mat$Odds <- c("Home Win", homeOdd, "Draw", drawOdd, "Away Win", awayOdd)

      # datatable(mat,
      #     rownames = TRUE,
      #     escape = FALSE,
      #     extensions = c('Responsive')
      # )

    })

  })

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

})