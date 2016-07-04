# All possible seasons are selected from the database for a country and leage. Copyright by Michael Bauer.
#
# Functions available:
# 	- Geometric(country, league)

Geometric <- function(country = "Germany", team = "Bayern Munich", season = '15/16') {
	# Get seasons in a given country for a given league.
	#
	#	country: A selected counrty (string).
	#	league: The selected league (string).
	# season: A selected season.
	#
	#	Return: Returns a vector with all possible seasons to select.

	InitDB()

	data <- data.frame("Goals" = 0:5, "Freq" = rep(x = 0, times = 6))

	for (i in 0:5) {

		home <- paste0("(select count(FTHG) from ", country, " where HomeTeam = '", team, "' and FTHG = '", i, "' and Season = '", season, "') + ")
		away <- paste0("(select count(FTAG) from ", country, " where AwayTeam = '", team, "' and FTAG = '", i, "' and Season = '", season, "')")

		if (season == "All") {
			home <- paste0("(select count(FTHG) from ", country, " where HomeTeam = '", team, "' and FTHG = '", i, "') + ")
			away <- paste0("(select count(FTAG) from ", country, " where AwayTeam = '", team, "' and FTAG = '", i, "')")
		}
		
		if (i == 5) {  # IF

			home <- paste0("(select count(FTHG) from ", country, " where HomeTeam = '", team, "' and FTHG >= '", i, "' and Season = '", season, "') + ")
			away <- paste0("(select count(FTAG) from ", country, " where AwayTeam = '", team, "' and FTAG >= '", i, "' and Season = '", season, "')")

			if (season == "All") {
				home <- paste0("(select count(FTHG) from ", country, " where HomeTeam = '", team, "' and FTHG >= '", i, "') + ")
				away <- paste0("(select count(FTAG) from ", country, " where AwayTeam = '", team, "' and FTAG >= '", i, "')")
			}

		}  # END IF

		query <- paste0("select ", home, away, " as SumCount")
		data[i+1, "Freq"] <- as.numeric(dbGetQuery(conn = ppConn, statement = query)[1, "SumCount"])

	}

	# data$Freq <- c(8, 6, 1, 1, 0, 0)

	dbDisconnect(conn = ppConn)

	lambda <- (sum(data$Goals*data$Freq))/sum(data$Freq)
	p <- 1.0/(lambda + 1.0)
	
	data$NewProbs <- round(dgeom(x = data$Goals, prob = p), 5)
	data$Predicted <- round(data$NewProbs*sum(data$Freq), 5)

	comp <- 1 - sum(data$NewProbs)
	test <- chisq.test(x = c(data$Freq, 0), p = c(data$NewProbs, comp), simulate.p.value = TRUE)

	# print(test)

	output <- list("Table" = data, "ChiSquare" = test, "Normal" = as.numeric(data$Freq/sum(data$Freq)))

	return (output)

}  # END Geometric