# All possible seasons are selected from the database for a country and leage. Copyright by Michael Bauer.
#
# Functions available:
# 	- Poisson(country, league)

Poisson <- function(country = "Germany", team = "Bayern Munich", season = '15/16') {
	# Get seasons in a given country for a given league.
	#
	#	country: A selected counrty (string).
	#	league: The selected league (string).
	# season: A selected season.
	#
	#	Return: Returns a vector with all possible seasons to select.

	InitDB()

	data <- data.frame("Goals" = 0:5, "Frequencies" = rep(x = 0, times = 6))

	for (i in 0:5) {

		home <- paste0("(select count(FTHG) from ", country, " where HomeTeam = '", team, "' and FTHG = '", i, "' and Season = '", season, "') + ")
		away <- paste0("(select count(FTAG) from ", country, " where AwayTeam = '", team, "' and FTAG = '", i, "' and Season = '", season, "')")
		
		if (i == 5) {  # IF

			home <- paste0("(select count(FTHG) from ", country, " where HomeTeam = '", team, "' and FTHG >= '", i, "' and Season = '", season, "') + ")
			away <- paste0("(select count(FTAG) from ", country, " where AwayTeam = '", team, "' and FTAG >= '", i, "' and Season = '", season, "')")

		}  # END IF

		query <- paste0("select ", home, away, " as SumCount")
		data[i+1, "Frequencies"] <- as.numeric(dbGetQuery(conn = ppConn, statement = query)[1, "SumCount"])

	}

	dbDisconnect(conn = ppConn)

	lambda <- (sum(data$Goals*data$Frequencies))/sum(data$Frequencies)
	
	data$Probabilities <- round(dpois(x = data$Goals, lambda = lambda), 5)
	data$Predicted <- round(data$Probabilities*sum(data$Frequencies), 5)

	comp <- 1 - sum(data$Probabilities)
	test <- chisq.test(x = c(data$Frequencies, 0), p = c(data$Probabilities, comp), simulate.p.value = TRUE)

	output <- list("Table" = data, "ChiSquare" = test, "Normal" = as.numeric(data$Frequencies/sum(data$Frequencies)))

	return (output)

}  # END Poisson