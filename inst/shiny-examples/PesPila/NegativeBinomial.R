# All possible seasons are selected from the database for a country and leage. Copyright by Michael Bauer.
#
# Functions available:
# 	- NBD(country, league)

OptParsNBD <- function(pars, A, B, C, D) {
	E <- sum(A*B*(dnbinom(x = C, size = pars[1], mu = pars[2])-D)^2)
	return (E)
}

NBD <- function(country = "Germany", team = "Bayern Munich", season = '15/16') {
	# Get seasons in a given country for a given league.
	#www
	#	country: A selected counrty (string).
	#	league: The selected league (string).
	# season: A selected season.
	#
	#	Return: Returns a vector with all possible seasons to select.

	k <- 1
	p <- 0.5
	data <- data.frame("Goals" = 0:5, "Freq" = rep(x = 0, times = 6))

	InitDB()

	for (i in 0:5) {

		home <- paste0("(select count(FTHG) from ", country, " where HomeTeam = '", team, "' and FTHG = '", i, "' and Season = '", season, "') + ")
		away <- paste0("(select count(FTAG) from ", country, " where AwayTeam = '", team, "' and FTAG = '", i, "' and Season = '", season, "')")
		
		if (i == 5) {  # IF

			home <- paste0("(select count(FTHG) from ", country, " where HomeTeam = '", team, "' and FTHG >= '", i, "' and Season = '", season, "') + ")
			away <- paste0("(select count(FTAG) from ", country, " where AwayTeam = '", team, "' and FTAG >= '", i, "' and Season = '", season, "')")

		}  # END IF

		query <- paste0("select ", home, away, " as SumCount")
		data[i+1, "Freq"] <- as.numeric(dbGetQuery(conn = ppConn, statement = query)[1, "SumCount"])

	}

	dbDisconnect(conn = ppConn)

	# data$Freq <- c(9, 5, 1, 1, 0, 0)

	pars <- c(k = k, p = p)

	data$RelFreq <- round(data$Freq/sum(data$Freq), 5)

	data$Probs <- round(dnbinom(x = data$Goals, mu = k, size = p), 5)

	data$FreqWeight <- rep(x = 0, times = 6)
	index <- which(data$RelFreq != 0)
	data$FreqWeight[index] <- round(1.0/(data$RelFreq[index]*(1.0 - data$RelFreq[index])), 5)
	
	data$SquareDev <- round((data$Probs - data$RelFreq)^2, 5)
	data$WeightedSquareDev <- round(data$Freq*data$FreqWeight*data$SquareDev, 5)
	
	Opt <- optim(pars, OptPars, A = data$Freq, B = data$FreqWeight, C = data$Goals, D = data$RelFreq)
	k <- Opt$par[1]
	p <- Opt$par[2]

	data$NewProbs <- round(dnbinom(x = data$Goals, mu = k, size = p), 5)
	data$Predicted <- round(data$NewProbs*sum(data$Freq), 5)

	comp <- 1 - sum(data$NewProbs)
	test <- chisq.test(x = c(data$Freq, 0), p = c(data$NewProbs, comp), simulate.p.value = TRUE)

	output <- list("Table" = data, "ChiSquare" = test, "Normal" = as.numeric(data$Freq/sum(data$Freq)), "k" = k, "p" = p)

	return (output)

}  # END NBD