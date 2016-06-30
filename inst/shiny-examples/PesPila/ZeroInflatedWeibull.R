# All possible seasons are selected from the database for a country and leage. Copyright by Michael Bauer.
#
# Functions available:
# 	- ZIW(country, league)

dziweibull <- function(x, shape, scale, phi) {
	zip <- c()
	if (scale < 0 || shape < 0) {
		return (rep(x = NaN, times = length(x)))
	}
	if (phi < 0 || phi > 1) {
		return (rep(x = NaN, times = length(x)))
	}
	index <- which(x == 0)
	if (length(index) > 0) {
		zip <- c(zip, (1-phi)+phi*dweibull(x[index], scale = scale, shape = shape))
		x <- x[-index]
	}
	zip <- c(zip, phi*dweibull(x, scale = scale, shape = shape))

	return (zip)
}

OptParsWeibull <- function(pars, A, B, C, D) {
	E <- sum(A*B*(dziweibull(x = C, shape = pars[1], scale = pars[2], phi = pars[3])-D)^2)
	return (E)
}

ZIW <- function(country = "Germany", team = "Bayern Munich", season = '15/16') {
	# Get seasons in a given country for a given league.
	#
	#	country: A selected counrty (string).
	#	league: The selected league (string).
	# season: A selected season.
	#
	#	Return: Returns a vector with all possible seasons to select.

	scale <- 1
	shape <- 1
	phi <- 0.5
	pars <- c(scale = scale, shape = shape, phi = phi)

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

	data$RelFreq <- round(data$Freq/sum(data$Freq), 5)

	data$Probs <- round(dziweibull(x = data$Goals, shape = shape, scale = scale, phi = phi), 5)

	data$FreqWeight <- rep(x = 0, times = 6)
	index <- which(data$RelFreq != 0)
	data$FreqWeight[index] <- round(1.0/(data$RelFreq[index]*(1.0 - data$RelFreq[index])), 5)
	
	data$SquareDev <- round((data$Probs - data$RelFreq)^2, 5)
	data$WeightedSquareDev <- round(data$Freq*data$FreqWeight*data$SquareDev, 5)
	
	Opt <- optim(pars, OptParsWeibull, A = data$Freq, B = data$FreqWeight, C = data$Goals, D = data$RelFreq)
	shape <- Opt$par[1]
	scale <- Opt$par[2]
	phi <- Opt$par[3]

	data$NewProbs <- round(dziweibull(x = data$Goals, shape = shape, scale = scale, phi = phi), 5)
	data$NewProbs[1] <- data$NewProbs[1]+0.00001
	data$Predicted <- round(data$NewProbs*sum(data$Freq), 5)

	comp <- 1 - sum(data$NewProbs)
	test <- chisq.test(x = c(data$Freq, 0), p = c(data$NewProbs, comp), simulate.p.value = TRUE)

	# index <- which(data$Predicted != 0)
	# chi <- sum((data$RelFreq[index]-data$Predicted[index])^2 / data$Predicted[index])
	# pval <- 1 - pchisq(chi, df = length(index) - 1)

	output <- list("Table" = data, "ChiSquare" = test, "Normal" = as.numeric(data$Freq/sum(data$Freq)), "shape" = shape, "scale" = scale, "phi" = phi)

	return (output)

}  # END ZIW