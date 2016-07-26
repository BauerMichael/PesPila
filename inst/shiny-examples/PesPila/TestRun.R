require(shiny)
require(shinydashboard)
require(hash)
require(DT)
require(ggplot2)
require(RSQLite)
options(warn=-1)

OptParsNBD <- function(pars, A, B, C, D) {
	if (pars[1] < 0 || pars[1] > 5) {
		C <- rep(x = NaN, times = length(C))
	}
	if (pars[2] < 0 || pars[2] > 5) {
		C <- rep(x = NaN, times = length(C))
	}
	E <- sum(A*B*(dnbinom(x = C, size = pars[1], mu = pars[2])-D)^2)
	return (E)
}

NBD <- function(freqs) {
	print("Entered NBD")
	goals <- 0:5
	k <- 1
	p <- 0.5
	pars <- c(k = k, p = p)

	relFreq <- freqs/sum(freqs)

	freqsWeight <- rep(x = 0, times = 6)
	index <- which(relFreq != 0)
	freqsWeight[index] <- 1.0/(relFreq[index]*(1.0 - relFreq[index]))
	
	Opt <- optim(pars, OptParsNBD, A = freqs, B = freqsWeight, C = goals, D = relFreq)
	k <- Opt$par[1]
	p <- Opt$par[2]

	probs <- dnbinom(x = goals, mu = k, size = p)

	sum <- 1 - sum(probs)
	test <- chisq.test(x = c(freqs, 0), p = c(probs, sum), simulate.p.value = TRUE)

	out <- c("k" = k, "p" = p, "pval" = test$p.value)

	return (out)
}

Geometric <- function(freqs) {
	print("Entered Geometric")
	goals <- 0:5
	lambda <- (sum(goals*freqs))/sum(freqs)
	p <- 1.0/(lambda + 1.0)
	
	probs <- dgeom(x = goals, prob = p)

	sum <- 1 - sum(probs)
	test <- chisq.test(x = c(freqs, 0), p = c(probs, sum), simulate.p.value = TRUE)

	out <- c("p" = p, "pval" = test$p.value)

	return (out)
}

Uniform <- function(freqs) {
	print("Entered Uniform")
	goals <- 0:5
	lambda <- (sum(goals*freqs))/sum(freqs)
	a <- 0
	b <- 2*lambda

	probs <- dunif(x = 1:6, min = a, max = b)
	predicted <- probs*sum(freqs)

	index <- which(predicted != 0)

	if (length(index) == 0) {
		return (c("a" = 0, "b" = 0, "pval" = 0))
	}

	chi <- sum((freqs[index]-predicted[index])^2 / predicted[index])
	pval <- 1 - pchisq(chi, df = length(index) - 1)

	out <- c("a" = a, "b" = b, "pval" = pval)

	return (out)

}

dzipois <- function(x, lambda, phi) {
	zip <- c()
	if (lambda < 0) {
		return (rep(x = NaN, times = length(x)))
	}
	if (phi < 0 || phi > 1) {
		return (rep(x = NaN, times = length(x)))
	}
	index <- which(x == 0)
	if (length(index) > 0) {
		zip <- c(zip, (1-phi)+phi*dpois(x[index], lambda = lambda))
		x <- x[-index]
	}
	zip <- c(zip, phi*dpois(x, lambda = lambda))

	return (zip)
}

OptParsZIP <- function(pars, A, B, C, D) {
		E <- sum(A*B*(dzipois(x = C, lambda = pars[1], phi = pars[2])-D)^2)
		return (E)
	}

ZIP <- function(freqs) {
	# Get seasons in a given country for a given league.
	#
	#	freqs: 
	#
	#	Return: Returns a vector with all possible seasons to select.

	print("Entered ZIP")

	lambda <- 2
	phi <- 0.5
	pars <- c(lambda = lambda, phi = phi)
	goals <- 0:5

	relFreq <- freqs/sum(freqs)
	probs <- dzipois(x = goals, lambda = lambda, phi = phi)
	freqsWeight <- rep(x = 0, times = 6)
	index <- which(relFreq != 0)
	freqsWeight[index] <- 1.0/(relFreq[index]*(1.0 - relFreq[index]))
	
	Opt <- optim(pars, OptParsZIP, A = freqs, B = freqsWeight, C = goals, D = relFreq)
	lambda <- Opt$par[1]
	phi <- Opt$par[2]

	newProbs <- dzipois(x = goals, lambda = lambda, phi = phi)
	sum <- 1 - sum(newProbs)
	test <- chisq.test(x = c(freqs, 0), p = c(newProbs, sum), simulate.p.value = TRUE)

	out <- c(lambda = lambda, phi = phi, pval = test$p.value)

	return (out)

}  # END ZIP

Poisson <- function(freqs) {
	# Get seasons in a given country for a given league.
	#
	#	freqs: 
	#
	#	Return: Returns a vector with all possible seasons to select.

	print("Entered Poisson")

	goals <- 0:5
	lambda <- (sum(goals*freqs))/sum(freqs)
	probs <- dpois(x = goals, lambda = lambda)
	sum <- 1 - sum(probs)
	test <- chisq.test(x = c(freqs, 0), p = c(probs, sum), simulate.p.value = TRUE)
	out <- c(lambda = lambda, pval = test$p.value)
	
	return (out)

}  # END Poisson

CalcDist <- function() {

	assign(x = "Conn",
         dbConnect(SQLite(), "inst/shiny-examples/PesPila/data/PesPilaDB.db"),
         envir = .GlobalEnv)
	# T.data <- c()
  seasons <- "select * from Seasons"
  seasons <- dbGetQuery(conn = Conn, statement = seasons)[31:53,]
  dist <- "select * from dist_store"
  dist <- dbGetQuery(conn = Conn, statement = dist)
  cols <- colnames(dist)
  print(cols)
  country <- "select * from Countries"
  country <- dbGetQuery(conn = Conn, statement = country)
	# teams <- "select * from Teams"
 #  teams <- dbGetQuery(conn = Conn, statement = teams)
  n <- 1

	for (l in 11:nrow(country)) {

		teams <- paste0("select distinct HomeTeam as Team from ", country[l, "Country"])
	  teams <- dbGetQuery(conn = Conn, statement = teams)
	  # T.data <- c(T.data, as.character(teams[, "Team"]))
	  # assign(x = "team.data",
			# 	         T.data,
			# 	         envir = .GlobalEnv)

	  for (i in 1:nrow(teams)) {

	  	team.id <- paste0("select Team_ID from Teams where Team = '", teams[i, "Team"], "'")
	  	team.id <- as.numeric(dbGetQuery(conn = Conn, statement = team.id))

	    for (j in 1:nrow(seasons)) {

	    	test.query <- paste0("select * from ", country[l, "Country"], " where (HomeTeam = '", teams[i, "Team"], "' or AwayTeam = '", teams[i, "Team"], "') and Season = '", seasons[j, "Season"], "'")
	    	test.data <- dbGetQuery(conn = Conn, statement = test.query)
	    	
	    	Win <- length(which((test.data[, "HomeTeam"] == teams[i, "Team"] & test.data[, "FTR"] == "H") | (test.data[, "AwayTeam"] == teams[i, "Team"] & test.data[, "FTR"] == "A")))
	    	Draw <- length(which((test.data[, "HomeTeam"] == teams[i, "Team"] & test.data[, "FTR"] == "D") | (test.data[, "AwayTeam"] == teams[i, "Team"] & test.data[, "FTR"] == "D")))
	    	Lost <- length(which((test.data[, "HomeTeam"] == teams[i, "Team"] & test.data[, "FTR"] == "A") | (test.data[, "AwayTeam"] == teams[i, "Team"] & test.data[, "FTR"] == "H")))
		  	Win_Home <- length(which((test.data[, "HomeTeam"] == teams[i, "Team"] & test.data[, "FTR"] == "H")))
		  	Draw_Home <- length(which((test.data[, "HomeTeam"] == teams[i, "Team"] & test.data[, "FTR"] == "D")))
		  	Lost_Home <- length(which((test.data[, "HomeTeam"] == teams[i, "Team"] & test.data[, "FTR"] == "A")))  	

	  		print(c("Country", country[l, "Country"], "Season", seasons[j, "Season"], "Team", teams[i, "Team"], "ID", team.id))
	    	if (nrow(test.data) > 0) {

	    		freqs <- c()
		      for (k in 0:5) {

						home <- paste0("(select count(FTHG) from ", country[l, "Country"], " where HomeTeam = '", teams[i, "Team"], "' and FTHG = ", k, " and Season = '", seasons[j, "Season"], "') + ")
						away <- paste0("(select count(FTAG) from ", country[l, "Country"], " where AwayTeam = '", teams[i, "Team"], "' and FTAG = ", k, " and Season = '", seasons[j, "Season"], "')")
						
						if (k == 5) {  # IF

							home <- paste0("(select count(FTHG) from ", country[l, "Country"], " where HomeTeam = '", teams[i, "Team"], "' and FTHG >= ", k, " and Season = '", seasons[j, "Season"], "') + ")
							away <- paste0("(select count(FTAG) from ", country[l, "Country"], " where AwayTeam = '", teams[i, "Team"], "' and FTAG >= ", k, " and Season = '", seasons[j, "Season"], "')")

						}  # END IF

						query <- paste0("select ", home, away, " as SumCount")
						freqs <- c(freqs, as.numeric(dbGetQuery(conn = Conn, statement = query)[1, "SumCount"]))
					}
					print(freqs)

		      stat <- list(
		        "Poisson" = Poisson(freqs),
		        "ZIP" = ZIP(freqs),
		        "Uniform" = Uniform(freqs),
		        "Geometric" = Geometric(freqs),
		        "NBD" = NBD(freqs)
		      )
		      row.data <- as.numeric(c(
		          country[l, "Country_ID"], seasons[j, "Season_ID"], team.id, freqs/sum(freqs), stat$Poisson, stat$ZIP, stat$Uniform, stat$Geometric, stat$NBD, Win, Draw, Lost, Win_Home, Draw_Home, Lost_Home, ""
		      ))
		      dist <- rbind(dist, row.data)
					colnames(dist) <- cols
		      cname <- c("Poisson_Pval", "ZIP_Pval", "Uniform_Pval", "Geometric_Pval", "NBD_Pval")
			  	pval <- as.numeric(dist[n, cname])
			  	iX <- which(pval == max(pval))
			  	dist[n, "Dist"] <- cname[iX[1]]
		      n <- n + 1
					assign(x = "dist.data",
				         dist,
				         envir = .GlobalEnv)
				}
			}
    }
  }

	dbDisconnect(conn = Conn)
	colnames(dist) <- cols

	return (dist)

}

CalcDist2 <- function() {

	assign(x = "Conn",
         dbConnect(SQLite(), "inst/shiny-examples/PesPila/data/PesPilaDB.db"),
         envir = .GlobalEnv)
	# T.data <- c()
  seasons <- "select * from Seasons"
  seasons <- dbGetQuery(conn = Conn, statement = seasons)[31:53,]
  dist <- "select * from conceded_store"
  dist <- dbGetQuery(conn = Conn, statement = dist)
  cols <- colnames(dist)
  print(cols)
  country <- "select * from Countries"
  country <- dbGetQuery(conn = Conn, statement = country)
	# teams <- "select * from Teams"
 #  teams <- dbGetQuery(conn = Conn, statement = teams)
  n <- 1

	for (l in 11:nrow(country)) {

		teams <- paste0("select distinct HomeTeam as Team from ", country[l, "Country"])
	  teams <- dbGetQuery(conn = Conn, statement = teams)

	  for (i in 1:nrow(teams)) {

	  	team.id <- paste0("select Team_ID from Teams where Team = '", teams[i, "Team"], "'")
	  	team.id <- as.numeric(dbGetQuery(conn = Conn, statement = team.id))

	    for (j in 1:nrow(seasons)) {

	    	test.query <- paste0("select * from ", country[l, "Country"], " where (HomeTeam = '", teams[i, "Team"], "' or AwayTeam = '", teams[i, "Team"], "') and Season = '", seasons[j, "Season"], "'")
	    	test.data <- dbGetQuery(conn = Conn, statement = test.query)

	  		print(c("Country", country[l, "Country"], "Season", seasons[j, "Season"], "Team", teams[i, "Team"], "ID", team.id))
	    	if (nrow(test.data) > 0) {

	    		query <- paste(c("
	    			select",
							"(select count(FTAG) from ", country[l, "Country"], " where HomeTeam = '", teams[i, "Team"], "' and FTAG = '0' and Season = '", seasons[j, "Season"], "') + ",
							"(select count(FTHG) from ", country[l, "Country"], " where AwayTeam = '", teams[i, "Team"], "' and FTHG = '0' and Season = '", seasons[j, "Season"], "') ",
						"as Zero,",
							"(select count(FTAG) from ", country[l, "Country"], " where HomeTeam = '", teams[i, "Team"], "' and FTAG = '1' and Season = '", seasons[j, "Season"], "') + ",
							"(select count(FTHG) from ", country[l, "Country"], " where AwayTeam = '", teams[i, "Team"], "' and FTHG = '1' and Season = '", seasons[j, "Season"], "') ",
						"as One,",
							"(select count(FTAG) from ", country[l, "Country"], " where HomeTeam = '", teams[i, "Team"], "' and FTAG = '2' and Season = '", seasons[j, "Season"], "') + ",
							"(select count(FTHG) from ", country[l, "Country"], " where AwayTeam = '", teams[i, "Team"], "' and FTHG = '2' and Season = '", seasons[j, "Season"], "') ",
						"as Two,",
							"(select count(FTAG) from ", country[l, "Country"], " where HomeTeam = '", teams[i, "Team"], "' and FTAG = '3' and Season = '", seasons[j, "Season"], "') + ",
							"(select count(FTHG) from ", country[l, "Country"], " where AwayTeam = '", teams[i, "Team"], "' and FTHG = '3' and Season = '", seasons[j, "Season"], "') ",
						"as Three,",
							"(select count(FTAG) from ", country[l, "Country"], " where HomeTeam = '", teams[i, "Team"], "' and FTAG = '4' and Season = '", seasons[j, "Season"], "') + ",
							"(select count(FTHG) from ", country[l, "Country"], " where AwayTeam = '", teams[i, "Team"], "' and FTHG = '4' and Season = '", seasons[j, "Season"], "') ",
						"as Four,",
							"(select count(FTAG) from ", country[l, "Country"], " where HomeTeam = '", teams[i, "Team"], "' and FTAG >= '5' and Season = '", seasons[j, "Season"], "') + ",
							"(select count(FTHG) from ", country[l, "Country"], " where AwayTeam = '", teams[i, "Team"], "' and FTHG >= '5' and Season = '", seasons[j, "Season"], "') ",
						"as Five"
	    		), collapse = "")
					freqs <- as.numeric(dbGetQuery(conn = Conn, statement = query)[1, ])
					print(freqs)

		      stat <- list(
		        "Poisson" = Poisson(freqs),
		        "ZIP" = ZIP(freqs),
		        "Uniform" = Uniform(freqs),
		        "Geometric" = Geometric(freqs),
		        "NBD" = NBD(freqs)
		      )
		      dist <- rbind(
		      	dist,
		        as.numeric(c(country[l, "Country_ID"], seasons[j, "Season_ID"], team.id, freqs/sum(freqs), stat$Poisson, stat$ZIP, stat$Uniform, stat$Geometric, stat$NBD, ""))
		     	)
					colnames(dist) <- cols
		      col.stat <- c("Poisson_Pval", "ZIP_Pval", "Uniform_Pval", "Geometric_Pval", "NBD_Pval")
		      p.stat <- dist[, col.stat]
		      dist[n, "Dist"] <- col.stat[which(p.stat == max(p.stat))][1]
		      n <- n + 1
		      # dist$Dist <- c(dist$Dist, col.stat[which(p.stat == max(p.stat))][1])
					assign(x = "conc.data",
				         dist,
				         envir = .GlobalEnv)
				}
			}
    }
  }

	dbDisconnect(conn = Conn)

	return (dist)

}

SetDist <- function() {
	assign(x = "Conn",
         dbConnect(SQLite(), "inst/shiny-examples/PesPila/data/PesPilaDB.db"),
         envir = .GlobalEnv)
	# T.data <- c()
  seasons <- "select * from Seasons"
  seasons <- dbGetQuery(conn = Conn, statement = seasons)[31:53,]
  dist <- "select * from Scored"
  dist <- dbGetQuery(conn = Conn, statement = dist)
  tmp <- rep(x = 0, times = nrow(dist))
  dist <- cbind(dist, "Win_Home" = tmp, "Draw_Home" = tmp, "Lost_Home" = tmp, "Dist" = tmp)
  cols <- colnames(dist)
  print(cols)

  for (i in 1:nrow(dist)) {
  	print(i)
  	cID <- dist[i, "Country_ID"]
  	sID <- dist[i, "Season_ID"]
  	tID <- dist[i, "Team_ID"]
  	print(c("Country", cID, "Season", sID, "Team", tID, "ID", i))
  	query <- paste0("select A.Country, B.Season, C.Team from Countries A, Seasons B, Teams C where A.Country_ID = '", cID, "' and B.Season_ID = '", sID, "' and C.Team_ID = '", tID, "'")
  	value <- dbGetQuery(conn = Conn, statement = query)

  	test.query <- paste0("select * from ", value[1, "Country"], " where HomeTeam = '", value[1, "Team"], "' and Season = '", value[1, "Season"], "'")
  	test.data <- dbGetQuery(conn = Conn, statement = test.query)
  	
  	Win <- length(which((test.data[, "HomeTeam"] == value[1, "Team"] & test.data[, "FTR"] == "H")))
  	Draw <- length(which((test.data[, "HomeTeam"] == value[1, "Team"] & test.data[, "FTR"] == "D")))
  	Lost <- length(which((test.data[, "HomeTeam"] == value[1, "Team"] & test.data[, "FTR"] == "A")))

  	dist[i, "Win_Home"] <- Win
  	dist[i, "Draw_Home"] <- Draw
  	dist[i, "Lost_Home"] <- Lost

  	cname <- c("Poisson_Pval", "ZIP_Pval", "Uniform_Pval", "Geometric_Pval", "NBD_Pval")
  	pval <- as.numeric(dist[i, cname])
  	iX <- which(pval == max(pval))
  	dist[i, "Dist"] <- cname[iX[1]]
  	# str <- strsplit(cname[iX], split = "_")[[1]]

  }

  dbDisconnect(conn = Conn)

  return (dist)
}

SetDist2 <- function() {
	assign(x = "Conn",
         dbConnect(SQLite(), "inst/shiny-examples/PesPila/data/PesPilaDB.db"),
         envir = .GlobalEnv)
	# T.data <- c()
  seasons <- "select * from Seasons"
  seasons <- dbGetQuery(conn = Conn, statement = seasons)[31:53,]
  dist <- "select * from Conceded"
  dist <- dbGetQuery(conn = Conn, statement = dist)
  cols <- colnames(dist)
  print(cols)
  dbDisconnect(conn = Conn)

  for (i in 1:nrow(dist)) {
  	print(i)
  	# cID <- dist[i, "Country_ID"]
  	# sID <- dist[i, "Season_ID"]
  	# tID <- dist[i, "Team_ID"]
  	# print(c("Country", cID, "Season", sID, "Team", tID, "ID", i))
  	# query <- paste0("select A.Country, B.Season, C.Team from Countries A, Seasons B, Teams C where A.Country_ID = '", cID, "' and B.Season_ID = '", sID, "' and C.Team_ID = '", tID, "'")
  	# value <- dbGetQuery(conn = Conn, statement = query)

  	# test.query <- paste0("select * from ", value[1, "Country"], " where HomeTeam = '", value[1, "Team"], "' and Season = '", value[1, "Season"], "'")
  	# test.data <- dbGetQuery(conn = Conn, statement = test.query)
  	
  	# Win <- length(which((test.data[, "HomeTeam"] == value[1, "Team"] & test.data[, "FTR"] == "H")))
  	# Draw <- length(which((test.data[, "HomeTeam"] == value[1, "Team"] & test.data[, "FTR"] == "D")))
  	# Lost <- length(which((test.data[, "HomeTeam"] == value[1, "Team"] & test.data[, "FTR"] == "A")))

  	# dist[i, "Win_Home"] <- Win
  	# dist[i, "Draw_Home"] <- Draw
  	# dist[i, "Lost_Home"] <- Lost

  	cname <- c("Poisson_Pval", "ZIP_Pval", "Uniform_Pval", "Geometric_Pval", "NBD_Pval")
  	pval <- as.numeric(dist[i, cname])
  	iX <- which(pval == max(pval))
  	dist[i, "Dist"] <- cname[iX[1]]
  	# str <- strsplit(cname[iX], split = "_")[[1]]

  }

  return (dist)
}