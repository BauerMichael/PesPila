# Home vs. Away table. Copyright by Michael Bauer.
#
# Functions available:
# 	- GetHomeVsAway(input, country, home, away)

GetHomeVsAway <- function(input, country = "Germany", home = "Bayern Munich", away = "Dortmund") {
	# Get the conditioned data from the database.
	#
	#	input: Input fields of the UI.
	#	country: A selected country (string).
	#	home: Home Team.
	#	away: Away Team.
	#
	#	Return: Returns a data.frame() with the corresponding home vs. away data.
  
	InitDB()
	
	query <- paste0("select * from ", country, " where HomeTeam = '", input$leagueTH, "' and AwayTeam = '", input$leagueTA, "'")
	
	data <- dbGetQuery(conn = ppConn, statement = query)
	
	dbDisconnect(conn = ppConn)

	return (data)

}  # END GetHomeVsAway