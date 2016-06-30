# Conditioned table for the league data site. Copyright by Michael Bauer.
#
# Functions available:
# 	- GetLeagueTable(country, league, season)

GetLeagueTable <- function(country = "Germany", league = "1. Bundesliga", season = "15/16") {
	# Get the conditioned data from the database.
	#
	#	country: A selected country (string).
	#	league: The selected league (string).
	#	season: The selected season (string).
	#
	#	Return: Returns a data.frame() with the corresponding league data.
  
	InitDB()
	
	query <- paste0("select * from ", country, " where Div = '", league, "' and Season = '", season, "'")
	
	if (league == "All" && season != "All") {query <- paste0("select * from ", country, " where Season = '", season, "'")}
	if (season == "All" && league != "All") {query <- paste0("select * from ", country, " where Div = '", league, "'")}
	if (league == "All" && season == "All") {query <- paste0("select * from ", country)}
	
	data <- dbGetQuery(conn = ppConn, statement = query)
	
	dbDisconnect(conn = ppConn)

	return (data)

}  # END GetLeagueTable