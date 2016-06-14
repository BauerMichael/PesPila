# All possible teams from one country are selected from the database. Copyright by Michael Bauer.
#
# Functions available:
# 	- GetTeams(country)

GetTeams <- function(country = "Germany") {
	# Get teams of a given country.
	#
	#	country: A selected country (string).
	#
	#	Return: Returns a vector with all possible teams in a country.

	query <- paste0("select distinct HomeTeam from ", country)
	
	data <- dbGetQuery(conn = ppConn, statement = query)

	return (as.character(data[, "HomeTeam"]))

}  # END GetTeams