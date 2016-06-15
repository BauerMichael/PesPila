# Update the league input field in the league data tab. Copyright by Michael Bauer.
#
# Functions available:
# 	- UpdateLeagueT(session, country)

UpdateLeagueT <- function(session, country = "Germany") {
	# Get seasons in a given country for a given league.
	#
	#	session: The shiny sesssion variable.
	#	country: A selected country (string).
	#
	#	Return: No return value.

	InitDB()
  teams <- GetTeams(country = country)
  dbDisconnect(conn = ppConn)
  updateSelectInput(session = session, inputId = "leagueTH",
                    choices = teams,
                    selected = teams[1])
  updateSelectInput(session = session, inputId = "leagueTA",
                    choices = teams,
                    selected = teams[2])

}  # END UpdateLeagueT