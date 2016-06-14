# Update the league input field in the league data tab. Copyright by Michael Bauer.
#
# Functions available:
# 	- UpdateLeagueL(session, country)

UpdateLeagueL <- function(session, country = "Germany") {
	# Get seasons in a given country for a given league.
	#
	#	session: The shiny session variable.
	#	country: A selected country (string).
	#
	#	Return: No return value.

	InitDB()
  divs <- GetLeagues(country = country)
  dbDisconnect(conn = ppConn)
  updateSelectInput(session = session, inputId = "leagueL",
                    choices = c("All", divs),
                    selected = divs[1])

}  # END UpdateLeagueL