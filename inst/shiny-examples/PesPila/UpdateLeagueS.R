# Update the season input field in the league data tab. Copyright by Michael Bauer.
#
# Functions available:
# 	- UpdateLeagueS(session, country, league)

UpdateLeagueS <- function(session, country = "Germany", league = "1. Bundesliga") {
	# Get seasons in a given country for a given league.
	#
	#	Return: No return value.

	InitDB()
  seas <- rev(GetSeasons(country = country, league = league))
  dbDisconnect(conn = ppConn)
  updateSelectInput(session = session, inputId = "leagueS",
                    choices = c("All", seas),
                    selected = seas[1])

}  # END UpdateLeagueS


	