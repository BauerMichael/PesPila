select
	(select count(FTHG) from Germany where HomeTeam = 'Aalen' and FTHG = '0' and Season = '15/16') + 
	(select count(FTAG) from Germany where AwayTeam = 'Aalen' and FTAG = '0' and Season = '15/16')
as SumCount

select * from Germany where HomeTeam = 'Aalen' and Season = '15/16'

select (select count(FTHG) from Germany where HomeTeam = 'Bayern Munich' and FTHG = 0  and Season = '93/94') + (select count(FTAG) from Germany where AwayTeam = 'Bayern Munich' and FTAG = '0' and Season = '93/94') as SumCount

select A.* from dist A, Seasons B, Teams C where B.Season = '15/16' and A.Season_ID = B.Season_ID and C.Team = 'Bayern Munich' and A.Team_ID = C.Team_ID