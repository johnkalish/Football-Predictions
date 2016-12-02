predict <- function(homeTeam,awayTeam)
  {
  curYear = 2016
lastYear = curYear - 1
twoYearsAgo = curYear - 2

`data` <- read.csv("~/R/football/2014-2015dataprocessedwk3.csv")

homeCurSeason <- `data`[`data`$Year==curYear & `data`$Team==homeTeam,]
homeVsAwayCurSeason <- `data`[`data`$Year==curYear & `data`$Team==homeTeam & `data`$Opponent==awayTeam,]

homeLastSeason <- `data`[`data`$Year==lastYear & `data`$Team==homeTeam,]
homeVsAwayLastSeason <- `data`[`data`$Year==lastYear & `data`$Team==homeTeam & `data`$Opponent==awayTeam,]

homeTwoSeasonsAgo <- `data`[`data`$Year==twoYearsAgo & `data`$Team==homeTeam,]
homeVsAwayTwoSeasonsAgo <- `data`[`data`$Year==twoYearsAgo & `data`$Team==homeTeam & `data`$Opponent==awayTeam,]

awayCurSeason <- `data`[`data`$Year==curYear & `data`$Team==awayTeam,]
awayVsHomeCurSeason <- `data`[`data`$Year==curYear & `data`$Team==awayTeam & `data`$Opponent==homeTeam,]

awayLastSeason <- `data`[`data`$Year==lastYear & `data`$Team==awayTeam,]
awayVsHomeLastSeason <- `data`[`data`$Year==lastYear & `data`$Team==awayTeam & `data`$Opponent==homeTeam,]

awayTwoSeasonsAgo <- `data`[`data`$Year==twoYearsAgo & `data`$Team==awayTeam,]
awayVsHomeTwoSeasonsAgo <- `data`[`data`$Year==twoYearsAgo & `data`$Team==awayTeam & `data`$Opponent==homeTeam,]

homeScoredPointsValues <- c(rep(homeVsAwayCurSeason$Pts,6)
                      ,rep(homeCurSeason$Pts,3)
                      ,rep(homeVsAwayLastSeason$Pts,4)
                      ,rep(homeLastSeason$Pts,2)
                      ,rep(homeVsAwayTwoSeasonsAgo$Pts,2)
                      ,rep(homeTwoSeasonsAgo$Pts,1)
)

homeYieldedPointsValues <- c(rep(homeVsAwayCurSeason$OppPts,6)
                            ,rep(homeCurSeason$OppPts,3)
                            ,rep(homeVsAwayLastSeason$OppPts,4)
                            ,rep(homeLastSeason$OppPts,2)
                            ,rep(homeVsAwayTwoSeasonsAgo$OppPts,2)
                            ,rep(homeTwoSeasonsAgo$OppPts,1)
)

awayScoredPointsValues <- c(rep(awayVsHomeCurSeason$Pts,6)
                            ,rep(awayCurSeason$Pts,3)
                            ,rep(awayVsHomeLastSeason$Pts,4)
                            ,rep(awayLastSeason$Pts,2)
                            ,rep(awayVsHomeTwoSeasonsAgo$Pts,2)
                            ,rep(awayTwoSeasonsAgo$Pts,1)
)

awayYieldedPointsValues <- c(rep(awayVsHomeCurSeason$OppPts,6)
                            ,rep(awayCurSeason$OppPts,3)
                            ,rep(awayVsHomeLastSeason$OppPts,4)
                            ,rep(awayLastSeason$OppPts,2)
                            ,rep(awayVsHomeTwoSeasonsAgo$OppPts,2)
                            ,rep(awayTwoSeasonsAgo$OppPts,1)
)

homeExpectedScoredPoints <- sample(homeScoredPointsValues,1)
homeExpectedYieldedPoints <- sample(homeYieldedPointsValues,1)
awayExpectedScoredPoints <- sample(awayScoredPointsValues,1)
awayExpectedYieldedPoints <- sample(awayYieldedPointsValues,1)
return (
  paste(
    c(homeTeam,homeExpectedScoredPoints,homeExpectedYieldedPoints,awayTeam,awayExpectedScoredPoints,awayExpectedYieldedPoints),collapse=",")
  )
}