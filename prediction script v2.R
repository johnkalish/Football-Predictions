library("dplyr", lib.loc="~/R/win-library/3.3")
##setwd("~/R/football")
curYear = 2016
curWeek = 13
lastYear = curYear - 1
twoYearsAgo = curYear - 2



loadhistory <- function(year)
{
  filename <- paste(year,"seasonraw.csv", sep="")
  `data` <- read.csv(filename)
  `data`$Year <- year
  `data` <- mutate(`data`, Winner.HomeAway = ifelse(X %in% c("@","N"), "A", "H"))
  `data` <- mutate(`data`, Loser.HomeAway = ifelse(X %in% c("@"), "H", "A"))
  
  `winners` <-  subset(`data`, select=c("Year", "Winner.tie","Loser.tie","Winner.HomeAway","PtsW","PtsL"))
  `winners` <- mutate(`winners`, Result = ifelse(PtsW == PtsL,"T","W")) 
  names(`winners`) <- c("Year","Team","OppTeam","HomeAway","Pts","OppPts","Result")
  
  `losers` <-  subset(`data`, select=c("Year", "Loser.tie","Winner.tie","Loser.HomeAway","PtsL","PtsW"))
  `losers` <- mutate(`losers`, Result = ifelse(PtsW == PtsL,"T","L"))
  names(`losers`) <- c("Year","Team","OppTeam","HomeAway","Pts","OppPts","Result")
  
  `output` <- rbind(`winners`,`losers`)
  return (`output`)
  
}

loadschedule <- function(year=curYear,week=curWeek)
{
  filename <- paste(year,"schedule.csv", sep="")
  `data` <- read.csv(filename)
  `output` <- subset(`data`, `data`$Week == week)
  return (`output`)
}

predict <- function(homeTeam,awayTeam,week,data=`history`)
{
  
  
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
  
  homeExpectedScoredPoints <- sample(homeScoredPointsValues,7)
  homeExpectedYieldedPoints <- sample(homeYieldedPointsValues,7)
  awayExpectedScoredPoints <- sample(awayScoredPointsValues,7)
  awayExpectedYieldedPoints <- sample(awayYieldedPointsValues,7)
  
  homeExpectedScores <- (homeExpectedScoredPoints + awayExpectedYieldedPoints)/2
  awayExpectedScores <- (awayExpectedScoredPoints + homeExpectedYieldedPoints)/2
  
  avgspread <- abs(mean(homeExpectedScores) - mean(awayExpectedScores))
  
  expectedScoreTotals <- homeExpectedScores + awayExpectedScores
  
  homeTeamNetScores <- homeExpectedScores - awayExpectedScores
  
  homeTeamWins <- homeTeamNetScores >= 0
  homeTeamWinCount <- length(subset(homeTeamWins,homeTeamWins))
  
  awayTeamWins <- homeTeamNetScores < 0 
  awayTeamWinCount <- length(subset(awayTeamWins,awayTeamWins))
  
  outcome <- ifelse(homeTeamWinCount > 3,homeTeam,awayTeam)
  
  outcomeCount <- ifelse(homeTeamWinCount >3,homeTeamWinCount,awayTeamWinCount)
  
  outcomesample <- ! xor(homeTeamWins,homeTeamWinCount >3)
  
  expectedScoreTotalsSample <- sample(subset(expectedScoreTotals,outcomesample),1)
  
  
  return (
    
    c(homeTeam,awayTeam,outcome,outcomeCount,avgspread,expectedScoreTotalsSample)
  )
}

`history` <- rbind(loadhistory(curYear),loadhistory(lastYear),loadhistory(twoYearsAgo))
`schedule` <- loadschedule()
results <- apply(`schedule`[,c('HomeTeam','AwayTeam')], 1, function(x) paste(predict(x[1],x[2]),sep=","))
View(results)
