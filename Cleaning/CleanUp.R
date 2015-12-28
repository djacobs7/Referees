library('tidyr')
library('dplyr')

#hit source on Save.  Then Everytime we save the file, the entire script runs

PROJECT_ROOT = "/Users/djacobs7/git/Referees"

localPath <- function(path){
  paste0( PROJECT_ROOT, '/', path)
}

gameLogsRaw=readRDS(localPath("/Data/gameLogs.Rda"))
seasonTotalsRaw=readRDS(localPath("/Data/seasonTotals.Rda"))


cleanupGameLogs <- function( gameLogs ){
  result = gameLogs %>%
    separate( Game, into=c('away', 'home'), sep=' @ ' ) %>% #tidyr function sepearate.  Take a factor variable (game), turn it into two columns [away and home]
    rename( name = nameVec, date = Year) #dplyr function.  rename the nameVec column to name
  result$away = as.factor(result$away)
  result$home = as.factor(result$home)
  
  result$VPts = as.numeric(result$VPts)
  result$VPen = as.numeric(result$VPen)
  result$VPYds = as.numeric(result$VPYds)
  
  result$HPts = as.numeric(result$HPts)
  result$HPen = as.numeric(result$HPen)
  result$HPYds = as.numeric(result$HPYds)
  
  result$date = as.Date(result$date)
   
  result
}

#mutate and mutate_each are dplyr
#separate is tidyr


cleanupSeasonTotals <- function( seasonTotals ){
  result = seasonTotals
  #Some of the columns have the same name, so we add prefixes.
  names(result)[12:15] = paste( "League_Average" , names(seasonTotals)[12:15], sep="_")
  names(result)[16:19] = paste( "Relative" , names(seasonTotals)[16:19], sep="_")
  
  result = result %>%
    #Rename columns with wierd characters in them
    rename( HomePct = `Home%`, HomeWinPct = `HWin%`, PenPerG = `Pen/G`, YdsPerG = `Yds/G`,
            League_Average_HomePct = `League_Average_Home%`, League_Average_HomeWinPct = `League_Average_HWin%`, League_Average_PenPerG = `League_Average_Pen/G`, League_Average_YdsPerG = `League_Average_Yds/G`,
            Relative_HomePct = `Relative_Home%`, Relative_HomeWinPct = `Relative_HWin%`, Relative_PenPerG = `Relative_Pen/G`, Relative_YdsPerG = `Relative_Yds/G`,
            name = nameVec
            ) %>%
    
    mutate_each( funs(as.numeric(.)), HPen, VPen, Tot, Yds, ends_with('PenPerG'), ends_with('YdsPerG'), ends_with('HomePct'), ends_with('HomeWinPct')) %>%
    #the above line takes the columns listed and converts them all to numeric.  First argument->use a function with a dot as the argument is funs
    separate( G, into=c("games", "playoff_games"), sep=' ', extra='drop') %>% #G is originally games and playoff games.  Not all refs have playoff games.  This will crash if no second argument, unless we have extra=drop
    mutate( playoff_games = gsub( "\\(", "", playoff_games)) %>%  #mutate can create a a column as function of other columns.
    mutate( playoff_games = gsub( "\\)", " ", playoff_games)) %>%  #this gsub for replacing a character.  In replacing a parentheses, we double slash in front for regexp. First argument is character to replace, second is character to put in string, third argument is the string to operate on.
    separate(playoff_games, into=c("playoff_games", "is_superbowl"), sep=' ', extra='drop' ) %>%
    mutate( playoff_games = as.numeric(playoff_games ) ) %>%
    mutate( playoff_games = ifelse( is.na(playoff_games) , 0, playoff_games ) ) %>%
    mutate( is_superbowl =  is_superbowl == "*" ) %>%    
    mutate( is_superbowl =  ifelse( is.na(is_superbowl), FALSE, is_superbowl ) ) %>%    
    mutate( games = as.numeric(games) )

  result
}

gameLogs = cleanupGameLogs( gameLogsRaw )
seasonTotals = cleanupSeasonTotals( seasonTotalsRaw )

