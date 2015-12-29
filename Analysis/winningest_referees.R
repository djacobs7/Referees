library('ggplot2')
library('scales')
plotHomeVersusVisitorPenalties <- function( gameLogs ){
  d = gameLogs %>% 
    group_by ( name ) %>%
    summarize(n = n(), totalHPen = sum(HPen), totalVPen = sum(VPen ) , meanHPen = mean(HPen ), meanVPen = mean(VPen ) ) %>%
    mutate( homePenaltyPct = totalHPen / ( totalHPen + totalVPen ) ) %>%
    filter(n > 10 ) %>%
    arrange( homePenaltyPct )

  
  ggplot( d, aes( x =  homePenaltyPct) ) +
    geom_histogram(binwidth = 0.003) +
    ylab("Num referees") + 
    scale_x_continuous(labels = percent)  +
    geom_vline( aes( xintercept = 0.5)) + 
    xlab( "HomePenaltyPct") +
    geom_text( label ="why does the home team have less penalties caused? \n who are the outlier referees?", x = 0.48, y = 10 )
    
}

plotHomeVersusVisitorPenaltiesPerTeam <- function( gameLogs ){
  d = gameLogs %>% 
    group_by ( home, name ) %>%
    summarize(n = n(), totalHPen = sum(HPen), totalVPen = sum(VPen ) , meanHPen = mean(HPen ), meanVPen = mean(VPen ) ) %>%
    mutate( homePenaltyPct = totalHPen / ( totalHPen + totalVPen ) ) %>%
    mutate( meanPenaltyPct = mean( homePenaltyPct)) %>%
    filter(n > 3 ) %>%
    arrange(( homePenaltyPct) )
  
  d$home = reorder( d$home, d$meanPenaltyPct)
  
  ggplot( d, aes( x =  homePenaltyPct) ) +
    geom_histogram(binwidth = 0.003) +
    facet_wrap(  ~home  , ncol = 2, nrow = 16 ) + 
    ylab("Num referees") + 
    scale_x_continuous(limits= c(0.4,0.5), labels = percent)  +
    geom_vline( aes( xintercept = meanPenaltyPct ), color = 'blue') + 
    xlab( "HomePenaltyPct") +
    theme(
      axis.text.y = element_blank(),
      axis.ticks.y = element_blank()
    )
}


plotMostLopsidedGames <- function(gameLogs){
  d = gameLogs %>%
    group_by( date, home, away ) %>%
    mutate(PenaltyYdsDiff = (VPYds - HPYds)  ) %>%
    summarize( PenaltyYdsDiff = first(PenaltyYdsDiff )) %>%
    ungroup() %>%
    arrange(abs( PenaltyYdsDiff ) )
  
  
  d = gameLogs %>%
    mutate(PenaltyYdsDiff = abs(VPYds - HPYds)  ) %>%
    arrange( PenaltyYdsDiff ) %>%
    mutate( rnk = dense_rank( PenaltyYdsDiff ) ) %>%
    group_by ( name ) %>%
    summarize(  n = n(), min(rnk), max(rnk), median(rnk), mean_rnk =  mean(rnk) ) %>% 
    filter( n > 5 ) %>%
    arrange( desc(mean_rnk))
  
  ggplot( d, aes( x = mean_rnk)) + 
    geom_histogram()
}