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
