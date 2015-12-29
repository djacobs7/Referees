homeVAwayPenalties<-function(penalties){
  d = penalties %>%
    group_by( Home, Penalty) %>%
    summarize(n =  n() ) %>%
    spread( Home, n ) %>%
    mutate( pctHome = Yes / (No + Yes ))
  
  d$Penalty = reorder(d$Penalty, d$pctHome )
  
  ggplot( d, aes( x = Penalty, y = pctHome) ) +
    geom_bar( stat='identity') + 
    theme( 
      axis.text.x = element_text(angle = 90, hjust = 1)
    ) +
    geom_hline( yintercept = 0.5)
  
  
  d = penalties %>%
    group_by(Penalty) %>%
    summarize( n = n()) %>%
    arrange( n )
  penalties$Penalty = ordered( penalties$Penalty,d$Penalty )
  ggplot( penalties, aes( x = Penalty, fill = Home )) +
    geom_bar(position='dodge') +
    theme( 
      axis.text.x = element_text(angle = 90, hjust = 1)
      ) +
    ggtitle("Most frequent penalties")
  
  
  

  p = penalties %>%
    group_by(Penalty) %>%
    mutate(n =n ()) %>%
    filter(n > 1000)
  
  d = p %>%
    group_by( Home, Penalty) %>%
    summarize(n =  n() ) %>%
    spread( Home, n, fill = 0  ) %>%
    mutate( pctHome = Yes / (No + Yes )) %>%
    arrange(pctHome)
    
  p$Penalty = ordered( p$Penalty,d$Penalty )
  ggplot( p, aes( x = Penalty, fill = Home )) +
    geom_bar(position='fill') +
    theme( 
      axis.text.x = element_text(angle = 90, hjust = 1)
    ) +
    geom_hline(yintercept = 0.5) + 
    geom_jitter(aes(y = -0.05,  width = 0.13, color=Home) , height = 0.1, alpha = 0.05) + 
    ggtitle("Most frequent penalties at home")
  
}


delayOfGameByStadium <- function( penalties) {
  
  p = penalties %>%
    filter( Penalty == "Delay of game") %>%
    filter( Home == "No") 
  
  d = p %>%
    group_by(Opp) %>%
    summarize( n = n() ) %>%
    arrange(n )
  
  d$Opp = ordered(d$Opp, d$Opp)
  
  ggplot( d, aes( x = Opp, y = n  ) )  +
    geom_bar(stat='identity' ) + 
    theme( 
      axis.text.x = element_text(angle = 90, hjust = 1)
    ) +
    ggtitle("False Starts called on the away team at each stadium")
} 