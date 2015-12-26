library(XML)
library(rvest)
library(RCurl)

url="http://www.pro-football-reference.com/officials/"

refTable=readHTMLTable(url,skip.rows=c(1,298))  #get rid of the extra label row and blank name
refTable=as.data.frame(refTable) #change type of the table to data frame
refNames=refTable$Name
#_____________Name Cleaning

#find the number of strings separated by spaces in x.  (Will only want to keep those of size two, so link standard remains clean)
filterNames<-function(x) {
  x=as.character(x)
  return (length(unlist(strsplit(x," "))))
}


nameLengths=unlist(lapply(refNames,filterNames))
refNames=refNames[which(nameLengths==2)] #Only keeps the names that have #number of strings=2
#do we need to handle all capital names differently?

#______________Name Cleaning complete


#______________generating the urls

#generate the link names (first four letters of last name, followed by first two letters of first name)
genLinkNames<-function (x) {
  
  
  x=as.character(x)
  print(x)
  split=unlist(strsplit(x," "))   #the unlist operation makes the returned value a useable vector
  first=split[1]
  last=split[2]
  
  fourL=substr(last,1,4)
  twoF=substr(first,1,2)
  link=paste("www.pro-football-reference.com/officials/",fourL,twoF,"0r.htm",sep="")
  return (link)
}

linkNames=unlist(lapply(refNames,genLinkNames))  

NameLinkFrame=data.frame(refNames,linkNames)  #referee name is one column.  Link associated with the referee is another column

#________________generating the urls complete

#________________generating the data

season_totals=data.frame()
game_logs=data.frame()


i=0
for (link in linkNames) {
  url=getURL(link)
  if (nchar(url)>0 )  {           #the page actually exists. 
    tables=readHTMLTable(url) 
    if (length(tables)==2) {     #there is season totals and a game log (so the capitalized names are excluded)
      
      refName=NameLinkFrame[NameLinkFrame$linkNames==link,1]
      print(refName)
      
      
      #generate season totals________________________
      sttemp=tables$season_totals  #get table for temp usage
      lastRow=dim(sttemp)[1] #find the index of the totals row
      seas_tots=tables$season_totals[-c(1,2,lastRow),]  #get seas_totals without garbage rows
      #______________________________________________
      
      
      #generate game log_______________
      gltemp=tables$game_logs #get table 
      game_log=tables$game_logs[-c(which(gltemp$Game=="Game")),] #get game logs without garbage rows
      #_________________________________
      
      
     
      #handling season totals___________
      rowsSt=dim(seas_tots)[1]
      nameVec=rep(refName,rowsSt)
      seas_tots=cbind(seas_tots,nameVec)
      print(seas_tots)
      #handling season totals end______
      
      
      #handling game logs _________________
      rowsGl=dim(game_log)[1]
      if (is.null(rowsGl)) {rowsGl=0}
      nameVec=rep(refName,rowsGl)
      game_log=cbind(game_log,nameVec)
      print(game_log)
      #handling game logs end_________
      
      #add to data frames
      season_totals=rbind(season_totals,seas_tots)
      game_logs=rbind(game_logs,game_log)
      i=i+1
      print(paste("Ref finished",i,sep=" "))
        
      
    }
  }
  
}

saveRDS(game_logs,file="~/Desktop/sports analysis/Referees/Data/gameLogs.Rda")

saveRDS(season_totals,file="~/Desktop/sports analysis/Referees/Data/seasonTotals.Rda")

#things to do:
#clean up this script. So Everyone knows what is going on.
#send gameLogs and seasonTotals to Daniel
#Next things with the data: Go through each column and figure out what it means.  Change data types as appropriate. In game logs separate the team names to home and away columns.
#Do row cleaning as nescessary









