curl "https://fantasysupercontest.com/nfl-export?year=2008&format=csv" > 2008.csv
curl "https://fantasysupercontest.com/nfl-export?year=2009&format=csv" > 2009.csv
curl "https://fantasysupercontest.com/nfl-export?year=2010&format=csv" > 2010.csv
curl "https://fantasysupercontest.com/nfl-export?year=2011&format=csv" > 2011.csv
curl "https://fantasysupercontest.com/nfl-export?year=2012&format=csv" > 2012.csv
curl "https://fantasysupercontest.com/nfl-export?year=2013&format=csv" > 2013.csv
curl "https://fantasysupercontest.com/nfl-export?year=2014&format=csv" > 2014.csv
curl "https://fantasysupercontest.com/nfl-export?year=2015&format=csv" > 2015.csv


awk 'FNR==1 && NR!=1{next;}{print}' *.csv > spreads.csv