# Joe Walsh
# May 1, 2013
# Chicago R User's Group Example
# Is there a relationship between NHL team fights and points?



######################
## SHOWN IN THE PRESENTATION
# install and load scrapeR package
install.packages("scrapeR", dependencies=TRUE)
library(scrapeR)

# scrape data
URLs <- c('http://www.nhl.com/ice/standings.htm?type=lea#&navid=nav-stn-league',
      'http://www.hockeyfights.com/leaders/teams/')
pageSource <- scrape(url=URLs, headers=FALSE, parse=TRUE)

# get two tables with info we need
NHL.tables <- readHTMLTable(pageSource[[1]])
  team.records <- NHL.tables[[3]]
  head(team.records)
HockeyFights.tables <- readHTMLTable(pageSource[[2]])
  team.fights <- HockeyFights.tables[[1]]
  head(team.fights)




######################
## NOT SHOWN IN PRESENTATION

## NHL.com and HockeyFights.com use different team names.
## Here I match them and include three-character codes for the plot.
teams <- matrix(c("Anaheim Ducks",		"Anaheim",		"ANA",
			"Boston Bruins",		"Boston",		"BOS",
			"Buffalo Sabres",		"Buffalo",		"BUF",
			"Calgary Flames",		"Calgary",		"CGY",
			"Carolina Hurricanes",	"Carolina",		"CAR",
			"Chicago Blackhawks",	"Chicago",		"CHI",
			"Colorado Avalanche",	"Colorado",		"COL",
			"Columbus Blue Jackets","Columbus",		"CBJ",
			"Dallas Stars",		"Dallas",		"DAL",
			"Detroit Red Wings",	"Detroit",		"DET",
			"Edmonton Oilers",	"Edmonton",		"EDM",
			"Florida Panthers",	"Florida",		"FLA",
			"Los Angeles Kings",	"Los Angeles",	"LAK",
			"Minnesota Wild",		"Minnesota",	"MIN",
			"Montreal Canadiens",	"Montréal",		"MTL",
			"Nashville Predators",	"Nashville",	"NSH",
			"New Jersey Devils",	"New Jersey",	"NJD",
			"New York Islanders",	"NY Islanders",	"NYI",
			"New York Rangers",	"NY Rangers",	"NYR",
			"Ottawa Senators",	"Ottawa",		"OTT",
			"Philadelphia Flyers",	"Philadelphia",	"PHI",
			"Phoenix Coyotes",	"Phoenix",		"PHX",
			"Pittsburgh Penguins",	"Pittsburgh",	"PIT",
			"San Jose Sharks",	"San Jose",		"SJS",
			"St. Louis Blues",	"St. Louis",	"STL",
			"Tampa Bay Lightning",	"Tampa Bay",	"TBL",
			"Toronto Maple Leafs",	"Toronto",		"TOR",
			"Vancouver Canucks",	"Vancouver",	"VAN",
			"Washington Capitals",	"Washington",	"WAS",
			"Winnipeg Jets",		"Winnipeg",		"WPG"),
		    byrow=TRUE, ncol=3)
colnames(teams) <- c("hockeyfights", "nhl", "abbrev")



## NHL modified team names to reflect President's Trophy, etc.  Delete that info.
team.records[,2] <- gsub('x - ','',team.records[,2]) #remove 'clinched playoff spot'
team.records[,2] <- gsub('y - ','',team.records[,2]) #remove 'clinched division'
team.records[,2] <- gsub('z - ','',team.records[,2]) #remove 'clinched conference'
team.records[,2] <- gsub('p - ','',team.records[,2]) #remove 'clinched Presidents Trophy'

## match points and team
points.position <- match(as.character(team.records[,2]), as.character(teams[,2]))  #where is team on static list of teams?
points <- rep(NA, length(points.position))
points[points.position] <- as.numeric(as.character(team.records$" P "))

## match fights and team
fight.position <- match(as.character(team.fights$Team), as.character(teams[,1]))  #where is team on static list of teams?
fights <- rep(NA, length(fight.position))
fights[fight.position] <- as.numeric(as.character(team.fights$"Fight Totals"))

## match abbreviation and team
team.abbrevs <- teams[,3]

## combine into one data frame
data <- data.frame(team.abbrevs, fights, points)
  colnames(data) <- c("abbrev","fights","points")

## some of the abbreviations overlap on the plot.  Adjust position.
text.data <- data
text.data[3,2] <- 23.5
  text.data[3,3] <- 49
text.data[4,2] <- 20.25
  text.data[4,3] <- 43
text.data[16,2] <- 20.5
  text.data[16,3] <- 40.5
text.data[29,2] <- 14.5
  text.data[29,3] <- 58


pdf("cherry hypothesis.pdf")
  par(mar=c(4.1,4,1,1), cex.axis=1.2, cex.lab=1.2)
    plot(as.numeric(as.character(data$fights[-10])), 
	   as.numeric(as.character(data$points[-10])),
	   xlim=c(min(as.numeric(as.character(data$fights))), max(as.numeric(as.character(data$fights)))+1.5),
	   xlab='fights', ylab='points', pch=20)
    points(data[10,2], data[10,3], pch=20, col='red')  #plot Detroit in red
    text(as.numeric(as.character(text.data$fights[-10])), 
	   as.numeric(as.character(text.data$points[-10])),
	   labels=as.character(text.data$abbrev[-10]), pos=4, cex=.7)
    text(as.numeric(as.character(text.data$fights[10])),  #Detroit text in red
	   as.numeric(as.character(text.data$points[10])),
	   labels=as.character(text.data$abbrev[10]), pos=4, cex=.7, col='red')

  reg <- lm(as.numeric(as.character(data$points))~as.numeric(as.character(data$fights)))
  abline(reg)
dev.off()
