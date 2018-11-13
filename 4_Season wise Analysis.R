#$4

#####Season wise Analysis
###...Dataset  userd:matches...Dataframe Name:match_df

library(dplyr)
library(ggplot2)

##Total No. of Seasons and names
print(c("Total number of Season:",length(unique(match_df$season))))
print("Seasons are:")
unique(match_df$season)

##Total No. matches
print(c("Total number of matches in all seasons(2008-2016):",length(unique(match_df$id))))

##Total No. of Team in all seasons
print("Total number of Teams in all seasons(2008-2016):")
length(unique(c(match_df$team1,match_df$team2)))
print("Team List:")
unique(c(match_df$team1,match_df$team2))

##Total no. of Cities
print("No. of cities where Matches played in all Seasons")
length(unique(na.omit(match_df$city)))
print("cities where Matches played in all Seasons")
unique(na.omit(match_df$city))

##Ground Details 
print("Total no. of Grounds used in all seasons(2008-2016):")
length(unique(na.omit(match_df$venue)))
print("Ground List:")
unique(na.omit(match_df$venue))

#Umpires Details:
na.omit(match_df$umpire1)->um1
na.omit(match_df$umpire2)->um2
c(um1,um2)->um3
print("Total No. of umpirs in all seasons(2008-2016)")
length(unique(um3))
print("Umpirs List for all seasons:")
unique(um3)


#Seasons wise matches Analysis :
match_df%>%tally()
match_df%>%count(season)->sea_ma
as.data.frame(sea_ma)->sea_ma1
colnames(sea_ma1)<-c("Seasons","Match_count")
sea_ma1
par(bg="yellow",bty="u",fg="blue",las=2,lty=2,mar=c(3, 4, 4, 2) + 0.1.)
plot(sea_ma1,main="Seasons wise Match Played",xlab="Years",ylab="Total Matches",col="red",type="l",col.main="red")










