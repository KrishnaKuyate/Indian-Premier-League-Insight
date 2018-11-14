#$7

####Team Match Win Analysis

install.packages("plotrix")
library(plotrix)

###Team and no. of mataches win in all seasons

paste0("No. of teams win mataches in all seasons(2008-2017) is :",length(unique(na.omit(match_df$winner))))
paste0("Team List which won matches in all seasons:")
unique(na.omit(match_df$winner))

##Team wise match win count in all seasons

#Table:
match_df%>%count(match_df$winner)->match_win
as.data.frame(match_win)->match_win1
na.omit(match_win1)->match_win2
colnames(match_win2)<-c("Team_name","Match_Win")
paste0("Team wise match win count in all seasons:")
match_win2

#Plot:


par(col.main="blue")
bisectors<-pie3D(match_win2$Match_Win,radius=0.8,height=0.1,theta=pi/3,main="Team wise match win count in all seasons",labels=match_win2$Match_Win,explode=0.2,shade=0.6)
legend("topright",legend=match_win2$Team_name,fill=match_win2$Match_Win,cex = 0.4)




