#$5

####Team wise data Analysis
###...Dataset  userd:matches...Dataframe Name:match_df

##Table:

as.data.frame(table(matches$team2) + table(matches$team1))->match_team_season
colnames(match_team_season)<-c("TeamName","NumberofMatches")
match_team_season

#Graph:

ggplot(match_team_season,aes(reorder(TeamName,-NumberofMatches),NumberofMatches,fill = TeamName))+
  geom_bar(stat = "identity",position="dodge")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  xlab("Team")+ theme(axis.text = element_text(colour = "blue"))+theme(axis.ticks = element_line(size = 2))+
  ylab("Number of Matches") +theme(axis.line = element_line(size = 2, colour = "grey80"))+ 
  theme(axis.text = element_text(colour = "blue"))+
  ggtitle("Team wise data Analysis")+ 
  theme(plot.title = element_text(face = "bold.italic"))+geom_point()



