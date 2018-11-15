#$13

###Man of the match:

##No. of player and thier list which win MOM

#No. of player win Man of the match in IPL in all Seasons:
match_df$player_of_match
na.omit(match_df$player_of_match)->MOM
unique(MOM)->MOM1
print("No. of player win Man of the match in IPL in all Seasons:")
length(MOM1)
#Player Name which  won Man of the Match (2008-2017)
print("Player Name which  won Man of the Match (2008-2017)")
MOM1

#Player Name and No. of Man of the match win in IPL(2008-2017)
match_df%>%count(match_df$player_of_match)->Pl_MOM
as.data.frame(Pl_MOM)->Pl_MOM
colnames(Pl_MOM)<-c("Player Name","No. Of Awards")
print("Player Name and No. of Man of the match win in IPL(2008-2017):")
Pl_MOM

#player who won most MOM
Pl_MOM[order(Pl_MOM$`No. Of Awards`,decreasing = TRUE),]->Most_MOM
print("Player who won most MOM :")
Most_MOM[1,]


#Top 5 Player according to MOM win:
print("Top 5 Player according to MOM win:")
Most_MOM[1:5,]->MOMT5_GRP
as.data.frame(MOMT5_GRP)->MOMT5_GRP
ggplot(MOMT5_GRP,aes(x=MOMT5_GRP$`Player Name`,y=MOMT5_GRP$`No. Of Awards`,fill=MOMT5_GRP$`Player Name`))+
  geom_bar(stat = "identity",position="dodge")+theme(axis.text.x = element_text(angle = 45, hjust = 1))+xlab("Player Name")+
  ylab("No. Of Awards")+ggtitle("Top 5 Player According to Man Of The Match win:(2008-2017)")+theme(plot.title = element_text(face = "bold.italic"))+geom_point()
