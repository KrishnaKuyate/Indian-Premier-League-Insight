#$16

###Wicket Analysis:
deliveries_df$dismissal_kind

#Total no. of wickets in all seasons:
na.omit(deliveries_df$dismissal_kind)->Total_wicket
length(Total_wicket)
paste0("Total no. of wickets in all seasons:",length(Total_wicket))

##Wicket Type wise stats:
deliveries_df%>%count(deliveries_df$dismissal_kind)->wicket_type_c
na.omit(wicket_type_c)->wicket_type_c
colnames(wicket_type_c)<-c("Wicket Type","Total Count")
#Table:
wicket_type_c
kable(wicket_type_c)
#graph:
ggplot(wicket_type_c,aes(x=`Wicket Type`,y=`Total Count`))+ggtitle("IPL Wicket Type and Total Count")+xlab("Wicket Type")+ylab("Total Count")+theme(plot.title = element_text(size = 20,color = "Orange"),axis.title = element_text(size = 18,color = "green"),axis.text = element_text(size = 15,color = "yellow",angle = 45,hjust = 1))+geom_point(col="tomato2", size=3)+coord_flip()+geom_segment(aes(x=`Wicket Type`, xend=`Wicket Type`, y=min(`Total Count`), yend=max(`Total Count`)), linetype="dashed", size=0.3)



##Wickets and Teams (2008-2017)
deliveries_df$bowling_team
deliveries_df$player_dismissed
data.frame("Bowling Team"=deliveries_df$bowling_team,"Player Out"=deliveries_df$player_dismissed)->Team_wicket
na.omit(Team_wicket)->Team_wicket
Team_wicket%>%count(Team_wicket$Bowling.Team)->Team_wicket1
as.data.frame(Team_wicket1)->Team_wicket1
colnames(Team_wicket1)<-c("Team","Total Wicket")

Team_wicket1

#Table:
kable(Team_wicket1)

#Graph:
ggplot(Team_wicket1,aes(x=Team_wicket1$Team,y=Team_wicket1$`Total Wicket`,fill=Team))+ geom_bar(stat = "identity",position="dodge")+xlab("Team")+ylab("Total Wicket")+ggtitle("Team vs Wickets")+theme(plot.title = element_text(size=25,colour = "pink"),axis.title =element_text(size = 18),axis.text = element_text(hjust = 1))+coord_flip()


###Bowler and wickets:

unique(deliveries_df$dismissal_kind)
data.frame(deliveries_df$bowler,deliveries_df$dismissal_kind)->Bw_wk
na.omit(Bw_wk)->Bw_wk
Bw_wk$deliveries_df.dismissal_kind
filter(Bw_wk,Bw_wk$deliveries_df.dismissal_kind %in% c("caught","bowled","lbw","caught and bowled"))->bw_wk1
bw_wk1%>%count(bw_wk1$deliveries_df.bowler)->bw_wk2
bw_wk2%>%arrange(desc(bw_wk2$n))->bw_wk3
bw_wk3[1:5,]->bw_wk4
colnames(bw_wk4)<-c("Bowlers Name","Total Wickets")
##Top 5 Bowlers:
#Table:
bw_wk4
kable(bw_wk4)


#Graph:
ggplot(bw_wk4,aes(x=bw_wk4$`Bowlers Name`,y=bw_wk4$`Total Wickets`,fill=`Bowlers Name`))+ geom_bar(stat = "identity",position="dodge")+xlab("Bowler Name")+ylab("Total Wicket")+ggtitle("Top 5 Bowlers " )+theme(plot.title = element_text(size=25,colour = "pink"),axis.title =element_text(size = 18),axis.text = element_text(angle=45,hjust = 1))


