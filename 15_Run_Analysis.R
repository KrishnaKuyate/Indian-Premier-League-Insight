#$15

library(knitr)
library(ggplot2)

#####Run :
data.frame("Team_name"=deliveries_df$batting_team,"Team_Run"=deliveries_df$total_runs)->team_run_df
team_run_df%>%group_by(Team_name)%>%summarise(total_run=sum(Team_Run))%>%arrange(desc(total_run))->team_run_df
bye 
##Team which scored maximum run :
team_run_df[1,]
kable(team_run_df[1,])

##Team which scored minimum run :
team_run_df[order(team_run_df$total_run),][1,]->mean_team_run
kable(mean_team_run)

##Team and Run Table:
team_run_df
kable(team_run_df)


##Team and Run (Graphical Presentation)
ggplot(team_run_df,aes(x=Team_name,y=total_run,fill=Team_name))+geom_bin2d(stat = "bin2d",position = "identity")+theme(axis.text = element_text(angle = 60,hjust = 1,color = "blue"))+xlab("Team")+
  ylab("Team Total Run")+theme(axis.title =element_text(colour = "green"))+geom_point(color="white")+ggtitle("Team Vs Run Analysis")


#####Extra Run Analysis:

##Total super over run :
deliveries_df$is_super_over
sum(deliveries_df$is_super_over)->total_super_over_run
paste0("Total super over run :",total_super_over_run)

##Total wide run:
deliveries_df$wide_runs
sum(deliveries_df$wide_runs)->Total_Wide_Run
paste0("Total wide run:",Total_Wide_Run)


##Total bye run
deliveries_df$bye_runs
sum(deliveries_df$bye_runs)->Total_Bye_Run
paste0("Total Bye Run :",Total_Bye_Run)  



##Total legbye run:
deliveries_df$legbye_runs
sum(deliveries_df$legbye_runs)->Total_LegBye_Run
paste0("Total legbye run:",Total_LegBye_Run)  


#Total no Ball Run :
deliveries_df$noball_runs
sum(deliveries_df$noball_runs)->Total_no_ball_run
paste0("Total No Ball Run:",Total_no_ball_run)  

##Total penalty Run :
deliveries_df$penalty_runs
sum(deliveries_df$penalty_runs)->Total_penalty_Run
paste0("Total Penalty Run:",Total_penalty_Run)
  
  
###Summary Table:

extra_run_df<-data.frame("Super_Over"=deliveries_df$is_super_over,
          "Wide_Run"=deliveries_df$wide_runs,
          "Bye_Run"=deliveries_df$bye_runs,
          "Leg_Bye_Run"=deliveries_df$legbye_runs,
          "No_Ball_Run"=deliveries_df$noball_runs,
          "Penalty_Run"=deliveries_df$penalty_runs)
sapply(extra_run_df,sum)->extra_run_summary
as.data.frame(extra_run_summary)->extra_run_summary

#Table:
extra_run_summary
kable(extra_run_summary)


###Batsman Run :
deliveries_df%>%group_by(batsman)%>%summarise(total_runs=sum(batsman_runs))%>%arrange(desc(total_runs))%>%top_n(n=10,wt=total_runs)->top_10_batsman

#table:
top_10_batsman
kable(top_10_batsman)

#Plot:
ggplot(top_10_batsman,aes(x=batsman,y=total_runs,fill=batsman))+geom_bar(stat = "identity")+theme(axis.text  = element_text(angle = 45,hjust = 1),plot.title = element_text(size = 20))+theme(axis.title = element_text(size = 15,color = "blue"))+ggtitle("Top 5 Batsman and Their Run")+ylab("Batsman Run")+xlab("Batsman Name")+geom_point()

####No. of 4th
deliveries_df$total_runs
filter(deliveries_df,deliveries_df$total_runs=="4")->Four_df
Four_df%>%count(Four_df$Team_name)->Four_df_count
as.data.frame(Four_df_count)->Four_df_count
colnames(Four_df_count)<-c("Team Name","No of Boundries")

#Table:Team and No. of Boundries:
Four_df_count
kable(Four_df_count)

#Team which has most no. of boundries:
Four_df_count%>%arrange(desc(`No of Boundries`))->top_bou_team
top_bou_team[1,]

##Graph:Team and No. of Boundries:
ggplot(Four_df_count,aes(x=`Team Name`,y=`No of Boundries`,fill=`Team Name`))+
  ggtitle("Team and No. of Boundries:")+xlab("Team")+ylab("No. of Boundries")+
  geom_bar(stat = "identity")+
  theme(plot.title = element_text(size = 20,colour = "green"),axis.title = element_text(size = 8,colour = "blue"),axis.text = element_text(angle = 45,color = "orange",hjust=1))

##Player and no.of  boundries:
filter(deliveries_df,deliveries_df$total_runs=="4")->player_4_top
player_4_top%>%count(player_4_top$batsman)->player_4_top_f
player_4_top_f%>%arrange(desc(player_4_top_f$n))->player_4_top_f
as.data.frame(player_4_top_f)->player_4_top_df
colnames(player_4_top_df)<-c("Batsman","No. of Four")

##Top player (4r):
player_4_top_df[1,]

#Top 10 player(4)
player_4_top_df[1:10,]


#Top 10 player (4):Graph
player_4_top_df[1:10,]->Top_6_pl

ggplot(Top_6_pl,aes(x=Top_6_pl$Batsman,y=Top_6_pl$`No. of Four`,fill=Batsman))+ggtitle("Top 10 player (4):Graph")+xlab("Batsman")+ylab("No. of Boundries")+geom_bar(stat = "identity")+theme(plot.title = element_text(size = 20,colour = "green"),axis.title = element_text(size = 8,colour = "blue"),axis.text = element_text(angle = 45,color = "orange",hjust=1))



##Team and no,of sixer:
deliveries_df$total_runs
filter(deliveries_df,deliveries_df$total_runs=="6")->Team_6
Team_6%>%count(Team_6$batting_team)->Team_6
as.data.frame(Team_6)->Team_6
colnames(Team_6)<-c("Team Name","No. of sixer")  

#Table:
Team_6

#Top Team(Sixer):
Team_6[1,]


#Graph:  

ggplot(Team_6,aes(x=`Team Name`,y=`No. of sixer`,fill=`Team Name`))+ggtitle("Team and No. of Sixer:")+xlab("Team")+ylab("No. of Sixer")+geom_bar(stat = "identity")+theme(plot.title = element_text(size = 20,colour = "green"),axis.title = element_text(size = 8,colour = "blue"),axis.text = element_text(angle = 45,color = "orange",hjust=1))


##Player and no. of six:
deliveries_df$total_runs
filter(deliveries_df,deliveries_df$total_runs=="6")->six_df
six_df
six_df%>%count(six_df$batsman)->six_coun_bat
as.data.frame(six_coun_bat)->six_coun_bat_df
colnames(six_coun_bat_df)<-c("Batmans name","No.of sixer")

#Table :Players and no .of sixer:
six_coun_bat_df%>%arrange(desc(six_coun_bat_df$`No.of sixer`))

##Top 10 player and no.of sixer:
six_coun_bat_df%>%arrange(desc(six_coun_bat_df$`No.of sixer`))->top10pla6
as.data.frame(top10pla6)->top10pla6
#Table
top10pla6[1:10,]

##Top player accroding to no.of sixer:
top10pla6[1,]


#Graph:
top10pla6[1:10,]->top_10_6_g
ggplot(top_10_6_g,aes(x=top_10_6_g$`Batmans name`,y=top_10_6_g$`No.of sixer`,fill=`Batmans name`))+ggtitle("Top 10Batsman and No. of Six:")+xlab("Batsman")+ylab("No. of Six")+geom_bar(stat = "identity")+theme(plot.title = element_text(size = 20,colour = "green"),axis.title = element_text(size = 8,colour = "blue"),axis.text = element_text(angle = 45,color = "orange",hjust=1))



  
  
  
  
  

















