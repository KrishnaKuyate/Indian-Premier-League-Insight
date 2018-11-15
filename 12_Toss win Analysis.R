#$12

####Toss win Analysis####
install.packages("plot3D")
library("plot3D")

###Team vs no. of time toss win
match_df%>%count(match_df$toss_winner)->toss_win
na.omit(toss_win)->toss_win
data.frame(toss_win)->toss_win
colnames(toss_win)<-c("Team_Name","Win_Count")
##Table:
print("Team vs no. of time toss win table:")
toss_win
##Graph:
ggplot(toss_win,aes(reorder(Team_Name,-Win_Count),Win_Count,fill =Team_Name))+
  geom_bar(stat = "identity",position="dodge")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  xlab("Team")+ theme(axis.text = element_text(colour = "blue"))+theme(axis.ticks = element_line(size = 2))+
  ylab("Number of time toss win") +theme(axis.line = element_line(size = 1, colour = "grey80"))+ 
  theme(axis.text = element_text(colour = "blue"))+
  ggtitle("Team vs no. of time toss win:")+ 
  theme(plot.title = element_text(face = "bold.italic",colour = "#800080",size =30))+geom_point()

###Team which win toss:maximun number of time 
print("Team which win toss:maximun number of time ")
toss_win[toss_win$Win_Count==max(toss_win$Win_Count),]

###Team which win toss:manimun number of time 
print("Team which win toss:minimum number of time ")
toss_win[toss_win$Win_Count==min(toss_win$Win_Count),]


###Toss win team vs match_wincount
cbind(toss_win,match_win_count=match_win2$Match_Win)->toss_match_win
as.data.frame(toss_match_win)->toss_match_win
colnames(toss_match_win)<-c("TeamName","Toss_Win","Match_Win")
#table:
toss_match_win

#Plot:

#plot(toss_match_win$Toss_Win,toss_match_win$Match_Win,ylab = "Match Win",xlab = "Toss Win",main ="Toss win team vs match win count",col="Blue",pch=10)
#hist(toss_match_win$Toss_Win,toss_match_win$Match_Win,ylab = "Match Win",xlab = "Toss Win", main ="Toss win team vs match win count")

ggplot(toss_match_win,aes(x=Toss_Win,y=Match_Win,col=TeamName))+geom_point()+xlab("Team Toss Win Count")+ylab("Team Match Win Count")+theme(axis.line = element_line(size = 2, colour = "red"))+theme(axis.text = element_text(colour = "blue"))+ggtitle("Toss win team vs match win count")+theme(plot.title = element_text(face = "bold.italic",colour = "#800080",size =30))



















