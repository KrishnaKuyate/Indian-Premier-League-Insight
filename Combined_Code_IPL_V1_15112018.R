######################@IPL Insight@###########################


####$1

#####Import data file###############
####The dataset contains 2 files: deliveries.csv and matches.csv.
##matches.csv import
library(dplyr)
library(ggplot2)
library(readr)
library(gridExtra)
library(plotrix)
library("plot3D")

matches <- read_csv("matches.csv")
View(matches)

##deliveries.csv 
deliveries <- read_csv("deliveries.csv")
View(deliveries)

#$2/$3

####Understand the structure of dataset####
###The dataset contains 2 files: deliveries.csv and matches.csv.
###Function to study structure of dataset
##Function defination

Data_set_str<- function(Data_set_name)
{
  
  Data_set_name_df<-as.data.frame(Data_set_name)#Convert into in data.frame
  paste("No.of variable in matches dataset: ",ncol(Data_set_name_df))#Check no. of variable in matches dataset
  paste("No.of observations in matches dataset: ",nrow(Data_set_name_df)) #check no. of observations in matches dataset
  paste("datatype of each variables in matches dataset")
  as.data.frame(sapply(Data_set_name_df,class))  #check datatype of each variables in matches dataset
  str(Data_set_name_df) #Check structure of matches dataset
  summary(Data_set_name_df)#Give summary of matches dataset
  
}

##Function call

#matches.csv.
Data_set_str(match_df)
#deliveries.csv 
Data_set_str(deliveries)


#$4

#####Season wise Analysis
###...Dataset  userd:matches...Dataframe Name:match_df


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


#$6

####Team wise data Analysis part -2
###...Dataset  userd:matches...Dataframe Name:match_df

unique(match_df$season)->vec_mat
vec_mat  
plot_grp<-function(t)
{
  match_df[match_df$season==vec_mat[t],]->df1
  as.data.frame(table(df1$team1)+table(df1$team2))->df2
  colnames(df2)<-c("Team_name","Match_Counts")
  as.character(vec_mat[t])->subt
  ggplot(df2,aes(reorder(Team_name,-Match_Counts),Match_Counts,fill = Team_name))+
    geom_bar(stat = "identity",position="dodge")+
    theme(axis.text.x = element_text(angle = 45, hjust = 1))+
    xlab("Team")+ theme(axis.text = element_text(colour = "blue"))+theme(axis.ticks = element_line(size = 2))+
    ylab("Number of Matches") +theme(axis.line = element_line(size = 1, colour = "grey80"))+ 
    theme(axis.text = element_text(colour = "blue"))+
    ggtitle("Team wise data Analysis",subtitle=subt)+ 
    theme(plot.title = element_text(face = "bold.italic"))+geom_point()+guides(fill=FALSE)
  
}

plot_grp(1)->p1
p1
plot_grp(2)->p2
p2
plot_grp(3)->p3
p3
plot_grp(4)->p4
p4
plot_grp(5)->p5
p5
plot_grp(6)->p6
p6
plot_grp(7)->p7
p7
plot_grp(8)->p8
p8
plot_grp(9)->p9
p9
plot_grp(10)->p10
p10

grid.arrange(p1,p2,p3,p4,p5,p6,p7,p8,p9,p10,ncol=5)


#$7

####Team Match Win Analysis

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


#$8
####Team Match Win Analysis Part -2
#######Team Match Win Analysis season wise

na.omit(match_df$season)->mt1
unique(mt1)->mt1_vect


win_seawise<-function(mty)
{
  ##Table
  match_df[match_df$season==mt1_vect[mty],]->mtt1
  as.data.frame(mtt1)->mtt2
  mtt2%>%count(mtt2$winner)->mtt3
  colnames(mtt3)<-c("team_name","team_win")
  paste0("season:",mt1_vect[mty],"match win by team")
  mtt3
  ##Plot:
  par(col.main="blue")
  pie3D(mtt3$team_win,radius=0.6,height=0.1,theta=pi/3,main="Team wise match win count in all seasons",labels=mtt3$team_win,explode=0.2,shade=0.6)
  legend("topright",legend=mtt3$team_name,fill=match_win2$Match_Win,cex = 0.4)
  
}

win_seawise(1)->w1
win_seawise(2)->w2
win_seawise(3)->w3
win_seawise(4)->w4
win_seawise(5)->w5
win_seawise(6)->w6
win_seawise(7)->w7
win_seawise(8)->w8
win_seawise(9)->w9
win_seawise(10)->w10

grid.arrange(w1,w2,w3,w4,w5,w6,w7,w8,w9,w10,ncol=2)



#$9
####Team Match Win Analysis Part -3
#######Team Match Win Analysis (win by run or by wicket)

library(magick)
library(ggplot2)
match_df[match_df$win_by_runs!=0,]->byrun
print("No. of matches win by run:")
nrow(byrun)
match_df[match_df$win_by_wickets!=0,]->bywkt
print("No. of matches win by wicket:")
nrow(bywkt)

c(nrow(byrun),nrow(bywkt))->stat
c("by_run","by_wicket")->when_by

data.frame(when_by,stat)->win_by
colnames(win_by)<-c("Match_win","Match_count")

##Table:
print("Matches win statistics:")
win_by

##Graph:
par(bg="lavender")
barplot(win_by$Match_count,names.arg = win_by$Match_win,xlab = "Win By",ylab = "Matches Count",
        col = c("lightblue", "mistyrose"),angle = 15+10*1:5, density = 220,legend=win_by$Match_win)

title(main = "Team Match Win Analysis (win by run or by wicket)", font.main = 4,col.main="dark blue")


#$10

####Venue wise match data analysis:

match_df$venue
na.omit(match_df$venue)->venue1
print("Name of stadium used in all IPL seasons:")
unique(venue1)
print("No. of stadium used in all IPL seasons:")
length(unique(venue1))

match_df%>%count(match_df$venue)->venu_df
as.data.frame(venu_df)->venu_df
colnames(venu_df)<-c("Stedium_name","Matches_Played")

#table:
print("Name of stadium used in all IPL seasons and no. of matches played:")
venu_df

#graph:
par(bg="gray")
pie(venu_df$Matches_Played,radius = 0.8,angle = 90,col=color.scale(1:35,c(0,300),35,90,color.spec="hcl"),main ="Venue wise match data analysis all seasons" )
legend("topright",legend=venu_df$Stedium_name,fill=venu_df$Matches_Played,cex = 0.3)



#$11

####city wise match  data analysis:

###Check no of city :

na.omit(match_df$city)->ma_city
unique(ma_city)->ma_city
print("List of cities in which IPL game played:")
ma_city
print("No. of of cities in which IPL game played:")
length(ma_city)

###check no city season wise:

match_df%>%count(match_df$city)->cities1
na.omit(cities1)->cities2
as.data.frame(cities2)->cities3
colnames(cities3)<-c("City_Name","Match_count")

###city vs match count:

#Table:
print("city wise match  data analysis: Table:")
cities3

#Graph:

par(bg="#ffe6ff")
ggplot(cities3,aes(reorder(City_Name,-Match_count),Match_count,fill =City_Name))+
  geom_bar(stat = "identity",position="dodge")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  xlab("City")+ theme(axis.text = element_text(colour = "blue"))+theme(axis.ticks = element_line(size = 2))+
  ylab("Number of Matches") +theme(axis.line = element_line(size = 1, colour = "grey80"))+ 
  theme(axis.text = element_text(colour = "blue"))+
  ggtitle("City wise IPL match  data analysis:")+ 
  theme(plot.title = element_text(face = "bold.italic",colour = "#800080",size =30))




#$12

####Toss win Analysis####
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
ggplot(MOMT5_GRP,aes(x=MOMT5_GRP$`Player Name`,y=MOMT5_GRP$`No. Of Awards`,fill=`Player Name`))+
  geom_bar(stat = "identity",position="dodge")+theme(axis.text.x = element_text(angle = 45, hjust = 1))+xlab("Player Name")+
  ylab("No. Of Awards")+ggtitle("Top 5 Player According to Man Of The Match win:(2008-2017)")+theme(plot.title = element_text(face = "bold.italic"))+geom_point()



