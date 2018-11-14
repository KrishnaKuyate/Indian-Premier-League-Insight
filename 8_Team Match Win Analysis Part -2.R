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
