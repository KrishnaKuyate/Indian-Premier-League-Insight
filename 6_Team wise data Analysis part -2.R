#$6

####Team wise data Analysis part -2
###...Dataset  userd:matches...Dataframe Name:match_df
install.packages("gridExtra")
library(ggplot2)
library("gridExtra")
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

