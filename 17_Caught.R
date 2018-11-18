##$17

####Caught 

data.frame(deliveries_df$fielder,deliveries_df$dismissal_kind)->Caught 
na.omit(Caught)->Caught 
colnames(Caught)<-c("Filder","c")
filter(Caught,Caught$c=="caught")->Caught
Caught%>%count(Caught$Filder)->Caught
Caught%>%arrange(desc(Caught$n))->Caught
Caught[1:5,]->Caught
colnames(Caught)<-c("Filder Name","No. of caughts")

#Table:
Caught
kable(Caught)

#Graph:
ggplot(Caught,aes(x=`Filder Name`,y=`No. of caughts`,fill=`Filder Name`))+geom_bar(stat = "identity",position="dodge")+ggtitle("Top 5 Filder and Caught")+theme(plot.title = element_text(size =25,color ="blue"),axis.title = element_text(size = 18,color = "pink"),axis.text = element_text(angle = 45,hjust = 1))
