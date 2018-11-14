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
  theme(plot.title = element_text(face = "bold.italic",colour = "#800080",size =30))+geom_point()
