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
