#$2

####Understand the structure of dataset####
###The dataset contains 2 files: deliveries.csv and matches.csv.


##matches.csv
#Convert into in data.frame

match_df<-as.data.frame(matches)

#Check no. of variable in matches dataset

paste("No.of variable in matches dataset: ",ncol(match_df))

#check no. of observations in matches dataset

paste("No.of observations in matches dataset: ",nrow(match_df))

#check datatype of each variables in matches dataset

#sapply(match_df,class)->data_type_match_df
#data_type_match_df
#as.data.frame(data_type_match_df)->data_type_match_df1

paste("datatype of each variables in matches dataset")
as.data.frame(sapply(match_df,class))

#Check structure of matches dataset
str(match_df)

#Give summary of matches dataset

summary(matches)


