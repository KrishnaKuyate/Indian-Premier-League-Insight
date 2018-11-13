#$3

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

Data_set_str(match_df)
Data_set_str(deliveries)
