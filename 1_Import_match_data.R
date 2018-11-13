######################@IPL Insight@###########################


#####Import data file###############

####The dataset contains 2 files: deliveries.csv and matches.csv.

##matches.csv import

library(readr)
matches <- read_csv("matches.csv")
View(matches)


##deliveries.csv 

deliveries <- read_csv("deliveries.csv")
View(deliveries)