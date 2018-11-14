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



