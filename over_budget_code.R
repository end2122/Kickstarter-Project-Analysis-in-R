setwd("~/Desktop/BusinessAnalytics/Project/raw_data/01-17-2019/")

directories = list.dirs(path=".",full.names=TRUE)
filenames = list.files(full.names=TRUE,recursive=TRUE)
df = do.call("rbind", lapply(filenames, read.csv, header=TRUE))
write.csv(df, file="~/Desktop/Business Analytics/Project/df_kickstarter3.csv",
          row.names=FALSE, 
          na='')
df["urls"]
nrow(df)

#Calculating overbudget
df$over_budget <- (df$pledged - df$goal)

#Calculating the mean of over budget
df$over_budget[which(df$over_budget >= 0)]
mean_over_budget<-mean(df$over_budget[which(df$over_budget >= 0)])

#Companies with donation above the mean of over budget are flagged as 1, others are flagged as zero
df$over_budget_flag <- ifelse(df$over_budget > mean_over_budget, 1, 0)