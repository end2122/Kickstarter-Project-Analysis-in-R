# Requirements
# 1. create folder called "raw_data"
# 2. save all kickstarter csv extracts downloaded from "https://webrobots.io/kickstarter-datasets/" in the raw_data folder
# 3. in line 7,12 change your wd to the folder path

# Merge all CSVs into master file
setwd("~/Columbia MSBA/Spring 2019/E4650 - Business Analytics/Project - Kickstarter Analysis/raw_data/01-17-2019/")

directories = list.dirs(path=".",full.names=TRUE)
filenames = list.files(full.names=TRUE,recursive=TRUE)
df = do.call("rbind", lapply(filenames, read.csv, header=TRUE))
write.csv(df, file="~/Columbia MSBA/Spring 2019/E4650 - Business Analytics/Project - Kickstarter Analysis/df_kickstarter3.csv",
          row.names=FALSE, 
          na='')
nrow(df)