# Hashtag Analysis
# Philip Lee and Corey Jackson 2017

# Load Libraries 
install.packages("plyr", dependencies = TRUE)
library(plyr)
install.packages("ggplot2")
library(ggplot2)
install.packages("data.table")
library(data.table)
install.packages("lme4")
library(lme4)
install.packages("reshape2")
library(reshape2)

# Import hashtag dataset
# File is here: https://www.dropbox.com/sh/5gl91pgxfwa9k4k/AAAO-OSR0HpOvXuN4LSffJ-0a?dl=0  Only you can dw on local machine
hashtags <- read.csv("")
hashtags$tag <- as.character(hashtags$tag)
hashtags$date <- as.Date(substring(hashtags$created_at, 1, 10))
hashtags$month <- substring(hashtags$date, 1, 7)
hashtags$hour <- substring(hashtags$created_at, 12, 19)
hashtags$time <- paste(hashtags$date,hashtags$hour)
hashtags$X <- hashtags$document.id <- hashtags$hour <- hashtags$created_at <- NULL
hashtags$time <- as.POSIXct(as.character(hashtags$time), format="%Y-%m-%d %H:%M:%S")
hashtags$type <- as.character(hashtags$type); hashtags$type[is.na(hashtags$type)] <- "Other"
hashtags$taggable_id[is.na(hashtags$taggable_id)] <- "0000000"
hashtags$array.index <- hashtags$id <- hashtags$user_id <- hashtags$type <- hashtags$comment_id <- hashtags$taggable_id <- NULL

# Create dataframe to get columns unique tag, date of first use, user_name, use count
hashtags_introduced <- ddply(hashtags, c("tag"), summarize, first.use=min(time), use.count=sum(length(tag)))
hashtags_introduced <- merge(hashtags_introduced, hashtags, by.x=c("tag", "first.use"), by.y=c("tag", "time"))[,c("tag", "first.use", "first.user"="user", "use.count")]
names(hashtags_introduced)[names(hashtags_introduced) == 'user'] <- 'first.user'
hashtags_introduced <- hashtags_introduced[hashtags_introduced$use.count != 1,]

# Create subset of tags using hashtags_introduced date > oct 12 (launch) and date < june 14 (1 mth before last observation) and use count > 1
hashtag_population <- ddply(hashtags, ~ tag, summarize, after.launch.date=min(date)>="2016/10/12", before.last.month=min(date)<=max(hashtags$date)-30)
hashtag_population <- hashtag_population[hashtag_population$after.launch.date == 1 & hashtag_population$before.last.month == 1,]["tag"]
hashtag_population <- merge(hashtag_population, hashtags_introduced, by=c("tag"))
hashtag_population <- merge(hashtag_population, hashtags, by=c("tag"))


remove(hashtags,hashtags_introduced)
