# Hashtag Analysis
# Philip Lee and Corey Jackson 2017

# Load Libraries
#install.packages("plyr", dependencies = TRUE)
library(plyr)
#install.packages("ggplot2")
library(ggplot2)
#install.packages("data.table")
library(data.table)
#install.packages("lme4")
library(lme4)
#install.packages("reshape2")
library(reshape2)

# Import hashtag dataset
# File is here: https://www.dropbox.com/sh/5gl91pgxfwa9k4k/AAAO-OSR0HpOvXuN4LSffJ-0a?dl=0  Only you can dw on local machine
hashtags <- read.csv("~/Documents/Academic/School/REU/hashtags/03/hashtags_format_2017-07-14.csv", header=TRUE, sep=",")
#hashtags <- read.csv("~/Dropbox/INSPIRE/REU Projects/Hashtag Use/Dataset/hashtags_format.csv", header=TRUE, sep=",")

hashtags$tag <- as.character(hashtags$tag)
hashtags$user <- as.character(hashtags$user)
hashtags$date <- as.Date(substring(hashtags$created_at, 1, 10))
hashtags$month <- substring(hashtags$date, 1, 7)
hashtags$hour <- substring(hashtags$created_at, 12, 19)
hashtags$time <- paste(hashtags$date,hashtags$hour)
hashtags$X <- hashtags$document.id <- hashtags$hour <- hashtags$created_at <- NULL
hashtags$time <- as.POSIXct(as.character(hashtags$time), format="%Y-%m-%d %H:%M:%S")
hashtags$type <- as.character(hashtags$type); hashtags$type[is.na(hashtags$type)] <- "Other"
hashtags$taggable_id[is.na(hashtags$taggable_id)] <- "0000000"
hashtags$array.index <- hashtags$id <- hashtags$user_id <- hashtags$type <- hashtags$comment_id <- hashtags$taggable_id <- NULL

# convert all tags to same case
hashtags$tag2 <-  tolower(hashtags$tag)

# Create dataframe to get columns unique tag, date of first use, user_name, use count
hashtags_introduced <- ddply(hashtags, c("tag"), summarize, 
                             first.use=min(time), 
                             use.count=sum(length(tag)))

hashtags_introduced <- merge(hashtags_introduced, hashtags, 
                             by.x=c("tag", "first.use"), 
                             by.y=c("tag", "time"))[,c("tag", "first.use", "user", "use.count")]

names(hashtags_introduced)[names(hashtags_introduced)=='user'] <- 'first.user'
hashtags_introduced <- hashtags_introduced[hashtags_introduced$use.count != 1,]

# Create subset of tags using hashtags_introduced date > Oct 12, 2016 (launch) and date < June 14, 2017 (1 mth before last observation) and use count > 1
hashtag_population <- ddply(hashtags, ~ tag, summarize, 
                            after.launch.date=min(date)>="2016/10/12", 
                            before.last.month=min(date)<=max(hashtags$date)-30)
hashtag_population <- hashtag_population[hashtag_population$after.launch.date == 1 & hashtag_population$before.last.month == 1,]["tag"]

hashtag_population <- merge(hashtag_population, hashtags_introduced, 
                            by=c("tag"))

hashtag_population <- merge(hashtag_population, hashtags, 
                            by=c("tag"))
hashtag_population$week <- format(hashtag_population$date, "%Y-%U")

hashtag_population_week <- data.table(ddply(hashtag_population, ~tag, summarize, week=unique(week)))
hashtag_population_week$week.no <- as.integer(as.Date(paste(hashtag_population_week$week,1,sep="-"), "%Y-%U-%u", origin="min(hashtag_population$date)"))
hashtag_population_week$week.no <- (hashtag_population_week$week.no - min(hashtag_population_week$week.no) + 7)/7
hashtag_population_week <- hashtag_population_week[with(hashtag_population_week, order(tag, week.no)), ]
hashtag_population_week$week.no <- ave(hashtag_population_week$week.no, hashtag_population_week$tag, FUN = function(x) c(1, diff(x)))
hashtag_population_week[,week.no:=cumsum(week.no), by=list(tag)]

#remove(hashtags,hashtags_introduced)

########## Dataset Descriptions ##########

# How many unique tags, tags total, unique users, and year-months tags introduced. 
sprintf("%d unique tags", length(unique(hashtag_population$tag))) # Returns number of unique tags: 979 tags
sprintf("%d tags total", sum(length(hashtag_population$tag))) # Returns total number of tags: 14064 tags
sprintf("%d unique users", length(unique(hashtag_population$user))) # Returns number of unique users: 429 users
View(ddply(hashtag_population, ~tag, summarize, intro.month=substring(min(first.use),1,7)))

# Create visualizations: 
# 1 Hashtags and occcassions used (histogram)
hashtag_occasions <- data.table(table(table(hashtag_population$tag)))
names(hashtag_occasions) <- c("frequency", "No.tags")
hashtag_occasions$No.tags <- as.integer(hashtag_occasions$No.tags)

ggplot(hashtag_occasions, aes(x=frequency,y=No.tags))+
  geom_bar(stat="identity")+
  ggtitle("Tag Usage")+
  xlab("Frequency of Tag Usage")+
  ylab("Number of Tags")+
  theme(axis.text.x=element_text(angle=90))

# 2 Growth chart showing the number of tags over time. x could be any measure of time
hashtag_weekly_growth <- data.table(table(hashtag_population$week))
names(hashtag_weekly_growth) <- c("week", "frequency")
hashtag_weekly_growth[, cumulative := cumsum(frequency)]

ggplot(hashtag_weekly_growth, aes(x=week,y=cumulative))+
  geom_point()+
  ggtitle("Weekly Hashtag Growth")+
  xlab("Week")+
  ylab("Total Tags")+
  theme(axis.text.x=element_text(angle=90))

# 3 No. tags introduced by users (histogram). Using hashtags_introduced simply plot of user to know N users contirbute 1 tag, N contribute 2 tags
no_users_introducing_hashtags <- ddply(hashtag_population, ~tag, summarize,
                                       first.user=first(first.user))

no_users_introducing_hashtags <- data.frame(table(table(no_users_introducing_hashtags$first.user)))
names(no_users_introducing_hashtags) <- c("No.tags","No.users")
no_users_introducing_hashtags$No.tags <- as.integer(as.character(no_users_introducing_hashtags$No.tags))

ggplot(data.frame(no_users_introducing_hashtags), aes(x=No.tags,y=No.users))+
  geom_bar(stat="identity")+
  ggtitle("Number of Tags Introduced by Users")+
  xlab("Number of Tags Introduced")+
  ylab("Number of Users")
 
# Add max time to hashtags_introduced and then compute difference (How can we visualize and control for date)
hashtags_introduced <- merge(hashtags_introduced,
                             ddply(hashtags, ~tag, summarize, last.use=max(time)),
                             by=c("tag"))

# Remove the following tags since they are already glitch classes: "BLIP","WHISTLE","NONEOFTHEABOVE","POWERLINE60HZ","KOIFISH","VIOLINMODEHARMONIC","CHIRP","LOWFREQUENCYBURST","NOGLITCH","SCATTEREDLIGHT","HELIX","LIGHTMODULATION","LOWFREQUENCYLINE","PAIREDDOVES","AIRCOMPRESSOR50HZ","REPEATINGBLIPS","SCRATCHY","TOMTE","WANDERINGLINE","EXTREMELYLOUD"
hashtag_population_noclass <- hashtag_population[hashtag_population$tag!="blip"&
                          hashtag_population$tag!="whistle"&
                          hashtag_population$tag!="noneoftheabove"&
                          hashtag_population$tag!="powerline60hz"&
                          hashtag_population$tag!="koifish"&
                          hashtag_population$tag!="violinmodeharmonic"&
                          hashtag_population$tag!="chirp"&
                          hashtag_population$tag!="lowfrequencyburst"&
                          hashtag_population$tag!="noglitch"&
                          hashtag_population$tag!="scatteredlight"&
                          hashtag_population$tag!="helix"&
                          hashtag_population$tag!="lightmodulation"&
                          hashtag_population$tag!="lowfrequencyline"&
                          hashtag_population$tag!="paireddoves"&
                          hashtag_population$tag!="aircompressor50hz"&
                          hashtag_population$tag!="repeatingblips"&
                          hashtag_population$tag!="scratchy"&
                          hashtag_population$tag!="tomte"&
                          hashtag_population$tag!="wanderingline"&
                          hashtag_population$tag!="extremelyloud",]
