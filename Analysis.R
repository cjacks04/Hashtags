# Hashtag Analysis
# Philip Lee and Corey Jackson 2017

# Load Libraries
library(plyr)
library(ggplot2)
library(data.table)
library(lme4)
library(reshape2)
library(scales)


# Import hashtag dataset
# File is here: https://www.dropbox.com/sh/5gl91pgxfwa9k4k/AAAO-OSR0HpOvXuN4LSffJ-0a?dl=0  Only you can dw on local machine
hashtags <- read.csv("~/Dropbox/INSPIRE/REU Projects/Hashtag Use/Dataset/hashtags_format.csv", header=TRUE, sep=",")

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

# Convert all hashtags to lowercase
hashtags$tag <- tolower(hashtags$tag)

# Remove the following tags since they are already glitch classes: "BLIP","WHISTLE","NONEOFTHEABOVE","POWERLINE60HZ","KOIFISH","VIOLINMODEHARMONIC","CHIRP","LOWFREQUENCYBURST","NOGLITCH","SCATTEREDLIGHT","HELIX","LIGHTMODULATION","LOWFREQUENCYLINE","PAIREDDOVES","AIRCOMPRESSOR50HZ","REPEATINGBLIPS","SCRATCHY","TOMTE","WANDERINGLINE","EXTREMELYLOUD"
hashtags <- hashtags[hashtags$tag!="blip"&
                               hashtags$tag!="whistle"&
                               hashtags$tag!="noneoftheabove"&
                               hashtags$tag!="powerline60hz"&
                               hashtags$tag!="koifish"&
                               hashtags$tag!="violinmodeharmonic"&
                               hashtags$tag!="chirp"&
                               hashtags$tag!="lowfrequencyburst"&
                               hashtags$tag!="noglitch"&
                               hashtags$tag!="scatteredlight"&
                               hashtags$tag!="helix"&
                               hashtags$tag!="lightmodulation"&
                               hashtags$tag!="lowfrequencyline"&
                               hashtags$tag!="paireddoves"&
                               hashtags$tag!="aircompressor50hz"&
                               hashtags$tag!="repeatingblips"&
                               hashtags$tag!="scratchy"&
                               hashtags$tag!="tomte"&
                               hashtags$tag!="wanderingline"&
                               hashtags$tag!="extremelyloud",]

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
hashtag_population_week$week.no <- ave(hashtag_population_week$week.no, hashtag_population_week$tag, FUN=function(x) c(1, diff(x)))
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


hashtag_population$Date <- as.Date(hashtag_population$time)
ggplot(data=hashtag_population, aes(x=Date)) + 
        geom_line(stat="count") +
        xlab("Week")+
        ylab("Tags Contributed")+
        scale_x_date(breaks=date_breaks("1 month"), labels=date_format("%Y-%m"),limits = as.Date(c('2016-09-20','2017-07-14')))+
        theme_bw() +
        theme(
          axis.text.x=element_text(angle=90,size = 12)
      ) 


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

hashtags_introduced$Date <- as.Date(hashtags_introduced$first.use)
ggplot(data=hashtags_introduced, aes(x=Date)) + 
        geom_line(stat="count") +
        xlab("Week")+
        ylab("New Tags")+
        scale_x_date(breaks=date_breaks("1 month"), labels=date_format("%Y-%m"),limits = as.Date(c('2016-09-20','2017-07-14')))+
        theme_bw() +
        theme(
          axis.text.x=element_text(angle=90,size = 12)
      )

# Count the tag use for the first two weeks by each day (D.01 ~ D.16)
hashtag_population_fw <- data.table(table(hashtag_population$tag,
                                          as.integer(hashtag_population$date)))
names(hashtag_population_fw) <- c("tag","day.no","total")
hashtag_population_fw <- hashtag_population_fw[hashtag_population_fw$total!=0,]
hashtag_population_fw <- hashtag_population_fw[with(hashtag_population_fw,
                                                    order(tag, day.no)), ]
hashtag_population_fw$day.no <- ave(as.integer(hashtag_population_fw$day.no),
                                    hashtag_population_fw$tag,
                                    FUN=function(x) c(1, diff(x)))
hashtag_population_fw[,day.no:=cumsum(day.no), by=list(tag)]
hashtag_population_fw <- data.frame(hashtag_population_fw[hashtag_population_fw$day.no <= 16])
hashtag_population_fw_once <- aggregate(. ~tag, data=hashtag_population_fw, sum)
hashtag_population_fw_once <- hashtag_population_fw_once[hashtag_population_fw_once$total!=1,]
hashtag_population_fw_once$day.no <- hashtag_population_fw_once$total <- NULL
#hashtag_population_fw_once <- data.frame(once=table(tag=hashtag_population_fw$tag)==1)
#hashtag_population_fw_once$tag <- row.names(hashtag_population_fw_once)
#hashtag_population_fw_once <- hashtag_population_fw_once[hashtag_population_fw_once$once!=1,]
#hashtag_population_fw_once$once <- NULL
hashtag_population_fw <- merge(hashtag_population_fw,hashtag_population_fw_once,
                               by="tag")
for(t in unique(hashtag_population_fw$day.no)){
  hashtag_population_fw[ifelse(nchar(t)==1,paste("D.0",t,sep=""),paste("D.",t,sep=""))] <-
    ifelse(hashtag_population_fw$day.no==t,hashtag_population_fw$total,0)}
hashtag_population_fw$day.no <- hashtag_population_fw$total <- NULL
hashtag_population_fw <- hashtag_population_fw[,order(colnames(hashtag_population_fw))]
hashtag_population_fw <- aggregate(. ~tag, data=hashtag_population_fw, sum)
remove(hashtag_population_fw_once,t)

# Count the tag use for the first month by each week (T.01 ~ T.04)
hashtag_population_om <- data.table(table(hashtag_population$tag,
                                          format(hashtag_population$date,
                                                 "%Y-%U")))
names(hashtag_population_om) <- c("tag","week","total")
hashtag_population_om <- hashtag_population_om[hashtag_population_om$total!=0,]
hashtag_population_om$week.no <- as.integer(as.Date(paste(hashtag_population_om$week,1,sep="-"),
                                                    "%Y-%U-%u",
                                                    origin="min(hashtag_population$date)"))
hashtag_population_om <- hashtag_population_om[with(hashtag_population_om,
                                                    order(tag, week.no)),]
hashtag_population_om$week.no <- (hashtag_population_om$week.no - min(hashtag_population_om$week.no)+7)/7
hashtag_population_om$week.no <- ave(hashtag_population_om$week.no, hashtag_population_om$tag,
                                     FUN=function(x) c(1, diff(x)))
hashtag_population_om[,week.no:=cumsum(week.no), by=list(tag)]
hashtag_population_om <- data.frame(hashtag_population_om[hashtag_population_om$week.no <= 4])
hashtag_population_om_once <- aggregate(. ~tag, data=hashtag_population_om[c(1,3,4)], sum)
hashtag_population_om_once <- hashtag_population_om_once[hashtag_population_om_once$total!=1,]
hashtag_population_om_once$week.no <- hashtag_population_om_once$total <- NULL
#hashtag_population_om_once <- data.frame(once=table(tag=hashtag_population_om$tag)==1)
#hashtag_population_om_once$tag <- row.names(hashtag_population_om_once)
#hashtag_population_om_once <- hashtag_population_om_once[hashtag_population_om_once$once!=1,]
#hashtag_population_om_once$once <- NULL
hashtag_population_om <- merge(hashtag_population_om,hashtag_population_om_once,
                               by="tag")
for(t in unique(hashtag_population_om$week.no)){
  hashtag_population_om[ifelse(nchar(t)==1,paste("T.0",t,sep=""),paste("T.",t,sep=""))] <-
    ifelse(hashtag_population_om$week.no==t,hashtag_population_om$total,0)}
hashtag_population_om$week <- hashtag_population_om$week.no <- hashtag_population_om$total <- NULL
hashtag_population_om <- hashtag_population_om[,order(colnames(hashtag_population_om))]
hashtag_population_om <- aggregate(. ~tag, data=hashtag_population_om, sum)
remove(hashtag_population_om_once,t)
