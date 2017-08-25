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
hashtags$week.of <- as.Date(paste(format(hashtags$date,"%Y-%U"),1,sep="-"),"%Y-%U-%u")
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

ggplot(hashtag_weekly_growth, aes(x=week,y=cumulative))+
  geom_point()+
  ggtitle("Weekly Hashtag Growth")+
  xlab("Week")+
  ylab("Total Tags")+
  theme(axis.text.x=element_text(angle=90))


# Daily Tags
hashtag_population$Date <- as.Date(hashtag_population$time)

setwd("~/~/Documents/Academic/School/REU/hashtags/03/Figures")
pdf("TagsContributed_Daily.pdf", height=5, width=11)
ggplot(data=hashtag_population, aes(x=Date)) + 
  geom_line(stat="count") +
  xlab("Week")+
  ylab("Tags Contributed")+
  scale_x_date(breaks=date_breaks("1 month"), 
               labels=date_format("%Y-%m"),
               limits = as.Date(c('2016-10-12','2017-07-14')))+
  theme_bw() +
  theme(
    axis.text.x=element_text(angle=90,size = 12)
  ) 
dev.off()

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

# First Tags
hashtags_introduced$Date <- as.Date(hashtags_introduced$first.use)
hashtags_introduced <- hashtags_introduced[complete.cases(hashtags_introduced[ ,6]),]

setwd("~/~/Documents/Academic/School/REU/hashtags/03/Figures")
pdf("NewTags_Daily.pdf", height=5, width=11)
ggplot(data=hashtags_introduced, aes(x=Date)) + 
  geom_line(stat="count") +
  xlab("Week")+
  ylab("New Tags")+
  scale_x_date(breaks=date_breaks("1 month"), 
               labels=date_format("%Y-%m"),
               limits = as.Date(c('2016-10-12','2017-07-14')))+
  theme_bw() +
  theme(
    axis.text.x=element_text(angle=90,size = 12)
  )
dev.off()

# Relationship between tags and users
tags_users <- ddply(hashtag_population, c("Date"), summarize,
                    users = length(unique(user)),
                    tags = length(tag))
tags_users.m <- melt(tags_users, id.var=c("Date"))
tags_users_range <- tags_users[which(tags_users$Date >= '2016-10-12'),]


setwd("~/~/Documents/Academic/School/REU/hashtags/03/Figures")
pdf("Tags_User_line.pdf", height=5, width=11)
ggplot(tags_users_range, aes(x = Date)) +
  geom_line(aes(y = users, colour = "Users")) +
  geom_line(aes(y = tags, colour = "Tags")) + 
  scale_y_continuous(sec.axis = sec_axis(~., name = "Number of Tags")) +
  scale_x_date(
    breaks=date_breaks("1 month"), 
    labels=date_format("%Y-%m")) +
  labs(y = " Number of Users",
       x = "Date",
       colour = "Variable") + 
  theme_bw() +
  theme(axis.text.x=element_text(angle=90,size = 12)) + 
  theme(legend.position = c(0.8, 0.9))
dev.off()

pdf("Tags_User_point.pdf", height=5, width=11)
ggplot(data=tags_users,aes(x=users, y=tags)) +
  geom_point(shape=1) +    # Use hollow circles
  geom_smooth(method=lm) + 
  labs(y = "Tags Contributed",
       x = "Users")
dev.off()


# Count the tag use for the first two weeks by each day (D.01 ~ D.16) -- hashtag_population
hashtag_population_fw <- data.table(table(hashtag_population$tag,
                                          hashtag_population$date))
names(hashtag_population_fw) <- c("tag","date","total")
hashtag_population_fw$date <- as.Date(hashtag_population_fw$date)
hashtag_population_fw <- hashtag_population_fw[hashtag_population_fw$total!=0,]
hashtag_population_fw <- hashtag_population_fw[with(hashtag_population_fw,
                                                    order(tag, date)), ]
hashtag_population_fw$day.no <- ave(as.integer(hashtag_population_fw$date),
                                    hashtag_population_fw$tag,
                                    FUN=function(x) c(1, diff(x)))
hashtag_population_fw[,day.no:=cumsum(day.no), by=list(tag)]
hashtag_population_fw <- data.frame(hashtag_population_fw[hashtag_population_fw$day.no <= 16])
hashtag_population_fw_ex_once <- aggregate(. ~tag, data=hashtag_population_fw[c(1,3,4)], sum)
hashtag_population_fw_ex_once <- hashtag_population_fw_ex_once[hashtag_population_fw_ex_once$total!=1,]
hashtag_population_fw_ex_once$day.no <- hashtag_population_fw_ex_once$total <- NULL
#hashtag_population_fw_ex_once <- data.frame(once=table(tag=hashtag_population_fw$tag)==1)
#hashtag_population_fw_ex_once$tag <- row.names(hashtag_population_fw_once)
#hashtag_population_fw_ex_once <- hashtag_population_fw_once[hashtag_population_fw_once$once!=1,]
#hashtag_population_fw_ex_once$once <- NULL
hashtag_population_fw_ex_once <- merge(hashtag_population_fw,hashtag_population_fw_ex_once,
                                       by="tag")
for(t in unique(hashtag_population_fw_ex_once$day.no)){
  hashtag_population_fw_ex_once[ifelse(nchar(t)==1,paste("D.0",t,sep=""),paste("D.",t,sep=""))] <-
    ifelse(hashtag_population_fw_ex_once$day.no==t,hashtag_population_fw_ex_once$total,0)}
hashtag_population_fw_ex_once$day.no <- hashtag_population_fw$total <- hashtag_population_fw_ex_once$date <- NULL
hashtag_population_fw_ex_once <- hashtag_population_fw_ex_once[,order(colnames(hashtag_population_fw_ex_once))]
hashtag_population_fw_ex_once <- aggregate(. ~tag, data=hashtag_population_fw_ex_once, sum)
hashtag_population_fw_ex_once_cumul <- data.frame(t(hashtag_population_fw_ex_once[-T]))
hashtag_population_fw_ex_once_cumul <- data.frame(hashtag_population_fw_ex_once["tag"],
                                                  t(cumsum(hashtag_population_fw_ex_once_cumul)))
rownames(hashtag_population_fw_ex_once_cumul) <- NULL
hashtag_population_fw_ex_once_cumul.melt <- melt(hashtag_population_fw_ex_once_cumul,id.vars=("tag"))
colnames(hashtag_population_fw_ex_once_cumul.melt)[c(2,3)] <- c("time","cumul")
hashtag_population_fw_date <- data.table(ddply(hashtags_fw,~tag,summarize,
                                               date=seq.Date(from=min(date),
                                                             by="day",
                                                             length.out=length(hashtag_population_fw_ex_once)-1)),
                                         time=1)
hashtag_population_fw_date[, time:=cumsum(time),by=list(tag)]
hashtag_population_fw_date$time <- ifelse(nchar(hashtag_population_fw_date$time)==1,
                                          paste("D.0",hashtag_population_fw_date$time,sep=""),
                                          paste("D.",hashtag_population_fw_date$time,sep=""))
hashtag_population_fw_ex_once_cumul.melt <- merge(hashtag_population_fw_ex_once_cumul.melt,
                                                  hashtag_population_fw_date,
                                                  by=c("tag","time"))
#hashtag_population_fw_project_tags <- data.table(ddply(hashtags_fw,~week.of,summarize,project.tags=sum(total)))
hashtag_population_fw_project_tags <- data.table(ddply(hashtags,~date,summarize,project.tags=length(tag)))
colnames(hashtag_population_fw_project_tags)[1] <- "date"
hashtag_population_fw_ex_once_cumul.melt <- merge(hashtag_population_fw_ex_once_cumul.melt,
                                                  hashtag_population_fw_project_tags,
                                                  by="date",
                                                  all.x=TRUE)
hashtag_population_fw_unique_users <- data.table(ddply(hashtags,~tag+date,summarize,
                                                       unique.users=length(unique(user))))
colnames(hashtag_population_fw_unique_users)[2] <- "date"
hashtag_population_fw_ex_once_cumul.melt <- merge(hashtag_population_fw_ex_once_cumul.melt,
                                                  hashtag_population_fw_unique_users,
                                                  by=c("tag","date"),
                                                  all.x=TRUE)
#hashtag_population_fw_tag_users <- merge(hashtags,hashtags_fw,by=c("tag","date"))
#hashtag_population_fw_tag_users <- data.frame(ddply(hashtag_population_fw_tag_users,~date,summarize,
#                                          tag_users=length(unique(user))))
hashtag_population_fw_tag_users <- data.frame(ddply(hashtags,~date,summarize,
                                                    tag_users=length(unique(user))))
hashtag_population_fw_ex_once_cumul.melt <- merge(hashtag_population_fw_ex_once_cumul.melt,
                                                  hashtag_population_fw_tag_users,
                                                  by="date",
                                                  all.x=TRUE)
hashtag_population_fw_ex_once.melt <- melt(hashtag_population_fw_ex_once,id.vars=("tag"))
colnames(hashtag_population_fw_ex_once.melt)[c(2,3)] <- c("time","count")
hashtag_population_fw_ex_once.melt <- merge(hashtag_population_fw_ex_once.melt,
                                            hashtag_population_fw_date,
                                            by=c("tag","time"))
hashtag_population_fw_ex_once.melt <- merge(hashtag_population_fw_ex_once.melt,
                                            hashtag_population_fw_project_tags,
                                            by="date",
                                            all.x=TRUE)
hashtag_population_fw_ex_once.melt <- merge(hashtag_population_fw_ex_once.melt,
                                            hashtag_population_fw_unique_users,
                                            by=c("tag","date"),
                                            all.x=TRUE)
hashtag_population_fw_ex_once.melt <- merge(hashtag_population_fw_ex_once.melt,
                                            hashtag_population_fw_tag_users,
                                            by="date",
                                            all.x=TRUE)
remove(t,hashtag_population_fw_date,hashtag_population_fw_project_tags,hashtag_population_fw_unique_users,hashtag_population_fw_tag_users)
hashtag_population_fw_ex_once_cumul.melt <- hashtag_population_fw_ex_once_cumul.melt[with(hashtag_population_fw_ex_once_cumul.melt,order(tag,date)),]
hashtag_population_fw_ex_once.melt <- hashtag_population_fw_ex_once.melt[with(hashtag_population_fw_ex_once.melt,order(tag,date)),]
hashtag_population_fw_ex_once_cumul.melt[is.na(hashtag_population_fw_ex_once_cumul.melt)]<-0
hashtag_population_fw_ex_once.melt[is.na(hashtag_population_fw_ex_once.melt)]<-0




# Count the tag use for the first month by each week (T.01 ~ T.16) -- hashtag_population
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
hashtag_population_om <- data.frame(hashtag_population_om[hashtag_population_om$week.no <= 16])
hashtag_population_om$week.of <- as.Date(paste(hashtag_population_om$week,1,sep="-"),format="%Y-%U-%u")
hashtag_population_om_ex_once <- aggregate(. ~tag, data=hashtag_population_om[c(1,3,4)], sum)
hashtag_population_om_ex_once <- hashtag_population_om_ex_once[hashtag_population_om_ex_once$total!=1,]
hashtag_population_om_ex_once$week.no <- hashtag_population_om_ex_once$total <- NULL
#hashtag_population_om_ex_once <- data.frame(once=table(tag=hashtag_population_om$tag)==1)
#hashtag_population_om_ex_once$tag <- row.names(hashtag_population_om_once)
#hashtag_population_om_ex_once <- hashtag_population_om_once[hashtag_population_om_once$once!=1,]
#hashtag_population_om_ex_once$once <- NULL
hashtag_population_om_ex_once <- merge(hashtag_population_om,hashtag_population_om_ex_once,
                                       by="tag")
for(t in unique(hashtag_population_om_ex_once$week.no)){
  hashtag_population_om_ex_once[ifelse(nchar(t)==1,paste("T.0",t,sep=""),paste("T.",t,sep=""))] <-
    ifelse(hashtag_population_om_ex_once$week.no==t,hashtag_population_om_ex_once$total,0)}
hashtag_population_om_ex_once$week <- hashtag_population_om_ex_once$week.no <- hashtag_population_om_ex_once$total <- hashtag_population_om_ex_once$week.of <- NULL
hashtag_population_om_ex_once <- hashtag_population_om_ex_once[,order(colnames(hashtag_population_om_ex_once))]
hashtag_population_om_ex_once <- aggregate(. ~tag, data=hashtag_population_om_ex_once, sum)
hashtag_population_om_ex_once_cumul <- data.frame(t(hashtag_population_om_ex_once[-T]))
hashtag_population_om_ex_once_cumul <- data.frame(hashtag_population_om_ex_once["tag"],
                                                  t(cumsum(hashtag_population_om_ex_once_cumul)))
rownames(hashtag_population_om_ex_once_cumul) <- NULL
hashtag_population_om_ex_once_cumul.melt <- melt(hashtag_population_om_ex_once_cumul,id.vars=("tag"))
colnames(hashtag_population_om_ex_once_cumul.melt)[c(2,3)] <- c("time","cumul")
hashtag_population_om_week <- data.table(ddply(hashtag_population_om,~tag,summarize,
                                               week.of=seq.Date(from=min(week.of),
                                                                by="week",
                                                                length.out=length(hashtag_population_om_ex_once)-1)),
                                         time=1)
hashtag_population_om_week[, time:=cumsum(time),by=list(tag)]
hashtag_population_om_week$time <- ifelse(nchar(hashtag_population_om_week$time)==1,
                                          paste("T.0",hashtag_population_om_week$time,sep=""),
                                          paste("T.",hashtag_population_om_week$time,sep=""))
hashtag_population_om_ex_once_cumul.melt <- merge(hashtag_population_om_ex_once_cumul.melt,
                                                  hashtag_population_om_week,
                                                  by=c("tag","time"))
#hashtag_population_om_project_tags <- data.table(ddply(hashtag_population_om,~week.of,summarize,project.tags=sum(total)))
hashtag_population_om_project_tags <- data.table(ddply(hashtag_population,~as.Date(paste(format(date,"%Y-%U"),1,sep="-"),"%Y-%U-%u"),summarize,
                                                       project.tags=length(tag)))
colnames(hashtag_population_om_project_tags)[1] <- "week.of"
hashtag_population_om_ex_once_cumul.melt <- merge(hashtag_population_om_ex_once_cumul.melt,
                                                  hashtag_population_om_project_tags,
                                                  by="week.of",
                                                  all.x=TRUE)
hashtag_population_om_unique_users <- data.table(ddply(hashtag_population,~tag+as.Date(paste(format(date,"%Y-%U"),1,sep="-"),"%Y-%U-%u"),summarize,
                                                       unique.users=length(unique(user))))
colnames(hashtag_population_om_unique_users)[2] <- "week.of"
hashtag_population_om_ex_once_cumul.melt <- merge(hashtag_population_om_ex_once_cumul.melt,
                                                  hashtag_population_om_unique_users,
                                                  by=c("tag","week.of"),
                                                  all.x=TRUE)
#hashtag_population_om_tag_users <- merge(hashtag_population,hashtag_population_om,by=c("tag","week.of"))
#hashtag_population_om_tag_users <- data.frame(ddply(hashtag_population_om_tag_users,~week.of,summarize,
#                                          tag_users=length(unique(user))))
hashtag_population_om_tag_users <- data.frame(ddply(hashtag_population,~week.of,summarize,
                                                    tag_users=length(unique(user))))
hashtag_population_om_ex_once_cumul.melt <- merge(hashtag_population_om_ex_once_cumul.melt,
                                                  hashtag_population_om_tag_users,
                                                  by="week.of",
                                                  all.x=TRUE)
hashtag_population_om_ex_once.melt <- melt(hashtag_population_om_ex_once,id.vars=("tag"))
colnames(hashtag_population_om_ex_once.melt)[c(2,3)] <- c("time","count")
hashtag_population_om_ex_once.melt <- merge(hashtag_population_om_ex_once.melt,
                                            hashtag_population_om_week,
                                            by=c("tag","time"))
hashtag_population_om_ex_once.melt <- merge(hashtag_population_om_ex_once.melt,
                                            hashtag_population_om_project_tags,
                                            by="week.of",
                                            all.x=TRUE)
hashtag_population_om_ex_once.melt <- merge(hashtag_population_om_ex_once.melt,
                                            hashtag_population_om_unique_users,
                                            by=c("tag","week.of"),
                                            all.x=TRUE)
hashtag_population_om_ex_once.melt <- merge(hashtag_population_om_ex_once.melt,
                                            hashtag_population_om_tag_users,
                                            by="week.of",
                                            all.x=TRUE)
remove(t,hashtag_population_om_week,hashtag_population_om_project_tags,hashtag_population_om_unique_users,hashtag_population_om_tag_users)
hashtag_population_om_ex_once_cumul.melt <- hashtag_population_om_ex_once_cumul.melt[with(hashtag_population_om_ex_once_cumul.melt,order(tag,week.of)),]
hashtag_population_om_ex_once.melt <- hashtag_population_om_ex_once.melt[with(hashtag_population_om_ex_once.melt,order(tag,week.of)),]
hashtag_population_om_ex_once_cumul.melt[is.na(hashtag_population_om_ex_once_cumul.melt)]<-0
hashtag_population_om_ex_once.melt[is.na(hashtag_population_om_ex_once.melt)]<-0




# Count the tag use for the first two weeks by each day (D.01 ~ D.16) -- hashtags
hashtags_fw <- data.table(table(hashtags$tag,
                                hashtags$date))
names(hashtags_fw) <- c("tag","date","total")
hashtags_fw$date <- as.Date(hashtags_fw$date)
hashtags_fw <- hashtags_fw[hashtags_fw$total!=0,]
hashtags_fw <- hashtags_fw[with(hashtags_fw,
                                order(tag, date)), ]
hashtags_fw$day.no <- ave(as.integer(hashtags_fw$date),
                          hashtags_fw$tag,
                          FUN=function(x) c(1, diff(x)))
hashtags_fw[,day.no:=cumsum(day.no), by=list(tag)]
hashtags_fw <- data.frame(hashtags_fw[hashtags_fw$day.no <= 16])
hashtags_fw_ex_once <- aggregate(. ~tag, data=hashtags_fw[c(1,3,4)], sum)
hashtags_fw_ex_once <- hashtags_fw_ex_once[hashtags_fw_ex_once$total!=1,]
hashtags_fw_ex_once$day.no <- hashtags_fw_ex_once$total <- NULL
#hashtags_fw_ex_once <- data.frame(once=table(tag=hashtags_fw$tag)==1)
#hashtags_fw_ex_once$tag <- row.names(hashtags_fw_once)
#hashtags_fw_ex_once <- hashtags_fw_once[hashtags_fw_once$once!=1,]
#hashtags_fw_ex_once$once <- NULL
hashtags_fw_ex_once <- merge(hashtags_fw,hashtags_fw_ex_once,
                             by="tag")
for(t in unique(hashtags_fw_ex_once$day.no)){
  hashtags_fw_ex_once[ifelse(nchar(t)==1,paste("D.0",t,sep=""),paste("D.",t,sep=""))] <-
    ifelse(hashtags_fw_ex_once$day.no==t,hashtags_fw_ex_once$total,0)}
hashtags_fw_ex_once$day.no <- hashtags_fw_ex_once$total <- hashtags_fw_ex_once$date <- NULL
hashtags_fw_ex_once <- hashtags_fw_ex_once[,order(colnames(hashtags_fw_ex_once))]
hashtags_fw_ex_once <- aggregate(. ~tag, data=hashtags_fw_ex_once, sum)
hashtags_fw_ex_once_cumul <- data.frame(t(hashtags_fw_ex_once[-T]))
hashtags_fw_ex_once_cumul <- data.frame(hashtags_fw_ex_once["tag"],
                                        t(cumsum(hashtags_fw_ex_once_cumul)))
rownames(hashtags_fw_ex_once_cumul) <- NULL
hashtags_fw_ex_once_cumul.melt <- melt(hashtags_fw_ex_once_cumul,id.vars=("tag"))
colnames(hashtags_fw_ex_once_cumul.melt)[c(2,3)] <- c("time","cumul")
hashtags_fw_date <- data.table(ddply(hashtags_fw,~tag,summarize,
                                     date=seq.Date(from=min(date),
                                                   by="day",
                                                   length.out=length(hashtags_fw_ex_once)-1)),
                               time=1)
hashtags_fw_date[, time:=cumsum(time),by=list(tag)]
hashtags_fw_date$time <- ifelse(nchar(hashtags_fw_date$time)==1,
                                paste("D.0",hashtags_fw_date$time,sep=""),
                                paste("D.",hashtags_fw_date$time,sep=""))
hashtags_fw_ex_once_cumul.melt <- merge(hashtags_fw_ex_once_cumul.melt,
                                        hashtags_fw_date,
                                        by=c("tag","time"))
#hashtags_fw_project_tags <- data.table(ddply(hashtags_fw,~week.of,summarize,project.tags=sum(total)))
hashtags_fw_project_tags <- data.table(ddply(hashtags,~date,summarize,project.tags=length(tag)))
colnames(hashtags_fw_project_tags)[1] <- "date"
hashtags_fw_ex_once_cumul.melt <- merge(hashtags_fw_ex_once_cumul.melt,
                                        hashtags_fw_project_tags,
                                        by="date",
                                        all.x=TRUE)
hashtags_fw_unique_users <- data.table(ddply(hashtags,~tag+date,summarize,
                                             unique.users=length(unique(user))))
colnames(hashtags_fw_unique_users)[2] <- "date"
hashtags_fw_ex_once_cumul.melt <- merge(hashtags_fw_ex_once_cumul.melt,
                                        hashtags_fw_unique_users,
                                        by=c("tag","date"),
                                        all.x=TRUE)
#hashtags_fw_tag_users <- merge(hashtags,hashtags_fw,by=c("tag","date"))
#hashtags_fw_tag_users <- data.frame(ddply(hashtags_fw_tag_users,~date,summarize,
#                                          tag_users=length(unique(user))))
hashtags_fw_tag_users <- data.frame(ddply(hashtags,~date,summarize,
                                          tag_users=length(unique(user))))
hashtags_fw_ex_once_cumul.melt <- merge(hashtags_fw_ex_once_cumul.melt,
                                        hashtags_fw_tag_users,
                                        by="date",
                                        all.x=TRUE)
hashtags_fw_ex_once.melt <- melt(hashtags_fw_ex_once,id.vars=("tag"))
colnames(hashtags_fw_ex_once.melt)[c(2,3)] <- c("time","count")
hashtags_fw_ex_once.melt <- merge(hashtags_fw_ex_once.melt,
                                  hashtags_fw_date,
                                  by=c("tag","time"))
hashtags_fw_ex_once.melt <- merge(hashtags_fw_ex_once.melt,
                                  hashtags_fw_project_tags,
                                  by="date",
                                  all.x=TRUE)
hashtags_fw_ex_once.melt <- merge(hashtags_fw_ex_once.melt,
                                  hashtags_fw_unique_users,
                                  by=c("tag","date"),
                                  all.x=TRUE)
hashtags_fw_ex_once.melt <- merge(hashtags_fw_ex_once.melt,
                                  hashtags_fw_tag_users,
                                  by="date",
                                  all.x=TRUE)
remove(t,hashtags_fw_date,hashtags_fw_project_tags,hashtags_fw_unique_users,hashtags_fw_tag_users)
hashtags_fw_ex_once_cumul.melt <- hashtags_fw_ex_once_cumul.melt[with(hashtags_fw_ex_once_cumul.melt,order(tag,date)),]
hashtags_fw_ex_once.melt <- hashtags_fw_ex_once.melt[with(hashtags_fw_ex_once.melt,order(tag,date)),]
hashtags_fw_ex_once_cumul.melt[is.na(hashtags_fw_ex_once_cumul.melt)]<-0
hashtags_fw_ex_once.melt[is.na(hashtags_fw_ex_once.melt)]<-0




# Count the tag use for the first month by each week (T.01 ~ T.16) -- hashtags
hashtags_om <- data.table(table(hashtags$tag,
                                format(hashtags$date,
                                       "%Y-%U")))
names(hashtags_om) <- c("tag","week","total")
hashtags_om <- hashtags_om[hashtags_om$total!=0,]
hashtags_om$week.no <- as.integer(as.Date(paste(hashtags_om$week,1,sep="-"),
                                          "%Y-%U-%u",
                                          origin="min(hashtags$date)"))
hashtags_om <- hashtags_om[with(hashtags_om,
                                order(tag, week.no)),]
hashtags_om$week.no <- (hashtags_om$week.no - min(hashtags_om$week.no)+7)/7
hashtags_om$week.no <- ave(hashtags_om$week.no, hashtags_om$tag,
                           FUN=function(x) c(1, diff(x)))
hashtags_om[,week.no:=cumsum(week.no), by=list(tag)]
hashtags_om <- data.frame(hashtags_om[hashtags_om$week.no <= 16])
hashtags_om$week.of <- as.Date(paste(hashtags_om$week,1,sep="-"),format="%Y-%U-%u")
hashtags_om_ex_once <- aggregate(. ~tag, data=hashtags_om[c(1,3,4)], sum)
hashtags_om_ex_once <- hashtags_om_ex_once[hashtags_om_ex_once$total!=1,]
hashtags_om_ex_once$week.no <- hashtags_om_ex_once$total <- NULL
#hashtags_om_ex_once <- data.frame(once=table(tag=hashtags_om$tag)==1)
#hashtags_om_ex_once$tag <- row.names(hashtags_om_once)
#hashtags_om_ex_once <- hashtags_om_once[hashtags_om_once$once!=1,]
#hashtags_om_ex_once$once <- NULL
hashtags_om_ex_once <- merge(hashtags_om,hashtags_om_ex_once,
                             by="tag")
for(t in unique(hashtags_om_ex_once$week.no)){
  hashtags_om_ex_once[ifelse(nchar(t)==1,paste("T.0",t,sep=""),paste("T.",t,sep=""))] <-
    ifelse(hashtags_om_ex_once$week.no==t,hashtags_om_ex_once$total,0)}
hashtags_om_ex_once$week <- hashtags_om_ex_once$week.no <- hashtags_om_ex_once$total <- hashtags_om_ex_once$week.of <- NULL
hashtags_om_ex_once <- hashtags_om_ex_once[,order(colnames(hashtags_om_ex_once))]
hashtags_om_ex_once <- aggregate(. ~tag, data=hashtags_om_ex_once, sum)
hashtags_om_ex_once_cumul <- data.frame(t(hashtags_om_ex_once[-T]))
hashtags_om_ex_once_cumul <- data.frame(hashtags_om_ex_once["tag"],
                                        t(cumsum(hashtags_om_ex_once_cumul)))
rownames(hashtags_om_ex_once_cumul) <- NULL
hashtags_om_ex_once_cumul.melt <- melt(hashtags_om_ex_once_cumul,id.vars=("tag"))
colnames(hashtags_om_ex_once_cumul.melt)[c(2,3)] <- c("time","cumul")
hashtags_om_week <- data.table(ddply(hashtags_om,~tag,summarize,
                                     week.of=seq.Date(from=min(week.of),
                                                      by="week",
                                                      length.out=length(hashtags_om_ex_once)-1)),
                               time=1)
hashtags_om_week[, time:=cumsum(time),by=list(tag)]
hashtags_om_week$time <- ifelse(nchar(hashtags_om_week$time)==1,
                                paste("T.0",hashtags_om_week$time,sep=""),
                                paste("T.",hashtags_om_week$time,sep=""))
hashtags_om_ex_once_cumul.melt <- merge(hashtags_om_ex_once_cumul.melt,
                                        hashtags_om_week,
                                        by=c("tag","time"))
#hashtags_om_project_tags <- data.table(ddply(hashtags_om,~week.of,summarize,project.tags=sum(total)))
hashtags_om_project_tags <- data.table(ddply(hashtags,~as.Date(paste(format(date,"%Y-%U"),1,sep="-"),"%Y-%U-%u"),summarize,project.tags=length(tag)))
colnames(hashtags_om_project_tags)[1] <- "week.of"
hashtags_om_ex_once_cumul.melt <- merge(hashtags_om_ex_once_cumul.melt,
                                        hashtags_om_project_tags,
                                        by="week.of",
                                        all.x=TRUE)
hashtags_om_unique_users <- data.table(ddply(hashtags,~tag+as.Date(paste(format(date,"%Y-%U"),1,sep="-"),"%Y-%U-%u"),summarize,
                                             unique.users=length(unique(user))))
colnames(hashtags_om_unique_users)[2] <- "week.of"
hashtags_om_ex_once_cumul.melt <- merge(hashtags_om_ex_once_cumul.melt,
                                        hashtags_om_unique_users,
                                        by=c("tag","week.of"),
                                        all.x=TRUE)
#hashtags_om_tag_users <- merge(hashtags,hashtags_om,by=c("tag","week.of"))
#hashtags_om_tag_users <- data.frame(ddply(hashtags_om_tag_users,~week.of,summarize,
#                                          tag_users=length(unique(user))))
hashtags_om_tag_users <- data.frame(ddply(hashtags,~week.of,summarize,
                                          tag_users=length(unique(user))))
hashtags_om_ex_once_cumul.melt <- merge(hashtags_om_ex_once_cumul.melt,
                                        hashtags_om_tag_users,
                                        by="week.of",
                                        all.x=TRUE)
hashtags_om_ex_once.melt <- melt(hashtags_om_ex_once,id.vars=("tag"))
colnames(hashtags_om_ex_once.melt)[c(2,3)] <- c("time","count")
hashtags_om_ex_once.melt <- merge(hashtags_om_ex_once.melt,
                                  hashtags_om_week,
                                  by=c("tag","time"))
hashtags_om_ex_once.melt <- merge(hashtags_om_ex_once.melt,
                                  hashtags_om_project_tags,
                                  by="week.of",
                                  all.x=TRUE)
hashtags_om_ex_once.melt <- merge(hashtags_om_ex_once.melt,
                                  hashtags_om_unique_users,
                                  by=c("tag","week.of"),
                                  all.x=TRUE)
hashtags_om_ex_once.melt <- merge(hashtags_om_ex_once.melt,
                                  hashtags_om_tag_users,
                                  by="week.of",
                                  all.x=TRUE)
remove(t,hashtags_om_week,hashtags_om_project_tags,hashtags_om_unique_users,hashtags_om_tag_users)
hashtags_om_ex_once_cumul.melt <- hashtags_om_ex_once_cumul.melt[with(hashtags_om_ex_once_cumul.melt,order(tag,week.of)),]
hashtags_om_ex_once.melt <- hashtags_om_ex_once.melt[with(hashtags_om_ex_once.melt,order(tag,week.of)),]
hashtags_om_ex_once_cumul.melt[is.na(hashtags_om_ex_once_cumul.melt)]<-0
hashtags_om_ex_once.melt[is.na(hashtags_om_ex_once.melt)]<-0
