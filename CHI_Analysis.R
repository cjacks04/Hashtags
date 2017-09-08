# Hashtag Analysis
# Philip Lee and Corey Jackson 2017

# Load Libraries
install.packages("plyr", dependencies=TRUE)
library(plyr)
install.packages("ggplot2")
library(ggplot2)
install.packages("data.table")
library(data.table)
install.packages("lme4")
library(lme4)
install.packages("reshape2")
library(reshape2)
library(scales)
library(plotly)

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
Glitches = c("blip","whistle","noneoftheabove","powerline60hz","koifish","violinmodeharmonic","chirp","lowfrequencyburst","noglitch","scatteredlight","helix","lightmodulation","lowfrequencyline","paireddoves","aircompressor50hz","repeatingblips","scratchy","tomte","wanderingline","extremelyloud")

known_hashtags <- hashtags[which(hashtags$tag %in% Glitches),]
user_hashtags <- hashtags[which(!hashtags$tag %in% Glitches),]




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
#hashtag_population <- hashtag_population[hashtag_population$after.launch.date == 1 & hashtag_population$before.last.month == 1,]["tag"]
hashtag_population <- hashtag_population[hashtag_population$before.last.month == 1,]["tag"]

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


# Add max time to hashtags_introduced and then compute difference (How can we visualize and control for date)
hashtags_introduced <- merge(hashtags_introduced,
                             ddply(hashtags, ~tag, summarize, last.use=max(time)),
                             by=c("tag"))

# First Tags
hashtags_introduced$Date <- as.Date(hashtags_introduced$first.use)
hashtags_introduced <- hashtags_introduced[complete.cases(hashtags_introduced[ ,6]),]



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
#                                          tag.users=length(unique(user))))
hashtags_om_tag_users <- data.frame(ddply(hashtags,~week.of,summarize,
                                          tag.users=length(unique(user))))
hashtags_om_ex_once_cumul.melt <- merge(hashtags_om_ex_once_cumul.melt,
                                        hashtags_om_tag_users,
                                        by="week.of",
                                        all.x=TRUE)
hashtags_om_new_users <- ddply(hashtags, ~tag+user,summarize,week.of=min(week.of))
hashtags_om_new_users <- ddply(hashtags_om_new_users,~tag+week.of,summarize,
                               new.users=length(unique(user)))
hashtags_om_ex_once_cumul.melt <- merge(hashtags_om_ex_once_cumul.melt,
                                        hashtags_om_new_users,
                                        by=c("tag","week.of"),
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
hashtags_om_ex_once.melt <- merge(hashtags_om_ex_once.melt,
                                  hashtags_om_new_users,
                                  by=c("tag","week.of"),
                                  all.x=TRUE)
remove(t,hashtags_om_week,hashtags_om_project_tags,hashtags_om_unique_users,hashtags_om_tag_users,hashtags_om_new_users)
hashtags_om_ex_once_cumul.melt <- hashtags_om_ex_once_cumul.melt[with(hashtags_om_ex_once_cumul.melt,order(tag,week.of)),]
hashtags_om_ex_once.melt <- hashtags_om_ex_once.melt[with(hashtags_om_ex_once.melt,order(tag,week.of)),]
hashtags_om_ex_once_cumul.melt[is.na(hashtags_om_ex_once_cumul.melt)] <- 0
hashtags_om_ex_once.melt[is.na(hashtags_om_ex_once.melt)] <- 0
hashtags_om_ex_once_cumul.melt <- data.table(hashtags_om_ex_once_cumul.melt)[,unique.users:=cumsum(unique.users), by=list(tag)]
hashtags_om_ex_once_cumul.melt <-data.frame(hashtags_om_ex_once_cumul.melt[,new.users:=cumsum(new.users), by=list(tag)])
colnames(hashtags_om_ex_once_cumul.melt)[c(6,8)] <- c("cumul.unique.users","cumul.new.users")
