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

#hashtag_population <- merge(hashtag_population, hashtags_introduced, 
#                            by=c("tag"))

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



# Count the tag use for the first month by each week (T.01 ~ T.16) -- hashtag_population
hashtag_population_om <- data.table(table(hashtag_population$tag,
                                format(hashtag_population$date,
                                       "%Y-%U")))
names(hashtag_population_om) <- c("tag","week","total")
hashtag_population_om <- hashtag_population_om[hashtag_population_om$total!=0,]
hashtag_population_om$week.no <- as.integer(as.Date(paste(hashtag_population_om$week,1,sep="-"),
                                          "%Y-%U-%u",
                                          origin="min(hashtags$date)"))
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
hashtag_population_om_project_tags <- data.table(ddply(hashtags,~as.Date(paste(format(date,"%Y-%U"),1,sep="-"),"%Y-%U-%u"),summarize,project.tags=length(tag)))
colnames(hashtag_population_om_project_tags)[1] <- "week.of"
hashtag_population_om_ex_once_cumul.melt <- merge(hashtag_population_om_ex_once_cumul.melt,
                                        hashtag_population_om_project_tags,
                                        by="week.of",
                                        all.x=TRUE)
hashtag_population_om_unique_users <- data.table(ddply(hashtags,~tag+as.Date(paste(format(date,"%Y-%U"),1,sep="-"),"%Y-%U-%u"),summarize,
                                             unique.users=length(unique(user))))
colnames(hashtag_population_om_unique_users)[2] <- "week.of"
hashtag_population_om_ex_once_cumul.melt <- merge(hashtag_population_om_ex_once_cumul.melt,
                                        hashtag_population_om_unique_users,
                                        by=c("tag","week.of"),
                                        all.x=TRUE)
#hashtag_population_om_tag_users <- merge(hashtags,hashtag_population_om,by=c("tag","week.of"))
#hashtag_population_om_tag_users <- data.frame(ddply(hashtag_population_om_tag_users,~week.of,summarize,
#                                          tag.users=length(unique(user))))
hashtag_population_om_tag_users <- data.frame(ddply(hashtags,~week.of,summarize,
                                          tag.users=length(unique(user))))
hashtag_population_om_ex_once_cumul.melt <- merge(hashtag_population_om_ex_once_cumul.melt,
                                        hashtag_population_om_tag_users,
                                        by="week.of",
                                        all.x=TRUE)
hashtag_population_om_new_users <- ddply(hashtags, ~tag+user,summarize,week.of=min(week.of))
hashtag_population_om_new_users <- ddply(hashtag_population_om_new_users,~tag+week.of,summarize,
                               new.users=length(unique(user)))
hashtag_population_om_ex_once_cumul.melt <- merge(hashtag_population_om_ex_once_cumul.melt,
                                        hashtag_population_om_new_users,
                                        by=c("tag","week.of"),
                                        all.x=TRUE)
hashtag_population_om_user <- ddply(hashtags,~tag+week.of,summarize,
                          user=user)
hashtag_population_om_ex_once_cumul.melt <- merge(hashtag_population_om_ex_once_cumul.melt,
                                        hashtag_population_om_user,
                                        by=c("tag","week.of"),
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
hashtag_population_om_ex_once.melt <- merge(hashtag_population_om_ex_once.melt,
                                  hashtag_population_om_new_users,
                                  by=c("tag","week.of"),
                                  all.x=TRUE)
hashtag_population_om_ex_once.melt <- merge(hashtag_population_om_ex_once.melt,
                                  hashtag_population_om_user,
                                  by=c("tag","week.of"),
                                  all.x=TRUE)
remove(t,hashtag_population_om_week,hashtag_population_om_project_tags,hashtag_population_om_unique_users,hashtag_population_om_tag_users,hashtag_population_om_new_users)
remove(hashtag_population_om_user)
hashtag_population_om_ex_once_cumul.melt <- hashtag_population_om_ex_once_cumul.melt[with(hashtag_population_om_ex_once_cumul.melt,order(tag,week.of)),]
hashtag_population_om_ex_once.melt <- hashtag_population_om_ex_once.melt[with(hashtag_population_om_ex_once.melt,order(tag,week.of)),]
hashtag_population_om_ex_once_cumul.melt[-9][is.na(hashtag_population_om_ex_once_cumul.melt[-9])] <- 0
hashtag_population_om_ex_once.melt[-9][is.na(hashtag_population_om_ex_once.melt[-9])] <- 0
hashtag_population_om_ex_once_cumul.melt <- data.table(hashtag_population_om_ex_once_cumul.melt)[,unique.users:=cumsum(unique.users), by=list(tag)]
hashtag_population_om_ex_once_cumul.melt <- data.frame(hashtag_population_om_ex_once_cumul.melt[,new.users:=cumsum(new.users), by=list(tag)])
colnames(hashtag_population_om_ex_once_cumul.melt)[c(6,8)] <- c("cumul.unique.users","cumul.new.users")

write.csv(hashtag_population_om_ex_once.melt, file="hashtag_population_om_ex_once.melt.csv")
write.csv(hashtag_population_om_ex_once_cumul.melt, file="hashtag_population_om_ex_once_cumul.melt.csv")

T_tags_ex_NA_user <- hashtag_population_om_ex_once.melt[is.na(hashtag_population_om_ex_once.melt$user)==0,]
T_tags_ex_NA_user_cumul <- hashtag_population_om_ex_once_cumul.melt[is.na(hashtag_population_om_ex_once_cumul.melt$user)==0,]

T01_tags <- T_tags_ex_NA_user[which(T_tags_ex_NA_user$time=="T.01"),]
T01_tags_cumul <- T_tags_ex_NA_user_cumul[which(T_tags_ex_NA_user_cumul$time=="T.01"),]
write.csv(T01_tags, file="T01_tags.csv")

T02_tags <- T_tags_ex_NA_user[which(T_tags_ex_NA_user$time=="T.02"),]
T02_tags_cumul <- T_tags_ex_NA_user_cumul[which(T_tags_ex_NA_user_cumul$time=="T.02"),]
write.csv(T01_tags, file="T02_tags.csv")

T03_tags <- T_tags_ex_NA_user[which(T_tags_ex_NA_user$time=="T.03"),]
T03_tags_cumul <- T_tags_ex_NA_user_cumul[which(T_tags_ex_NA_user_cumul$time=="T.03"),]
write.csv(T01_tags, file="T03_tags.csv")

T04_tags <- T_tags_ex_NA_user[which(T_tags_ex_NA_user$time=="T.04"),]
T04_tags_cumul <- T_tags_ex_NA_user_cumul[which(T_tags_ex_NA_user_cumul$time=="T.04"),]
write.csv(T01_tags, file="T04_tags.csv")

T05_tags <- T_tags_ex_NA_user[which(T_tags_ex_NA_user$time=="T.05"),]
T05_tags_cumul <- T_tags_ex_NA_user_cumul[which(T_tags_ex_NA_user_cumul$time=="T.05"),]
write.csv(T01_tags, file="T05_tags.csv")

T06_tags <- T_tags_ex_NA_user[which(T_tags_ex_NA_user$time=="T.06"),]
T06_tags_cumul <- T_tags_ex_NA_user_cumul[which(T_tags_ex_NA_user_cumul$time=="T.06"),]
write.csv(T01_tags, file="T06_tags.csv")

T07_tags <- T_tags_ex_NA_user[which(T_tags_ex_NA_user$time=="T.07"),]
T07_tags_cumul <- T_tags_ex_NA_user_cumul[which(T_tags_ex_NA_user_cumul$time=="T.07"),]
write.csv(T01_tags, file="T07_tags.csv")

T08_tags <- T_tags_ex_NA_user[which(T_tags_ex_NA_user$time=="T.08"),]
T08_tags_cumul <- T_tags_ex_NA_user_cumul[which(T_tags_ex_NA_user_cumul$time=="T.08"),]
write.csv(T01_tags, file="T08_tags.csv")

T09_tags <- T_tags_ex_NA_user[which(T_tags_ex_NA_user$time=="T.09"),]
T09_tags_cumul <- T_tags_ex_NA_user_cumul[which(T_tags_ex_NA_user_cumul$time=="T.09"),]
write.csv(T01_tags, file="T09_tags.csv")

T10_tags <- T_tags_ex_NA_user[which(T_tags_ex_NA_user$time=="T.10"),]
T10_tags_cumul <- T_tags_ex_NA_user_cumul[which(T_tags_ex_NA_user_cumul$time=="T.10"),]
write.csv(T01_tags, file="T10_tags.csv")

T11_tags <- T_tags_ex_NA_user[which(T_tags_ex_NA_user$time=="T.11"),]
T11_tags_cumul <- T_tags_ex_NA_user_cumul[which(T_tags_ex_NA_user_cumul$time=="T.11"),]
write.csv(T01_tags, file="T11_tags.csv")

T12_tags <- T_tags_ex_NA_user[which(T_tags_ex_NA_user$time=="T.12"),]
T12_tags_cumul <- T_tags_ex_NA_user_cumul[which(T_tags_ex_NA_user_cumul$time=="T.12"),]
write.csv(T01_tags, file="T12_tags.csv")

T13_tags <- T_tags_ex_NA_user[which(T_tags_ex_NA_user$time=="T.13"),]
T13_tags_cumul <- T_tags_ex_NA_user_cumul[which(T_tags_ex_NA_user_cumul$time=="T.13"),]
write.csv(T01_tags, file="T13_tags.csv")

T14_tags <- T_tags_ex_NA_user[which(T_tags_ex_NA_user$time=="T.14"),]
T14_tags_cumul <- T_tags_ex_NA_user_cumul[which(T_tags_ex_NA_user_cumul$time=="T.14"),]
write.csv(T01_tags, file="T14_tags.csv")

T15_tags <- T_tags_ex_NA_user[which(T_tags_ex_NA_user$time=="T.15"),]
T15_tags_cumul <- T_tags_ex_NA_user_cumul[which(T_tags_ex_NA_user_cumul$time=="T.15"),]
write.csv(T01_tags, file="T15_tags.csv")

T16_tags <- T_tags_ex_NA_user[which(T_tags_ex_NA_user$time=="T.16"),]
T16_tags_cumul <- T_tags_ex_NA_user_cumul[which(T_tags_ex_NA_user_cumul$time=="T.16"),]
write.csv(T01_tags, file="T16_tags.csv")
