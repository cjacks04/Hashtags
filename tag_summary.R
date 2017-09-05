# Analysis for Hashtag paper

#### Some requirements for the dataset
# Data only after the launch
# Remove tags that first appeared before launch

#libraries
library(lme4)
library(plotly)
library(plyr)
library(directlabels)


summarySE <- function(data=NULL, measurevar, groupvars=NULL, na.rm=FALSE,
                      conf.interval=.95, .drop=TRUE) {
    library(plyr)

    # New version of length which can handle NA's: if na.rm==T, don't count them
    length2 <- function (x, na.rm=FALSE) {
        if (na.rm) sum(!is.na(x))
        else       length(x)
    }

    # This does the summary. For each group's data frame, return a vector with
    # N, mean, and sd
    datac <- ddply(data, groupvars, .drop=.drop,
      .fun = function(xx, col) {
        c(N    = length2(xx[[col]], na.rm=na.rm),
          mean = mean   (xx[[col]], na.rm=na.rm),
          sd   = sd     (xx[[col]], na.rm=na.rm)
        )
      },
      measurevar
    )

    # Rename the "mean" column    
    datac <- rename(datac, c("mean" = measurevar))

    datac$se <- datac$sd / sqrt(datac$N)  # Calculate standard error of the mean

    # Confidence interval multiplier for standard error
    # Calculate t-statistic for confidence interval: 
    # e.g., if conf.interval is .95, use .975 (above/below), and use df=N-1
    ciMult <- qt(conf.interval/2 + .5, datac$N-1)
    datac$ci <- datac$se * ciMult

    return(datac)
}

############ New Tags in System
setwd("~/Dropbox/INSPIRE/REU Projects/Hashtag Use/Figures")
#setwd("~/Documents/Academic/School/REU/hashtags/03/Figures")
pdf("NewTags_Daily.pdf", height=5, width=11)
ggplot(data=hashtags_introduced, aes(x=Date)) + 
  geom_bar(stat="count") +
  xlab("Week")+
  ylab("New Tags")+
  scale_x_date(breaks=date_breaks("1 month"), 
               labels=date_format("%Y-%m"))+
  theme_bw() +
  theme(
    axis.text.x=element_text(angle=90,size = 12)
  )
dev.off()


rm(list = ls()[grep("hashtags_", ls())])

PopularFWPeriodDay = hashtag_population_fw_ex_once$tag[which(hashtag_population_fw_ex_once$total>48)]

hashtag_population_om_ex_once$total <- rowSums(hashtag_population_om_ex_once[2:17])
PopularPeriodWeek = hashtag_population_om_ex_once$tag[which(hashtag_population_om_ex_once$total>124)]
remove(hashtag_population_fw,hashtag_population_om)


hashtag_population_fw_ex_once.melt$tagfrequency <- hashtag_population_fw_ex_once.melt$count/hashtag_population_fw_ex_once.melt$project.tags
hashtag_population_fw_ex_once.melt$userfrequency <- hashtag_population_fw_ex_once.melt$unique.users/hashtag_population_fw_ex_once.melt$tag_users
hashtag_population_om_ex_once.melt$tagfrequency <- hashtag_population_om_ex_once.melt$count/hashtag_population_om_ex_once.melt$project.tags
hashtag_population_om_ex_once.melt$userfrequency <- hashtag_population_om_ex_once.melt$unique.users/hashtag_population_om_ex_once.melt$tag_users

hashtag_population$Date <- as.Date(hashtag_population$time)


################################################
################################################
############### Dataset Summaries ##############
################################################
################################################

#setwd("~/Documents/Academic/School/REU/hashtags/03/Figures")
setwd("~/Dropbox/INSPIRE/REU Projects/Hashtag Use/Figures")
pdf("TagsContributed_Daily.pdf", height=5, width=11)
ggplot(data=hashtag_population, aes(x=Date)) + 
  geom_line(stat="count") +
  xlab("Week")+
  ylab("Tags Contributed")+
  scale_x_date(breaks=date_breaks("1 month"), 
               labels=date_format("%Y-%m"))+
  theme_bw() +
  theme(
    axis.text.x=element_text(angle=90,size = 12)
  ) 
dev.off()

############ Relationship between tags and users
tags_users <- ddply(hashtag_population, c("Date"), summarize,
                    users = length(unique(user)),
                    tags = length(tag))

setwd("~/Dropbox/INSPIRE/REU Projects/Hashtag Use/Figures")
#setwd("~/Documents/Academic/School/REU/hashtags/03/Figures")
pdf("Tags_User_line.pdf", height=5, width=11)
ggplot(tags_users, aes(x = Date)) +
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
remove(tags_users)

################################################
################################################
############# FW Dataset Summaries #############
################################################
################################################

fw_tag_count_summary <- ddply(hashtag_population_fw_ex_once.melt, c("tag"), summarize,
								count_mean = mean(count),
								tag_freq_mean = mean(tagfrequency,na.rm=TRUE),
								users_mean = mean(unique.users),
								user_freq_mean = mean(userfrequency,,na.rm=TRUE)
										)


fw_time_count_summary <- ddply(hashtag_population_fw_ex_once.melt, c("time"), summarize,
								count_mean = mean(count),
								tag_freq_mean = mean(tagfrequency,na.rm=TRUE),
								users_mean = mean(unique.users),
								user_freq_mean = mean(userfrequency,,na.rm=TRUE)
										)

### Visualizations for cumulative 
setwd("~/Dropbox/INSPIRE/REU Projects/Hashtag Use/Figures")
#setwd("~/Documents/Academic/School/REU/hashtags/03/Figures")
pdf("Popular_Growth_Daily.pdf", height=5, width=11)
ggplot(subset(hashtag_population_fw_ex_once_cumul.melt,tag %in% PopularFWPeriodDay)
  , aes(x = time, y=cumul,group=tag, colour=tag)) +
  geom_line(aes(linetype = tag)) +
  labs(x = "Observation Period",
       y = "Cumulative Use",
       colour= "Tag Name") + 
  theme_bw() +
  guides(colour=FALSE) +
  theme(axis.text.x=element_text(angle=90,size = 12)
    )
dev.off()


### 3D plot.
plot_ly(hashtag_population_fw_ex_once_cumul.melt.2, x = ~time, z = ~count, y = ~tag, type = 'scatter3d', mode = 'lines') %>%
add_markers() %>%
  layout(scene = list(xaxis = list(title = 'Unique Users'),
                     yaxis = list(title = 'Observation Period (Day)'),
                     zaxis = list(title = 'Number of Tags')))


ggplot(subset(hashtag_population_fw_ex_once.melt,tag %in% PopularFWPeriodDay),
       aes(x=time, y=count, group=tag)) +
       geom_line()

ggplot(hashtag_population_fw_ex_once.melt,
       aes(x=time, y=count, group=tag)) +
       geom_line()

setwd("~/Dropbox/INSPIRE/REU Projects/Hashtag Use/Figures")
# Average Day Growth
pdf("AverageFW_Growth.pdf", height=5, width=11)
ggplot(hashtag_population_fw_ex_once_cumul.melt, aes(time, cumul)) +
    stat_summary(fun.y=mean, geom="line") +
    stat_summary(fun.data=mean_se, geom="pointrange") + 
    labs( x = "Observation Period",
          y = "Average Use",
          colour= "Tag Name") + 
    theme_bw() +
    guides(colour=FALSE) +
    theme(axis.text.x=element_text(angle=90,size = 12),
          axis.text.y=element_text(size = 10)
      )
dev.off()
# Average Day Growth Popular
pdf("AverageFW_Growth_Popular.pdf", height=5, width=11)
ggplot(subset(hashtag_population_fw_ex_once_cumul.melt,tag %in% PopularFWPeriodDay), aes(time, cumul)) +
    stat_summary(fun.y=mean, geom="line") +
    stat_summary(fun.data=mean_se, geom="pointrange") + 
    labs( x = "Observation Period",
          y = "Average Use",
          colour= "Tag Name") + 
    theme_bw() +
    guides(colour=FALSE) +
    theme(axis.text.x=element_text(angle=90,size = 12),
          axis.text.y=element_text(size = 10)
      )
dev.off()

################################################
################################################
############# OM Dataset Summaries #############
################################################
################################################
om_tag_count_summary <- ddply(hashtag_population_om_ex_once.melt, c("tag"), summarize,
								count_mean = mean(count),
								tag_freq_mean = mean(tagfrequency,na.rm=TRUE),
								users_mean = mean(unique.users),
								user_freq_mean = mean(userfrequency,,na.rm=TRUE)
										)

om_time_count_summary <- ddply(hashtag_population_om_ex_once.melt, c("time"), summarize,
								count_mean = mean(count),
								tag_freq_mean = mean(tagfrequency,na.rm=TRUE),
								users_mean = mean(unique.users),
								user_freq_mean = mean(userfrequency,,na.rm=TRUE)
										)


setwd("~/Dropbox/INSPIRE/REU Projects/Hashtag Use/Figures")
#setwd("~/Documents/Academic/School/REU/hashtags/03/Figures")
pdf("Popular_Growth_Week.pdf", height=5, width=11)
ggplot(subset(hashtag_population_om_ex_once_cumul.melt,tag %in% PopularPeriodWeek)
  , aes(x = time, y=cumul,group=tag, colour=tag)) +
  geom_line(aes(linetype = tag)) +
  labs(x = "Observation Period",
       y = "Cumulative Use",
       colour= "Tag Name") + 
  theme_bw() +
  guides(colour=FALSE) +
  theme(axis.text.x=element_text(angle=90,size = 12)
    )
dev.off()

ggplot(subset(hashtag_population_om_ex_once.melt,tag %in% PopularPeriodWeek),
       aes(x=time, y=log(count), group=tag)) +
       geom_line(aes(colour=tag))  
       #geom_dl(aes(label = tag), method = list(dl.combine("last.points"), cex = 0.8))

hashtag_population_om_ex_once.melt$Popular[hashtag_population_om_ex_once.melt$tag %in% PopularPeriodWeek] <- "Top_10"
hashtag_population_om_ex_once.melt$Popular[!hashtag_population_om_ex_once.melt$tag %in% PopularPeriodWeek] <- "Other"

ggplot(hashtag_population_om_ex_once.melt,
       aes(x=time, y=count)) +
       geom_line(aes(linetype = tag))  
       #scale_colour_manual(values=c(Other="gray78",'Top_10'="gray20"))
hashtag_population_om_ex_once.melt$Popular <- NULL



setwd("~/Dropbox/INSPIRE/REU Projects/Hashtag Use/Figures")
#Average Growth
pdf("AverageOM_Growth.pdf", height=5, width=11)
ggplot(hashtag_population_om_ex_once.melt, aes(time, count)) +
    stat_summary(fun.y=mean, geom="line") +
    stat_summary(fun.data=mean_se, geom="pointrange") + 
    labs( x = "Observation Period",
          y = "Average Use",
          colour= "Tag Name") + 
    theme_bw() +
    guides(colour=FALSE) +
    theme(axis.text.x=element_text(angle=90,size = 12),
          axis.text.y=element_text(size = 10)
      )
dev.off()

#Average Popular Growth
pdf("AverageOM_Growth_Popular.pdf", height=5, width=11)
ggplot(subset(hashtag_population_om_ex_once.melt,tag %in% PopularPeriodWeek), aes(x=time, y=count)) +
    stat_summary(fun.y=mean, geom="line") +
    stat_summary(fun.data=mean_se, geom="pointrange") + 
    labs( x = "Observation Period",
          y = "Average Use",
          colour= "Tag Name") + 
    theme_bw() +
    guides(colour=FALSE) +
    theme(axis.text.x=element_text(angle=90,size = 12),
          axis.text.y=element_text(size = 10)
      )
dev.off()

pdf("AverageOM_Growth_Popular.pdf", height=5, width=11)
ggplot(hashtag_population_om_ex_once.melt, aes(time, count)) +
	stat_summary(fun.y=mean, geom="line",size=2) +
	stat_summary(fun.data=mean_se, geom="pointrange",size=.5) +
	theme_bw(base_size=10) +
	coord_cartesian(ylim=c(0, 10))
dev.off() 





