# Analysis for Hashtag paper

#### Some requirements for the dataset
# Data only after the launch
# Remove tags that first appeared before launch

#libraries
library(lme4)
library(plotly)
library(plyr)
library(directlabels)
library(ggplot2)
library(scales)


Glitches = c("blip","whistle","noneoftheabove","powerline60hz","koifish","violinmodeharmonic","chirp","lowfrequencyburst","noglitch","scatteredlight","helix","lightmodulation","lowfrequencyline","paireddoves","aircompressor50hz","repeatingblips","scratchy","tomte","wanderingline","extremelyloud")

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


rm(list = ls()[grep("hashtags_fw", ls())])
rm(list = ls()[grep("hashtags_om", ls())])

<<<<<<< Updated upstream
hashtag_population_fw_ex_once$total <- rowSums(hashtag_population_fw_ex_once[2:17])
PopularFWPeriodDay = hashtag_population_fw_ex_once$tag[which(hashtag_population_fw_ex_once$total>48)]
=======
>>>>>>> Stashed changes

hashtag_population_fw_ex_once$total <- rowSums(hashtag_population_fw_ex_once[2:17])
hashtag_population_om_ex_once$total <- rowSums(hashtag_population_om_ex_once[2:17])

hashtag_population_om_ex_once_nocontrolled <- hashtag_population_om_ex_once[which(!hashtag_population_om_ex_once$tag %in% Glitches),]
hashtag_population_fw_ex_once_nocontrolled <- hashtag_population_fw_ex_once[which(!hashtag_population_fw_ex_once$tag %in% Glitches),]

#PopularFWPeriodDay = hashtag_population_fw_ex_once_nocontrolled$tag[which(hashtag_population_fw_ex_once_nocontrolled$total> sd(hashtag_population_fw_ex_once_nocontrolled$total)*2)]
#PopularPeriodWeek = hashtag_population_om_ex_once_nocontrolled$tag[which(hashtag_population_om_ex_once_nocontrolled$total> sd(hashtag_population_om_ex_once_nocontrolled$total)*2)]
#remove(hashtag_population_om_ex_once_nocontrolled,hashtag_population_fw_ex_once_nocontrolled)

hashtag_population_fw_ex_once.melt$tagfrequency <- hashtag_population_fw_ex_once.melt$count/hashtag_population_fw_ex_once.melt$project.tags
hashtag_population_fw_ex_once.melt$userfrequency <- hashtag_population_fw_ex_once.melt$unique.users/hashtag_population_fw_ex_once.melt$tag_users
hashtag_population_om_ex_once.melt$tagfrequency <- hashtag_population_om_ex_once.melt$count/hashtag_population_om_ex_once.melt$project.tags
hashtag_population_om_ex_once.melt$userfrequency <- hashtag_population_om_ex_once.melt$unique.users/hashtag_population_om_ex_once.melt$tag_users

hashtags$Date <- as.Date(hashtags$time)


################################################
################################################
############### Dataset Summaries ##############
################################################
################################################

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

<<<<<<< Updated upstream
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
=======
fw_tag_count_summary <- ddply(hashtag_population_fx_ex_once.melt, c("tag"), summarize,
								count_mean = mean(count),
								tag_freq_mean = mean(tagfrequency,na.rm=TRUE),
								users_mean = mean(unique.users),
								user_freq_mean = mean(userfrequency,,na.rm=TRUE)
										)


fw_time_count_summary <- ddply(hashtag_population_fx_ex_once.melt, c("time"), summarize,
								count_mean = mean(count),
								tag_freq_mean = mean(tagfrequency,na.rm=TRUE),
								users_mean = mean(unique.users),
								user_freq_mean = mean(userfrequency,,na.rm=TRUE)
										)
>>>>>>> Stashed changes

### Visualizations for cumulative 
setwd("~/Dropbox/INSPIRE/REU Projects/Hashtag Use/Figures")
pdf("Popular_Growth_Daily.pdf", height=5, width=11)
<<<<<<< Updated upstream
ggplot(subset(hashtag_population_fw_ex_once_cumul.melt,tag %in% PopularFWPeriodDay)
       , aes(x = time, y=cumul,group=tag, colour=tag)) +
=======
ggplot(subset(hashtag_population_fx_ex_once_cumul.melt,tag %in% Glitches)
  , aes(x = time, y=cumul,group=tag, colour=tag)) +
>>>>>>> Stashed changes
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
<<<<<<< Updated upstream
plot_ly(hashtag_population_fw_ex_once_cumul.melt.2, x = ~time, z = ~count, y = ~tag, type = 'scatter3d', mode = 'lines') %>%
  add_markers() %>%
=======
plot_ly(hashtag_population_fx_ex_once_cumul.melt.2, x = ~time, z = ~count, y = ~tag, type = 'scatter3d', mode = 'lines') %>%
add_markers() %>%
>>>>>>> Stashed changes
  layout(scene = list(xaxis = list(title = 'Unique Users'),
                      yaxis = list(title = 'Observation Period (Day)'),
                      zaxis = list(title = 'Number of Tags')))


ggplot(subset(hashtag_population_fx_ex_once.melt,tag %in% PopularFWPeriodDay),
       aes(x=time, y=count, group=tag)) +
  geom_line()

ggplot(hashtag_population_fx_ex_once.melt,
       aes(x=time, y=count, group=tag)) +
  geom_line()

setwd("~/Dropbox/INSPIRE/REU Projects/Hashtag Use/Figures")
# Average Day Growth
pdf("AverageFW_Growth.pdf", height=5, width=11)
<<<<<<< Updated upstream
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
=======
ggplot(hashtag_population_fx_ex_once_cumul.melt, aes(time, cumul)) +
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
ggplot(subset(hashtag_population_fx_ex_once_cumul.melt,tag %in% PopularFWPeriodDay), aes(time, cumul)) +
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
>>>>>>> Stashed changes
dev.off()

################################################
################################################
############# OM Dataset Summaries #############
################################################
################################################
<<<<<<< Updated upstream
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
=======
om_tag_count_summary <- ddply(hashtag_population_om_ex_once_cumul.melt, c("tag"), summarize,
								count_mean = mean(count),
								tag_freq_mean = mean(tagfrequency,na.rm=TRUE),
								users_mean = mean(unique.users),
								user_freq_mean = mean(userfrequency,,na.rm=TRUE)
										)

om_time_count_summary <- ddply(hashtag_population_om_ex_once_cumul.melt, c("time"), summarize,
								count_mean = mean(count),
								tag_freq_mean = mean(tagfrequency,na.rm=TRUE),
								users_mean = mean(unique.users),
								user_freq_mean = mean(userfrequency,,na.rm=TRUE)
										)
>>>>>>> Stashed changes


setwd("~/Dropbox/INSPIRE/REU Projects/Hashtag Use/Figures")
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


ggplot(hashtag_population_om_ex_once_cumul.melt, aes(x = time, y=log(cumul),group=tag)) +
  geom_line(aes(linetype = tag)) +
  labs(x = "Observation Period",
       y = "Cumulative Use",
       colour= "Tag Name") + 
  theme_bw() +
  theme(axis.text.x=element_text(angle=90,size = 12),
    legend.position="none"
    )

ggplot(subset(hashtag_population_om_ex_once_cumul.melt,tag %in% PopularPeriodWeek),
       aes(x=time, y=log(count), group=tag)) +
  geom_line(aes(colour=tag))  
#geom_dl(aes(label = tag), method = list(dl.combine("last.points"), cex = 0.8))

hashtag_population_om_ex_once_cumul.melt$Popular[hashtag_population_om_ex_once_cumul.melt$tag %in% PopularPeriodWeek] <- "Top_10"
hashtag_population_om_ex_once_cumul.melt$Popular[!hashtag_population_om_ex_once_cumul.melt$tag %in% PopularPeriodWeek] <- "Other"

ggplot(hashtag_population_om_ex_once_cumul.melt,
       aes(x=time, y=count)) +
<<<<<<< Updated upstream
  geom_line(aes(linetype = tag))  
#scale_colour_manual(values=c(Other="gray78",'Top_10'="gray20"))
hashtag_population_om_ex_once.melt$Popular <- NULL
=======
       geom_line(aes(linetype = tag))  
       #scale_colour_manual(values=c(Other="gray78",'Top_10'="gray20"))
hashtag_population_om_ex_once_cumul.melt$Popular <- NULL
>>>>>>> Stashed changes



setwd("~/Dropbox/INSPIRE/REU Projects/Hashtag Use/Figures")
#Average Growth
pdf("AverageOM_Growth.pdf", height=5, width=11)
<<<<<<< Updated upstream
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
=======
ggplot(hashtag_population_om_ex_once_cumul.melt, aes(time, count)) +
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
>>>>>>> Stashed changes
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
<<<<<<< Updated upstream
  stat_summary(fun.y=mean, geom="line",size=2) +
  stat_summary(fun.data=mean_se, geom="pointrange",size=.5) +
  theme_bw(base_size=10) +
  coord_cartesian(ylim=c(0, 10))
dev.off()
=======
	stat_summary(fun.y=mean, geom="line",size=2) +
	stat_summary(fun.data=mean_se, geom="pointrange",size=.5) +
	theme_bw(base_size=10) +
	coord_cartesian(ylim=c(0, 10))
dev.off() 

######################
####### GUILDS #######
######################

hashtags_introduced$guild[hashtags_introduced$unique.users == 1 ] <- "1" 
hashtags_introduced$guild[hashtags_introduced$unique.users > 1 & hashtags_introduced$unique.users <= 4] <- "2-4" 
hashtags_introduced$guild[hashtags_introduced$unique.users >= 5 & hashtags_introduced$unique.users <= 9] <- "5-9" 
hashtags_introduced$guild[hashtags_introduced$unique.users >= 10] <- ">= 10" 

hashtags_introduced_folks <- hashtags_introduced[which(!hashtags_introduced$tag %in% Glitches),]

hashtags_guild <- ddply(hashtags_introduced_folks, c("guild"), summarize,
                    size = length(unique(tag)),
                    users = sum(unique.users),
                    average_use = mean(use.count),
                    sd_use = sd(use.count),
                    guild_lifespan = mean(as.Date(as.character(last.use), format="%Y-%m-%d %H:%M:%S")-
                  as.Date(as.character(first.use), format="%Y-%m-%d %H:%M:%S")),
                    guild_median = median(as.Date(as.character(last.use), format="%Y-%m-%d %H:%M:%S")-
                  as.Date(as.character(first.use), format="%Y-%m-%d %H:%M:%S")),
                    guild_sd = sd(as.Date(as.character(last.use), format="%Y-%m-%d %H:%M:%S")-
                  as.Date(as.character(first.use), format="%Y-%m-%d %H:%M:%S")),
                    guild_start = mean(as.Date(as.character("2017-06-14"), format="%Y-%m-%d")-
                  as.Date(as.character(first.use), format="%Y-%m-%d %H:%M:%S"))
  )

#############################
####### Tag Gardening #######
#############################





#############################
######## Sub Classes ########
#############################
# Get tags scratchy
              # crown
              # elf
              # noiseband
                  # breeze
                  # scratchy

# Create histogram of tags x=time, y=
scratchy <- c("scratchy","crown","elf","noiseband","breeze")


scratchy_sub <- hashtag_population[(grepl(paste(scratchy, collapse = "|"), hashtag_population$tag)),]
scratchy_sub$class[grepl("scratchy", scratchy_sub$tag)] <- "scratchy"
scratchy_sub$class[grepl("crown", scratchy_sub$tag)] <- "crown"
scratchy_sub$class[grepl("elf", scratchy_sub$tag)] <- "elf"
scratchy_sub$class[grepl("noiseband", scratchy_sub$tag)] <- "noiseband"
scratchy_sub$class[grepl("breeze", scratchy_sub$tag)] <- "breeze"

scratchy_sub$class[grepl("breeze", scratchy_sub$tag)] <- "breeze"


ggplot(scratchy_sub,aes(x=date, y=tag,colour=class))+
geom_point()+ 
facet_grid(. ~ class) +
theme(axis.text.x=element_text(angle=90,size = 10),
      axis.text.y=element_text(size = 10)
      )






#############################
####### To Controlled #######
#############################



>>>>>>> Stashed changes
