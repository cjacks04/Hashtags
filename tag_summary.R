# Analysis for Hashtag paper

#### Some requirements for the dataset
# Data only after the launch
# Remove tags that first appeared before launch

#libraries
library(lme4)



##### Summary Stats #####
# How many tags in dataset:
# Average time in dataset:
# How many users user hashtags: 
# How many users introduced tags: 
# Days after project start are tags in system (create Figure. x = date, y = count of tags)
# Distribution of uses. How many times tags used (e.g., 20% of tags used once, 40% used more than 10 occassions)
# Distribution of duration. (e.g., 20% tags around 1-5 days) You should create bins up to 30
# Distribution of users. (e.g., 20% tags were used by 1-5 users). You should create bins up to 20


##### Hypothesis Testing/Growth Models #####

# http://www.danmirman.org/gca
# https://www.khanacademy.org/science/biology/ecology/population-growth-and-regulation/a/exponential-logistic-growth


# H2 Tags introduced by authority sources (i.e., Power Users, moderators. You should create binary in dataset. In the example below I've named it type) have significantly greater growth curves.
m2.base <- lmer(value ~  1 + (1 | tag), data=hashtag_population_om.l, REML=F)
m2.type <- lmer(value ~  variable + (1 | tag), data=hashtag_population_om.l, REML=F)
anova(m2.base,m2.type)

# H1 Tags accompanied with use materials (e.g., Talk pages describing use) have significantly greater growth curves than those with no Talk pages.
m1.base lmer(use ~  time + users + materials + (1 | tag), data=hashtags, REML=F)
m1.materials lmer(use ~  time users + (1 | tag), data=hashtags, REML=F)
anova(m1.materials,m1.base)

##### Predicting Hashtag Growth #####
# Based on popular hashtags can we predict which tags will go viral in future. 

# Import comments and search for tags
new.data.frame <- old.data.frame[grep("word1", data$Column2, perl=TRUE), ]
 # or 

  dat[ with(dat,  grepl("ADN", bName)  &  pName == "2011-02-10_R2" ) , ]
