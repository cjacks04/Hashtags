# Corey Jackson
# JSON to CSV for Zoonvierse Hashtag file


library(RJSONIO)
library(reshape)
library(reshape2)
library(dplyr)
library(tidyjson)  #https://github.com/sailthru/tidyjson
library(rjson)
library(readr)
library(plyr)

# Import file
hashtags <- read_file("~/Dropbox/INSPIRE/Data/System dumps/GS Comments/project-1104-tags_2017-07-14.json")

# Take json to csv  
hashtags_format <- hashtags %>% as.tbl_json %>% 
    gather_array %>% 
     spread_values(
    id = jstring("id"),
    tag = jstring("name"),
    user = jstring("user_login"),
    user_id = jstring("user_id"),
    created_at = jstring("created_at"),
    type = jstring("taggable_type"),
    comment_id = jstring("comment_id"),
    taggable_id = jstring("taggable_id")
    ) 

remove(hashtags)

# Export file to Dropbox
setwd("~/Dropbox/INSPIRE/REU Projects/Hashtag Use/Dataset")
write.csv(hashtags_format, "hashtags_format.csv")