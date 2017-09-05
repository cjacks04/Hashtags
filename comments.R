# Corey Jackson
# Comments in GS  


library(RJSONIO)
library(reshape)
library(reshape2)
library(dplyr)
library(tidyjson)  #https://github.com/sailthru/tidyjson
library(rjson)
library(readr)
library(plyr)

# Setwd to 
setwd("~/Dropbox/INSPIRE/Data/System dumps/GS Comments")
json <- read_file("project-1104-comments_2017-09-04.json")

# Import Survey results
comments <- json %>% as.tbl_json %>% 
    gather_array %>% 
    spread_values(
    board_id = jstring("board_id"),
    board_title = jstring("board_title"),
    board_description  = jstring("board_description"),
    discussion_id  = jstring("discussion_id"),
    discussion_title  = jstring("discussion_title"),
    comment_id  = jstring("comment_id"),
    comment_body  = jstring("comment_body"),
    comment_focus_type  = jstring("comment_focus_type"),
    comment_user_id  = jstring("comment_user_id"),
    comment_user_login	  = jstring("comment_user_login"),
    comment_created_at	  = jstring("comment_created_at")
    )

# Format Date for analysis
comments$date <- as.Date(substring(comments$comment_created_at, 1, 10))
comments$hour <- substring(comments$comment_created_at, 12, 19)
comments$created_at <- as.POSIXct(as.character(paste(comments$date,comments$hour)),, format="%Y-%m-%d %H:%M:%S")
comments$date <- comments$hour <- comments$comment_created_at <- NULL

 # Align with hashtag period
comments2 <- comments[which(comments$created_at <= '2017-07-14 17:23:04'),]

# Number of comments with hashtags
length(grep("#+", comments2$comment_body, perl=TRUE, value=TRUE))


comment_summary <- ddply(comments2, c("board_title"), summarise,
                               unique_users = length(unique(comment_user_login)),
                               total_comments = length(board_id),
                               tags = length(grep("#+",comment_body, perl=TRUE, value=TRUE)),
                               unique_tags = length(unique(grep("#+",comment_body, perl=TRUE, value=TRUE))),
                               mean_length = mean(nchar(grep("#+",comment_body, perl=TRUE, value=TRUE))),
                               sd_length = sd(nchar(grep("#+",comment_body, perl=TRUE, value=TRUE))),
                               median_length = median(nchar(grep("#+",comment_body, perl=TRUE, value=TRUE)))
                               )
















