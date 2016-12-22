clean_tweet_data <- function(tweet_data, 
                             start_time, 
                             end_time, 
                             home, 
                             away, 
                             week_num, 
                             all_tweets) {

      tweets_out <- unique(tweet_data) %>%
            rename(tweet_ID = tweetsDF.id) %>%
            filter(tweet_ID != "this is not an ID") %>%
            select(tweetCreated, tweet_ID) %>%
            filter(tweetCreated > start_time) %>%
            filter(tweetCreated < end_time) %>%
            mutate(home = home, away = away, week = week_num) %>%
            mutate(minute = as.integer(difftime(tweetCreated, start_time, units = "mins"))) %>%
            mutate(quarter = ceiling(minute*4/minute[1]))
      
      all_tweets <<- rbind(all_tweets, tweets_out)
      tweets_out
}