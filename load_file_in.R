load_from_file <- function(file_name_in) {
      tweets_in <- fread(file_name_in)
      colnames(tweets_in) <- c("first", 
                               "tweetCreated", 
                               "time", 
                               "ESTTweetCreated", 
                               "ESTTime",
                               "tweetsDF.id" )
      
      tweets_in <- select(tweets_in, -first) %>%
            mutate(tweetCreated = paste(tweetCreated, time, sep = " ")) %>%
            mutate(ESTTweetCreated = paste(ESTTweetCreated, ESTTime, sep = " ")) %>%
            select(tweetCreated, tweetsDF.id)
}