correct_quarter <- function(in_game, all_tweets) {
      
      tweets_out <- mutate(in_game, quarter = ceiling(minute*4/minute[1]))
      all_tweets <<- rbind(all_tweets, tweets_out)
      tweets_out
}