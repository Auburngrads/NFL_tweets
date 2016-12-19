## This function will get the created value from tweets 
## for a specified date and a certain hastag.


getTimeOfTweets <- function(hashtag, numberToScrape, dateTo, dateFrom, max = NULL) {
      
      
      tweets <- searchTwitter(hashtag, n=numberToScrape, lang="en", since = dateTo, until = dateFrom, maxID = max)
      tweetsDF <- twListToDF(tweets)
      tweetCreated <- ymd_hms(tweetsDF$created)
      ESTTweetCreated <- with_tz(tweetCreated, tzone = "America/New_York")
      tweetDFOut <- data.frame(tweetCreated, 
                               ESTTweetCreated, 
                               tweetsDF$text, 
                               tweetsDF$id,
                               stringsAsFactors=FALSE)
      
}

startTimeOfTweets <- function(gameName, numberToScrape, dateTo, dateFrom, startTime) {
      maxIn <- NULL
      hashtag <- paste('#', gameName, sep = '')
      timeToTest <- as.POSIXct(paste(dateFrom, "23:59:00"),  format="%Y-%m-%d %H:%M:%S")
      startTime <- as.POSIXct(startTime,  format="%Y-%m-%d %H:%M:%S")
    
      tweetsOut <- data.frame(tweetCreated = as.Date(character()),
                        ESTTweetCreated = as.Date(character()), 
                        tweetsDF.text = character(),
                        tweetsDF.id = character(),
                        stringsAsFactors = FALSE)
      timesOut <- data.frame(tweetCreated = as.Date(character()),
                             ESTTweetCreated = as.Date(character()),
                             tweetsDF.id = character(),
                             stringsAsFactors = FALSE)
      ##print("waiting....")
      ##Sys.sleep(500)
      
      while (timeToTest > startTime) {
            tweetsSearched <- getTimeOfTweets(hashtag, numberToScrape, dateTo, dateFrom, maxIn)
            timesSearched <-data.frame(tweetCreated = tweetsSearched$tweetCreated, 
                                       ESTTweetCreated = tweetsSearched$ESTTweetCreated, 
                                       tweetsDF.id = tweetsSearched$tweetsDF.id,
                                       stringsAsFactors = FALSE)
            len <- length(tweetsSearched$tweetCreated)
            maxIn <- as.numeric(tweetsSearched[len, c('tweetsDF.id')])
            print(maxIn)
            timeToTest <- ymd_hms(tweetsSearched[length(tweetsSearched$tweetCreated), c('tweetCreated')])
            print(timeToTest)
            write.table(tweetsSearched, file = paste(gameName, '_tweets.txt', sep = ""), append = TRUE, col.names = FALSE)
            write.table(timesSearched, file = paste(gameName, '_data.txt', sep = ""), append = TRUE, col.names = FALSE)
            tweetsOut <- unique(rbind(tweetsOut, tweetsSearched))
            timesOut <- unique(rbind(timesOut, timesSearched))
            print("waiting....")
            Sys.sleep(100)
            print("searching...")
      }
      timesOut
}