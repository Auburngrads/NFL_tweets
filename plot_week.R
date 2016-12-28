plot_week <- function(cur_week, all_tweets) {

      week_selected <- filter(all_tweets, week == cur_week) %>%
            mutate(home = as.factor(home))
      
      ggplot(week_selected, aes(x=minute)) + 
            geom_histogram(aes(weights=minute, fill=home), binwidth = 1) + 
            facet_wrap( ~ home, ncol=4, scales = "free")

}