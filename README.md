# "Is there a 'hot-hand' effect in football ?"


Following a discussion with a friend, I wanted to check if the chances
of winning a football (soccer) game improve when the team has won the
two previous games.

This is called the ['Hot-Hand
fallacy'](https://en.wikipedia.org/wiki/Hot-hand_fallacy)

The data is from [Football-data.co.uk](http://www.football-data.co.uk/francem.php).
We have the Ligue 1 results from July 1993 to February 2016.
And the Ligue 2 results from August 1996 to February 2016.
It is a total of 15 874 games over 23 years.

The Null hypotesis is that there is no 'hot hand' effect: **the expected win-rate after two consecutive wins is not different than what should be expected from the previous games of the season**.

I'll perform a chi-squared test to compare the expected win-rate with the oberved win-rate after 2 consecutive wins.   


    #Gather the dataset to have one row per result (a result for the Home team, and one for the Away team)
    results <- games %>%
      gather(location, equipe, 4:5) %>%
      select(Div, season, Date, equipe, location, FTHG, FTAG, FTR)

    #Set the Victories as TRUE, and the Defeats and Draws as FALSE
    results$result <- NA
    results$result[(results$location ==  'HomeTeam' & results$FTR == 'H') | (results$location ==  'AwayTeam' & results$FTR == 'A') ] <- 'Win'
    results$result[results$FTR == 'D'] <- 'Draw'
    results$result[is.na(results$result)] <- 'Loss'

    #Sort by Team and Date, and add the streaks (number of consecutive wins)
    results <- results %>%
      arrange(equipe, Date)
    results$streak <- unlist(lapply(rle(paste(results$equipe, results$result))$length, function(x) rep(x, x)))
    results$streak_index <- unlist(lapply(rle(paste(results$equipe, results$result))$length, function(x) seq(1, x)))

    #Add the expected victory, based on all the season's previous games
    results <- results %>%
      group_by(equipe) %>%
      mutate(after_x_wins = ifelse(lag(result == 'Win'), lag(streak_index), NA),
             expected_wins_season = cummean(result == 'Win'),
             expected_wins_season = lag(expected_wins_season),
             expected_draw_season = cummean(result == 'Draw'),
             expected_draw_season = lag(expected_draw_season),
             expected_loss_season = cummean(result == 'Loss'),
             expected_loss_season = lag(expected_loss_season)) %>%
      ungroup()

    #We only keep the games played after two consecutive wins, and add-up all the expected and oberved wins.
    after_2_wins <- results %>%
      filter(after_x_wins == 2) %>%
      summarize(Won = sum(result == 'Win'),
                expected_Won = sum(expected_wins_season),
                Draw = sum(result == 'Draw'),
                expected_Draw = sum(expected_draw_season),
                Loss = sum(result == 'Loss'),
                expected_Loss = sum(expected_loss_season))

    after_2_wins <-data.frame(matrix(unlist(after_2_wins), ncol = 2, byrow = T, dimnames = list(c('Won', 'Draw', 'Loss'), c( 'Observed', 'Expected'))))

The contingency table :

    after_2_wins

    ##      Observed Expected
    ## Won       850 842.9788
    ## Draw      701 686.7956
    ## Loss      721 742.2256

    chisq.test(x= after_2_wins$Observed, p = after_2_wins$Expected/sum(after_2_wins$Expected))

    ## 
    ##  Chi-squared test for given probabilities
    ## 
    ## data:  after_2_wins$Observed
    ## X-squared = 0.95925, df = 2, p-value = 0.619

We cannot reject the Null Hypotesis based on this data.  
We could not detect any 'hot hand effect' in this data.
