devtools::load_all()
batting_df <- Lahman::Batting
system.time(batting_df %>% group_by(playerID) %>% summarise(ab = mean(AB)))
