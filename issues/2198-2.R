benchmark <- function(df, col, summarize) {
  force(df)
  gc()
  if (summarize) {
    system.time(group_by_(df, col) %>% count())
  } else {
    system.time(group_by_(df, col))
  }
}

devtools::load_all()

benchmark(Lahman::Batting %>% mutate(id = paste(teamID, yearID, playerID)) %>% sample_frac() %>% transmute(id, n = 0), ~id, summarize = FALSE)
# benchmark(Lahman::Batting, ~playerId, ~teamId, summarize = FALSE)
