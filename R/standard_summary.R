standard_summary <- function(df
                            , vars
                            , functions = list(
                              mean = ~mean(.x,na.rm=T)
                              , sd = ~sd(.x,na.rm=T)
                              , min = ~min(.x,na.rm=T)
                              , q10 = ~quantile(.x,0.1,na.rm=T)
                              , q25 = ~quantile(.x,0.25,na.rm=T)
                              , med = ~quantile(.x,0.5,na.rm=T)
                              , q75 = ~quantile(.x,0.75,na.rm=T)
                              , q90 = ~quantile(.x,0.90,na.rm=T)
                              , max = ~max(.x,na.rm=T)
                              , n = ~n()
                              , nmiss = ~length(which(is.na(.x))
                            ){
  gg <- as.character(groups(df))
  summary_res <- df %>% 
                    select(!!vars) %>%
                    summarise(across(.cols=c(!!vars),.fns = functions, .names="{.col}xx_xx{.fn}")) %>%
                    ungroup() %>%
                    pivot_longer(cols=contains("xx")) %>%
                    separate(name, into = c("VARIABLE", "STAT"),sep="xx_xx") %>%
                    pivot_wider(id_cols = c(gg, "VARIABLE"), values_from = value, names_from = STAT)            
}
