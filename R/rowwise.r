rowwise <- function(data){
  structure( data, class = c("rowwise_df", "tbl_df", "data.frame") )
}
