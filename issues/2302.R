library('dplyr')
d <- data.frame(x=c(1,2,2),y=c(3,5,NA),z=c(NA,'a','b'),
                rowNum=1:3,
                stringsAsFactors = FALSE)
print(d)

fnam <- tempfile(pattern = "dplyr_doc_narm", tmpdir = tempdir(), fileext = "sqlite3")
my_db <- dplyr::src_sqlite(fnam, create = TRUE)
class(my_db)
dRemote <- copy_to(my_db,d,'d',rowNumberColumn='rowNum',overwrite=TRUE)


# correct calculation
dRemote %>% mutate(nna=0) %>%
  mutate(nna=nna+ifelse(is.na(x),1,0)) %>%
  mutate(nna=nna+ifelse(is.na(y),1,0)) %>%
  mutate(nna=nna+ifelse(is.na(z),1,0))

# incorrect calculation (last step seems to always clobber the previous result)
dRemote %>% mutate(nna=0) %>%
  mutate(nna=nna+is.na(x)) %>%
  mutate(nna=nna+is.na(y)) %>%
  mutate(nna=nna+is.na(z))

# clean up
rm(list=setdiff(ls(),'fnam'))
if(!is.null(fnam)) {
  file.remove(fnam)
}
gc()
