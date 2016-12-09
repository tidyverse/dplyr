devtools::load_all()

df1 <- data.frame(a = c(1,2,NA), b = c(5,NA, NA))
df2 <- data.frame(a = c(1,NA,NA), c = c(9,8, NA))
left_join(df1, df2)

src <- src_sqlite("", create = TRUE)
sqlite1 <- copy_to(src, df1)
sqlite2 <- copy_to(src, df2)
left_join(sqlite1, sqlite2)

src <- src_postgres()
postgres1 <- copy_to(src, df1, temporary = TRUE, name = random_table_name())
postgres2 <- copy_to(src, df2, temporary = TRUE, name = random_table_name())
left_join(postgres1, postgres2)
