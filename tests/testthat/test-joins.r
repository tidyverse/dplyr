context("Joins")

# Univariate keys --------------------------------------------------------------

a <- data.frame(x = c(1, 1, 2, 3), y = 1:4)
b <- data.frame(x = c(1, 2, 2, 4), z = 1:4)

test_that("univariate inner join has all columns, repeated matching rows", {
  j <- inner_join(a, b, "x")

  expect_equal(names(j), c("x", "y", "z"))
  expect_equal(j$y, c(1, 2, 3, 3))
  expect_equal(j$z, c(1, 1, 2, 3))
})

test_that("univariate left join has all columns, all rows", {
  j1 <- left_join(a, b, "x")
  j2 <- left_join(b, a, "x")

  expect_equal(names(j1), c("x", "y", "z"))
  expect_equal(names(j2), c("x", "z", "y"))

  expect_equal(j1$z, c(1, 1, 2, 3, NA))
  expect_equal(j2$y, c(1, 2, 3, 3, NA))
})

test_that("univariate semi join has x columns, matching rows", {
  j1 <- semi_join(a, b, "x")
  j2 <- semi_join(b, a, "x")

  expect_equal(names(j1), c("x", "y"))
  expect_equal(names(j2), c("x", "z"))

  expect_equal(j1$y, 1:3)
  expect_equal(j2$z, 1:3)
})

test_that("univariate anti join has x columns, missing rows", {
  j1 <- anti_join(a, b, "x")
  j2 <- anti_join(b, a, "x")

  expect_equal(names(j1), c("x", "y"))
  expect_equal(names(j2), c("x", "z"))

  expect_equal(j1$y, 4)
  expect_equal(j2$z, 4)
})

test_that("univariate right join has all columns, all rows", {
  j1 <- right_join(a, b, "x")
  j2 <- right_join(b, a, "x")

  expect_equal(names(j1), c("x", "y", "z"))
  expect_equal(names(j2), c("x", "z", "y"))

  expect_equal(j1$x, c(1, 1, 2, 2, 4))
  expect_equal(j1$y, c(1, 2, 3, 3, NA))
  expect_equal(j1$z, c(1, 1, 2, 3, 4))

  expect_equal(j2$x, c(1, 1, 2, 2, 3))
  expect_equal(j2$y, c(1, 2, 3, 3, 4))
  expect_equal(j2$z, c(1, 1, 2, 3, NA))
})

# Bivariate keys ---------------------------------------------------------------

c <- data.frame(
  x = c(1, 1, 2, 3),
  y = c(1, 1, 2, 3),
  a = 1:4)
d <- data.frame(
  x = c(1, 2, 2, 4),
  y = c(1, 2, 2, 4),
  b = 1:4)

test_that("bivariate inner join has all columns, repeated matching rows", {
  j <- inner_join(c, d, c("x", "y"))

  expect_equal(names(j), c("x", "y", "a", "b"))
  expect_equal(j$a, c(1, 2, 3, 3))
  expect_equal(j$b, c(1, 1, 2, 3))
})

test_that("bivariate left join has all columns, all rows", {
  j1 <- left_join(c, d, c("x", "y"))
  j2 <- left_join(d, c, c("x", "y"))

  expect_equal(names(j1), c("x", "y", "a", "b"))
  expect_equal(names(j2), c("x", "y", "b", "a"))

  expect_equal(j1$b, c(1, 1, 2, 3, NA))
  expect_equal(j2$a, c(1, 2, 3, 3, NA))
})

test_that("bivariate semi join has x columns, matching rows", {
  j1 <- semi_join(c, d, c("x", "y"))
  j2 <- semi_join(d, c, c("x", "y"))

  expect_equal(names(j1), c("x", "y", "a"))
  expect_equal(names(j2), c("x", "y", "b"))

  expect_equal(j1$a, 1:3)
  expect_equal(j2$b, 1:3)
})

test_that("bivariate anti join has x columns, missing rows", {
  j1 <- anti_join(c, d, c("x", "y"))
  j2 <- anti_join(d, c, c("x", "y"))

  expect_equal(names(j1), c("x", "y", "a"))
  expect_equal(names(j2), c("x", "y", "b"))

  expect_equal(j1$a, 4)
  expect_equal(j2$b, 4)
})


# Duplicate column names --------------------------------------------------

e <- data.frame(x = c(1, 1, 2, 3), z = 1:4)
f <- data.frame(x = c(1, 2, 2, 4), z = 1:4)

test_that("univariate inner join has all columns, repeated matching rows", {
  j <- inner_join(e, f, "x")

  expect_equal(names(j), c("x", "z.x", "z.y"))
  expect_equal(j$z.x, c(1, 2, 3, 3))
  expect_equal(j$z.y, c(1, 1, 2, 3))
})

test_that("univariate left join has all columns, all rows", {
  j1 <- left_join(e, f, "x")
  j2 <- left_join(f, e, "x")

  expect_equal(names(j1), c("x", "z.x", "z.y"))
  expect_equal(names(j2), c("x", "z.x", "z.y"))

  expect_equal(j1$z.y, c(1, 1, 2, 3, NA))
  expect_equal(j2$z.y, c(1, 2, 3, 3, NA))
})

test_that("can control suffixes with suffix argument", {
  j1 <- inner_join(e, f, "x", suffix = c("1", "2"))
  j2 <- left_join(e, f, "x", suffix = c("1", "2"))
  j3 <- right_join(e, f, "x", suffix = c("1", "2"))
  j4 <- full_join(e, f, "x", suffix = c("1", "2"))

  expect_named(j1, c("x", "z1", "z2"))
  expect_named(j2, c("x", "z1", "z2"))
  expect_named(j3, c("x", "z1", "z2"))
  expect_named(j4, c("x", "z1", "z2"))
})

test_that("inner_join does not segfault on NA in factors (#306)", {
  a <- data.frame(x=c("p", "q", NA), y=c(1, 2, 3), stringsAsFactors=TRUE)
  b <- data.frame(x=c("p", "q", "r"), z=c(4,5,6), stringsAsFactors=TRUE)
  expect_warning(res <- inner_join(a, b, "x"), "joining factors with different levels")
  expect_equal( nrow(res), 2L )
})

test_that("joins don't reorder columns #328", {
  a <- data.frame(a=1:3)
  b <- data.frame(a=1:3, b=1, c=2, d=3, e=4, f=5)
  res <- left_join(a, b, "a")
  expect_equal( names(res), names(b) )
})

test_that("join handles type promotions #123", {
  df <- data.frame(
    V1 = c(rep("a",5), rep("b",5)),
    V2 = rep(c(1:5), 2),
    V3 = c(101:110),
    stringsAsFactors = FALSE
  )

  match <- data.frame(
    V1 = c("a", "b"),
    V2 = c(3.0, 4.0),
    stringsAsFactors = FALSE
  )
  res <- semi_join(df, match, c("V1", "V2"))
  expect_equal( res$V2, 3:4 )
  expect_equal( res$V3, c(103L, 109L) )
})

test_that("indices don't get mixed up when nrow(x) > nrow(y). #365",{
  a <- data.frame(V1 = c(0, 1, 2), V2 = c("a", "b", "c"), stringsAsFactors = FALSE)
  b <- data.frame(V1 = c(0, 1), V3 = c("n", "m"), stringsAsFactors = FALSE)
  res <- inner_join(a, b, by = "V1")
  expect_equal( res$V1, c(0,1) )
  expect_equal( res$V2, c("a", "b"))
  expect_equal( res$V3, c("n", "m"))
})

test_that("join functions error on column not found #371", {
  expect_error(
    left_join(data.frame(x=1:5), data.frame(y=1:5), by="x"),
    "column not found in lhs"
  )
  expect_error(
    left_join(data.frame(x=1:5), data.frame(y=1:5), by="y"),
    "column not found in rhs"
  )
  expect_error(
    left_join(data.frame(x=1:5), data.frame(y=1:5)),
    "No common variables"
  )
})

test_that("inner_join is symmetric (even when joining on character & factor)", {
  foo <- data_frame(id = factor(c("a", "b")), var1 = "foo")
  bar <- data_frame(id = c("a", "b"), var2 = "bar")

  expect_warning(tmp1 <- inner_join(foo, bar, by="id"), "joining factor and character")
  expect_warning(tmp2 <- inner_join(bar, foo, by="id"), "joining character vector and factor")

  expect_is(tmp1$id, "character")
  expect_is(tmp2$id, "character")

  expect_equal(names(tmp1), c("id", "var1", "var2"))
  expect_equal(names(tmp2), c("id", "var2", "var1"))

  expect_equal(tmp1, tmp2)
})

test_that("inner_join is symmetric, even when type of join var is different (#450)", {
  foo <- tbl_df(data.frame(id = 1:10, var1 = "foo"))
  bar <- tbl_df(data.frame(id = as.numeric(rep(1:10, 5)), var2 = "bar"))

  tmp1 <- inner_join(foo, bar, by="id")
  tmp2 <- inner_join(bar, foo, by="id")

  expect_equal(names(tmp1), c("id", "var1", "var2"))
  expect_equal(names(tmp2), c("id", "var2", "var1"))

  expect_equal(tmp1, tmp2)
})

test_that("left_join by different variable names (#617)",{
  x <- data_frame(x1 = c(1, 3, 2))
  y <- data_frame(y1 = c(1, 2, 3), y2 = c("foo", "foo", "bar"))
  res <- left_join(x, y, by = c("x1" = "y1"))
  expect_equal(names(res), c("x1", "y2" ) )
  expect_equal(res$x1, c(1,3,2))
  expect_equal(res$y2, c("foo", "bar", "foo"))
})

test_that("joins support comple vectors" ,{
  a <- data.frame(x = c(1, 1, 2, 3)*1i, y = 1:4)
  b <- data.frame(x = c(1, 2, 2, 4)*1i, z = 1:4)
  j <- inner_join(a, b, "x")

  expect_equal(names(j), c("x", "y", "z"))
  expect_equal(j$y, c(1, 2, 3, 3))
  expect_equal(j$z, c(1, 1, 2, 3))
})

test_that("joins suffix variable names (#655)" ,{
  a <- data.frame(x=1:10,y=2:11)
  b <- data.frame(z=5:14,x=3:12) # x from this gets suffixed by .y
  res <- left_join(a,b,by=c('x'='z'))
  expect_equal(names(res), c("x", "y", "x.y" ) )

  a <- data.frame(x=1:10,z=2:11)
  b <- data.frame(z=5:14,x=3:12) # x from this gets suffixed by .y
  res <- left_join(a,b,by=c('x'='z'))

})

test_that("right_join gets the column in the right order #96", {
  a <- data.frame(x=1:10,y=2:11)
  b <- data.frame(x=5:14,z=3:12)
  res <- right_join(a,b)
  expect_equal(names(res), c("x", "y", "z"))

  a <- data.frame(x=1:10,y=2:11)
  b <- data.frame(z=5:14,a=3:12)
  res <- right_join(a,b, by= c("x"="z"))
  expect_equal(names(res), c("x", "y", "a"))

})

test_that("full_join #96",{
  a <- data.frame(x=1:3,y=2:4)
  b <- data.frame(x=3:5,z=3:5)
  res <- full_join(a,b, "x")
  expect_equal(res$x, 1:5)
  expect_equal(res$y[1:3], 2:4)
  expect_true( all(is.na(res$y[4:5]) ))

  expect_true( all(is.na(res$z[1:2]) ))
  expect_equal( res$z[3:5], 3:5 )

})

test_that("JoinStringFactorVisitor and JoinFactorStringVisitor handle NA #688", {
  x <- data.frame(Greek = c("Alpha", "Beta", NA), numbers= 1:3)
  y <- data.frame(Greek = c("Alpha", "Beta", "Gamma"),
                        Letters = c("C", "B", "C"), stringsAsFactors = F)

  expect_warning(res <- left_join(x, y, by = "Greek"), "joining character vector")
  expect_true( is.na(res$Greek[3]) )
  expect_true( is.na(res$Letters[3]) )
  expect_equal( res$numbers, 1:3 )

  expect_warning(res <- left_join(y, x, by="Greek" ), "joining factor")
  expect_equal( res$Greek, y$Greek)
  expect_equal( res$Letters, y$Letters )
  expect_equal( res$numbers[1:2], 1:2 )
  expect_true( is.na(res$numbers[3]) )
})


test_that("JoinFactorFactorVisitor_SameLevels preserve levels order (#675)",{
  input <- data.frame(g1 = factor(c('A','B','C'), levels = c('B','A','C')))
  output <- data.frame(
    g1 = factor(c('A','B','C'), levels = c('B','A','C')),
    g2 = factor(c('A','B','C'), levels = c('B','A','C'))
  )

  res <- inner_join(group_by(input, g1), group_by(output, g1))
  expect_equal( levels(res$g1), levels(input$g1))
  expect_equal( levels(res$g2), levels(output$g2))
})

test_that("inner_join does not reorder (#684)", {
  test <- data_frame(Greek = c("Alpha", "Beta", "Gamma"), Letters = LETTERS[1:3])
  lookup <- data_frame(Letters = c("C", "B", "C"))
  res <- inner_join(lookup, test)
  expect_equal( res$Letters, c("C", "B", "C" ) )
})

test_that("joins coerce factors with different levels to character (#684)", {
  d1 <- data_frame( a = factor( c("a", "b", "c" ) ) )
  d2 <- data_frame( a = factor( c("a", "e" ) ) )
  expect_warning( { res <- inner_join( d1, d2 ) })
  expect_is( res$a, "character" )

  # different orders
  d2 <- d1
  attr( d2$a, "levels" ) <- c("c", "b", "a" )
  expect_warning( { res <- inner_join( d1, d2 ) })
  expect_is( res$a, "character" )

})

test_that("joins between factor and character coerces to character with a warning (#684)", {
  d1 <- data_frame( a = factor( c("a", "b", "c" ) ) )
  d2 <- data_frame( a = c("a", "e" ) )
  expect_warning( { res <- inner_join( d1, d2 ) })
  expect_is( res$a, "character" )

  expect_warning( { res <- inner_join( d2, d1 ) })
  expect_is( res$a, "character" )

})

# Guessing variables in x and y ------------------------------------------------

test_that("unnamed vars are the same in both tables", {
  by1 <- common_by(c("x", "y", "z"))
  expect_equal(by1$x, c("x", "y", "z"))
  expect_equal(by1$y, c("x", "y", "z"))

  by2 <- common_by(c("x" = "a", "y", "z"))
  expect_equal(by2$x, c("x", "y", "z"))
  expect_equal(by2$y, c("a", "y", "z"))
})

test_that("join columns are not moved to the left (#802)", {
  df1 <- data.frame(x = 1, y = 1:5)
  df2 <- data.frame(y = 1:5, z = 2)

  out <- left_join(df1, df2)
  expect_equal(names(out), c("x", "y", "z"))
})

test_that("join can handle multiple encodings (#769)", {
  x <- data_frame(name=c("\xC9lise","Pierre","Fran\xE7ois"),score=c(5,7,6))
  y <- data_frame(name=c("\xC9lise","Pierre","Fran\xE7ois"),attendance=c(8,10,9))
  res <- left_join(x, y, by = "name")
  expect_equal( nrow(res), 3L)
  expect_equal( res$name, x$name)

  x <- data_frame(name=factor(c("\xC9lise","Pierre","Fran\xE7ois")),score=c(5,7,6))
  y <- data_frame(name=c("\xC9lise","Pierre","Fran\xE7ois"),attendance=c(8,10,9))
  res <- suppressWarnings( left_join(x, y, by = "name") )
  expect_equal( nrow(res), 3L)
  expect_equal( res$name, y$name)

  x <- data_frame(name=c("\xC9lise","Pierre","Fran\xE7ois"),score=c(5,7,6))
  y <- data_frame(name=factor(c("\xC9lise","Pierre","Fran\xE7ois")),attendance=c(8,10,9))
  res <- suppressWarnings( left_join(x, y, by = "name") )
  expect_equal( nrow(res), 3L)
  expect_equal( res$name, x$name)

  x <- data_frame(name=factor(c("\xC9lise","Fran\xE7ois","Pierre")),score=c(5,7,6))
  y <- data_frame(name=factor(c("\xC9lise","Pierre","Fran\xE7ois")),attendance=c(8,10,9))
  res <- suppressWarnings( left_join(x, y, by = "name") )
  expect_equal( nrow(res), 3L)
  expect_equal( res$name, x$name)
})

test_that("join creates correctly named results (#855)", {
  x <- data.frame(q=c("a","b","c"),r=c("d","e","f"),s=c("1","2","3"))
  y <- data.frame(q=c("a","b","c"),r=c("d","e","f"),t=c("xxx","xxx","xxx"))
  res <- left_join(x,y,by=c("r","q"))
  expect_equal(names(res), c("q", "r", "s", "t") )
  expect_equal(res$q, x$q)
  expect_equal(res$r, x$r)
})

test_that("inner join gives same result as merge. #1281", {
  set.seed(75)
  x <- data.frame(cat1 = sample(c("A", "B", NA), 5, 1),
    cat2 = sample(c(1, 2, NA), 5, 1), v = rpois(5, 3),
    stringsAsFactors = FALSE)
  y <- data.frame(cat1 = sample(c("A", "B", NA), 5, 1),
    cat2 = sample(c(1, 2, NA), 5, 1), v = rpois(5, 3),
    stringsAsFactors = FALSE)
  ij <- inner_join(x, y, by = c("cat1","cat2"))
  me <- merge(x, y, by = c("cat1","cat2"))
  expect_true( equal_data_frame(ij, me) )
})

test_that("join handles matrices #1230", {
  df1 <- data_frame( x = 1:10, text = letters[1:10])
  df2 <- data_frame( x = 1:5,  text = "" )
  df2$text <- matrix( LETTERS[1:10], nrow=5)

  res <- left_join( df1, df2, by = c("x"="x")) %>% filter( x > 5)
  text.y <- res$text.y
  expect_true( is.matrix(text.y) )
  expect_equal( dim(text.y), c(5,2) )
  expect_true( all(is.na(text.y)) )
})

test_that( "ordering of strings is not confused by R's collate order (#1315)", {
  a= data.frame(character = c("\u0663"),set=c("arabic_the_language"),stringsAsFactors=F)
  b = data.frame(character = c("3"),set=c("arabic_the_numeral_set"),stringsAsFactors = F)
  res <- b %>% inner_join(a,by=c("character"))
  expect_equal( nrow(res), 0L)
  res <- a %>% inner_join(b,by=c("character"))
  expect_equal( nrow(res), 0L)
})

test_that("joins handle tzone differences (#819)", {
  date1 <- structure(-1735660800, tzone = "America/Chicago", class = c("POSIXct", "POSIXt"))
  date2 <- structure(-1735660800, tzone = "UTC", class = c("POSIXct", "POSIXt"))

  df1 <- data.frame( date = date1 )
  df2 <- data.frame( date = date2 )

  expect_equal( attr(left_join(df1, df1)$date, "tzone"), "America/Chicago" )
})

test_that("joins matches NA in character vector (#892)", {
  x <- data.frame(id = c(NA_character_, NA_character_),
                   stringsAsFactors = F)

  y <- expand.grid(id = c(NA_character_, NA_character_),
                    LETTER = LETTERS[1:2],
                    stringsAsFactors = F)

  res <- left_join(x, y, by = 'id')
  expect_true( all( is.na(res$id)) )
  expect_equal( res$LETTER, rep(rep(c("A", "B"), each = 2), 2) )
})

test_that( "joins avoid name repetition (#1460)", {
  d1 <- data.frame(id=1:5, foo=rnorm(5))
  d2 <- data.frame(id=1:5, foo=rnorm(5))
  d3 <- data.frame(id=1:5, foo=rnorm(5))
  d <- d1 %>% left_join(d1, by="id") %>%
    left_join(d2, by="id") %>%
    left_join(d3, by="id")
  expect_equal( names(d), c("id", "foo.x", "foo.y", "foo.x.x", "foo.y.y"))
})

test_that("join functions are protected against empty by (#1496)", {
  x <- data.frame()
  y <- data.frame(a=1)
  expect_error( left_join(x,y, by = names(x) ), "no variable to join by" )
  expect_error( right_join(x,y, by = names(x) ), "no variable to join by" )
  expect_error( semi_join(x,y, by = names(x) ), "no variable to join by" )
  expect_error( full_join(x,y, by = names(x) ), "no variable to join by" )
  expect_error( anti_join(x,y, by = names(x) ), "no variable to join by" )
  expect_error( inner_join(x,y, by = names(x) ), "no variable to join by" )
})

test_that("joins takes care of duplicates in by (#1192)", {
  data2 <- data_frame(a=1:3)
  data1 <- data_frame(a=1:3, c=3:5)

  res1 <- left_join(data1, data2, by=c("a","a"))
  res2 <- left_join(data1, data2, by=c("a" = "a"))
  expect_equal(res1, res2)
})

# Joined columns result in correct type ----------------------------------------

test_that("result of joining POSIXct is POSIXct (#1578)", {
  data1 <- data_frame(t=seq(as.POSIXct("2015-12-01", tz="UTC"), length.out=2,
                            by="days"), x=1:2)
  data2 <- inner_join(data1, data1, by = "t")
  res1 <- class(data2$t)
  expected <- c("POSIXct", "POSIXt")
  expect_identical(res1, expected)
})

test_that("joins allows extra attributes if they are identical (#1636)", {

  tbl_left <- data_frame(
    i = rep(c(1.0, 2.0, 3.0), each = 2),
    x1 = letters[1:6]
  )
  tbl_right <- data_frame(
    i = c(1.0, 2.0, 3.0),
    x2 = letters[1:3]
  )

  attr(tbl_left$i, 'label') <- 'iterator'
  attr(tbl_right$i, 'label') <- 'iterator'

  res <- left_join(tbl_left, tbl_right, by = 'i')
  expect_equal( attr(res$i, "label"), "iterator" )

  attr(tbl_left$i, "foo" ) <- "bar"
  attributes(tbl_right$i) <- NULL
  attr(tbl_right$i, "foo" ) <- "bar"
  attr(tbl_right$i, 'label') <- 'iterator'

  res <- left_join(tbl_left, tbl_right, by = 'i')
  expect_equal( attr(res$i, "label"), "iterator" )
  expect_equal( attr(res$i, "foo"), "bar" )
})

test_that("joins work with factors of different levels (#1712)", {
  d1 <- iris[, c("Species", "Sepal.Length")]
  d2 <- iris[, c("Species", "Sepal.Width")]
  d2$Species <- factor(as.character(d2$Species), levels=rev(levels(d1$Species)))
  expect_warning( res1 <- left_join(d1, d2, by="Species") )

  d1$Species <- as.character(d1$Species)
  d2$Species <- as.character(d2$Species)
  res2 <- left_join(d1, d2, by="Species")
  expect_equal(res1, res2)
})

test_that("anti and semi joins give correct result when by variable is a factor (#1571)", {
  big <- data.frame(letter = rep(c('a', 'b'), each = 2), number = 1:2)
  small <- data.frame(letter = 'b')
  expect_warning( aj_result <- anti_join(big, small, by = "letter"), NA )
  expect_equal( aj_result$number, 1:2)
  expect_equal( aj_result$letter, factor(c("a", "a"), levels = c("a", "b")) )

  expect_warning( sj_result <- semi_join(big, small, by = "letter"), NA )
  expect_equal( sj_result$number, 1:2)
  expect_equal( sj_result$letter, factor(c("b", "b"), levels = c("a", "b")) )
})

test_that("inner join not crashing (#1559)", {
  df3 <- data_frame(
    id = c(102, 102, 102, 121),
    name = c("qwer", "qwer", "qwer", "asdf"),
    k = factor(c("one", "two", "total", "one"), levels = c("one", "two", "total")),
    total = factor(c("tot", "tot", "tot", "tot"), levels = c("tot", "plan", "fact")),
    v = c(NA_real_, NA_real_, NA_real_, NA_real_),
    btm = c(25654.957609, 29375.7547216667, 55030.7123306667, 10469.3523273333),
    top = c(22238.368946, 30341.516924, 52579.88587, 9541.893144)
  )
  df4 <- data_frame(
    id = c(102, 102, 102, 121),
    name = c("qwer", "qwer", "qwer", "asdf"),
    k = factor(c("one", "two", "total", "one"), levels = c("one", "two", "total")),
    type = factor(c("fact", "fact", "fact", "fact"), levels = c("tot", "plan", "fact")),
    perc = c(0.15363485835208, -0.0318297270618471, 0.0466114830816894, 0.0971986553754823)
  )
  # all we want here is to test that this does not crash
  expect_message( res <- replicate(100, df3 %>% inner_join(df4)) )
  for( i in 2:100)
    expect_equal( res[,1], res[,i] )
})

test_that( "left_join handles mix of encodings in column names (#1571)", {

  df1 <- tibble::data_frame(x = 1:6, foo = 1:6)
  names(df1)[1] <- "l\u00f8penummer"

  df2 <- tibble::data_frame(x = 1:6, baz = 1:6)
  names(df2)[1] <- iconv(  "l\u00f8penummer", from = "UTF-8", to  = "latin1" )

  expect_message( res <- left_join( df1, df2 ) )
  expect_equal( names(res), c("l\u00f8penummer", "foo", "baz") )
  expect_equal( res$foo, 1:6)
  expect_equal( res$baz, 1:6)
  expect_equal( res[["l\u00f8penummer"]], 1:6)

})
