* http://www.sqlite.org/lang_corefunc.html
* http://www.sqlite.org/lang_datefunc.html
* http://www.sqlite.org/lang_aggfunc.html

abs(X) -> abs(x)
coalesce(X,Y,...) -> X %||% Y %||% Z %||% ...
glob(X,Y) -> glob(X, Y)
ifnull(X,Y) -> X %||% Y 
hex(X)
length(X) -> nchar(X) / str_length(X)
like(X,Y) -> no R eqiv
like(X,Y,Z)
lower(X) -> tolower(x)
ltrim(X)
ltrim(X,Y)
max(X,Y,...)
min(X,Y,...)
nullif(X,Y)
replace(X,Y,Z)
round(X)
round(X,Y)
rtrim(X)
rtrim(X,Y)
soundex(X)
substr(X,Y,Z)
substr(X,Y)
trim(X)
trim(X,Y)
upper(X)

date(timestring, modifier, modifier, ...)
time(timestring, modifier, modifier, ...)
datetime(timestring, modifier, modifier, ...)
julianday(timestring, modifier, modifier, ...)
strftime(format, timestring, modifier, modifier, ...)

avg
count
group_concat
max
min
sum
total