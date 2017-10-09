library(dplyr)
library(dbplyr)


################ Connection with MySQL

con <- src_mysql(
  dbname = "test",
  host = "localhost",
  username = "root",
  password = "abc123")

src_tbls(con)
beerclub <- tbl(con,"beerclub")
beerclub_members <- tbl(con,"beerclub_members")