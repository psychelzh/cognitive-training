library(tidyverse)
library(odbc)
iquizoo_db <- dbConnect(odbc(), "iquizoo-old", database = "azy2")
training_logs <- dbGetQuery(
  iquizoo_db, read_file("training/user_data.sql")
)
user_info <- dbGetQuery(
  iquizoo_db, read_file("training/users.sql")
)
write_tsv(training_logs, "training/logs.tsv", na = "")
write_tsv(user_info, "training/users.tsv", na = "")
dbDisconnect(iquizoo_db)
