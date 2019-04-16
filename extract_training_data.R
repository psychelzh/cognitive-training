library(tidyverse)
iquizoo_db <- odbc::dbConnect(odbc::odbc(), "iquizoo-MySQL", database = "azy2")
training_logs <- odbc::dbGetQuery(
  iquizoo_db, read_file("training/user_data.sql")
)
user_info <- odbc::dbGetQuery(
  iquizoo_db, read_file("training/users.sql")
)
write_tsv(training_logs, "training/logs.tsv", na = "")
write_tsv(user_info, "training/users.tsv", na = "")
odbc::dbDisconnect(iquizoo_db)
