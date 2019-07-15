# load packages ----
library(tidyverse)
library(DBI)
library(odbc)

# connect to database ----
iquizoo_online <- dbConnect(odbc(), "iquizoo-old", database = "eval_core")

# fetch data ----
raw_data <- dbGetQuery(
  iquizoo_online,
  read_file("test/current_data.sql")
)
users <- dbGetQuery(
  iquizoo_online,
  read_file("test/current_users.sql")
) %>%
  unique() %>%
  spread(key, propertyValue) %>%
  filter(no < 1000 | no == 1009 | no == 1042)

# output rawdata and user information ----
users %>%
  left_join(raw_data, by = "userId") %>%
  write_tsv("test/user_raw_data.tsv")

# combine user and data ----
user_data <- users %>%
  left_join(raw_data, by = "userId") %>%
  select(
    userId, no, name, gender, birthDay, grade, cls,
    createTime, item_title, item_rawscore
  )
writexl::write_xlsx(user_data, "test/user_data.xlsx")

# close connection ----
dbDisconnect(iquizoo_online)
