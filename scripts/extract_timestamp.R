library(tidyverse)
test_data <- read_tsv("test/user_raw_data.tsv") %>%
  group_by(userId, item_title) %>%
  mutate(
    times = row_number(createTime),
    occasion = case_when(
      max(times) < 3 & times == 1 ~ "pre_test",
      max(times) < 3 & times == 2 ~ "post_test",
      max(times) == 3 & times == 1 ~ "pre_test",
      max(times) == 3 & times == 2 ~ "post_test",
      max(times) == 3 & times == 3 ~ "follow_test",
    )
  ) %>%
  ungroup() %>%
  group_by(
    userId, no, name, gender, birthDay,
    school, grade, cls, occasion
  ) %>%
  summarise(assess_date = as.Date(min(createTime))) %>%
  ungroup() %>%
  pivot_wider(names_from = occasion, values_from = assess_date)
training_data <- read_tsv("training/users.tsv") %>%
  mutate(no = str_sub(app_id, start = -3L)) %>%
  inner_join(read_tsv("training/logs.tsv"), by = "user_id") %>%
  group_by(no) %>%
  summarise(
    training_start = as.Date(min(game_finish_time, na.rm = TRUE)),
    training_end = as.Date(max(game_finish_time, na.rm = TRUE))
  ) %>%
  ungroup()
part_times <- test_data %>%
  inner_join(training_data, by = "no")
write_tsv(part_times, "info/part_times.tsv")
writexl::write_xlsx(part_times, "info/part_times.xlsx")
