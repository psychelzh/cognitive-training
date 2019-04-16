library(tidyverse)
library(lubridate)
user_raw_data <- read_tsv("test/user_raw_data.tsv") %>%
  mutate(
    item_raw_data = map_chr(
      item_raw_data,
      ~ .x %>%
        jsonlite::fromJSON() %>%
        `$`("dataJson") %||% "[]"
    ) %>%
      map(~jsonlite::fromJSON(.x, simplifyDataFrame = TRUE))
  )
# span tasks ----
scores_span <- user_raw_data %>%
  filter(item_title %in% c("位置记忆-v4", "顺背数-v4", "倒背数-v4")) %>%
  mutate(
    score = map_dbl(
      item_raw_data,
      ~ .x %>%
        group_by(SLen) %>%
        summarise(PC = mean(ACC)) %>%
        arrange(SLen) %>%
        summarise(score = SLen[1] - 0.5 + sum(PC)) %>%
        pull(score)
    )
  )
# tests that just count number of correct ----
scores_count <- user_raw_data %>%
  filter(
    item_title %in% c("速算师 C2", "二维心理旋转测试A", "二维心理旋转测试B")
  ) %>%
  mutate(
    score = map_dbl(
      item_raw_data,
      ~ sum(.x$ACC)
    )
  )
scores_firefly <- user_raw_data %>%
  filter(item_title == "森林萤火虫-科研版") %>%
  mutate(
    score = map_dbl(
      item_raw_data,
      ~ sum(.x$NAcc)
    )
  )
scores_butterfly <- user_raw_data %>%
  filter(item_title == "蝴蝶照相机") %>%
  mutate(
    score = map_dbl(
      item_raw_data,
      ~ .x$AccLoc %>%
        paste(collapse = "") %>%
        str_count("1")
    )
  )
# number sense ----
scores_number <- user_raw_data %>%
  filter(item_title == "数感-v4") %>%
  mutate(
    score = map_dbl(
      item_raw_data,
      ~ .x %>%
        mutate(ACC = if_else(RT < 100, 0L, ACC)) %>%
        summarise(PC = mean(ACC == 1)) %>%
        pull(PC)
    )
  )
# the last two tasks ----
scores_last <- user_raw_data %>%
  filter(item_title %in% c("数字大小比较-v4", "数字魔法师中级-v4")) %>%
  mutate(score = item_rawscore)
# combine all the scores ----
scores <- rbind(
  scores_butterfly, scores_count, scores_firefly, scores_number, scores_span, scores_last
) %>%
  select(
    no, name, gender, birthDay, school, grade, cls,
    item_title, createTime, score
  ) %>%
  mutate(
    item_title = recode(
      item_title,
      `二维心理旋转测试A` = "二维心理旋转测试",
      `二维心理旋转测试B` = "二维心理旋转测试"
    )
  ) %>%
  group_by(no, item_title) %>%
  mutate(
    time = row_number(createTime) %>%
      recode(`1` = "pre", `2` = "post"),
  ) %>%
  ungroup() %>%
  select(-birthDay, -createTime)
write_tsv(scores, "test/scores.tsv")
