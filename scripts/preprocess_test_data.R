library(tidyverse)
library(lubridate)
user_raw_data <- read_tsv("test/user_raw_data.tsv") %>%
  mutate(
    item_data = map_chr(
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
      item_data,
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
      item_data,
      ~ sum(.x$ACC)
    )
  )
scores_firefly <- user_raw_data %>%
  filter(item_title == "森林萤火虫-科研版") %>%
  mutate(
    score = map_dbl(
      item_data,
      ~ sum(.x$NAcc)
    )
  )
scores_butterfly <- user_raw_data %>%
  filter(item_title == "蝴蝶照相机") %>%
  mutate(
    score = map_dbl(
      item_data,
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
      item_data,
      ~ .x %>%
        mutate(ACC = if_else(RT < 100, 0L, ACC)) %>%
        summarise(PC = mean(ACC == 1)) %>%
        pull(PC)
    )
  )
# 2-back test ----
scores_two_back <- user_raw_data %>%
  filter(item_title == "数字魔法师中级-v4") %>%
  mutate(
    item_data = map(
      item_raw_data,
      ~ jsonlite::fromJSON(.x) %>%
        `$`("params") %>%
        `$`("data") %>%
        str_replace_all(",", "\n") %>%
        read_delim(":", col_names = c("Item", "CResp", "ACC", "RT"))
    ),
    score = map_dbl(
      item_data,
      ~ .x %>%
        filter(CResp != -1) %>%
        mutate(PC = mean(ACC), N_Trial = n()) %>%
        group_by(PC, N_Trial, CResp) %>%
        summarise(PC_each = mean(ACC)) %>%
        mutate(
          PC_each = case_when(
            PC_each == 1 ~ 1 - 1 / N_Trial,
            PC_each == 0 ~ 1 / N_Trial,
            TRUE ~ PC_each
          )
        ) %>%
        pivot_wider(
          names_from = "CResp", values_from = "PC_each", names_prefix = "PC"
        ) %>%
        mutate(dprime = qnorm(PC0) - qnorm(1 - PC1)) %>%
        pull(dprime)
    )
  )
# digit comparison test
scores_digit_cmp <- user_raw_data %>%
  filter(item_title == "数字大小比较-v4") %>%
  mutate(score = item_rawscore)
# combine all the scores ----
scores <- rbind(
  scores_butterfly, scores_count, scores_firefly, scores_number,
  scores_span, scores_two_back, scores_digit_cmp
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
    test_time_post = max(createTime),
    times = row_number(createTime),
    occasion = case_when(
      max(times) < 3 & times == 1 ~ "pre",
      max(times) < 3 & times == 2 ~ "post",
      max(times) == 3 & times == 1 ~ "sham",
      max(times) == 3 & times == 2 ~ "pre",
      max(times) == 3 & times == 3 ~ "post",
    )
  ) %>%
  ungroup() %>%
  select(-birthDay, -createTime, -times)
write_tsv(scores, "test/scores.tsv")
