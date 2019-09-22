library(tidyverse)
library(lubridate)
library(readxl)
library(ggrepel)
library(ggthemes)
library(lmerTest)
library(emmeans)
library(extrafont)
user_demography <- read_tsv("test/user_raw_data.tsv") %>%
  group_by(no, gender, birthDay) %>%
  summarise(assess_time = first(createTime)) %>%
  ungroup() %>%
  mutate(age = (birthDay %--% assess_time) / dyears())
user_data_survey <- read_excel("info/被试测试情况记录.xlsx")
user_grouping <- user_data_survey %>%
  transmute(
    no = ID编号,
    group = case_when(
      str_detect(组别, "实验组|训练") ~ "training",
      str_detect(组别, "控制组") ~ "control",
      TRUE ~ NA_character_
    ),
    IQ = parse_number(韦氏)
  ) %>%
  filter(!is.na(group))
user_info <- user_demography %>%
  inner_join(user_grouping, by = "no")
write_tsv(user_info, "info/user_info.tsv")
writexl::write_xlsx(user_info, "info/user_info.xlsx")
raven_scores <- user_data_survey %>%
  transmute(
    no = ID编号,
    item_title = "raven",
    pre = if_else(
      !is.na(填充测试卷别),
      填充测试正确题数 / 填充测试完成题数,
      前测正确数目 / 前测总完成题目
    ),
    post = if_else(
      !is.na(填充测试卷别),
      前测正确数目 / 前测总完成题目,
      后测正确数目 / 后测总完成题目
    )
  ) %>%
  filter(!is.na(pre), !is.na(post)) %>%
  pivot_longer(c("pre", "post"), names_to = "occasion", values_to = "score")
all_scores <- read_tsv("test/scores.tsv") %>%
  mutate(item_title = str_extract(item_title, "\\w+")) %>%
  select(no, item_title, occasion, score) %>%
  rbind(raven_scores)
training_data_test <- user_info %>%
  inner_join(all_scores, by = "no") %>%
  mutate(
    occasion = factor(occasion, levels = c("pre", "post")),
    occasion_cn = factor(occasion, labels = c("前测", "后测")),
    group_cn = factor(
      group, levels = c("training", "control"), labels = c("训练组", "控制组")
    )
  )
write_tsv(training_data_test, "test/dataset.tsv")
writexl::write_xlsx(training_data_test, "test/grouping_scores.xlsx")
stats <- training_data_test %>%
  group_by(item_title) %>%
  nest() %>%
  ungroup() %>%
  # filter(!item_title %in% c("数感", "二维心理旋转测试", "蝴蝶照相机")) %>%
  mutate(
    fml = map(
      data,
      ~ lmer(score ~ (group_cn + IQ + age + gender) * occasion_cn + (1|no), .x)
    ),
    anova_inter = map(
      fml,
      ~ .x %>%
        anova() %>%
        broom::tidy() %>%
        filter(term == "group_cn:occasion_cn")
    ),
    plots = map(
      fml,
      ~ emmip(.x, group_cn ~ occasion_cn) +
        labs(x = "", y = "预测分数", color = "") +
        scale_color_few() +
        theme_classic(base_family = "SimHei", base_size = 12) +
        theme(plot.title = element_text(hjust = 0.5))
    ),
    comp_plots = map(
      data,
      ~ ggplot(.x, aes(occasion_cn, score, color = group_cn, group = no, label = no)) +
        geom_line() +
        geom_point() +
        geom_text_repel(show.legend = FALSE) +
        scale_color_hc() +
        theme_classic(base_family = "SimHei", base_size = 18) +
        theme(plot.title = element_text(hjust = 0.5))
    )
  ) %>%
  unnest(anova_inter) %>%
  mutate(p.adjusted = p.adjust(p.value, method = "holm")) %>%
  mutate(
    save_res = walk2(
      item_title, plots,
      ~ ggsave(
        file.path("test", "interaction", paste0(.x, ".jpg")),
        .y + labs(title = .x),
        type = "cairo",
        width = 7,
        height = 4
      )
    ),
    save_res2 = walk2(
      item_title, comp_plots,
      ~ ggsave(
        file.path("test", "compare-individuals", paste0(.x, ".jpg")),
        .y + labs(title = .x),
        type = "cairo",
        width = 8,
        height = 6
      )
    )
  )
stats %>%
  select(item_title, NumDF, DenDF, statistic, p.value, p.adjusted) %>%
  writexl::write_xlsx(file.path("test/anova_results_corrected.xlsx"))
