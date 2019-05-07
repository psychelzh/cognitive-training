library(tidyverse)
library(ggrepel)
library(ggthemes)
library(haven)
library(lmerTest)
library(emmeans)
library(extrafont)
conners_user_info <- read_sav("info/26人 性别年龄瑞文韦氏conners 0412年龄更新.sav") %>%
  select(-contains("瑞文"))
raven_scores <- read_sav("info/25人瑞文数据.sav") %>%
  select(name, contains("瑞文")) %>%
  add_column(item_title = "raven", .after = 1) %>%
  rename(no = name, pre = pre瑞文, post = post瑞文) %>%
  pivot_longer(c("pre", "post"), names_to = "time", values_to = "score")
user_info <- conners_user_info %>%
  select(-contains("pre"), -contains("post"))
conners <- conners_user_info %>%
  select(no, contains("pre"), contains("post")) %>%
  gather(variable, score, preA行为:postF多动指数) %>%
  separate(variable, c("time", "item_title"), sep = "[A-F]")
all_scores <- read_tsv("test/scores.tsv") %>%
  mutate(
    no = parse_number(no),
    item_title = str_extract(item_title, "\\w+")
  ) %>%
  select(no, item_title, time, score) %>%
  rbind(conners) %>%
  rbind(raven_scores)
training_data_test <- user_info %>%
  inner_join(all_scores, by = "no") %>%
  mutate(
    time = factor(time, levels = c("pre", "post")),
    time_cn = factor(time, labels = c("前测", "后测")),
    group_cn = factor(
      group, levels = c("training", "control"), labels = c("训练组", "控制组")
    )
  )
write_tsv(training_data_test, "test/dataset.tsv")
writexl::write_xlsx(training_data_test, "test/grouping_scores.xlsx")
stats <- training_data_test %>%
  group_by(item_title) %>%
  nest() %>%
  filter(!item_title %in% c("数感", "二维心理旋转测试", "蝴蝶照相机")) %>%
  mutate(
    fml = map(
      data,
      ~ lmer(score ~ (group_cn + IQ + age + gender) * time_cn + (1|no), .x)
    ),
    anova_inter = map(
      fml,
      ~ .x %>%
        anova() %>%
        broom::tidy() %>%
        filter(term == "group_cn:time_cn")
    ),
    plots = map(
      fml,
      ~ emmip(.x, group_cn ~ time_cn) +
        labs(x = "", y = "预测分数", color = "") +
        scale_color_few() +
        theme_classic(base_family = "SimHei", base_size = 12) +
        theme(plot.title = element_text(hjust = 0.5))
    ),
    comp_plots = map(
      data,
      ~ ggplot(.x, aes(time_cn, score, color = group_cn, group = no, label = no)) +
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
