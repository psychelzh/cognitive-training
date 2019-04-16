library(tidyverse)
library(ggthemes)
library(haven)
library(lmerTest)
library(emmeans)
library(extrafont)
conners_user_info <- read_sav("info/26人 性别年龄瑞文韦氏conners 0412年龄更新.sav") %>%
  select(-contains("瑞文"))
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
  rbind(conners)
training_data_test <- user_info %>%
  inner_join(all_scores, by = "no") %>%
  mutate(time = factor(time, levels = c("pre", "post")))
writexl::write_xlsx(training_data_test, "test/grouping_scores.xlsx")
stats <- training_data_test %>%
  group_by(item_title) %>%
  nest() %>%
  mutate(
    fml = map(
      data,
      ~ lmer(score ~ (group + IQ + age + gender) * time + (1|no), .x)
    ),
    plots = map(
      fml,
      ~ emmip(.x, group ~ time, CIs = TRUE) +
        labs(x = "") +
        scale_color_few() +
        theme_hc(base_family = "SimHei", base_size = 18) +
        theme(plot.title = element_text(hjust = 0.5))
    ),
    anova_inter = map(
      fml,
      ~ .x %>%
        anova() %>%
        broom::tidy() %>%
        filter(term == "group:time")
    )
  ) %>%
  unnest(anova_inter) %>%
  mutate(p_adjusted = p.adjust(p.value, method = "fdr"))
stats %>%
  mutate(
    status = walk2(
      item_title, plots,
      ~ ggsave(
        file.path("test", paste0(.x, ".jpg")),
        .y + labs(title = .x),
        type = "cairo",
        width = 7,
        height = 4
      )
    )
  )
stats %>%
  select(item_title, NumDF, DenDF, statistic, p.value, p_adjusted) %>%
  writexl::write_xlsx(file.path("test/anova_results_corrected.xlsx"))
