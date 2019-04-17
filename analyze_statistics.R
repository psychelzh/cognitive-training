library(tidyverse)
library(ggrepel)
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
    anova_inter = map(
      fml,
      ~ .x %>%
        anova() %>%
        broom::tidy() %>%
        filter(term == "group:time")
    ),
    plots = map(
      fml,
      ~ emmip(.x, group ~ time, CIs = TRUE) +
        labs(x = "") +
        scale_color_few() +
        theme_hc(base_family = "SimHei", base_size = 18) +
        theme(plot.title = element_text(hjust = 0.5))
    ),
    comp_plots = map(
      data,
      ~ ggplot(.x, aes(time, score, color = group, group = no, label = no)) +
        geom_line() +
        geom_point() +
        geom_text_repel(show.legend = FALSE) +
        scale_color_hc() +
        theme_classic(base_family = "SimHei", base_size = 18) +
        theme(plot.title = element_text(hjust = 0.5))
    )
  ) %>%
  unnest(anova_inter) %>%
  mutate(p.adjusted = p.adjust(p.value, method = "fdr")) %>%
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
