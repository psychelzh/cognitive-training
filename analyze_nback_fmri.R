library(tidyverse)
library(writexl)
library(ggrepel)
library(ggthemes)
library(haven)
library(lmerTest)
results_nback <- read_tsv('n-back/results.txt', na = c("", "NaN"))
scores_nback <- results_nback %>%
  mutate(occasion = factor(occasion, c("pre", "post"))) %>%
  filter(!type %in% c("cue", "filler"), !is.na(task)) %>%
  group_by(id, occasion) %>%
  mutate(
    acc = if_else(
      rt < 0.1 & rt > 0, 0, acc
    ),
    n_trial_total = n(),
    PC_total = mean(acc == 1)
  ) %>%
  group_by(id, occasion, task, n_trial_total, PC_total) %>%
  nest() %>%
  mutate(
    score = map(
      data,
      ~ .x %>%
        mutate(
          n_trial = n(),
          PC = mean(acc == 1),
          MRT = mean(rt[acc == 1])
        ) %>%
        group_by(type, n_trial, PC, MRT) %>%
        summarise(PC_cond = mean(acc == 1)) %>%
        mutate(
          PC_cond = case_when(
            PC_cond == 1 ~ 1 - 1 / n_trial,
            PC_cond == 0 ~ 1 / n_trial,
            TRUE ~ PC_cond
          )
        ) %>%
        spread(type, PC_cond) %>%
        mutate(dprime = qnorm(target) - qnorm(1 - distractor))
    )
  ) %>%
  unnest(score) %>%
  mutate(valid = PC_total > qbinom(0.95, n_trial_total, 0.5) / n_trial_total)
user_group <- read_sav("info/26人 性别年龄瑞文韦氏conners 0412年龄更新.sav") %>%
  select(no, IQ, group, gender, age)
stats <- user_group %>%
  inner_join(scores_nback, by = c("no" = "id")) %>%
  filter(!no %in% c(20, 22, 36)) %>%
  gather(index, score, PC, dprime, MRT) %>%
  group_by(task, index) %>%
  nest() %>%
  mutate(
    compare_plots = map(
      data,
      ~ ggplot(.x, aes(occasion, score, color = group, group = no, label = no)) +
        geom_line() +
        geom_point() +
        geom_text_repel(show.legend = FALSE) +
        scale_color_hc() +
        theme_classic(base_family = "Gill Sans MT", base_size = 18) +
        theme(plot.title = element_text(hjust = 0.5))
    ),
    fml = map(
      data,
      ~ lmer(score ~ (group + IQ + gender + age) * occasion + (1 | no), .x)
    ),
    anova_results = map(
      fml,
      anova
    )
  )
stats %>%
  select(task, index, compare_plots) %>%
  pwalk(
    function(task, index, compare_plots) {
      ggsave(
        file.path("n-back", "compare_plots", str_glue("{task}_{index}.jpg")),
        compare_plots + labs(title = str_glue("{task}_{index}")),
        type = "cairo"
      )
    }
  )
stats %>%
  select(task, index, anova_results) %>%
  pwalk(
    function(task, index, anova_results) {
      write_xlsx(
        anova_results %>% broom::tidy(),
        file.path("n-back", "anova_results", str_glue("{task}_{index}.xlsx"))
      )
    }
  )
writexl::write_xlsx(scores_nback, "n-back/scores.xlsx")
