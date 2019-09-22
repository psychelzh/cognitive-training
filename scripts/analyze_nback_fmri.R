library(tidyverse)
library(lubridate)
library(ggrepel)
library(ggthemes)
library(lmerTest)
library(emmeans)

# load and clean data, extracting occasion info ----
nback_files <- list.files("n-back/logs", full.names = TRUE)
results_nback <- nback_files %>%
  map(
    ~ read_tsv(.x, na = c("", "NaN")) %>%
      add_column(
        id = str_extract(.x, "(?<=Sub_)\\d+"),
        time = str_extract(.x, "\\d{8}_\\d{6}") %>%
          ymd_hms(),
        .before = 1
      )
  ) %>%
  bind_rows() %>%
  # remove empty recordings
  group_by(id, time, run) %>%
  nest() %>%
  filter(!map_lgl(data, ~ all(is.na(.x)))) %>%
  # extract occasion info
  group_by(id, run) %>%
  mutate(
    occur = row_number(time)
  ) %>%
  ungroup() %>%
  mutate(
    occasion = recode_factor(
      occur,
      `1` = "pre",
      `2` = "post",
      .default = NA_character_
    )
  ) %>%
  filter(!is.na(occasion)) %>%
  unnest(data)

# calculate scores of interest ----
scores_nback <- results_nback %>%
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

# join group info and analyze ----
user_group <- read_tsv("info/user_info.tsv")
stats <- user_group %>%
  inner_join(scores_nback, by = c("no" = "id")) %>%
  # filter(!no %in% c(20, 22, 36)) %>%
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
    anova_results_raw = map(fml, anova),
    anova_results = map(anova_results_raw, broom::tidy)
  )
stats %>%
  pwalk(
    function(task, index, compare_plots, ...) {
      save_folder <- file.path("n-back", "compare_plots")
      if (!dir.exists(save_folder))
        dir.create(save_folder)
      ggsave(
        file.path(save_folder, str_glue("{task}_{index}.jpg")),
        compare_plots + labs(title = str_glue("{task}_{index}")),
        type = "cairo"
      )
    }
  ) %>%
  pwalk(
    function(task, index, anova_results, ...) {
      save_folder <- file.path("n-back", "anova_results")
      if (!dir.exists(save_folder))
        dir.create(save_folder)
      writexl::write_xlsx(
        anova_results,
        file.path(save_folder, str_glue("{task}_{index}.xlsx"))
      )
    }
  )
writexl::write_xlsx(scores_nback, "n-back/scores.xlsx")
stats %>%
  select(task, index, anova_results) %>%
  unnest(anova_results) %>%
  writexl::write_xlsx("n-back/all_anova_results.xlsx")
