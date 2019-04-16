library(tidyverse)
library(readxl)
library(ggthemes)
library(extrafont)
funcShaded <- function(x, mean, sd, upper_bound) {
  y <- dnorm(x, mean = mean, sd = sd)
  y[x > upper_bound] <- NA
  return(y)
}
level_descr <- config::get("level_descr")
item_info <- config::get("item_info") %>%
  as_tibble()
user_info <- read_excel("info/被试测试情况记录.xlsx") %>%
  filter(有效 == "有", 组别 %in% c("控制组", "实验组")) %>%
  select(姓名, 前测正确数目, 前测总完成题目, 后测正确数目, 后测总完成题目) %>%
  mutate(
    raven_pre = paste(前测正确数目, 前测总完成题目, sep = "/"),
    raven_post = paste(后测正确数目, 后测总完成题目, sep = "/"),
  ) %>%
  rename(name = 姓名) %>%
  select(name, raven_pre, raven_post)
scores <- read_tsv("test/scores.tsv") %>%
  spread(time, score) %>%
  group_by(name) %>%
  mutate(test_time = format(min(test_time_post), "%Y年%b%e日")) %>%
  group_by(item_title) %>%
  mutate(
    item = str_extract(item_title, "\\w+"),
    center = mean(pretest, na.rm = TRUE),
    scale = sd(pretest, na.rm = TRUE)
  ) %>%
  ungroup() %>%
  pivot_longer(ends_with("test"), names_to = "time", values_to = "score") %>%
  mutate(
    score = (score - center) / scale * 15 + 100,
    percent = pnorm(score, 100, 15),
    level = cut(
      percent,
      breaks = c(0, 0.3, 0.7, 1),
      labels = c("待提高", "中等", "优秀")
    )
  ) %>%
  pivot_wider(names_from = "time", values_from = c("score", "percent", "level")) %>%
  mutate(
    percent_up = if_else(
      percent_posttest - percent_pretest > 0,
      percent_posttest - percent_pretest, 0
    )
  ) %>%
  group_by(name, gender, school, test_time) %>%
  nest()
reports <- scores %>%
  inner_join(user_info, by = "name") %>%
  mutate(
    elements = map(
      data,
      ~ {
        item_scores <- .x %>%
          inner_join(item_info, by = "item")
        score_descr <- character(nrow(item_scores))
        level_post_descr <- character(nrow(item_scores))
        plots_pre <- list()
        plots_post <- list()
        for (i_item in 1:nrow(item_scores)) {
          item <- item_scores[[i_item, "item"]]
          ab_name <- item_scores[[i_item, "ab_name"]]
          score_pretest <- item_scores[[i_item, "score_pretest"]] %>%
            round(digits = 0)
          percent_pretest <- sprintf(
            "%d%%", round(item_scores[[i_item, "percent_pretest"]] * 100, 0)
          )
          level_pretest <- item_scores[[i_item, "level_pretest"]]
          score_posttest <- item_scores[[i_item, "score_posttest"]] %>%
            round(digits = 0)
          percent_posttest <- sprintf(
            "%d%%", round(item_scores[[i_item, "percent_posttest"]] * 100, 0)
          )
          level_posttest <- item_scores[[i_item, "level_posttest"]]
          percent_up <- sprintf(
            "%d%%", round(item_scores[[i_item, "percent_up"]] * 100, 0)
          )
          score_descr[i_item] <- str_glue(item_scores[[i_item, "descr"]])
          plots_pre[[i_item]] <- ggplot(data.frame(x = c(55, 145)), aes(x)) +
            stat_function(
              fun = dnorm, args = list(mean = 100, sd = 15),
              color = "cornFlowerBlue"
            ) +
            stat_function(
              fun = funcShaded, args = list(mean = 100, sd = 15, upper_bound = score_pretest),
              geom = "area", fill = "lightblue", alpha = 0.5
            ) +
            scale_y_continuous(expand = c(0, 0)) +
            labs(x = "得分", y = "") +
            theme_minimal(base_family = "SimHei", base_size = 18) +
            theme(
              panel.grid = element_blank(),
              axis.text.y = element_blank(),
              axis.ticks.y = element_blank()
            )
          plots_post[[i_item]] <- ggplot(data.frame(x = c(55, 145)), aes(x)) +
            stat_function(
              fun = dnorm, args = list(mean = 100, sd = 15),
              color = "cornFlowerBlue"
            ) +
            stat_function(
              fun = funcShaded, args = list(mean = 100, sd = 15, upper_bound = score_posttest),
              geom = "area", fill = "lightblue", alpha = 0.5
            ) +
            scale_y_continuous(expand = c(0, 0)) +
            labs(x = "得分", y = "") +
            theme_minimal(base_family = "SimHei", base_size = 18) +
            theme(
              panel.grid = element_blank(),
              axis.text.y = element_blank(),
              axis.ticks.y = element_blank()
            )
          level_post_descr[i_item] <- str_glue(level_descr[[as.character(level_posttest)]])
        }
        tibble(
          item = item_scores$item,
          score_descr, level_post_descr,
          plots_pre, plots_post
        )
      }
    )
  ) %>%
  select(-data) %>%
  unnest(elements)
# output plots
for (i_row in 1:nrow(reports)) {
  user_name <- reports[[i_row, "name"]]
  item_name <- reports[[i_row, "item"]]
  plot_pre <- reports[[i_row, "plots_pre"]]
  plot_post <- reports[[i_row, "plots_post"]]
  store_path <- file.path("reports", user_name)
  if (!dir.exists(store_path))
    dir.create(store_path)
  ggsave(file.path(store_path, paste0(item_name, "_前测.jpg")), plot_pre, type = "cairo")
  ggsave(file.path(store_path, paste0(item_name, "_后测.jpg")), plot_post, type = "cairo")
}
# output descriptions
reports %>%
  select(-starts_with("plots")) %>%
  writexl::write_xlsx(file.path("reports", "descriptions.xlsx"))
