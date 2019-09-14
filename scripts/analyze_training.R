library(tidyverse)
library(readxl)
library(extrafont)
library(ggthemes)
training_logs <- read_tsv("training/logs.tsv")
users <- read_tsv("training/users.tsv")
user_info <- read_excel("info/测评报告情况记录.xlsx") %>%
  filter(!is.na(`未收到报告-完成后测`)) %>%
  rename(name_real = 姓名) %>%
  mutate(app_id = parse_number(paste0("2018", ID)))
training_data <- users %>%
  inner_join(training_logs, by = "user_id") %>%
  inner_join(user_info, by = "app_id")
plots <- training_data %>%
  group_by(name_real, game_id, game_name) %>%
  nest() %>%
  mutate(
    plot = map(
      data,
      ~ .x %>%
        mutate(order = row_number(game_finish_time)) %>%
        mutate(game_star = factor(game_star)) %>%
        ggplot(aes(order, game_stage, group = 1)) +
        geom_point(aes(color = game_star)) +
        geom_line() +
        scale_color_gdocs() +
        labs(x = "顺序", y = "关卡", color = "星星数") +
        theme_hc(base_family = "SimHei", base_size = 12)
    )
  )
plots %>%
  pwalk(
    function(plot, name_real, game_name, ...) {
      dir_fig <- file.path("reports", name_real, "training")
      if (!dir.exists(dir_fig))
        dir.create(dir_fig, recursive = TRUE)
      ggsave(
        file.path(dir_fig, str_glue("{game_name}.png")),
        plot +
          labs(title = game_name) +
          theme(plot.title = element_text(hjust = 0.5)),
        type = "cairo", width = 6, height = 4
      )
    }
  )
