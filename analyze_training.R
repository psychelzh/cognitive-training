library(tidyverse)
library(readxl)
library(extrafont)
training_logs <- read_tsv("training/logs.tsv")
users <- read_tsv("training/users.tsv")
user_info <- read_excel("info/被试测试情况记录.xlsx") %>%
  filter(组别 == "实验组") %>%
  rename(name_real = 姓名) %>%
  mutate(app_id = parse_number(paste0("2018", ID编号)))
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
        ggplot(aes(order, game_stage, shape = game_star, group = 1)) +
        geom_point() +
        geom_line() +
        labs(x = "Order", y = "Stage", shape = "Star") +
        theme_classic(base_family = "SimHei", base_size = 18)
    )
  )
plots %>%
  select(-game_id, -data) %>%
  pmap(
    function(plot, name_real, game_name)
      ggsave(
        file.path("training", "figures", str_glue("{name_real}-{game_name}.jpg")),
        plot + labs(title = str_glue("{name_real}-{game_name}")) + theme(plot.title = element_text(hjust = 0.5)), type = "cairo"
      )
  )
