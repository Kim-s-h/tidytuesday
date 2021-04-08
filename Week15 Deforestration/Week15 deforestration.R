library(tidyverse)
library(viridis)   # for "scale_color_viridis" and "scale_fill_viridis"

# Get the Data
brazil_loss <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-04-06/brazil_loss.csv')


brazil_loss %>%
  mutate(across(commercial_crops:small_scale_clearing, ~(.x/sum(.x)))) %>%
  pivot_longer(cols = commercial_crops:small_scale_clearing, names_to = "variable", values_to = "value") %>%
  ggplot(aes(x = value, y=variable, fill = as.factor(desc(year)))) +
  geom_bar(position="stack", stat="identity") +
  scale_x_continuous(labels=scales::percent)

brazil_loss %>%
  rowwise() %>%
  mutate(total = sum(c_across(commercial_crops:small_scale_clearing))) %>%
  mutate(across(commercial_crops:small_scale_clearing, ~(./total))) %>%
  pivot_longer(cols = commercial_crops:small_scale_clearing, names_to = "variable", values_to = "value") %>%
  ggplot(aes(x = value, y= as.factor(year), fill = as.factor(variable))) +
  geom_bar(position="stack", stat="identity") +
  scale_x_continuous(labels=scales::percent)

brazil_loss %>%
  pivot_longer(cols = commercial_crops:small_scale_clearing, names_to = "variable", values_to = "value") %>%
  ggplot(aes(x = value, y= as.factor(year), fill = as.factor(variable))) +
  geom_bar(position="stack", stat="identity") +
  scale_x_continuous(labels = scales::comma) +
  scale_color_viridis(discrete = TRUE, option = "D") +
  scale_fill_viridis(discrete = TRUE) +
  theme_minimal(14) +
  theme(legend.title = element_blank()) +
  labs(y = NULL, x = NULL)
ggsave(file = "Week15 Deforestration/brazil_loss.png", width = 8,  height = 5)  

