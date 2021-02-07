library(tidyverse)
library(lubridate)
library(ggrepel)
library(ggthemes)
library(bbplot)


# --Credits to Julia (https://github.com/juliacat23) for code and idea about data

dat <- read_csv("Week1 Netflix/NetflixViewingHistory.csv",
                col_types = cols(Date = col_date(format = "%D")))

dat <- dat %>%
  separate(Title, sep = ":", into = c("Name", "Season", "Episode"))

tv.show <- dat %>%
  filter(!is.na(Episode)) %>%
  filter(Date < "2021-01-01" & Date >="2017-01-01")

count.tv.show <- tv.show %>%
  group_by(Name, Date) %>%
  summarise(episodes_n = n()) %>%
  group_by(Name) %>%
  summarise(
    days_n = n(),
    episodes_n = sum(episodes_n)
  ) %>%
  arrange(desc(episodes_n)) 
  
print(count.tv.show, n=30)  
  
binge.tv.show <- tv.show %>% 
  filter(Name %in% c("The Office (U.S.)", "The Great British Baking Show", "Bates Motel",
                     "Chef's Table", "13 Reasons Why", "Anne with an E", "Kim's Convenience",
                     "Mr. Sunshine", "Grace and Frankie", "Hyori's Bed & Breakfast")) %>% 
  group_by(Name, Date) %>% 
  summarize(
    episodes_n = n(), 
    year = lubridate::year(Date)) %>% 
  group_by(Name, year) %>% 
  summarize(
    episodes = n())  
  
font <- "Helvetica"

binge.tv.show %>% 
  ggplot(aes(x = reorder(Name, episodes, sum), y = episodes, fill = as.factor(desc(year)))) + 
  scale_fill_manual(values = c("#003f5c", "#7a5195", "#ef5675", "#ffa600"), labels = c("2020", "2019", "2018", "2017")) + 
  geom_bar(position="stack", stat="identity") + 
  coord_flip() + 
  bbc_style() +
  theme(axis.text.x = element_text(margin=margin(t = 7, b = 10)), 
        legend.position = "bottom", 
        axis.title.x = element_text(size = 20, color = "#222222"),
        plot.caption = element_text(size = 12, color = "#222222")) + 
  labs(
    title = "Top 10 TV Shows I watched the most, 2017 - 2020", 
    caption = "Data: Netflix Viewing History",
    fill = "Year",
    y = "Total Number of Episodes Watched", 
    x = "")

ggsave("Week1 Netflix/netflix_ep.png", width = 16, height = 9, units = "in")  
  

binge.tv.show %>% 
  ggplot(aes(x = reorder(Name, episodes, sum), y = episodes, fill = as.factor(desc(year)))) + 
  scale_fill_manual(values = c("#003f5c", "#7a5195", "#ef5675", "#ffa600")) +
  geom_bar(position="stack", stat="identity") + 
  coord_flip() + 
  bbc_style() +
  theme(axis.text.x = element_text(margin=margin(t = 7, b = 10)), 
        legend.position = "none", 
        axis.title.x = element_text(size = 20, color = "#222222"),
        plot.caption = element_text(size = 12, color = "#222222")) + 
  labs(
    title = "Top 10 TV Shows I watched the most, 2017 - 2020", 
    caption = "Data: Netflix Viewing History",
    fill = "Year",
    y = "Total Number of Episodes Watched", 
    x = "") +
  facet_wrap(~year)


ggsave("Week1 Netflix/netflix_ep_by_year.png", width = 16, height = 9, units = "in")  

