library(tidyverse)
library(sf)
library(maps)
library(rnaturalearth)
library(rnaturalearthdata)
library(rvest) # scraping data from World Population

post_offices <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-04-13/post_offices.csv')

# Post offices in Virginia
VA_post_offices <- post_offices %>% 
  filter(state=="VA") 

total_office <- VA_post_offices %>%
  filter(continuous == TRUE,
         !is.na(latitude)) %>% 
  mutate(county1 = tolower(county1)) %>%
  group_by(county1) %>%
  summarise(sum_post_offices = n()) 

pop_by_county <- read_html("https://worldpopulationreview.com/us-counties/states/va") %>%
  html_node("table") %>%
  html_table(fill = TRUE) %>%
  janitor::clean_names() %>%
  mutate(name = tolower(str_remove(name, " County"))) %>%
  mutate(name = case_when(name == "alexandria city" ~ "alexandria",
                          name == "hampton city" ~ "hampton",
                          name == "norfolk city" ~ "norfolk",
                          name == "suffolk city" ~ "suffolk",
                          name == "virginia beach city" ~ "virginia beach",
                          TRUE ~ name)) %>%
  mutate(pop2021 = parse_number(x2021_population)) %>%
  select(-x2021_population)
  
county_post_pop <- total_office %>%
  left_join(., pop_by_county, by =c("county1" = "name")) %>%
  mutate(people_per_post = pop2021/sum_post_offices)

# number of people per post office by county
world <- ne_countries(scale = "medium", returnclass = "sf")
counties <- st_as_sf(map("county", plot = FALSE, fill = TRUE))
counties <- subset(counties, grepl("virginia", counties$ID))
counties <- subset(counties, !grepl("west virginia", counties$ID) )
counties$ID2 <- str_remove(counties$ID, "virginia,")

counties_post_info <- counties %>%
  left_join(., county_post_pop, by=c("ID2" ="county1")) 


ggplot(data = world) +
  geom_sf() +
  geom_sf(data = counties_post_info, aes(fill = pop2021)) +
  scale_fill_viridis_c(trans = "sqrt", alpha = .4) +
  coord_sf(xlim = c(-84, -75), ylim = c(36.4, 39.7), expand = FALSE) +
  labs(title = "Virginia population by county")

ggplot(data = world) +
  geom_sf() +
  geom_sf(data = counties_post_info, aes(fill = people_per_post)) +
  scale_fill_viridis_c(trans = "sqrt", alpha = .4) +
  coord_sf(xlim = c(-84, -75), ylim = c(36.4, 39.7), expand = FALSE) +
  labs(title = "Number of people per post office by county in Virginia 2021", color = "white") +
  theme(legend.position = "right", 
        legend.title = element_blank(),
        plot.title = element_text(colour = "#004B87", size = 16, face = "bold"))
ggsave("Week 16 Post offices/virginia_postoffice.png", width = 10, height = 8, units = "in")  
