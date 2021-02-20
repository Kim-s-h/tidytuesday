library(tidyverse)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(fuzzyjoin)

world <- ne_countries(scale = "medium", returnclass = "sf")
artwork <- read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-01-12/artwork.csv")
artists <- read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-01-12/artists.csv")

artwork %>% print(width = Inf)
artists %>% print(width = Inf)


art_all <- artwork %>%
  select(artistId, acquisitionYear, artist) %>%
  left_join(., artists, by=c("artistId" = "id"))

art_all <- art_all %>%
  separate(col=placeOfBirth, sep=",", into = c("city1", "city2", "country"), fill = "left") %>%
  mutate(country = str_trim(country)) %>%
  filter(!is.na(country))
  
freq_country <- art_all %>%
  select(acquisitionYear, country) %>%
  count(country) %>%
  mutate(country = replace(country, country=="United States", "United States of America"),
         country = replace(country, country=="London", "United Kingdom"),
         country = replace(country, country=="Edinburgh", "United Kingdom"))


# world_with_count <- freq_country %>%
#   stringdist_left_join(., world, by=c("country" ="name"), max_dist=2)
# world_with_count %>% filter(is.na(name)) %>%select(country, name) %>% print(n=Inf)

world_with_count <- world %>%
  stringdist_left_join(., freq_country, by=c("name" ="country"), max_dist=2) %>%
  st_as_sf

world_with_count %>%
  filter(!is.na(name),
         name != "United Kingdom") %>%
  select(n,geometry) %>%
  ggplot() +
  geom_sf(aes(fill = n)) +
  scale_fill_viridis_c(option = "plasma") +
  labs(title = "Birth place of artists except for United Kingdom for artworks acquired by Tate Center (1823-2013)")

ggsave("Week3 Art Collections/world_map_birthPlace.png", width = 16, height = 9, units = "in")  
