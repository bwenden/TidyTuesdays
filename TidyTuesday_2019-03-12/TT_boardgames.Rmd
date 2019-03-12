---
title: "TidyTuesday_2019-03-12_boardgames"
author: "B. Wenden"
date: "12 March 2019"
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
library(tidyverse)
library(widyr)
library(igraph)
library(ggraph)
library(RColorBrewer)
theme_set(theme_light())
```

```{r import_data}
data <- read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-03-12/board_games.csv", col_types = cols())

```

###Data exploration
I first explored the data with a couple of plots, with a focus on the categories.

```{r }
##Average playing time by year published
data %>%
  group_by(year_published) %>%
  summarise(playing_time = mean(playing_time)) %>%
  ggplot(aes(x = year_published, playing_time))+
  geom_line()+
  labs(x = "Year published", y = "Playing time (min)")


###focus on categories
data %>%
  separate_rows(category, sep=",") %>%   ###Separate categories into one row by category for each game
  filter(!is.na(category)) %>%
  mutate(category = fct_lump(category, 40)) %>%
  count(category) %>%
  arrange(desc(n))

###playing time by category
data %>%
  separate_rows(category, sep=",") %>%
  filter(!is.na(category),
         playing_time > 0) %>%  ###Remove data with 0 playing time
  mutate(category = str_to_title(category),
    category = fct_lump(category, 20)) %>%
  filter(category !="Other") %>%
  ggplot(aes(x = fct_reorder(category, playing_time, mean), y = playing_time)) +
  geom_boxplot()+
  scale_y_log10( breaks = scales::trans_breaks("log10", function(x) 10^x),
                 labels = scales::trans_format("log10", scales::math_format(10^.x)))+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  annotation_logticks(sides = "l") +
  labs(x="Category", y = "Playing time (min)")

##Ratings by category
data %>%
  separate_rows(category, sep=",") %>%
  filter(!is.na(category)) %>% 
  mutate(category = str_to_title(category),
         category = fct_lump(category, 25)) %>%
  filter(category !="Other") %>%
  group_by(category) %>%
  mutate(playing_time = mean(playing_time)) %>%
  ggplot(aes(x = fct_reorder(category, average_rating, mean), y = average_rating)) +
  geom_boxplot(aes(fill = playing_time))+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  scale_fill_distiller(palette = "RdBu") +
  labs(x = "Category", y = "Average rating on Board Games Geek (1-10)", fill = "Playing time\n(min)",
       title = "The highest rates for Wargames and miniatures boardgames")
```


###Network of categories
I investigated how categories are linked using the pairwise_cor function from widyr and the igraph and gggraph packages. 
```{r}
top_category_cor <- data %>%
  separate_rows(category, sep=",") %>%
  filter(!is.na(category)) %>%
  pairwise_cor(category, game_id, sort = T) %>%
  head(150)

vertices <- data %>%
  separate_rows(category, sep=",") %>%
  filter(category %in% top_category_cor$item1|
           category %in% top_category_cor$item2 ) %>%
  group_by(category) %>%
  summarise(playing_time = mean(playing_time),
            average_rating = mean(average_rating), 
            min_age = mean(min_age),
            year_published = mean(year_published))

top_category_cor %>%
  graph_from_data_frame(vertices = vertices) %>%
  ggraph() +
  geom_edge_link() +
  geom_node_point(aes(fill = average_rating), color = "black" ,shape = 21, size = 4) +
  geom_node_text(aes(label = name), repel = T)+
  scale_fill_distiller(palette = "RdBu", labels = c(3, 4, 5, 6, 7)) +
  theme_void() +
  theme(legend.position = "bottom",
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 13),
        title = element_text(size = 16))+
  labs(fill = "Average rating on Board Games Geek (1-10)",
       title  = "Game board categories commonly found together")
```

