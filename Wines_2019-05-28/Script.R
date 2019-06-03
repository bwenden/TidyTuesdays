library(tidyverse)
library(tidytext)
library(ggthemes)

#Import data
wine_ratings <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-05-28/winemag-data-130k-v2.csv") %>%
  select(-X1) %>%
  distinct()

###Extract flavors and taste characteristics
wine_words <- wine_ratings %>%
  unnest_tokens(description_word, description) %>%
  anti_join(stop_words, by = c("description_word" = "word")) %>%
  filter(!str_detect(description_word, "[:digit:]")) %>%
  add_count(description_word)

  
###Percentage of occurrences of the words in the review score range
###I used the classification system from WineEnthusiast

description_words <- wine_words %>%
  filter(n >= 20) %>%
  mutate(score = case_when((points<=82) ~ "Acceptable",
                           (points >= 83)&(points<= 86) ~ "Good",
                           (points >= 87)&(points<= 89) ~ "Very Good",
                           (points >= 90)&(points<= 93) ~ "Excellent",
                           (points >= 94)&(points<= 97) ~ "Superb",
                           (points >=98) ~ "Classic" )) %>%
  count(description_word, score, sort = TRUE) %>%
  group_by(description_word) %>%
  mutate(percent = n / sum(n)) 

###Enrichment analysis
wine_words_enrichment <- wine_words %>%
  filter(n >= 20) %>%
  mutate(score = case_when((points<=82) ~ "Acceptable",
                           (points >= 83)&(points<= 86) ~ "Good",
                           (points >= 87)&(points<= 89) ~ "Very Good",
                           (points >= 90)&(points<= 93) ~ "Excellent",
                           (points >= 94)&(points<= 97) ~ "Superb",
                           (points >=98) ~ "Classic" )) %>%
  select(description_word, score) %>%
  add_count(score, name = "Total_word_number_in_score") %>%
  add_count(description_word, name = "Total_occurrence") %>%
  add_count(description_word, score, name = "word_occurrence_in_this_score") %>%
  add_count(name = "Total_words") %>%
  distinct() %>%
  group_by(score) %>%
  mutate(
    pvalue = phyper(
      q = word_occurrence_in_this_score,
      m = Total_occurrence,
      n = Total_words - Total_occurrence,
      k = Total_word_number_in_score,
      lower.tail = F, log.p = FALSE),
    qvalue = p.adjust(pvalue, method = "fdr")
  )
  
wine_words_enrichment %>%
  group_by(score) %>%
  mutate(rank = rank(qvalue, ties.method = "first")) %>%
  filter(rank <= 10) %>%
  ungroup() %>%
  mutate(qvalue = ifelse(qvalue == 0, (min(qvalue[qvalue > 0])), qvalue)) %>%
  arrange(rank) %>% 
  ggplot(aes(y = qvalue, x = fct_reorder(description_word, -rank)))+
  geom_point(aes(color = qvalue, size = word_occurrence_in_this_score/Total_word_number_in_score), alpha = 0.5)+
  scale_color_gradient(low = "#c03728", high="#f5c04a", 
                       trans = "log",
                       breaks = c(1e-250, 1e-150,1e-50))+
  scale_size_continuous(labels = scales::percent_format(accuracy = 1))+
  coord_flip()+
  facet_wrap(~fct_relevel(score,c("Classic", "Superb", "Excellent", "Very Good", "Good","Acceptable")),
             scales = "free")+
  scale_y_log10()+
  theme_tufte()+
  labs(y = "", x = "", 
       color = "Enrichment\nadjusted p.value",
       size = "Word occurrence",
       title = "Which words are preferentially used to describe superb or bad wines?",
       caption = "Source: Wine Enthusiast - 2017\nVisualization by Bénédicte Wenden @cherrysearch")+
  theme(legend.position = "bottom",
        legend.direction = "horizontal",
        title = element_text(color = "#6f5438", size = 14),
        strip.text = element_text(size = 12, face = "italic",color = "#6f5438"),
        axis.text.y = element_text(size = 10, color = "#6f5438"),
        plot.background = element_rect(fill = "#fffeea"),
        axis.line.y.left = element_line(color = "#6f5438"),
        axis.ticks.y.left = element_blank(),
        panel.spacing.x = unit(2, "lines"),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        legend.title = element_text(size = 12, color = "#6f5438"),
        legend.text = element_text(size = 10,color = "#6f5438")
  )

ggsave("TidyTuesday_wines.png", width = 12, height = 7)
