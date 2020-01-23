###Tidytuesday 21/01/2020 Spotify songs
library(tidyverse)
library(scico)
library(gridExtra)
library(tidytext)
library(tm)
library(Matrix)
library(widyr)
library(glmnet)


spotify_songs <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-01-21/spotify_songs.csv')

spotify_songs %>%
  ggplot(aes(x = track_popularity))+
  geom_density()

###Lasso regression on words in track and album names
#Extract words from album and track names
track_name_words <- spotify_songs %>%
  mutate(song_id = row_number()) %>%
  select(song_id, track_popularity, track_name) %>%
  unnest_tokens(word, track_name) 

album_name_words <- spotify_songs %>%
  mutate(song_id = row_number()) %>%
  select(song_id, track_popularity, track_album_name) %>%
  unnest_tokens(word, track_album_name) 

sw <- data.frame(word = c(stopwords(kind = "en"),
                          stopwords(kind = "fr"),
                          stopwords(kind = "sp")
                          ) )
  

track_words <- bind_rows(track_name_words, album_name_words) %>%
  anti_join(stop_words, by = "word")  %>%
  anti_join(sw, by = "word") %>%
  filter(!word %in% c("version", "radio", "mix", "remastered", "edition", "remaster", "vol"),
         str_detect(word, "[a-z]"))

track_words %>%
  count(word, sort = TRUE) %>%
  head(20) %>%
  mutate(word = fct_reorder(word, n)) %>%
  ggplot(aes(word, n)) +
  geom_col() +
  coord_flip()

##filter on most frequent words
track_words_filtered <- track_words %>%
  distinct(song_id, word) %>%
  add_count(word) %>%
  filter(n >= 10)

track_words_filtered %>%
  pairwise_cor(word, song_id, sort = TRUE)

####

track_word_matrix <- track_words_filtered %>%
  cast_sparse(song_id, word)

song_ids <- as.integer(rownames(track_word_matrix))

scores <- spotify_songs$track_popularity[song_ids]

cv_glmnet_model <- cv.glmnet(track_word_matrix, scores)
plot(cv_glmnet_model)


###lexicon
lexicon <- cv_glmnet_model$glmnet.fit %>%
  tidy() %>%
  filter(lambda == cv_glmnet_model$lambda.1se,
         term != "(Intercept)",
         term != "log_price") %>%
  select(word = term, coefficient = estimate)

###Positive words
positive <- lexicon  %>%
  arrange(coefficient) %>%
  mutate(direction = ifelse(coefficient < 0, "Negative", "Positive")) %>%
  filter(direction == "Positive") %>%
  top_n(200, abs(coefficient)) %>%
  rowid_to_column()

positive_words_labels <- positive %>%
  mutate(n = n(),
    angle = 90 - 360 * (rowid - 0.5) / n,
    hjust = ifelse(angle < -90, 0, 1),
    angle = ifelse(angle < -90, angle + 180, angle))

pos <- positive %>%
  mutate(rowid= factor(rowid)) %>%
  ggplot(aes(rowid, coefficient)) +
  geom_col(aes(fill = coefficient), color = "#414141", show.legend = FALSE) +
  scale_y_continuous(limits = c(-110, 40), expand = c(0, 0)) +
  scale_x_discrete(expand = c(0, 0)) +
  scale_fill_scico(palette = "buda")+
  coord_polar()+
  geom_text(data = positive_words_labels,
    aes(x = factor(rowid),
      y = -1,
      label = word,
      hjust = hjust,
      angle = angle),
    color = "white",
    size = 4,
    inherit.aes = FALSE )+
  ggplot2::annotate("text", x = 0, y =-110, label = "Which words in track and\nalbum names are associated\nwith high popularity?",
           color = "#E5E068", size = 13)  +
  theme(plot.background = element_rect(fill = "#414141", color = "#414141"),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        axis.line = element_blank(),
        panel.grid = element_blank(),
        panel.background = element_blank())

ggsave("Tidytuesday_2020_w3_positive_words.png", plot = pos, width = 14.5, height = 14.5)

###Negative words
negative <- lexicon  %>%
  arrange(coefficient) %>%
  mutate(direction = ifelse(coefficient < 0, "Negative", "Positive")) %>%
  filter(direction == "Negative") %>%
  top_n(200, abs(coefficient)) %>%
  rowid_to_column()

negative_words_labels <- negative %>%
  mutate(n = n(),
         angle = 90 - 360 * (rowid - 0.5) / n,
         hjust = ifelse(angle < -90, 1,0),
         angle = ifelse(angle < -90, angle + 180, angle))

neg <- negative %>%
  mutate(rowid= factor(rowid)) %>%
  ggplot(aes(rowid, coefficient)) +
  geom_col(aes(fill = coefficient), color = "#414141",show.legend = FALSE) +
  scale_y_continuous(limits = c(-50, 10), expand = c(0, 0)) +
  scale_x_discrete(expand = c(0, 0)) +
  scale_fill_scico(palette = "devon", begin = 0.1, end = 0.7)+
  coord_polar()+
  geom_text(  data = negative_words_labels,
    aes(x = factor(rowid),
      y = 1,
      label = word,
      hjust = hjust,
      angle = angle ),
    color = "white",
    size = 5,
    inherit.aes = FALSE) +
  ggplot2::annotate("text", x = 0, y =-50, label = "Which words in track and\nalbum names are associated\nwith low popularity?",
                    color = "#DFDDF9", size = 13)  +
  theme(plot.background = element_rect(fill = "#414141", color = "#414141"),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        axis.line = element_blank(), 
        panel.grid = element_blank(),
        panel.background = element_blank())

ggsave("Tidytuesday_2020_w3_negative_words.png", plot = neg, width = 14.5, height = 14.5)
