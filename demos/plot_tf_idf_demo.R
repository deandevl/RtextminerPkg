library(dplyr)
library(stringr)
library(tidytext)
library(janeaustenr)
library(tokenizers)
library(data.table)
library(RplotterPkg)
library(RtextminerPkg)

ja_books_df <- janeaustenr::austen_books()
head(ja_books_df)

# looking at "Pride & Prejudice"
pride_prejudice_df <- ja_books_df %>%
  filter(book == "Pride & Prejudice")

# create of column of chapter id's
pride_prejudice_df <- pride_prejudice_df %>%
  mutate(
    chapter = cumsum(stringr::str_detect(text, regex("^chapter [\\divxlc]", ignore_case = TRUE)))
  ) %>%
  filter(chapter > 0) %>%
  select(-book)

# get tf_idf where the feature is "chapter"
tf_idf_df <- RtextminerPkg::get_tf_idf(
  x = pride_prejudice_df,
  type = "word",
  feature_id = "chapter",
  feature_text = "text",
  strip_numeric = T,
  stopwords = tidytext::stop_words
)
head(tf_idf_df)

# plot the top 5 tf-idf values for words in chapter 1
chapter_1_tf_idf_plot <- RtextminerPkg::plot_tf_idf(
  tf_idf = tf_idf_df,
  N = 5,
  title = "High tf_idf Words",
  subtitle = "Jane Austen's Pride & Prejudice-Chapter 1",
  feature_id_val= 1,
  do_coord_flip = T,
  rot_y_tic_label = T,
  bar_fill = "purple",
  bar_color = "gold",
  bar_alpha = 0.7,
  show_minor_grids = F
)
chapter_1_tf_idf_plot

# plot the top 5 tf-idf values for words in chapter 60
chapter_60_tf_idf_plot <- RtextminerPkg::plot_tf_idf(
  tf_idf = tf_idf_df,
  N = 5,
  title = "High tf_idf Words",
  subtitle = "Jane Austen's Pride & Prejudice-Chapter 60",
  feature_id_val= 60,
  do_coord_flip = T,
  rot_y_tic_label = T,
  bar_fill = "purple",
  bar_color = "gold",
  bar_alpha = 0.7,
  show_minor_grids = F
)
chapter_60_tf_idf_plot
