library(data.table)
library(syuzhet)
library(janeaustenr)
library(RtextminerPkg)
library(RplotterPkg)


afinn_dict_df <- syuzhet::get_sentiment_dictionary("afinn", language = "english")

books <- austen_books()

# tokenize the austen books by word
austen_words_lst <- RtextminerPkg::tokenize_text(
  x = books,
  input_col = "text",
  stopwords = stopwords::stopwords(language = "en"),
  strip_punct = TRUE,
  strip_numeric = TRUE
)

# join austen words with AFINN sentiment words
austen_words_join_dt <- RtextminerPkg::join_words(
  outer_df = austen_words_lst$tokens_dt,
  inner_df = afinn_dict_df,
  key_name = "word")

# create an ordered sentiment value data.frame grouped by book to plot from
plot_dt <- austen_words_join_dt[, value, by = book][order(value)]

# plot sentiment values by book
RplotterPkg::multi_bar_plot(
  df = austen_words_join_dt,
  factor_var = "book",
  factor_x = "value",
  rot_y_tic_label = T,
  title = "Sentiment Counts Across the AFFIN Negative to Positive Word Scale",
  subtitle = "Jane Austen's Novels",
  x_limits = c(-5.0, 5.0),
  x_breaks = seq(-5.0, 5.0, 1.0),
  col_width = 5
)
