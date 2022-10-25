library(data.table)
library(dplyr)
library(tidytext)
library(magrittr)
library(RtextminerPkg)

# The following data example originated from
# https://sci2lab.github.io/ml_tutorial/tfidf/

stop_words <- data.frame(
  word = c("the", "is", "in", "we", ".", ",")
)

sentence_text <- c(
  "The sky is blue.",
  "The sun is bright today.",
  "The sun in the sky is bright.",
  "We can see the shining sun, the bright sun."
)
sentence_id <- c(
  "s1",
  "s2",
  "s3",
  "s4"
)
# create a data frame of sentences and their id
sentences_df <- data.frame(
  sentence_id = sentence_id,
  sentence_text = sentence_text
)

# get tf_idf with tokenization by words
tf_idf_dt <- RtextminerPkg::get_tf_idf(
  x = sentences_df,
  feature_id = "sentence_id",
  feature_text = "sentence_text",
  stopwords = stop_words$word
)

# get tf_idf with tokenization by ngram
tf_idf_bigram_dt <- RtextminerPkg::get_tf_idf(
  x = sentences_df,
  type = "ngram",
  n_gram = 2L,
  feature_id = "sentence_id",
  feature_text = "sentence_text",
  stopwords = stop_words$word
)

# -------------------------tidytext::bind_tf_idf()------------
# run the same text through tidytext::bind_tf_idf() to compare.
# tokenize sentences to words, remove stopwords, get word counts
tidy_words_df <- sentences_df %>%
  tidytext::unnest_tokens(word, "sentence_text") %>%
  anti_join(stop_words) %>%
  count(sentence_id,word, sort = T) %>%
  ungroup()

# get count of words in sentences
total_words_df <- tidy_words_df %>%
  group_by(sentence_id) %>%
  summarise(total = sum(n))

# left join tidy_words_df with total_words_df by sentence
join_words_df <- left_join(tidy_words_df, total_words_df)

# call tidytext::bind_tf_idf()
idf_tf_idf_df <- tidytext::bind_tf_idf(
  tbl = join_words_df,
  term = "word",
  document = "sentence_id",
  n = "n"
)
