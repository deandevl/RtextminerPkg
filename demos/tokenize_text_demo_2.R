library(data.table)
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
  text = sentence_text,
  sentence = sentence_id
)

# tokenize sentences to words
datatable_words_dt <- RtextminerPkg::tokenize_text(
  x = sentences_df,
  stopwords = stop_words$word
)