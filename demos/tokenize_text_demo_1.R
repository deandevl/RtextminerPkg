library(data.table)
library(tokenizers)
library(tm)
library(RtextminerPkg)

# Define a character vector of length 1:
song_txt <-  paste0("How many roads must a man walk down\n",
                "Before you call him a man?\n",
                "How many seas must a white dove sail\n",
                "Before she sleeps in the sand?\n",
                "\n",
                "How many times must the cannonballs fly\n",  #new paragraph
                "Before they're forever banned?\n",
                "The answer, my friend, is blowin'in the wind.\n",
                "The answer is blowin'in the wind.\n")

# Define a data.frame with 4 character vectors of length 1:
emily_vec <- c("Because I could not stop for Death",
                "He kindly stopped for me",
                "The Carriage held but just Ourselves",
                "and Immortality")
emily_df <- data.frame(
  line = 1:4,
  text = emily_vec
)

# Tokenize song_txt (a vector) via character
# The output column for the characters is "song_char"
song_char_lst <- RtextminerPkg::tokenize_text(
  x = song_txt,
  type = "character",
  output_col = "song_char"
)

# Tokenize emily_df (a dataframe) via character
emily_char_lst <- RtextminerPkg::tokenize_text(
  x = emily_df,
  type = "character",
  input_col = "text"
)

# Tokenize song_txt via word
song_word_lst <- RtextminerPkg::tokenize_text(
  x = song_txt,
  type = "word"
)

# Tokenize emily_df via word
# use stopwords from tm package
emily_word_lst <- RtextminerPkg::tokenize_text(
  x = emily_df,
  type = "word",
  input_col = "text",
  stopwords = tm::stopwords()
)

# Tokenize song_txt via n-grams of 2 words
song_2_grams_lst <- RtextminerPkg::tokenize_text(
  x = song_txt,
  type = "ngram",
  n_gram = 2L
)

# Tokenize emily_df via n-grams of 2 words
emily_2_grams_lst <- RtextminerPkg::tokenize_text(
  x = emily_df,
  type = "ngram",
  n_gram = 2L,
  input_col = "text"
)

# Tokenize song_txt via chunck
song_chuncks_lst <- RtextminerPkg::tokenize_text(
  x = song_txt,
  type = "chunck",
  chunck_size = 4
)

# Tokenize emily_df via chunck
# use stopwords from tm package
emily_chuncks_lst <- RtextminerPkg::tokenize_text(
  x = emily_df,
  type = "chunck",
  chunck_size = 4,
  input_col = "text",
  stopwords = tm::stopwords()
)

# Tokenize song_text via line
song_lines_lst <- RtextminerPkg::tokenize_text(
  x = song_txt,
  type = "line"
)

# Tokenize song_txt via sentence
song_sentences_lst <- RtextminerPkg::tokenize_text(
  x = song_txt,
  type = "sentence"
)

# Tokenize song_text via paragraph
song_paragraphs_lst <- RtextminerPkg::tokenize_text(
  x = song_txt,
  type = "paragraph"
)



