library(data.table)
library(tokenizers)
library(RtextminerPkg)

# Define a character vector of length 1:
song_txt <-  paste0("How many roads must a man walk down\n",
                "Before you call him a man?\n",
                "How many seas must a white dove sail\n",
                "Before she sleeps in the sand?\n",
                "\n",
                "How many times must the cannonballs fly\n",
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

# Tokenize song_txt by character
song_char_lst <- RtextminerPkg::tokenize_text(
  x = song_txt,
  type = "characters",
  output_col = "char"
)

# Tokenize emily_df via characters
emily_char_lst <- RtextminerPkg::tokenize_text(
  x = emily_df,
  type = "characters",
  input_col = "text"
)

# Tokenize song_txt via words
song_word_lst <- RtextminerPkg::tokenize_text(
  x = song_txt,
  type = "words"
)

# Tokenize emily_df via words
emily_word_lst <- RtextminerPkg::tokenize_text(
  x = emily_df,
  type = "words",
  input_col = "text"
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

# Tokenize song_txt via chuncks
song_chuncks_lst <- RtextminerPkg::tokenize_text(
  x = song_txt,
  type = "chuncks",
  chunck_size = 4,
  output_col = "chunck"
)

# Tokenize emily_df via chuncks
emily_chuncks_lst <- RtextminerPkg::tokenize_text(
  x = emily_df,
  type = "chuncks",
  chunck_size = 4,
  input_col = "text",
  output_col = "chunck"
)

# Tokenize song_text via lines
song_lines_lst <- RtextminerPkg::tokenize_text(
  x = song_txt,
  type = "lines",
  output_col = "line"
)

# Tokenize song_txt via sentences
song_sentences_lst <- RtextminerPkg::tokenize_text(
  x = song_txt,
  type = "sentences",
  output_col = "sentence"
)

# Tokenize song_text via paragraphs
song_paragraphs_lst <- RtextminerPkg::tokenize_text(
  x = song_txt,
  type = "paragraphs",
  output_col = "paragraph"
)



