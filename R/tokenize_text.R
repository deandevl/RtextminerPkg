#' Function is a wrapper around various text tokenizations offered by the \code{tokenizers}
#'  package with some added functionality.
#'
#'
#' @param x A character vector of any length or data frame with a column of character vectors
#'  where each vector has a length of 1.
#' @param type A string that sets the type of tokenization.  Acceptable values are \dQuote{character},
#'  \dQuote{word}, \dQuote{sentence}, \dQuote{paragraph}, \dQuote{line}, \dQuote{chunck}, \dQuote{ngram},
#'  \dQuote{ngram}, \dQuote{regex}.
#' @param chunck_size An integer that defines the number of words in a "chunck" when
#'  \code{type} is \dQuote{chunck}
#' @param n_gram The number of words in the n-gram if \code{type} is \dQuote{ngram}.
#'  Must be an integer greater than or equal to 1L.
#' @param ngram_delimiter The separator between words in a n-gram if \code{type} is \dQuote{ngram}.
#' @param input_col If \code{x} is a data frame then this is name of the column
#'  with the list of character vectors of length 1.
#' @param output_col The name of the column in the returned data.table containing the tokens.
#' @param stopwords A character vector of stop words to be excluded from the input text. Stopwords
#'  work for tokenize \code{type}'s \dQuote{words}, \dQuote{chuncks}, and \dQuote{ngram}.
#' @param lowercase A logical which if TRUE then input text will converted to lower case
#'  with the exception of \dQuote{lines} and \dQuote{paragraphs}.
#' @param strip_numeric A logical which if TRUE input text will be stripped of numbers
#'  if \code{type} is \dQuote{words}.
#' @param strip_punct A logical which if TRUE input text will be stripped of punctuation
#'  if \code{type} is \dQuote{words} or \dQuote{sentences}
#' @param strip_non_alphanum A logical which if TRUE then input text will be stripped of
#'  non-alphanumerics if \code{type} is \dQuote{characters}
#' @param paragraph_break A string that defines the delimiter of paragraphs within the input text.
#' @param regex_pattern A string that defines the pattern for tokenization with regex.
#' @param regex_return A string that defines the return value for a regex. A value of
#'  \dQuote{logical} returns TRUE or FALSE if the pattern is found. A value of \dQuote{string}
#'   returns the actual string detected.
#' @param na.rm A logical which if TRUE removes NA values from among the resulting words.
#'
#' @importFrom tokenizers tokenize_characters
#' @importFrom tokenizers tokenize_words
#' @importFrom tokenizers tokenize_sentences
#' @importFrom tokenizers tokenize_lines
#' @importFrom tokenizers tokenize_paragraphs
#' @importFrom tokenizers tokenize_ngrams
#' @importFrom tokenizers chunk_text
#' @importFrom stringr str_extract
#' @importFrom stringr str_detect
#' @importFrom stringr regex
#' @importFrom data.table data.table
#' @importFrom data.table setDT
#' @importFrom data.table as.data.table
#' @importFrom data.table tstrsplit
#'
#' @return A named list containing:
#'
#'  1. \dQuote{tokens_dt}:  A data.table with a column of tokens defined by \code{type}.
#'     The column name is defined by \code{output_col} argument.
#'
#'  2. \dQuote{tokens_count} A data.table of token counts. If \code{type} is
#'     \dQuote{ngram} it is showing paired n-gram counts. For \code{type} \dQuote{word}
#'     it is showing total word counts. Otherwise the value is NULL.
#'
#' @author Rick Dean
#'
#' @export
tokenize_text <- function(
  x,
  type = "word",
  chunck_size = 80,
  n_gram = 2L,
  ngram_delimiter = " ",
  input_col = "text",
  output_col = NULL,
  stopwords = NULL,
  lowercase = TRUE,
  strip_numeric = FALSE,
  strip_punct = FALSE,
  strip_non_alphanum = FALSE,
  paragraph_break = "\n\n",
  regex_pattern = "\\s+",
  regex_return = "logical",
  na.rm = TRUE) {

  if(is.null(output_col)){
    output_col <- type
  }

  if(is.data.frame(x)){
    text_dt <- data.table::setDT(x)
  }else{
    text_dt <- data.table::as.data.table(x)
    names(text_dt) <- input_col
  }

  if(is.null(stopwords) & type == "ngram"){
    stopwords <- character()
  }

  if(type == "character"){
    tokens_lst <- tokenizers::tokenize_characters(
      x = text_dt[[input_col]],
      lowercase = lowercase,
      strip_non_alphanum = strip_non_alphanum
    )
  }else if(type == "word"){
    tokens_lst <- tokenizers::tokenize_words(
      x = text_dt[[input_col]],
      lowercase = lowercase,
      stopwords = stopwords,
      strip_punct = strip_punct,
      strip_numeric = strip_numeric
    )
  }else if(type == "sentence"){
    tokens_lst <- tokenizers::tokenize_sentences(
      x = text_dt[[input_col]],
      lowercase = lowercase,
      strip_punct = strip_punct
    )
  }else if(type == "line"){
    tokens_lst <- tokenizers::tokenize_lines(
      x = text_dt[[input_col]]
    )
  }else if(type == "paragraph"){
    tokens_lst <- tokenizers::tokenize_paragraphs(
      x = text_dt[[input_col]],
      paragraph_break = paragraph_break
    )
  }else if(type == "regex") {
    if(regex_return == "string"){
      tokens_lst <- stringr::str_extract(
        text_dt[[input_col]],
        stringr::regex(regex_pattern, ignore_case = T)
      )
    }else {
      tokens_lst <- stringr::str_detect(
        text_dt[[input_col]],
        stringr::regex(regex_pattern, ignore_case = T)
      )
    }
  }else if(type == "chunck") {
    text <- paste(text_dt[[input_col]], collapse = " ")

    tokens_lst <- tokenizers::chunk_text(
      x = text,
      chunk_size = chunck_size,
      lowercase = lowercase,
      stopwords = stopwords,
      strip_punct = strip_punct,
      strip_numeric = strip_numeric
    )

    return(
      list(
       # tokens_dt = unlist(tokens_lst),
        tokens_dt = data.frame(
          chunk = unlist(tokens_lst)
        ),
        tokens_count = NULL
      )
    )
  }else if(type == "ngram"){
    tokens_lst <- tokenizers::tokenize_ngrams(
      x = text_dt[[input_col]],
      n = n_gram,
      lowercase = lowercase,
      stopwords = stopwords,
      ngram_delim = ngram_delimiter
    )
    # tokens_lst is a list of length 1, with a character vector of combined ngrams
    #   separated by a " ".
    col_names <- sapply(1:n_gram, function(i)paste0("token_",i))

    # unlist tokens_lst, transpose into rows, split each row by " "
    # returns a list of character vectors of separated ngrams
    tokens_combined <- unlist(tokens_lst)
    tokens_split_lst <- data.table::tstrsplit(tokens_combined, " ", fixed = T, names = col_names)

    # convert by reference tokens_split_lst to a data.table
    tokens_split_dt <- data.table::setDT(tokens_split_lst)

    # construct our return data.table which contains all the columns
    #  in our input data.table text_dt
    tokens_dt <- text_dt[rep(seq_len(nrow(text_dt)), lengths(tokens_lst)), , drop = F]

    # we don't need the input text column
    tokens_dt[, (input_col) := NULL]

    # column bind tokens_dt with our split tokens data.table
    tokens_dt <- cbind(tokens_dt, tokens_split_dt)

    # add the combined ngrams (tokens_lst) to tokens_dt
    tokens_dt[, ngram := tokens_combined]

    # remove rows with NA's for any ngram values if requested
    if(na.rm){
      tokens_dt <- na.omit(tokens_dt, cols = col_names)
    }

    # create a data.table of counts of the ngram values in descending order
    tokens_count <- tokens_dt[, .(.N), by = col_names][order(-N)]

    return(list(
      tokens_dt = tokens_dt,
      tokens_count = tokens_count
    ))
  }else{
    stop(paste0("Could not recognize type '", type, "'"))
  }

  tokens_v <- unlist(tokens_lst)

  tokens_dt <- text_dt[rep(seq_len(nrow(text_dt)), lengths(tokens_lst)), , drop = F]

  tokens_dt[[output_col]] <- tokens_v
  # we don't need the input text column
  tokens_dt[, (input_col) := NULL]

  if(na.rm){
    tokens_dt <- na.omit(tokens_dt, cols = output_col)
  }

  tokens_count <- NULL
  if(type == "character" | type == "word" | type == "regex"){
    tokens_count <- tokens_dt[, .(.N), by = output_col][order(-N)]
  }

  return(list(
    tokens_dt = tokens_dt,
    tokens_count = tokens_count))
}
