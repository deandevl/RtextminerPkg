#' Function computes tf_idf values for a collection of text features
#'
#' @description The input data frame consist of a column of text broken down as a
#'  collection of books, chapters, sections, paragraphs, etc. (called \code{feature_text})
#'  and a corresponding \code{feature_id} that identifies the origin of the text.
#'  The function tokenizes the column of text (by word, sentence, ngram, etc) and computes
#'  the term frequency(tf), inverse document frequency(idf) and their product(tf-idf).
#'
#' @param x a data frame with a column of feature text (by book, chapter, etc.) and a
#'  corresponding column that identifies the source of the text (book name, chapter number,
#'  etc). Each row of text should be a character vector of length of 1.
#' @param type A string that sets the type of tokenization. Acceptable values
#'  are \dQuote{character}, \dQuote{word}, \dQuote{sentence}, \dQuote{ngram},
#'  \dQuote{line}, \dQuote{paragraph}.
#' @param n_gram The number of words in the n-gram if \code{type} is \dQuote{ngram}.
#'  Must be an integer greater than or equal to 1L.
#' @param ngram_delimiter The separator between words in a n-gram if \code{type}
#'  is \dQuote{ngram}.
#' @param feature_id The column name from \code{x} that identifies each individual
#'  feature.
#' @param feature_text The column name from \code{x} that contains the text for a feature.
#' @param stopwords A vector of stop words to be used during tokenization.
#' @param lowercase A logical which if TRUE then input text will converted to lower case
#'  with the exception of \dQuote{line} and \dQuote{paragraph}.
#' @param strip_numeric A logical which if TRUE input text will be stripped of numbers
#'  if \code{type} is \dQuote{word}.
#' @param strip_punct A logical which if TRUE input text will be stripped of punctuation
#'  if \code{type} is \dQuote{word} or \dQuote{sentence}
#' @param strip_non_alphanum A logical which if TRUE then input text will be stripped of
#'  non-alphanumerics if \code{type} is \dQuote{character}
#'
#' @importFrom tokenizers tokenize_words
#' @importFrom data.table as.data.table
#' @importFrom data.table setkey
#' @importFrom data.table setkeyv
#' @importFrom data.table setnames
#'
#' @return A data frame with columns for the feature id, token value, tf, idf,
#'  and tf-idf
#'
#' @author Rick Dean
#'
#' @export
get_tf_idf <- function(
  x,
  type = "word",
  n_gram = 2L,
  ngram_delimiter = " ",
  feature_id = "document",
  feature_text = "text",
  stopwords = NULL,
  lowercase = TRUE,
  strip_numeric = FALSE,
  strip_punct = FALSE,
  strip_non_alphanum = FALSE
  ){
  # tokenize documents by the type specified
  tokens_lst <- RtextminerPkg::tokenize_text(
    x = x,
    type = type,
    n_gram = n_gram,
    ngram_delimiter = ngram_delimiter,
    input_col = feature_text,
    stopwords = stopwords,
    lowercase = lowercase,
    strip_numeric = strip_numeric,
    strip_punct = strip_punct,
    strip_non_alphanum = strip_non_alphanum
  )

  # compute the idf
  select_cols <- c(feature_id, type)

  tokens_dt <- tokens_lst$tokens_dt[, ..select_cols]
  tokens_table <- table(tokens_dt)

  non_zero_token_n <- colSums(tokens_table != 0)
  n_features <- length(unique(x[[feature_id]]))
  idf_dt <- data.table::as.data.table(log(n_features/non_zero_token_n), keep.rownames = T)
  colnames(idf_dt) = c("token", "idf")

  features_n_dt <- tokens_dt[, .(..feature_n = .N), by = get(feature_id)]
  colnames(features_n_dt) <- c("feature_id", "feature_n")

  # get tokens count
  token_n_dt <- tokens_dt[, .(token_n = .N), by = mget(select_cols)][order(-token_n)]
  data.table::setnames(token_n_dt, old = c(feature_id,type), new = c("feature_id","token"))

  # left join token_n_dt with features_n_dt
  data.table::setkey(token_n_dt,feature_id)
  data.table::setkey(features_n_dt, feature_id)
  feature_token_n_dt <- token_n_dt[features_n_dt]

  # add a tf column
  feature_token_n_dt[, tf := token_n/feature_n]

  #join doc_token_n_dt with idf_dt by token
  data.table::setkeyv(feature_token_n_dt, "token")
  data.table::setkeyv(idf_dt, "token")
  tf_idf_dt <- feature_token_n_dt[idf_dt]

  # add a tf_idf column
  tf_idf_dt <-  tf_idf_dt[, tf_idf := tf * idf][order(-tf_idf)]

  return(tf_idf_dt)
}
