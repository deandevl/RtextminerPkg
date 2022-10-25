#' Function joins two character vectors of words
#'
#' @param outer_df The outer data frame of words
#' @param inner_df The inner data frame of words
#' @param key_name The column name to use as the common key between
#'  \code{outer_v} and \code{inner_v}
#'
#' @importFrom data.table as.data.table
#' @importFrom data.table setkeyv
#'
#' @return A data.table of the inner joined words between \code{outer_v} and \code{inner_v}
#'
#' @author Rick Dean
#'
#' @export
join_words <- function(outer_df, inner_df, key_name = "word"){
  outer_dt <- data.table::as.data.table(outer_df)
  inner_dt <- data.table::as.data.table(inner_df)

  data.table::setkeyv(outer_dt, key_name)
  data.table::setkeyv(inner_dt, key_name)

  outer_join_dt <- outer_dt[inner_dt, nomatch = 0]
  return(outer_join_dt)
}