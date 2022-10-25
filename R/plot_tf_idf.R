# Title     : plot_tf_idf
# Objective : plot the top tf_idf values in a bar chart
# Created by: Rick Dean
# Created on: 2021-01-07 8:43 AM
#
#' Function plots the top \code{N} ordered tf_idf values
#'
#' @description Function brings together RtextminerPkg::get_tf_idf() and
#'  RplotterPkg::create_bar_plot() to plot the top \code{N} ordered tf_idf
#'  values from a data frame of text.
#'
#' @param tf_idf the data frame output from RtextminerPkg::get_tf_idf().
#' @param N the number of top tf_idf values to bar chart
#' @param feature_id_val the feature_id character string for which the top tf_idf values are
#'  to be plotted.
#' @param title A string that sets the overall title.
#' @param subtitle A string that sets the overall subtitle.
#' @param x_title A string that sets the x axis title.
#' @param y_title A string that sets the y axis title.
#' @param center_titles A logical which if \code{TRUE} centers both the \code{title} and \code{subtitle}.
#' @param rot_y_tic_label A logical which if TRUE rotates the y tic labels 90 degrees for enhanced readability.
#' @param bar_fill A string that sets the fill color attribute for the bars.
#' @param bar_color A string that sets the outline color attribute for the bars.
#' @param bar_alpha A numeric that sets the alpha component attribute to \code{bar_color}.
#' @param bar_size A numeric that sets the outline thickness attribute of the bars.
#' @param bar_width A numeric that sets the width attribute of the bars.
#' @param y_limits A numeric 2 element vector that sets the minimum and maximum for the y axis.
#'  Use NA to refer to the existing minimum and maximum.
#' @param y_major_breaks A numeric vector or function that defines the exact major tic locations along the y axis.
#' @param y_minor_breaks A numeric vector or function that defines the exact minor tic locations along the y axis.
#' @param y_labels A character vector with the same length as \code{y_major_breaks}, that labels the major tics.
#' @param axis_text_size A numeric that sets the font size along the axis'. Default is 11.
#' @param do_coord_flip A logical which if \code{TRUE} will flip the x and y axis'.
#' @param bar_labels A logical which if \code{TRUE} will label each bar with its value.
#' @param bar_label_size A numeric that sets the size of the bar labels
#' @param bar_label_color A string that sets the color of the bar labels
#' @param show_major_grids A logical that controls the appearance of major grids.
#' @param show_minor_grids A logical that controls the appearance of minor grids.
#'
#' @importFrom tokenizers tokenize_words
#' @importFrom data.table as.data.table
#' @importFrom data.table setkey
#' @importFrom data.table setkeyv
#' @importFrom data.table setnames
#' @importFrom data.table setorderv
#' @importFrom RplotterPkg create_bar_plot
#' @import ggplot2
#'
#' @return A plot object.
#'
#' @author Rick Dean
#'
#' @export
plot_tf_idf <- function(
  tf_idf = NULL,
  N = 15,
  feature_id_val = NULL,
  title = NULL,
  subtitle = NULL,
  x_title = "token",
  y_title = "tf_idf",
  center_titles = FALSE,
  rot_y_tic_label = FALSE,
  bar_fill = NA,
  bar_color = "black",
  bar_alpha = 1.0,
  bar_size = 1.0,
  bar_width = NULL,
  y_limits = NULL,
  y_major_breaks = waiver(),
  y_minor_breaks = waiver(),
  y_labels = waiver(),
  axis_text_size = 11,
  do_coord_flip = FALSE,
  bar_labels = FALSE,
  bar_label_size = 4,
  bar_label_color = "black",
  show_major_grids = TRUE,
  show_minor_grids = TRUE
) {
  tf_idf_top_dt <- tf_idf[feature_id == feature_id_val]
  tf_idf_plot <- RplotterPkg::create_bar_plot(
    df = tf_idf_top_dt[1:N],
    aes_x = "token",
    aes_y = "tf_idf",
    title = title,
    subtitle = subtitle,
    x_title = x_title,
    y_title = y_title,
    center_titles = center_titles,
    rot_y_tic_label = rot_y_tic_label,
    bar_fill = bar_fill,
    bar_color = bar_color,
    bar_alpha = bar_alpha,
    bar_size = bar_size,
    bar_width = bar_width,
    y_limits = y_limits,
    y_major_breaks = y_major_breaks,
    y_minor_breaks = y_minor_breaks,
    y_labels = y_labels,
    axis_text_size = axis_text_size,
    do_coord_flip = do_coord_flip,
    bar_labels = bar_labels,
    bar_label_size = bar_label_size,
    bar_label_color = bar_label_color,
    show_major_grids = show_major_grids,
    show_minor_grids = show_minor_grids,
    order_bars = "asc"
  )
  return(tf_idf_plot)
}
