#' create_cloud
#'
#' @param text_col character
#' @param path character - path to data
#' @param ... fundals passed to count_words.
#'
#' @return wordcloud figure
#' @export
#'
#' @examples
create_cloud <- function(text_col, path, ...) {
  text <- load_data(text_col, path)
  count <- count_words(text_data = text, ...)

  ggwordcloud::ggwordcloud(count$word, count$mentions, colors=epinionR::epi_colors)
}
