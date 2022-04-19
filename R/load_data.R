#' load_data
#'
#' @param text_col character
#' @param path character - path to the raw data
#'
#' @return text for count_words
#' @export
#'
#' @examples
#' @importFrom rlang .data
load_data <- function(text_col, path) {
  text <- haven::read_sav(file = path) %>%
    haven::as_factor() %>%
    dplyr::mutate(text = paste(.data[[text_col]], sep = " "),
           text = stringr::str_trim(string = .data$text,
                           side = "both")) %>%
    dplyr::filter(.data$text != "") %>%
    dplyr::select(.data$text)
}


#' load_stopwords
#'
#' @return character vector
#' @export
load_stopwords <- function() {
  stop
}
