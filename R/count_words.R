#' count_words
#'
#' @param text_data data.frame with text as first column
#' @param include_2gram boolean
#' @param stopwords list of stopwords
#' @param mention_limit int of where to cutoff words
#' @param patterns list of named replacements of the form "regex" = "replacement"
#'
#' @return a dataframe with words and their counts
#' @export
#' @importFrom rlang .data
#'
#' @examples
count_words <- function(text_data,
                        include_2gram=F,
                        stopwords=load_stopwords(),
                        mention_limit=10,
                        patterns=NULL
) {
  tkns <- quanteda::tokens(text_data[[1]],
                           remove_punct = TRUE,
                           remove_symbols = TRUE,
                           remove_numbers = TRUE,
                           remove_url = TRUE,
                           remove_separators = TRUE,
                           split_hyphens = TRUE,
                           include_docvars = TRUE,
                           padding = F)

  if (include_2gram) {
    tkns <- quanteda::tokens_ngrams(tkns, n=1:2, concatenator = " ")
  }

  dfm_question <- quanteda::dfm(x = tkns, tolower = TRUE)

  # problems? Search for dplyr and NSE (non standard evaluation)
  # there is no abundant information about how to do it properly.
  # https://shipt.tech/https-shipt-tech-advanced-programming-and-non-standard-evaluation-with-dplyr-e043f89deb3d
  # This is the solution used (under the section 'a note on tidyverse')
  # https://www.r-bloggers.com/2019/08/no-visible-binding-for-global-variable/
  df <- dfm_question %>%
    quanteda::convert(to = "data.frame") %>%
    tidyr::pivot_longer(!.data$doc_id,
                 names_to = "word",
                 values_to = "count") %>%
    dplyr::filter(.data$count > 0) %>%
    dplyr::filter(!.data$word %in% stopwords)

  if (!missing(patterns)) {
    df <- df %>%
      dplyr::mutate(word = stringr::str_replace_all(string = .data$word,
                                    patterns))
  }

  count <- df %>%
    dplyr::group_by(.data$word) %>%
    dplyr::summarise(mentions = sum(.data$count, na.rm = TRUE)) %>%
    dplyr::ungroup() %>%
    dplyr::filter(.data$mentions >= mention_limit)
}
