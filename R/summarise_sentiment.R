#' Summarize positive and negative words
#'
#' Uses match_words() to compare text words with positive and negative
#' lexicons, returning counts and ratio.
#'
#' @param words Vector of cleaned words from preprocess_text().
#' @param positive_lexicon Vector of positive patterns.
#' @param negative_lexicon Vector of negative patterns.
#'
#' @return A list containing sentiment summary metrics.
#'
#' @examples
#' words <- c("happy", "nauseous", "joyful", "nauseated")
#' pos <- c("happ*", "joy*")
#' neg <- c("nause*")
#' summarize_sentiment(words, pos, neg)
#' @export

summarize_sentiment <- function(words, positive_lexicon, negative_lexicon) {

  positives <- match_words(words, positive_lexicon)
  negatives <- match_words(words, negative_lexicon)

  ratio <- length(positives) / max(length(negatives), 1)

  list(
    n_positive = length(positives),
    n_negative = length(negatives),
    ratio = ratio,
    positive_words = positives,
    negative_words = negatives
  )
}
