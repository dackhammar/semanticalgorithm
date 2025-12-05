#' match words
#'
#' @description
#' Match words in text using preloaded positive and negative lexicons
#'
#' This function compares a vector of words from a text to the internal
#' Positive and Negative vectors. It supports "*" as a wildcard suffix
#' or prefix.
#'
#' @param words A character vector of words from the text.
#' @param lexicon A vector with a lexicon.
#'
#' @return A vector of matched words found in the text.
#' @examples
#' words <- c("happy", "joyful", "nauseous", "nauseated")
#' lexicon <- c("happ*", "joy*")
#' match_words(words, lexicon)
#' @export
match_words <- function(words, lexicon) {

  # Ensure words are lowercase
  words <- tolower(words)

  matches <- c()

  for(pattern in lexicon) {

    pattern <- tolower(pattern)  # make lexicon lowercase as well

    if(grepl("\\*$", pattern)) {
      # wildcard at end -> prefix match
      prefix <- sub("\\*$", "", pattern)
      matched <- words[startsWith(words, prefix)]

    } else if(grepl("^\\*", pattern)) {
      # wildcard at beginning -> suffix match
      suffix <- sub("^\\*", "", pattern)
      matched <- words[endsWith(words, suffix)]

    } else {
      # exact match
      matched <- words[words == pattern]
    }

    matches <- c(matches, matched)
  }

  return(matches)
}
