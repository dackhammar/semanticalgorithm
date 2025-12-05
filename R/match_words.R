#' Match words in text using preloaded positive and negative lexicons
#'
#' This function compares a vector of words from a text to the internal
#' Positive and Negative vectors. It supports "*" as a wildcard suffix
#' or prefix.
#'
#' @param words A character vector of words from the text.
#' @param type Either "positive" or "negative" to select the lexicon.
#'
#' @return A vector of matched words found in the text.
#' @examples
#' words <- c("happy", "joyful", "nauseous", "nauseated")
#' match_words(words, type = "positive")
#' match_words(words, type = "negative")
#' @export
match_words <- function(words, type = c("positive", "negative")) {

  type <- match.arg(type)

  # Use the appropriate preprocessed vector
  if(type == "positive") {
    lexicon <- Positive
  } else {
    lexicon <- Negative
  }

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

  # Remove duplicates if needed
  matches <- unique(matches)

  return(matches)
}
