#' preprocess text
#'
#' @description
#' Preprocess text from a .txt file
#'
#' This function reads a .txt file using read.delim(), converts the text
#' to lowercase, removes punctuation, splits into words, and returns a
#' clean character vector.
#'
#' @param filepath Path to a .txt file containing raw text.
#'
#' @return A character vector of cleaned words.
#' @examples
#'\dontrun{
#' words <- preprocess_text("inst/extdata/example.txt")
#' }
#' @export

preprocess_text <- function(filepath) {
  # Read the file as lines
  text_raw <- readLines(filepath, warn = FALSE)

  # Collapse into one single string
  text <- paste(text_raw, collapse = " ")

  # Convert to lowercase
  text <- tolower(text)

  # Remove punctuation
  text <- gsub("[[:punct:]]", " ", text)

  # Remove extra spaces
  text <- gsub("\\s+", " ", text)
  text <- trimws(text)

  # Split into words
  words <- unlist(strsplit(text, " "))

  # Remove empty strings if any
  words <- words[words != ""]

  return(words)
}
