#' N-gram tokenizer
#'
#' Preparing n-grams based on tokenized text.
#'
#' @param token a list of character vectors of tokens
#' @param n the maximum number of words in the n-gram, integer more than 1, default is 3
#' @param n_min the minimum number of words in the n-gram, integer more than 1, default is same as n
#' @param ngram_sep the separating character between words, default is ";"
#' @return a list of character vectors containing n-grams
#'
#' @details When a n-gram tibble is needed, use tokenizer(text, token = "ngram") instead.
#'
#' See examples in \href{https://github.com/junhewk/ktm}{Github}.
#'
#' @examples
#' \dontrun{
#' # a variable tokens tokenized with `pos()`
#' # generate a trigram
#' ngramer(tokens)
#'
#' # generate a bigram, and a trigram at the same time
#' ngramer(tokens, n = 3, n_min = 2)
#' }
#'
#' @export
ngramer <- function(token, n = 3L, n_min = n, ngram_sep = ";") {
  if (!is.list(token)) {
    stop("Input must be a list.")
  }

  if (!all(vapply(token, is.character, logical(1)))) {
    stop("Input list must be consisted with character vectors.")
  }

  if (n < n_min || n_min <= 0) {
    stop("n and n_min must be integers, and n_min must be less than ",
         "n and greater than 1.")
  }

  ret <- vector("list", length(token))

  for (i in seq_along(token)) {
    term <- character()

    if (length(token[[i]]) > n_min) {
      for (j in seq(n_min, n, 1)) {
        term <- c(term, ngramize(token[[i]], n = j, ngram_sep = ngram_sep,
                                 len = (length(token[[i]]) - j + 1)))
      }
    }

    ret[[i]] <- term
  }

  names(ret) <- names(token)
  ret
}

# internal function for n-gram tokenizer
ngramize <- function(token, n, ngram_sep, len) {
  ret <- character(len)

  for (i in seq_len(len)) {
    ret[i] <- paste(token[i:(i + n - 1)], collapse = ngram_sep)
  }

  ret
}
