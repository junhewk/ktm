#' Tokenizing Korean text to morpheme
#'
#' Tokenizing Korean texts to morpheme with or without POS tagging based on `Seunjeon`
#' morpheme analyzer and `Rscala` scala interface.
#'
#' @param corpus a character vector of any length or a list of characters or list of character vectors of length 1
#' @param token unit for tokenizing, options are "word" (default), "ngram", "tag". "word" and "ngram" will return
#' a double column tibble (token and text id), and "tag" will return a tripple column tibble (token, tag, and text id).
#' @param annotate if FALSE (default), the function will return morphemes only. If FALSE, the function will return POS tagging also.
#' @param deinflect if FALSE (default), the function will return the morpheme with inflected forms. If TRUE, morphemes will have its original form.
#' @param sep a character string to separate between token and tag
#' @param strip_punct bool, if you want to preserve punctuations in the phrase, set this as FALSE
#' @param strip_number bool, if you want to preserve numerics in the phrase, set this as FALSE
#' @param n the maximum number of words in n-gram
#' @param n_min the minimun number of words in n-gram
#' @param ngram_sep a character string to separate n-gram
#' @return a tibble with text ids, morphemes (and POS tag when token == "tag")
#'
#' See examples in \href{https://github.com/junhewk/ktm}{Github}.
#'
#' @examples
#' \dontrun{
#' # a variable textKor declared with Korean text
#' # get a tibble with morpheme tokens, POS tag in a single column and text ids
#' tokenizer(textKor)
#'
#' # get a tibble with morpheme tokens without POS tag and text ids
#' tokenizer(textKor, annotate = FALSE)
#'
#' # get a tibble with morpheme tokens, POS tag, and text ids in separate columns
#' tokenizer(textKor, token = "tag")
#'
#' # get a tibble with trigram tokens and text ids
#' tokenizer(textKor, token = "ngram")
#'
#' # get a tibble with bigram and trigram tokens and text ids
#' tokenizer(textKor, token = "ngram", n = 3, n_min = 2)
#' }
#'
#' @import rJava
#' @import dplyr
#' @import tibble
#' @importFrom stats setNames
#' @importFrom utils stack
#' @export
tokenizer <- function(corpus, token = c("word", "ngram", "tag"), annotate = TRUE, sep = "/", deinflect = FALSE,
                  strip_punct = TRUE, strip_number = TRUE, n = 3, n_min = n, ngram_sep = ";") {
  #check_input(corpus)

  token <- match.arg(token)

  # stripping character vectors
  if (strip_punct == TRUE) corpus <- gsub("[[:punct:]]", " ", corpus)
  if (strip_number == TRUE) corpus <- gsub("[0-9]", "", corpus)
  # if (strip_eng == TRUE) text <- gsub("[A-Za-z]", "", text)

  analyzer <- rJava::J("org.bitbucket.eunjeon.seunjeon.Analyzer")
  node <- rJava::J("org.bitbucket.eunjeon.seunjeon.LNode")
  inflect <- rJava::J("org.bitbucket.eunjeon.seunjeon.MorphemeType")$INFLECT()

  termList <- list()
  tagList <- list()

  for (i in seq_along(corpus)) {
    # text[i] <- iconv(text[i], to = "UTF-8") # double check for input encoding
    result <- rJava::.jcast(analyzer$parseJava(corpus[[i]]), node)
    term <- c()
    tag <- c()

    for (ns in as.list(result)) {
      if (deinflect == TRUE) {
        for (n in as.list(ns$deInflectJava())) {
          if (token == "tag") {
            term <- c(term, n$morpheme()$surface())
            tag <- c(tag, n$morpheme()$feature()$head())
          } else if (annotate == TRUE) {
            if (n$morpheme()$mType() == inflect) {
              for (tm in strsplit(n$morpheme()$feature()$array()[8], "+", fixed = TRUE)) {
                term <- c(term, substr(tm, 1, (nchar(tm) - 2)))
              }
            } else {
              term <- c(term, paste0(n$morpheme()$surface(), sep, n$morpheme()$feature()$head()))
            }
          } else {
            if (n$morpheme()$mType() == inflect) {
              for (tm in strsplit(n$morpheme()$feature()$array()[8], "+", fixed = TRUE)) {
                term <- c(term, strsplit(tm, "/")[[1]][1])
              }
            } else {
              term <- c(term, n$morpheme()$surface())
            }
          }
        }
      } else {
        if (token == "tag") {
          term <- c(term, ns$morpheme()$surface())
          tag <- c(tag, ns$morpheme()$feature()$head())
        } else if (annotate == TRUE) {
          if (ns$morpheme()$mType() == inflect) {
            for (tm in strsplit(ns$morpheme()$feature()$array()[8], "+", fixed = TRUE)) {
              term <- c(term, substr(tm, 1, (nchar(tm) - 2)))
            }
          } else {
            term <- c(term, paste0(ns$morpheme()$surface(), sep, ns$morpheme()$feature()$head()))
          }
        } else {
          if (ns$morpheme()$mType() == inflect) {
            for (tm in strsplit(ns$morpheme()$feature()$array()[8], "+", fixed = TRUE)) {
              term <- c(term, strsplit(tm, "/")[[1]][1])
            }
          } else {
            term <- c(term, ns$morpheme()$surface())
          }
        }
      }
    }

    Encoding(term) <- "UTF-8"
    termList[[i]] <- term
    if (token == "tag") {
      Encoding(tag) <- "UTF-8"
      tagList[[i]] <- tag
    }
  }

  if (token == "ngram") {
    termList <- ngramer(termList, n, n_min, ngram_sep)
  }

  ret <- tibble::as_tibble(stats::setNames(utils::stack(stats::setNames(termList, seq_along(termList))), c(token, "text_id")))

  if (token == "tag") {
    ret$tags <- unlist(tagList)
    ret <- ret[c(token, "tags", "text_id")]
    colnames(ret) <- c("word", "tag", "text_id")
    ret
  } else {
    ret <- ret[c(token, "text_id")]
    ret
  }
}
