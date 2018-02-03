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
  check_input(corpus)

  corpus <- enc_preprocess(corpus)

  token <- match.arg(token)

  # stripping character vectors
  if (strip_punct == TRUE) corpus <- gsub("[[:punct:]]", " ", corpus)
  if (strip_number == TRUE) corpus <- gsub("[0-9]", "", corpus)
  # if (strip_eng == TRUE) text <- gsub("[A-Za-z]", "", text)

  seinterface <- rJava::.jnew("io/github/junhewk/ktm/SEInterface")

  termList <- tagList <- vector("list", length(corpus))

  for (i in seq_along(corpus)) {
    if (deinflect == TRUE) {
      if (token == "tag") {
        result <- tryCatch(rJava::.jcall(seinterface, "[S", "separatedTaggerDeinflect", corpus[[i]]),
                           error = function(e) {
                             warning(sprintf("'%s' can't be processed.\n", corpus[[i]]))
                             character(0)
                           })
        term <- result[1:(length(result) / 2)]
        tag <- result[(length(result) / 2):length(result)]
      } else if (annotate == TRUE) {
        term <- tryCatch(rJava::.jcall(seinterface, "[S", "taggerDeinflect", corpus[[i]], sep),
                           error = function(e) {
                             warning(sprintf("'%s' can't be processed.\n", corpus[[i]]))
                             character(0)
                           })
      } else {
        term <- tryCatch(rJava::.jcall(seinterface, "[S", "tokenDeinflect", corpus[[i]], sep),
                           error = function(e) {
                             warning(sprintf("'%s' can't be processed.\n", corpus[[i]]))
                             character(0)
                           })
      }
    } else {
      if (token == "tag") {
        term <- tryCatch(rJava::.jcall(seinterface, "[S", "separatedTagger", corpus[[i]]),
                           error = function(e) {
                             warning(sprintf("'%s' can't be processed.\n", corpus[[i]]))
                             character(0)
                           })
        term <- result[1:(length(result) / 2)]
        tag <- result[(length(result) / 2):length(result)]
      } else if (annotate == TRUE) {
        term <- tryCatch(rJava::.jcall(seinterface, "[S", "unpackedTaggerSep", corpus[[i]], sep),
                           error = function(e) {
                             warning(sprintf("'%s' can't be processed.\n", corpus[[i]]))
                             character(0)
                           })
      } else {
        term <- tryCatch(rJava::.jcall(seinterface, "[S", "unpackedTaggerSep", corpus[[i]], sep),
                           error = function(e) {
                             warning(sprintf("'%s' can't be processed.\n", corpus[[i]]))
                             character(0)
                           })
      }
    }

    if (!is.null(term)) Encoding(term) <- "UTF-8"
    termList[[i]] <- term
    names(termList)[[i]] <- i
    if (token == "tag") {
      if (!is.null(tag)) Encoding(tag) <- "UTF-8"
      tagList[[i]] <- tag
    }
  }

  if (token == "ngram") {
    termList <- ngramer(termList, n, n_min, ngram_sep)
  }

  ret <- tibble::as_tibble(stats::setNames(utils::stack(termList), c(token, "text_id")))
  ret$text_id <- as.integer(as.character(ret$text_id))

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
