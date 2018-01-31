#' POS tagger in morpheme form
#'
#' Analyzing Korean texts to morpheme with or without POS tagging based on `Seunjeon`
#' morpheme analyzer and `Rscala` scala interface.
#'
#' @param corpus a character vector of any length or a list of characters or list of character vectors of length 1
#' @param annotate If FALSE (default), the function will return morphemes only. If FALSE, the function will return POS tagging also.
#' @param deinflect If FALSE (default), the function will return the morpheme with inflected forms. If TRUE, morphemes will have its original form.
#' @param sep character vector, separating character between morpheme and POS tagging (default = `/`)
#' @param strip_punct bool, if you want to preserve punctuations in the phrase, set this as FALSE
#' @param strip_number bool, if you want to preserve numerics in the phrase, set this as FALSE
#' @return a list with character vectors of morphemes with POS tag
#'
#' See examples in \href{https://github.com/junhewk/ktm}{Github}.
#'
#' @examples
#' \dontrun{
#' # a variable textKor declared with Korean text
#' tagger(textKor)
#'
#' # without POS tag
#' tagger(textKor, annotate = FALSE)
#'
#' # remove punctuations and numbers
#' tagger(textKor, strip_punct = TRUE, strip_number = TRUE)
#' }
#'
#' @import rJava
#' @export
tagger <- function(corpus, sep = "/", annotate = TRUE, deinflect = FALSE, strip_punct = FALSE, strip_number = FALSE) {
  check_input(corpus)

  corpus <- enc_preprocess(corpus)

  if (strip_punct == TRUE) {
    corpus <- gsub("[[:punct:]]", " ", corpus)
  }
  if (strip_number == TRUE) {
    corpus <- gsub("[0-9]", " ", corpus)
  }

  ret <- vector("list", length(corpus))

  analyzer <- rJava::J("org.bitbucket.eunjeon.seunjeon.Analyzer")
  node <- rJava::J("org.bitbucket.eunjeon.seunjeon.LNode")

  for (i in seq_along(corpus)) {

    result <- tryCatch(rJava::.jcast(analyzer$parseJava(corpus[[i]]), node),
                       error = function(e) {
                         warning(sprintf("'%s' can't be processed.", corpus[[i]]))
                         NULL
                       })

    term <- character()

    for (ns in as.list(result)) {
      if (deinflect == TRUE) {
        for (n in as.list(ns$deInflectJava())) {
          if (annotate == FALSE) {
            term <- c(term, n$morpheme()$surface())
          } else {
            term <- c(term, paste0(n$morpheme()$surface(), sep, n$morpheme()$feature()$head()))
          }
        }
      } else {
        if (annotate == FALSE) {
          term <- c(term, ns$morpheme()$surface())
        } else {
          term <- c(term, paste0(ns$morpheme()$surface(), sep, ns$morpheme()$feature()$head()))
        }
      }
    }
    Encoding(term) <- "UTF-8"
    ret[[i]] <- term
  }

  ret
}

# Deprecated functions
#
# morph <- function(text) {
#   check_input(text)
#   morph_text <- list()
#
#   for (i in seq(1, length(text), 1)) {
#     txt_orig <- iconv(text[i], to = "UTF-8")
#     s$txt <- txt_orig
#     morph_text[[i]] <- s %~% 'Analyzer.parse(txt).map(_.morpheme.surface).toArray'
#     Encoding(morph_text[[i]]) <- "UTF-8"
#   }
#
#   morph_text
# }
#
# deinflect <- function(text, sep = "/") {
#   check_input(text)
#   pos_text <- list()
#   s$sep <- sep
#
#   for (i in seq(1, length(text), 1)) {
#     text[i] <- iconv(text[i], to = "UTF-8")
#     s$txt <- text[i]
#
#     Encoding(pos_text[[i]]) <- "UTF-8"
#   }
#
#   pos_text
# }
#
# annotate <- function(text) {
#   check_input(text)
#   morph_text <- data.frame()
#
#   for (i in seq(1, length(text), 1)) {
#     text[i] <- iconv(text[i], to = "UTF-8")
#     s$txt <- text[i]
#     morphed <- s %~% 'Analyzer.parse(txt).map(_.morpheme.surface).toArray'
#     tag <- s %~% 'Analyzer.parse(txt).map(_morpheme.feature.head).toArray'
#     Encoding(morphed) <- "UTF-8"
#     morph_text <- rbind(morph_text, data.frame(sentence = text[i], sentence_id = i, token_id = seq(1, length(morphed), 1), token = morphed, tag = tag))
#   }
#
#   morph_text
# }
