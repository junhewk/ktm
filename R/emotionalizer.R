#' Calculating sentiment scores of the texts based on the `KOSAC` polarity dataset
#'
#' If input data is character vectors or a list of a character vector, this function will make
#' 1 to 3 n-grams with `Seunjeon` morpheme analyzer and gathering the
#' `KOSAC` polarity dataset to calculating sentiment scores of the texts. If input data is
#' a tibble made by `tokenizer` function, then this function will combine the `KOSAC` polarity
#' dataset to the input data. The result has the `tibble` format, with text, text id, and each text's
#' sentiment or conjoined tibble with input token data frame and polarity data set.
#' Text's sentiment is determined based on the `KOSAC` classification of positive (POS),
#' negative (NEG), neutral (NEUT), and complex (COMP).
#'
#' @param corpus a character vector of any length or a list of characters for sentiment analysis,
#' or a tibble made from `tokenizer(tag = "ngram")`
#' @param rate in "proportion", the proportion of the largest sentiment will be returned in the `prop` column,
#' in "number", the number of the largest sentiment will be returned in the `n` column. this is
#' meaningful when `corpus` is a character vector or a list
#' @return a tibble with input texts, text ids, and sentiments
#'
#' See examples in \href{https://github.com/junhewk/ktm}{Github}.
#'
#' @examples
#' \dontrun{
#' # a variable textKor declared with Korean text
#' emotionalizer(textKor)
#'
#' # or
#' textTibble <- tokenizer(textKor, token = "ngram", n = 2, n_min = 1)
#' emotionalizer(textTibble)
#' }
#' @import dplyr
#' @import tibble
#' @importFrom stats na.omit
#' @export
emotionalizer <- function(corpus, rate = c("proportion", "number")) {
  UseMethod("emotionalizer")
}

#' @export
emotionalizer.default <- function(corpus, rate = c("proportion", "number")) {
  check_input(corpus)

  rate <- match.arg(rate)

  corpus <- enc_preprocess(corpus)

  corpus <- gsub("[[:punct:]]", " ", corpus)
  corpus <- gsub("[0-9]", "", corpus)
  corpus <- gsub("[A-Za-z]", "", corpus)

  seinterface <- rJava::.jnew("io/github/junhewk/ktm/SEInterface")

  termList <- lapply(corpus, function(x) {
    result <- tryCatch(rJava::.jcall(seinterface, "[S", "unpackedTaggerSep", x, "/"),
                       error = function(e) {
                         warning(sprintf("'%s' can't be processed.\n", x))
                         character(0)
                       })
    Encoding(result) <- "UTF-8"
    result})

  names(termList) <- seq_along(termList)

  termList <- ngramer(termList, n = 3, n_min = 1, ngram_sep = ";")
  ngramDf <- tibble(ngram = unlist(termList), text_id = rep(seq_along(termList), lengths(termList)))
  ngramDf <- dplyr::left_join(ngramDf, dplyr::as_tibble(polarity))

  ngramDf <- dplyr::group_by(ngramDf, text_id)
  ngramDf <- stats::na.omit(ngramDf)
  ngramDf <- dplyr::count(ngramDf, max.value)
  if (rate == "proportion") ngramDf <- dplyr::mutate(ngramDf, prop = prop.table(n))
  ngramDf <- dplyr::filter(ngramDf, n == max(n))
  ngramDf <- ngramDf[!duplicated(ngramDf$text_id), ]
  if (rate == "proportion") ngramDf <- dplyr::select(ngramDf, -n)

  ret <- dplyr::left_join(tibble::tibble(text = corpus, text_id = seq_along(corpus)), ngramDf)

  ret
}

#' @export
emotionalizer.tbl_df <- function(corpus, rate = c("proportion", "number")) {
  first_col_name <- colnames(corpus)[1]

  if (!(first_col_name %in% c("word", "ngram"))) {
    stop("The input tibble should have the name word or ngram as its first column.")
  }

  colnames(corpus)[1] <- "ngram"

  # merge the polarity data frame
  ret <- dplyr::left_join(corpus, dplyr::select(dplyr::as_tibble(polarity), ngram, max.value, max.prop))

  colnames(ret)[1] <- first_col_name

  # return the data frame as defined
  ret
}
