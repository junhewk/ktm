check_input <- function(x) {
  check_character <- is.character(x) |
    if (is.list(x)) {
      check_list <- all(vapply(x, is.character, logical(1))) &
        all(vapply(x, length, integer(1)) == 1L)
    } else {
      check_list <- FALSE
    }
  if (!(check_character | check_list))
    stop("Input must be a character vector of any length or a list of character\n",
         "  vectors, each of which has a length of 1.")
}

check_dictword <- function(x) {
  if (!is.character(x))
    stop("Input must be a character vector of any length")
}

enc_preprocess <- function(x) {
  if (!Encoding(x) == "UTF-8") {
    warning("ktm suppose the character vector's encoding is EUC-KR. Input will be coerced the encoding to UTF-8.")
    x <- iconv(x, from = "EUC-KR", to = "UTF-8")
    x
  } else {
    x
  }
}
