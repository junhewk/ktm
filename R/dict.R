#' Setting user dictionary word
#'
#' Basic form of user dictionary word is "word" or "word, cost". `cost` means an appearance
#' cost, hence as the lower cost is given, the more the word will used for the morpheme analysis.
#' A compound noun can be declared by using `+` sign. For using `+` sign for the subcharactor
#' of the word, using `\\+` instead.
#'
#' @param dictWord a character vector of any length, with "word" or "word, cost" shape
#'
#' See examples in \href{https://github.com/junhewk/ktm}{Github}.
#'
#' @examples
#' \dontrun{
#' # a variable `word`` declared with Korean word
#' setUserDict(word)
#' }
#'
#' @import rJava
#' @export
set_userdict <- function(dictWord) {
  check_dictword(dictWord)

  seinterface <- rJava::.jnew("io/github/junhewk/ktm/SEInterface")
  rJava::.jcall(seinterface, "V", "setDict", .jarray(dictWord))
}

#' Resetting user dictionary
#'
#' For reset user dictionary, use `resetUserDict()`.
#'
#' @examples
#' \dontrun{
#' resetUserDict()
#' }
#'
#' @import rJava
#' @export
reset_userdict <- function() {
  seinterface <- rJava::.jnew("io/github/junhewk/ktm/SEInterface")
  rJava::.jcall(seinterface, "V", "resetDict")
}
