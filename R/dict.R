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

  analyzer <- rJava::J("org.bitbucket.eunjeon.seunjeon.Analyzer")
  arrays <- rJava::J("java.util.Arrays")

  userDict <- arrays$asList(rJava::.jarray(dictWord))$iterator()
  analyzer$setUserDict(userDict)
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
  analyzer <- rJava::J("org.bitbucket.eunjeon.seunjeon.Analyzer")

  analyzer$resetUserDict
}
