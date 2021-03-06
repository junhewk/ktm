% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/tokenizer.R
\name{tokenizer}
\alias{tokenizer}
\title{Tokenizing Korean text to morpheme}
\usage{
tokenizer(corpus, token = c("word", "ngram", "tag"), annotate = TRUE,
  sep = "/", deinflect = FALSE, strip_punct = TRUE, strip_number = TRUE,
  n = 3, n_min = n, ngram_sep = ";")
}
\arguments{
\item{corpus}{a character vector of any length or a list of characters or list of character vectors of length 1}

\item{token}{unit for tokenizing, options are "word" (default), "ngram", "tag". "word" and "ngram" will return
a double column tibble (token and text id), and "tag" will return a tripple column tibble (token, tag, and text id).}

\item{annotate}{if FALSE (default), the function will return morphemes only. If FALSE, the function will return POS tagging also.}

\item{sep}{a character string to separate between token and tag}

\item{deinflect}{if FALSE (default), the function will return the morpheme with inflected forms. If TRUE, morphemes will have its original form.}

\item{strip_punct}{bool, if you want to preserve punctuations in the phrase, set this as FALSE}

\item{strip_number}{bool, if you want to preserve numerics in the phrase, set this as FALSE}

\item{n}{the maximum number of words in n-gram}

\item{n_min}{the minimun number of words in n-gram}

\item{ngram_sep}{a character string to separate n-gram}
}
\value{
a tibble with text ids, morphemes (and POS tag when token == "tag")

See examples in \href{https://github.com/junhewk/ktm}{Github}.
}
\description{
Tokenizing Korean texts to morpheme with or without POS tagging based on `Seunjeon`
morpheme analyzer and `Rscala` scala interface.
}
\examples{
\dontrun{
# a variable textKor declared with Korean text
# get a tibble with morpheme tokens, POS tag in a single column and text ids
tokenizer(textKor)

# get a tibble with morpheme tokens without POS tag and text ids
tokenizer(textKor, annotate = FALSE)

# get a tibble with morpheme tokens, POS tag, and text ids in separate columns
tokenizer(textKor, token = "tag")

# get a tibble with trigram tokens and text ids
tokenizer(textKor, token = "ngram")

# get a tibble with bigram and trigram tokens and text ids
tokenizer(textKor, token = "ngram", n = 3, n_min = 2)
}

}
