# ktm: Korean Text Mining toolkit

This package intends to offer a seamless analyzing environment for Korean text mining toolkit regardless of operational system. Since many natural language process packages for R are anglocentric, and R is highly sensitive to encoding of character vectors, there are consistent problems in Korean text analysis with R language. Therefore this package aims to develop a interface for such packages without encoding support, such as `tm` and `tidytext` packages.

This package utilizes [rJava](https://cran.r-project.org/package=rJava),  [dplyr](https://cran.r-project.org/package=dplyr), and [stringi](https:://cran.r-project.org/package=stringi). [rJava](https://cran.r-project.org/package=rJava) is used for utilizing scala interface to morpheme analyzing with [seunjeon](https://bitbucket.org/eunjeon/seunjeon).  [dplyr](https://cran.r-project.org/package=dplyr) gives tidy data set for further evalution of the data. [stringi](https:://cran.r-project.org/package=stringi) is for checking encoding and other string operations.

This package is based on two projects, one is [seunjeon](https://bitbucket.org/eunjeon/seunjeon) (by YH Yoo and YW Lee, Apache License 2.0) and other is [KOSAC](http://word.snu.ac.kr/kosac) sentiment lexicon (by Department of Linguistics, Seoul National University, South Korea).

Korean version of README is on the [github](https://github.com/junhewk/ktm/blob/master/README.rmd).

## Installation

```
# java jre should be installed properly and JAVA_HOME environment variable setting is needed
# install.packages("rJava")
install.packages("devtools")
# for Windows 64 bit, user must set the no multiarch option for the devtools package
options(devtools.install.args = "--no-multiarch") # only in windows 64 bit
devtools::install_github("junhewk/ktm")
```

***

## Example

### POS tagging: the `tagger` function

```
library(ktm)

melonLyrics <- "https://github.com/junhewk/melonchartlyrics/raw/master/melon_ranking_lyrics_1964-2016.csv"
lyricsData <- read.csv(melonLyrics, stringsAsFactors = FALSE) # loading Korean pop song lyrics data set based on the Melon chart

tagger(lyricsData$lyric[1:6]) # morphemes/tags
tagger(lyricsData$lyric[1:6], annotate = FALSE) # only morphemes
tagger(lyricsData$lyric[1:6], deinflect = FALSE) # original form of inflected morpheme
tagger(lyricsData$lyric[1:6], strip_number = TRUE, strip_punct = TRUE) # stripping numbers and punctuations
```

### Tokenizing: the `tokenizer` function

```
tokenizer(lyricsData$lyric[1:6]) # morphemes/tags in a tibble
tokenizer(lyricsData$lyric[1:6], annotate = FALSE) # only morphemes in a tibble
tokenizer(lyricsData$lyric[1:6], token = "tag") # morphemes and tags in separated columns in a tibble
tokenizer(lyricsData$lyric[1:6], token = "ngram", n = 4, n_min = 2, ngram_sep = " ") # bigram to quadgram morphemes in a tibble
```

### N-gram tokenizing: the `ngramer` function

```
ngramer(tagger(lyricsData$lyric[1:6])) # a list of morphemes/tags by `tagger` to n-gram (unigram to trigram)
ngramer(tagger(lyricsData$lyric[1:6]), n = 3, n_min = 2, ngram_sep = "+") # bigrams to trigrams with separation character "+"
```

### Sentiment score for Korean sentences: the `emotionalizer` function

```
emotionalizer(lyricsData$lyric[1:6]) # a representative sentiment and its fraction of each text in a tibble
emotionalizer(tokenizer(lyricsData$lyrics[1:6], token = "ngram", n = 3, n_min = 2, ngram_sep = ";")) # to get a sentiment result based on bigrams and trigrams
```

***

## Author

Junhewk Kim (junhewk.kim@gmail.com)
