# ktm: Korean Text Mining toolkit

This package intends to offer a seamless analyzing environment for Korean text mining toolkit regardless of operational system. Since many natural language process packages for R are anglocentric, and R is highly sensitive to encoding of character vectors, there are consistent problems in Korean text analysis with R language. Therefore this package aims to develop a interface for such packages without encoding support, such as `tm` and `tidytext` packages.

This package is based on [rJava](https://cran.r-project.org/package=rJava),  [dplyr](https://cran.r-project.org/package=dplyr), and [stringi](https:://cran.r-project.org/package=stringi). [rJava](https://cran.r-project.org/package=rJava) is used for utilizing scala interface to morpheme analyzing with [seunjeon](https://bitbucket.org/eunjeon/seunjeon).  [dplyr](https://cran.r-project.org/package=dplyr) gives tidy data set for further evalution of the data. [stringi](https:://cran.r-project.org/package=stringi) is for checking encoding and other string operations.

This package is based on two projects, one is [seunjeon](https://bitbucket.org/eunjeon/seunjeon) (by YH Yoo and YW Lee, Apache License 2.0) and other is [KOSAC](http://word.snu.ac.kr/kosac) sentiment lexicon (by Department of Linguistics, Seoul National University, South Korea).

Korean version of README is on the [github](https://github.com/junhewk/ktm/blob/master/README.rmd).

## Installation

```
# java jre should be installed properly and JAVA_HOME environment variable setting is needed
# install.packages("rJava")
install.packages("devtools")
devtools::install_github("junhewk/ktm")
```

## Example

### POS tagging: the `tagger` function

### Tokenizing: the `tokenizer` function

### N-gram tokenizing: the `ngramer` function

### Sentiment score for Korean sentences: the `emotionalizer` function

## Author

Junhewk Kim (junhewk.kim@gmail.com)
