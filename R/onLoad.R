.onLoad <- function(libname, pkgname) {
  # this code is from HW Jeon's KoNLP package.
  # Seunjeon needs more memory heap than Hannanum.
  # If it's available, 2G of memory heap is recommended.

  baseOpt <- c("-Xmx2G", "-Dfile.encoding=UTF-8")
  limitOpt <- c("-Xmx1G", "-Dfile.encoding=UTF-8")
  jOpt <- getOption("java.parameters")

  if (is.null(jOpt)) {
    options(java.parameters = baseOpt)
  } else {
    if (rJava::.jniInitialized && !any(grepl("-Dfile\\.encoding=UTF-8", jOpt, ignore.case = TRUE))) {
      stop("rJava couldn't be parsed with UTF-8. Please reload the package.")
    }

    memJOpt <- jOpt[which(grepl("^-Xmx", jOpt))]

    if (nchar(memJOpt) > 0) {
      memSize <- tolower(gsub("^-Xmx", "", memJOpt))
      memUnit <- gsub("[[:digit:]]", "", memSize)
      memVal <- gsub("[m|g]$", "", memSize)

      if (memUnit == "g") {
        memSizeM <- as.numeric(memVal) * 1024
      } else {
        memSizeM <- as.numeric(memVal)
      }

      if (memSizeM < 1024) {
        options(java.parameters = limitOpt)
      } else {
        options(java.parameters = c(memJOpt, baseOpt[2]))
      }
    } else {
      options(java.parameters = c(jOpt, baseOpt))
    }
  }
  rJava::.jpackage(pkgname, lib.loc = libname)
}

#' @importFrom utils localeToCharset
.onAttach <- function(libname, pkgname) {
  if (all((utils::localeToCharset()[1] == c("UTF-8", "CP949", "EUC-KR")) == FALSE)) {
    packageStartupMessage("This R console doesn't support any Korean encodings.")
  }
}
