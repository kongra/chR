# Copyright (c) Konrad Grzanek
# Created:     2017-05-05
# Re-designed: 2018-04-24

#' @import stringr
#' @import purrr
#' @import readr
#' @useDynLib chR
#' @importFrom Rcpp sourceCpp
NULL

# ESSENTIALS
errMessage <- function(x) {
  r <- paste(capture.output(str(x)), collapse = "\n")
  paste0(" ch(eck) failed on\n", r, "\nof type ", typeof(x),
         " and class ", class(x))
}

#' @export
chP <- function(expr, arg = x) {
  arg <- substitute(arg)

  substitute({
    function(`<arg>`) {
      if (!(expr)) stop(errMessage(`<arg>`))
      `<arg>`
    }}) %>%
    deparse %>%
    str_replace_all("<arg>", as.character(arg)) %>%
    parse(text = .) %>%
    eval %>% compiler::cmpfun(f = .)
}

#' @export
chP1 <- function(expr, arg = x) {
  substitute(chP(expr && length(arg) == 1L, arg = arg)) %>% eval
}

#' @export
chStrings <- chP(is.character(x))
#' @export
chString  <- chP1(is.character(x))

#' @export
chInstance <- function(class) {
  chString(class)
  substitute(chP(inherits(x, class))) %>% eval
}

#' @export
chInstance1 <- function(class) {
  chString(class)
  substitute(chP(inherits(x, class) && length(x) == 1L)) %>% eval
}

# ESSENTIAL CH(ECK)S

#' @export
chUnit <- chP(is.null(x))

#' @export
chScalar <- chP1(is.atomic(x))

#' @export
chFun <- chP(is.function(x))

# REGISTRY

CHSREG <- list()

#' Registeres the ch(eck) using an optional name (ch(eck) name by default)
#' @export
chReg <- function(ch, name = NA) { # BEWARE: THREAD UNSAFE
  if (is.na(name)) name <- as.character(substitute(ch))
  CHSREG[[as.character(name)]] <<- as.function(ch)
  NULL
}

chReg(chStrings)
chReg(chString)
chReg(chUnit)
chReg(chScalar)
chReg(chFun)

# REST OF THE CH(ECK)S

#' @export
chSome <- chP(!is.null(x))
chReg(chSome)

#' @export
chMaybe <- function(check, x) if (is.null(x)) NULL else check(x)

#' @export
chBools <- chP(is.logical(x))
chReg(chBools)

#' @export
chBool <- chP1(is.logical(x))
chReg(chBool)

#' @export
chInts <- chP(is.integer(x))
chReg(chInts)

#' @export
chInt <- chP1(is.integer(x))
chReg(chInt)

#' @export
chDoubles <- chP(is.double(x))
chReg(chDoubles)

#' @export
chDouble <- chP1(is.double(x))
chReg(chDouble)

#' @export
chComplexes <- chP(is.complex(x))
chReg(chComplexes)

#' @export
chComplex <- chP1(is.complex(x))
chReg(chComplex)

#' @export
chNumerics <- chP(is.numeric(x))
chReg(chNumerics)

#' @export
chNumeric <- chP1(is.numeric(x))
chReg(chNumeric)

#' @export
chPosInts <- chP(is.integer(x) && arePosInts(x))
chReg(chPosInts)

#' @export
chPosInt <- chP1(is.integer(x) && arePosInts(x))
chReg(chPosInt)

#' @export
chPosNAInts <- chP(is.integer(x) && arePosIntsOrNAs(x))
chReg(chPosNAInts)

#' @export
chPosNAInt <- chP1(is.integer(x) && arePosIntsOrNAs(x))
chReg(chPosNAInt)

#' @export
chPosDoubles <- chP(is.double(x) && arePosDoubles(x))
chReg(chPosDoubles)

#' @export
chPosDouble <- chP1(is.double(x) && arePosDoubles(x))
chReg(chPosDouble)

#' @export
chNegInts <- chP(is.integer(x) && areNegInts(x))
chReg(chNegInts)

#' @export
chNegInt <- chP1(is.integer(x) && areNegInts(x))
chReg(chNegInt)

#' @export
chNegDoubles <- chP(is.double(x) && areNegDoubles(x))
chReg(chNegDoubles)

#' @export
chNegDouble <- chP1(is.double(x) && areNegDoubles(x))
chReg(chNegDouble)

#' @export
chNatInts <- chP(is.integer(x) && areNatInts(x))
chReg(chNatInts)

#' @export
chNatInt <- chP1(is.integer(x) && areNatInts(x))
chReg(chNatInt)

#' @export
chNonNegDoubles <- chP(is.double(x) && areNonNegDoubles(x))
chReg(chNonNegDoubles)

#' @export
chNonNegDouble <- chP1(is.double(x) && areNonNegDoubles(x))
chReg(chNonNegDouble)

#' @export
chEvenInt <- chP(is.integer(x) && length(x) == 1L && x %% 2L == 0L)
chReg(chEvenInt)

#' @export
chOddInt <- chP(is.integer(x) && length(x) == 1L && x %% 2L != 0L)
chReg(chOddInt)

#' @export
chList <- chP(is.list(x))
chReg(chList)

#' @export
chVector <- chP(is.vector(x))
chReg(chVector)

#' @export
chFactors <- chP(is.factor(x))
chReg(chFactors)

#' @export
chFactor <- chP1(is.factor(x))
chReg(chFactor)

#' @export
chDF <- chInstance("data.frame")
chReg(chDF)

#' @export
chDT <- chInstance("data.table")
chReg(chDT)

#' @export
chDTn <- function(n) {
  # chNatInt(n)
  substitute(chP(inherits(x, "data.table") && nrow(x) == n)) %>% eval
}

#' @export
chDT0n <- function(n) {
  # chPosInt(n)
  substitute(chP(inherits(x, "data.table") && {
    i <- nrow(x)
    i == 0L || i == n
  })) %>% eval
}

#' @export
chDT0 <- chDTn(0L)

#' @export
chDT1 <- chDTn(1L)

#' @export
chDT01 <- chDT0n(1L)

#' @export
chGgplot <- chP(ggplot2::is.ggplot(x))
chReg(chGgplot)

#' @export
chTibble <- chP(tibble::is.tibble(x)) # Consider chInstance
chReg(chTibble)

#' @export
chArray <- chP(is.array(x))
chReg(chArray)

#' @export
chAtomic <- chP(is.atomic(x))
chReg(chAtomic)

#' @export
chRecursive <- chP(is.recursive(x))
chReg(chRecursive)

#' @export
chObject <- chP(is.object(x))
chReg(chObject)

#' @export
chMatrix <- chP(is.matrix(x))
chReg(chMatrix)

#' @export
chTable <- chP(is.table(x))
chReg(chTable)

#' @export
chEnv <- chP(is.environment(x))
chReg(chEnv)

#' @export
chCall <- chP(is.call(x))
chReg(chCall)

#' @export
chExpr <- chP(is.expression(x))
chReg(chExpr)

#' @export
chSymbol <- chP(is.symbol(x))
chReg(chSymbol)

#' @export
chDates <- chInstance("Date")
chReg(chDates)

#' @export
chDate  <- chInstance1("Date")
chReg(chDate)

#' @export
chNonBlank <- chP(is.character(x) && length(x) == 1L && !is.na(parse_character(x)))
chReg(chNonBlank)

# QUERYING THE REGISTRY

asPred <- function(check, x) chBool({
  tryCatch({
    check(x)
    TRUE
  }, error = function(e) FALSE)
})

#' Returns a vector of ch(eck)s names that x passes
#' @export
chs <- function(x) chStrings ({
  CHSREG %>% keep(function(check) asPred(check, x)) %>% names %>% sort
})

#' Returns a vector of ch(eck)s names passed by all the arguments
#' @export
chsAll <- function(...) chStrings ({
  list(...) %>% map(~ chs(.x)) %>% reduce(intersect)
})

#' Returns a vector of ch(eck)s names passed by the first argument, but not
#' passed by the rest of the args
#' @export
chsDiff <- function(...) chStrings ({
  list(...) %>% map(~ chs(.x)) %>% reduce(setdiff)
})
