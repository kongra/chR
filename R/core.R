# Copyright (c) Konrad Grzanek
# Created: 2017-05-05

#' @import stringr
#' @import purrr
#' @useDynLib chR
#' @importFrom Rcpp sourceCpp
NULL

# ESSENTIALS

errMessage <- function(x) {
  r <- paste(capture.output(str(x)), collapse = "\n")
  paste0(" ch(eck) failed on\n", r)
}

#' Executes a ch(eck) of pred on x
#' @export
ch <- function(pred, x, asPred = FALSE) {
  r <- pred(x)
  if (asPred) return(r)
  if (!r)     stop(errMessage(x))
  x
}

#' Returns a ch(eck) based on the pred
#' @export
chP <- function(pred) {
  function(x, asPred = FALSE) ch  (pred, x, asPred)
# function(x, asPred = FALSE) chLL(pred, x, asPred, errMessage)
}

#' Returns a \code{inherits(., cls)} ch(eck)
#' @export
chInstance <- function(class) chP(function(x) inherits(x, class))

#' Returns a ch(eck) that is a negation of the passed ch(eck)
#' @export
chNot <- function(c) chP(function(x) !c(x, asPred = TRUE))

#' Returns a ch(eck) that &s all the passed ch(eck)s
#' @export
chAnd <- function(...) {
  chs  <- list(...)
  chP(function(x) {
    for (c in chs) if (!c(x, asPred = TRUE)) return (FALSE)
    TRUE
  })
}

#' Returns a ch(eck) that |s all the passed ch(eck)s
#' @export
chOr <- function(...) {
  chs <- list(...)
  chP(function(x) {
    for (c in chs) if (c(x, asPred = TRUE)) return (TRUE)
    FALSE
  })
}

# ESSENTIAL CH(ECK)S

#' \code{is.null} ch(eck)
#' @export
chUnit <- chP(is.null)

#' Scalar \code{is.atomic} & \code{length == 1L} value ch(eck)
#' @export
chScalar <- chP(function(x) is.atomic(x) && length(x) == 1L)

#' \code{is.character} ch(eck)
#' @export
chStrings <- chP(is.character)

#' \code{chScalar} & \code{chStrings} ch(eck)
#' @export
chString <- chAnd(chScalar, chStrings)

#' \code{is.function} ch(eck)
#' @export
chFun <- chP(is.function)

# REGISTRY

CHSREG <- list()

#' Registeres the ch(eck) using an optional name (ch(eck) name by default)
#' @export
chReg <- function(ch, name = NA) { # BEWARE: THREAD UNSAFE
  if (is.na(name)) name <- as.character(substitute(ch))
  CHSREG[[as.character(name)]] <<- as.function(ch)
  NULL
}

chReg(chUnit)
chReg(chScalar)
chReg(chString)
chReg(chStrings)
chReg(chFun)

# REST OF THE CH(ECK)S

#' \code{!is.null} ch(eck)
#' @export
chSome <- chP(function(x) !is.null(x))
chReg(chSome)

#' \code{is.na} ch(eck)
#' @export
chNA <- chAnd(chScalar, chP(is.na))
chReg(chNA)

#' Either ch(eck) where the left and right types are expressed by checks
#' cl and cr.
#' @export
chEither <- function(cl, cr, x, asPred = FALSE) chOr(cl, cr)(x, asPred)

#' Maybe ch(eck)
#' @export
chMaybe <- function(c, x, asPred = FALSE) chEither(chUnit, c, x, asPred)

#' \code{is.logical} ch(eck)
#' @export
chBools <- chP(is.logical)
chReg(chBools)

#' \code{chScalar} & \code{chBools} ch(eck)
#' @export
chBool <- chAnd(chScalar, chBools)
chReg(chBool)

#' \code{is.integer} ch(eck)
#' @export
chInts <- chP(is.integer)
chReg(chInts)

#' \code{chScalar} & \code{chInts} ch(eck)
#' @export
chInt <- chAnd(chScalar, chInts)
chReg(chInt)

#' \code{is.double} ch(eck)
#' @export
chDoubles <- chP(is.double)
chReg(chDoubles)

#' \code{chScalar} & \code{chDoubles} ch(eck)
#' @export
chDouble <- chAnd(chScalar, chDoubles)
chReg(chDouble)

#' \code{is.complex} ch(eck)
#' @export
chComplexes <- chP(is.complex)
chReg(chComplexes)

#' \code{chScalar} & \code{chComplexes} ch(eck)
#' @export
chComplex <- chAnd(chScalar, chComplexes)
chReg(chComplex)

#' \code{is.numeric} ch(eck)
#' @export
chNumerics <- chP(is.numeric)
chReg(chNumerics)

#' \code{chScalar} & \code{chNumerics} ch(eck)
#' @export
chNumeric  <- chAnd(chScalar, chNumerics)
chReg(chNumeric)

#' \code{chInts} & > 0 ch(eck)
#' @export
chPosInts <- chAnd(chInts, chP(arePosInts))
chReg(chPosInts)

#' \code{chInt} & > 0 ch(eck)
#' @export
chPosInt <- chAnd(chInt, chP(arePosInts))
chReg(chPosInt)

#' \code{chDoubles} & > 0 ch(eck)
#' @export
chPosDoubles <- chAnd(chDoubles, chP(arePosDoubles))
chReg(chPosDoubles)

#' \code{chDouble} & > 0 ch(eck)
#' @export
chPosDouble <- chAnd(chDouble, chP(arePosDoubles))
chReg(chPosDouble)

#' \code{chInts} & < 0 ch(eck)
#' @export
chNegInts <- chAnd(chInts, chP(areNegInts))
chReg(chNegInts)

#' \code{chInt} & < 0 ch(eck)
#' @export
chNegInt <- chAnd(chInt, chP(areNegInts))
chReg(chNegInt)

#' \code{chDoubles} & < 0 ch(eck)
#' @export
chNegDoubles <- chAnd(chDoubles, chP(areNegDoubles))
chReg(chNegDoubles)

#' \code{chDouble} & < 0 ch(eck)
#' @export
chNegDouble <- chAnd(chDouble, chP(areNegDoubles))
chReg(chNegDouble)

#' \code{chInts} & >= 0 ch(eck)
#' @export
chNatInts <- chAnd(chInts, chP(areNatInts))
chReg(chNatInts)

#' \code{chInt} & >= 0 ch(eck)
#' @export
chNatInt <- chAnd(chInt, chP(areNatInts))
chReg(chNatInt)

#' \code{chDoubles} & >= 0 ch(eck)
#' @export
chNonNegDoubles <- chAnd(chDoubles, chP(areNonNegDoubles))
chReg(chNonNegDoubles)

#' \code{chDouble} & >= 0 ch(eck)
#' @export
chNonNegDouble <- chAnd(chDouble, chP(areNonNegDoubles))
chReg(chNonNegDouble)

#' \code{chInt} & even? check
#' @export
chEvenInt <- chAnd(chInt, chP(function (x) x %% 2L == 0L))
chReg(chEvenInt)

#' \code{chInt} & odd? check
#' @export
chOddInt <- chAnd(chInt, chP(function (x) x %% 2L != 0L))
chReg(chOddInt)

#' \code{is.list} ch(eck)
#' @export
chList <- chP(is.list)
chReg(chList)

#' \code{is.vector} ch(eck)
#' @export
chVector <- chP(is.vector)
chReg(chVector)

#' \code{is.factor} ch(eck)
#' @export
chFactor <- chP(is.factor)
chReg(chFactor)

#' \code{is.data.frame} ch(eck)
#' @export
chDF <- chP(is.data.frame)
chReg(chDF)

#' \code{data.table::is.data.table} ch(eck)
#' @export
chDT <- chP(data.table::is.data.table)
chReg(chDT)

#' Returns a check for the data.table having exactly n rows
#' @export
chDTn <- function(n) {
  chNatInt(n)
  chAnd(chDT, chP(function(dt) nrow(dt) == n))
}

#' Returns a check for the data.table having exactly 0 or n rows
#' @export
chDT0n <- function(n) {
  chNatInt(n)
  chAnd(chDT, chP(function(dt) {
    nr <- nrow(dt)
    nr == 0L || nr == n
  }))
}

#' Ch(eck) for an empty data.table
#' @export
chDT0 <- NULL
delayedAssign("chDT0", chDTn(0L))

#' Ch(eck) for a single-row data.table
#' @export
chDT1 <- NULL
delayedAssign("chDT1", chDTn(1L))

#' Ch(eck) for an empty or single-row data.table
#' @export
chDT01 <- NULL
delayedAssign("chDT01", chDT0n(1L))

#' \code{ggplot2::is.ggplot} ch(eck)
#' @export
chGgplot <- chP(ggplot2::is.ggplot)
chReg(chGgplot)

#' \code{tibble::is.tibble} ch(eck)
#' @export
chTibble <- chP(tibble::is.tibble)
chReg(chTibble)

#' \code{is.array} ch(eck)
#' @export
chArray <- chP(is.array)
chReg(chArray)

#' \code{is.atomic} ch(eck)
#' @export
chAtomic <- chP(is.atomic)
chReg(chAtomic)

#' \code{is.recursive} ch(eck)
#' @export
chRecursive <- chP(is.recursive)
chReg(chRecursive)

#' \code{is.object} ch(eck)
#' @export
chObject <- chP(is.object)
chReg(chObject)

#' \code{is.matrix} ch(eck)
#' @export
chMatrix <- chP(is.matrix)
chReg(chMatrix)

#' \code{is.table} ch(eck)
#' @export
chTable <- chP(is.table)
chReg(chTable)

#' \code{is.environment} ch(eck)
#' @export
chEnv <- chP(is.environment)
chReg(chEnv)

#' \code{is.call} ch(eck)
#' @export
chCall <- chP(is.call)
chReg(chCall)

#' \code{is.expression} ch(eck)
#' @export
chExpr <- chP(is.expression)
chReg(chExpr)

#' \code{is.symbol} ch(eck)
#' @export
chSymbol <- chP(is.symbol)
chReg(chSymbol)

#' \code{s == ""} ch(eck) for String, deliberately not chReg-ed
#' @export
chEmptyString <- chAnd(chString, chP(function(s) s == ""))

#' \code{s != ""} ch(eck) for String, deliberately not chReg-ed
#' @export
chNonEmptyString <- chNot(chEmptyString)

#' Blank-ness ch(eck) for String, deliberately not chReg-ed
#' @export
chBlank <- chAnd(chString, chP(function(s) is.na(readr::parse_character(s))))

#' Non blank-ness ch(eck) for String, deliberately not chReg-ed
#' @export
chNonBlank <- chNot(chBlank)

#' \code{lubridate::is.Date} ch(eck)
#' @export
chDates <- chP(lubridate::is.Date)
chReg(chDates)

#' \code{chScalar} & \code{chDates} ch(eck)
#' @export
chDate  <- chAnd(chScalar, chDates)
chReg(chDate)

# QUERYING THE REGISTRY

#' Returns a vector of ch(eck)s names that x passes
#' @export
chs <- function(x) chStrings ({
  CHSREG %>% keep(~ .x(x, asPred = TRUE)) %>% names() %>% sort()
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

# TAGGED (CONSTRAINED) VALUES

#' Returns x tagged with class=tag
#' @export
tagged <- function(x, tag) {
  structure(list(value = x), class = tag)
}
