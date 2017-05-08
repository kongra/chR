# Copyright (c) Konrad Grzanek
# Created: 2017-05-05

#' @import data.table
#' @import ggplot2
#' @import tibble
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
  function(x, asPred = FALSE) ch(pred, x, asPred)
}

#' Returns a \code{inherits(., cls)} ch(eck)
#' @export
chInstance <- function(class) chP(function(x) inherits(x, class))

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

# CH(ECK)S

#' \code{is.null} ch(eck)
#' @export
chUnit <- chP(is.null)

#' \code{!is.null} ch(eck)
#' @export
chSome <- chP(function(x) !is.null(x))

#' \code{is.na} ch(eck)
#' @export
chNA <- chP(is.na)

#' Either ch(eck) where the left and right types are expressed by checks
#' cl and cr.
#' @export
chEither <- function(cl, cr, x, asPred = FALSE) chOr(cl, cr)(x, asPred)

#' Maybe ch(eck)
#' @export
chMaybe <- function(c, x, asPred = FALSE) chEither(chUnit, c, x, asPred)

#' Scalar \code{is.atomic} & \code{length == 1L} value ch(eck)
#' @export
chScalar <- chP(function(x) is.atomic(x) && length(x) == 1L)

#' \code{is.logical} ch(eck)
#' @export
chBools <- chP(is.logical)

#' \code{chScalar} & \code{chBools} ch(eck)
#' @export
chBool <- chAnd(chScalar, chBools)

#' \code{is.integer} ch(eck)
#' @export
chInts <- chP(is.integer)

#' \code{chScalar} & \code{chInts} ch(eck)
#' @export
chInt <- chAnd(chScalar, chInts)

#' \code{is.double} ch(eck)
#' @export
chDoubles <- chP(is.double)

#' \code{chScalar} & \code{chDoubles} ch(eck)
#' @export
chDouble <- chAnd(chScalar, chDoubles)

#' \code{is.complex} ch(eck)
#' @export
chComplexes <- chP(is.complex)

#' \code{chScalar} & \code{chComplexes} ch(eck)
#' @export
chComplex <- chAnd(chScalar, chComplexes)

#' \code{is.numeric} ch(eck)
#' @export
chNumerics <- chP(is.numeric)

#' \code{chScalar} & \code{chNumerics} ch(eck)
#' @export
chNumeric  <- chAnd(chScalar, chNumerics)

#' \code{is.infinite} ch(eck)
#' @export
chInfinites <- chP(is.infinite)

#' \code{chScalar} & \code{chInfinites} ch(eck)
#' @export
chInfinite  <- chAnd(chScalar, chInfinites)

#' \code{is.Finite} ch(eck)
#' @export
chFinites <- chP(is.finite)

#' \code{chScalar} & \code{chFinites} ch(eck)
#' @export
chFinite  <- chAnd(chScalar, chFinites)

#' \code{chInt} & > 0 ch(eck)
#' @export
chPosInt <- chAnd(chInt, chP(function (x) x > 0))

#' \code{chInt} & >= 0 ch(eck)
#' @export
chNatInt <- chAnd(chInt, chP(function (x) x >= 0))

#' \code{is.character} ch(eck)
#' @export
chStrings <- chP(is.character)

#' \code{chScalar} & \code{chStrings} ch(eck)
#' @export
chString <- chAnd(chScalar, chStrings)

#' \code{is.list} ch(eck)
#' @export
chList <- chP(is.list)

#' \code{is.vector} ch(eck)
#' @export
chVector <- chP(is.vector)

#' \code{is.factor} ch(eck)
#' @export
chFactor <- chP(is.factor)

#' \code{is.data.frame} ch(eck)
#' @export
chDF <- chP(is.data.frame)

#' \code{data.table::is.data.table} ch(eck)
#' @export
chDT <- chP(data.table::is.data.table)

#' \code{ggplot2::is.ggplot} ch(eck)
#' @export
chGgplot <- chP(ggplot2::is.ggplot)

#' \code{tibble::is.tibble} ch(eck)
#' @export
chTibble <- chP(tibble::is.tibble)

#' \code{is.array} ch(eck)
#' @export
chArray <- chP(is.array)

#' \code{is.atomic} ch(eck)
#' @export
chAtomic <- chP(is.atomic)

#' \code{is.function} ch(eck)
#' @export
chFun <- chP(is.function)

#' \code{is.matrix} ch(eck)
#' @export
chMatrix <- chP(is.matrix)

#' \code{is.table} ch(eck)
#' @export
chTable <- chP(is.table)

#' \code{is.environment} ch(eck)
#' @export
chEnv <- chP(is.environment)

#' \code{is.call} ch(eck)
#' @export
chCall <- chP(is.call)

#' \code{is.expression} ch(eck)
#' @export
chExpr <- chP(is.expression)

#' \code{is.symbol} ch(eck)
#' @export
chSymbol <- chP(is.symbol)

# TAGGED (CONSTRAINED) VALUES

tagged <- function(class, x) {
  structure(list(value = x), class = class)
}
