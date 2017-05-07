# Copyright (c) Konrad Grzanek
# Created: 2017-05-05

options(show.error.locations = TRUE)

chMessage <- function(x) {
  r <- paste(capture.output(str(x)), collapse = "\n")
  paste0(" ch(eck) failed on\n", r)
}

#' Executes a ch(eck) of pred on x
#' @export
ch <- function(pred, x, asPred = FALSE) {
  r <- pred(x)
  if (asPred) return(r)
  if (!r)     stop(chMessage(x))
  x
}

#' Returns a ch(eck) based on the pred
#' @export
chP <- function(pred) {
  function(x, asPred = FALSE) ch(pred, x, asPred)
}

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
predOr <- function(...) {
  chs <- list(...)
  chP(function(x) {
    for (c in chs) if (c(x, asPred = TRUE)) return (TRUE)
    FALSE
  })
}

#' Scalar \code{is.atomic} & \code{length == 1L} value ch(eck)
#' @export
chScalar <- chP(function(x) is.atomic(x) && length(x) == 1L)

#' \code{is.logical} ch(eck)
#' @export
chBools  <- chP(is.logical)

#' \code{chScalar} & \code{chBools} ch(eck)
#' @export
chBool <- chAnd(chScalar, chBools)

#' \code{is.integer} ch(eck)
#' @export
chInts <- chP(is.integer)

#' \code{chScalar} & \code{chInts} ch(eck)
#' @export
chInt  <- chAnd(chScalar, chInts)

# is.double(s)
# is.numeric(s)

# is.nan(s)
# is.infinite(s)
# is.finite(s)

# natural int(s)
# positive int(s)

# is.character (isString isStrings)

# list
# vector
# data.frame
# data.table

# chUnit  (chNULL)
# chUnits (chNULLs)

# chSome
# chSomes not null

# chNo
# chNos null | na | nan

# chNA
# chNAs

# chMaybe
# chEither

# is.array
# is.atomic
# is.call
# is.complex
# is.environment
# is.expression
# is.factor
# is.function
# is.matrix
# is.symbol
# is.table

# CUSTOM TAGS AND THEIR ch(eck)s
