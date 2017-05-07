# Copyright (c) Konrad Grzanek
# Created: 2017-05-05

options(show.error.locations = TRUE)

chMessage <- function(x) {
  r <- paste(capture.output(str(x)), collapse = "\n")
  paste0(" ch(eck) failed on\n", r)
}

#' Generates a predicate that &s all the arguments
#' @export
predAnd <- function(...) {
  preds <- list(...)
  function(x) {
    for (p in preds) if (!p(x)) return (FALSE)
    TRUE
  }
}

#' Generates a predicate that |s all the arguments
#' @export
predOr <- function(...) {
  preds <- list(...)
  function(x) {
    for (p in preds) if (p(x)) return (TRUE)
    FALSE
  }
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
defch <- function(pred) {
  function(x, asPred = FALSE) ch(pred, x, asPred)
}

#' Tells if x is a scalar value
#' @export
is.scalar <- function(x) is.atomic(x) && length(x) == 1L

#' \code{is.scalar} ch(eck)
#' @export
chScalar <- defch(is.scalar)

#' \code{is.logical} ch(eck)
#' @export
chBools  <- defch(is.logical)

#' \code{is.scalar} & \code{is.logical} ch(eck)
#' @export
chBool   <- defch(predAnd(is.scalar, is.logical))

#' \code{is.integer} ch(eck)
#' @export
chInts <- defch(is.integer)

#' \code{is.scalar} & \code{is.integer} ch(eck)
#' @export
chInt  <- defch(predAnd(is.scalar, is.integer))

# is.integer(s)
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
