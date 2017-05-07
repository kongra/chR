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

#' isScalar ch(eck)
#' @export
chScalar <- defch(is.scalar)

#' is.logical ch(eck)
#' @export
chBools  <- defch(is.logical)

#' isScalar & is.logical ch(eck)
#' @export
chBool   <- defch(chAnd(is.scalar, is.logical))

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

# chNA
# chNAs

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
