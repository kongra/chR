# Copyright (c) Konrad Grzanek
# Created: 2017-05-05

chMessage <- function(x, checkName) {
  r <- paste(capture.output(str(x)), collapse = "\n")
  paste0("The following object violated ch(eck) ", checkName, "\n", r)
}

#' Tells if x is a scalar value
#' @export
isScalar <- function(x) is.atomic(x) && length(x) == 1L
