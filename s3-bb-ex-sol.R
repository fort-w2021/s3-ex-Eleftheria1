# constructor for a new bb object
new_bb <- function(text) {
  if (!is.character(text) & !is.factor(text)) {
    stop("Input must be a character vector/ array or factor.")
  }
  match <- "([^aeiouäöüAEIOUÄÜÖ]*)([aeiouäöü]{1,2})([^aeiouäöü]*)"
  gsub(pattern = match, replacement = "\\1\\2b\\2\\3", x = text)
}

# bb generic function
bb <- function(x) {
  UseMethod("bb")
}

# default method for bb (for character vectors/ matrices and arrays)
bb.default <- function(x) {
  if (!is.character(x)) {
    stop("Input must be a character vector/ array,
         factor or a list of character vectors/ arrays.")
  }
  x_new <- new_bb(x)
  structure(x_new,
    class = c("bb", class(x))
  )
}

# bb method for factors
bb.factor <- function(x) {
  if (!is.factor(x)) {
    stop("Input must be a character vector/ array,
         factor or a list of character vectors/ arrays.")
  }
  factorlevels <- levels(x)
  is_ordered <- is.ordered(x)
  x_new <- new_bb(x)
  x_new <- factor(x_new,
    levels = new_bb(factorlevels),
    ordered = is_ordered
  )
  structure(x_new,
    class = c("bb", class(x))
  )
}

# bb method for lists (recursive)
bb.list <- function(x) {
  x_new <- lapply(x, bb)
  structure(x_new,
    class = c("bb", class(x))
  )
}
