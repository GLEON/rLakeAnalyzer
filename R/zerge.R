#' @export
#' @title zerge (was called zerge)
#' @description merge interval i and i+1
#' @param i [INTEGER] interval number of the first segment to merge
#' @param ni [INTEGER(NR)] current array with interval start point numbers

zerge = function(i,ni) {
  stopifnot(i > 1) # Can't merge the first interval
  stopifnot(i<length(ni)) # Must have an element at n[i+1]
  stopifnot(length(ni)>2) # Only one interval so can't merge
  c( ni[1:i-1], ni[(i+1):length(ni)] ) # Just removes element i
}
