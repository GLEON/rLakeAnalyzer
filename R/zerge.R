#' @export
#' @title zerge (was called zerge)
#' @description merge interval i and i+1
#' @param i [INTEGER] interval number of the first segment to merge
#' @param ni [INTEGER(NR)] current array with interval start point numbers

zerge = function(i,ni) {
  stopifnot(i<length(ni)-1) # Must have an element at n[i+2]
  stopifnot(length(ni)>2)
  c( ni[1:i], ni[(i+2):length(ni)] )
}
