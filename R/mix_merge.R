
#' @title mix_merge (was called zerge)
#' @description merge interval i and i+1
#' @param i [INTEGER] interval number of the first segment to merge
#' @param nr [INTEGER] current maximum of the interval number
#' @param ni [INTEGER(NR)] current array with interval start point numbers
#' @return 'nn- [INTEGER(NR)] new array with interval start point numbers


mix_merge = function(i,nr,ni) {
  nn = ni
  nn[i:nr] = ni[(i+1):(nr+1)]
  nn
}
