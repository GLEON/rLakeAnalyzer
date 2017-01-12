
#SUBROUTINE spl(i,Nr,Ni)
#INTEGER NR,NI(nr+2)
#
#' @export
#' @title spliting interval i into 2 pieces
#' @param i [INTEGER] interval number, should be less than NR+1
#' @param ni [INTEGER(NR)] current array with interval start point number
#'
#' @return new array with interval start point number
#' @description spliting interval i into 2 pieces
#' @examples
#' ni = c(1,5,10,15,20)
#' nr = 4
#' ni
#' ni = spl(ni,i=2)
#' ni
#' ni = spl(ni,i=5)
#' ni
#' ni = spl(ni,i=2)
#' ni
#' ni = spl(ni,i=1)
#' ni
#' ni = spl(ni,i=length(ni)-1)
#' ni
#' ni = spl(ni,i=length(ni)-1)
#' ni
#' ni = spl(ni,i=length(ni)-1)
#' ni


#' ## Interval too small
#' ni =  c(10, 20)
#' i = 2
#'
#' spl(ni, i)
#'
#' ## i too small
#' ni =  c(10, 20, 30)
#' i = 3
#'
#' spl(ni, i)


#spl <- function(ni, i, nr) {
#  if( i >=nr+1 ) {"i needs to be less that nr+1"}
#  else {
#  k1=ni[i]
#  k2=ni[i+1]
#
#  ## Feels a little silly to defining this explicitly
#  #jsplit_cond=floor((k1+k2)/2)
#
#  ## Using double condition in an attempt to be defensive
#  jsplit <- ifelse(jsplit_cond >= k2-1, k2-2,
#                   ifelse(jsplit_cond <= k1+1, k1+2,
#                          "Condition Not Satisfied")
#  )
#
##  ## I think the following achieves the same result:
##  jsplit <- max(min(floor((k1 + k2) / 2), k2 - 2), k1 + 2)
#
#  nn = ni
#  nn[i:nr] = ni[(nr+1):(i+1)]
#  nn[i+1] =jsplit
#  nn
#}
#}

spl <- function(ni, i) {
  if ( length(ni) > 2 ) {
    if ( i <= length(ni) - 1 ) {
      k1 = ni[i]
      k2 = ni[i + 1]
      jsplit = max(min(floor((k1 + k2) / 2), k2), k1)
      c(ni[1:i], jsplit, ni[(i + 1):length(ni)])
    } else {
      "i must be less length(ni)-1"
    }
  }
  else {
    "ni interval needs to be greater than 2"
  }
}
