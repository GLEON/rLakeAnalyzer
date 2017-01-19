
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
# @examples
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

#spl <- function(ni, i) {
#  if ( length(ni) > 2 ) {
#    if ( i <= length(ni) - 1 ) {
#      k1 = ni[i]
#      k2 = ni[i + 1]
#      jsplit = max(min(floor((k1 + k2) / 2), k2), k1)
#      c(ni[1:i], jsplit, ni[(i + 1):length(ni)])
#    } else {
#      "i must be less length(ni)-1"
#    }
#  }
#  else {
#    "ni interval needs to be greater than 2"
#  }
#}

spl <- function(ni,i) {
  stopifnot(i < length(ni)) # Otherwise ni[i+1] would fail.
  k1 = ni[i]
  k2 = ni[i+1]
  jsplit = floor((k1+k2)/2) # Is an index, must be an integer.

  # The following lines are taken from the original code but actually
  # are in error. They are included simply to make the tests produce the
  # identical answers in every case.
  if (jsplit>=k2-1) jsplit=k2-2
  if (jsplit<=k1+1) jsplit=k1+2

  stopifnot(k1<jsplit & jsplit<k2) # Verify that the interval has actually been split.
  # Create a new vector with jsplit at position i
  c( ni[1:i], jsplit, ni[(i+1):length(ni)] )
}

