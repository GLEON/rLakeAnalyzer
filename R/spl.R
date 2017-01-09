
#SUBROUTINE spl(i,Nr,Ni)
#INTEGER NR,NI(nr+2)
#
#' @title spliting interval i into 2 pieces
#' @param i [INTEGER] interval number, should be less than NR+1
#' @param nr- [INTEGER] current maximum of interval number
#' @param ni- [INTEGER(NR)] current array with interval start point number
#'
#' @return nn- [INTEGER(NR)] new array with interval start point number


spl <- function(ni, i, nr) {
  if( i >=nr+1 ) {"i needs to be less that nr+1"}
  else {
    k1=ni[i]
    k2=ni[i+1]

    ## Feels a little silly to defining this explicitly
    #jsplit_cond=floor((k1+k2)/2)

    ## Using double condition in an attempt to be defensive
    #jsplit <- ifelse(jsplit_cond >= k2-1, k2-2,
    #                 ifelse(jsplit_cond <= k1+1, k1+2,
    #                        "Condition Not Satisfied")
    #)

    ## I think the following achieves the same result:
    jsplit <- max(min(floor((k1 + k2) / 2), k2 - 2), k1 + 2)

    nn = ni
    nn[i:nr] = ni[(nr+1):(i+1)]
    nn[i+1] =jsplit
    nn
  }
}
