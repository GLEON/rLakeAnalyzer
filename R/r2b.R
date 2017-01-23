#' @export
#' @title computing a norm value for the segment from point k1 to k2-1
#' @param k1 [INTEGER] start point
#' @param k2 [INTEGER] end point+1
#' @param x [REAL(?)] input x-axis array (predictor)
#' @param y [REAL(?)] input y-axis array (response)
#' @return A -[REAL] coefficient of linear regression (will zero if K1=0) (y=Ax+B), B -[REAL] coefficient of linear regression, R2B -[REAL] norm of the segment (maximum "distance" measured perpendicular to the regression line)
#' @description  A -[REAL] coefficient of linear regression (will zero if K1=0) (y=Ax+B), B -[REAL] coefficient of linear regression, R2B -[REAL] norm of the segment (maximum "distance" measured perpendicular to the regression line)
#' @examples
#' ni <- c( 1, 201, 402 )
#' i <- 1
#' k1 <- ni[i]
#' k2 <- ni[i+1]
#'
#'
#' r2b(k1, k2, y=t11$temper, x=t11$depth)

# Doug could not find anywhere in the original code where outputs a and b are actually used
# he has taken then out to avoid having to return a data.frame.

## Sam has replaced r2b (again)

r2b = function(k1,k2,x,y) {
  if(k2-k1<=2) {
    r2b=0
    a = 0  # original code does not produce a or b?
    b = 0
  } else {
    is = k1:(k2-1)
    
    ## Taken from http://stackoverflow.com/a/40142643/5596534
    ## Keeping in other parameters for future considerations
    simplelm <- function (x, y) {
      ## number of data
      #n <- length(x)
      ## centring
      y0 <- .Internal(mean(y)); yc <- y - y0
      x0 <- .Internal(mean(x)); xc <- x - x0
      ## fitting an intercept-free model: yc ~ xc + 0
      xty <- drop(crossprod(xc, yc))
      xtx <- drop(crossprod(xc))
      slope <- xty / xtx
      #rc <- yc - xc * slope
      ## Pearson estimate of residual standard error
      #sigma2 <- drop(crossprod(rc)) / (n - 2)
      ## standard error for slope
      #slope_se <- sqrt(sigma2 / xtx)
      ## t-score and p-value for slope
      #tscore <- slope / slope_se
      #pvalue <- 2 * pt(abs(tscore), n - 2, lower.tail = FALSE)
      ## return estimation summary for slope
      intercept <- y0 - x0 * slope
      return(c(slope, intercept))
      #c("Estimate" = slope, "Std. Error" = slope_se, "t value" = tscore, "Pr(>|t|)" = pvalue)
    }
    
    
    lmResult <- simplelm(y=y[is],x=x[is])
    n = k2-k1
    
    
    if(k1 > 1) {
      a = lmResult[1]
      b = lmResult[2]
    } else {
      a = 0.0
      b = (sum(y[is]) - a * sum(x[is]))/n
    }
    
    
    r2b = max( abs( y[is] - a*x[is] - b )/sqrt(a^2 + 1) )
  }
  
  #return(data.frame(r2b=r2b,a=a,b=b))
  return(r2b)
}